{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.GLFW.Module
Description : Monad transformer of the module
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains declaration of monad transformer of the core module and
instance for 'GameModule' class.
-}
module Game.GoreAndAsh.GLFW.Module(
    GLFWT(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Extra
import Control.Monad.Fix 
import Control.Monad.IO.Class
import Control.Monad.State.Strict 
import Data.IORef
import Data.Proxy 
import Graphics.UI.GLFW
import qualified Data.HashMap.Strict as M 

import Game.GoreAndAsh
import Game.GoreAndAsh.GLFW.State

-- | Monad transformer that handles GLFW specific API
--
-- [@s@] - State of next core module in modules chain;
--
-- [@m@] - Next monad in modules monad stack;
--
-- [@a@] - Type of result value;
--
-- How to embed module:
-- 
-- @
-- type AppStack = ModuleStack [GLFWT, ... other modules ... ] IO
--
-- newtype AppMonad a = AppMonad (AppStack a)
--   deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadSDL)
-- @
--
-- The module is NOT pure within first phase (see 'ModuleStack' docs), therefore currently only 'IO' end monad can handler the module.
newtype GLFWT s m a = GLFWT { runGLFWT :: StateT (GLFWState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (GLFWState s), MonadFix, MonadThrow, MonadCatch, MonadMask)

instance GameModule m s => GameModule (GLFWT s m) (GLFWState s) where 
  type ModuleState (GLFWT s m) = GLFWState s
  
  runModule (GLFWT m) s_ = do
    liftIO $ pollEvents
    close <- readCloseEvent s_
    let s = s_ { glfwClose = close }

    ((a, s'@GLFWState{..}), nextState) <- runModule (runStateT m s) (glfwNextState s)
    bindWindow glfwPrevWindow glfwWindow glfwKeyChannel glfwMouseButtonChannel 
      glfwMousePosChannel glfwWindowSizeChannel glfwScrollChannel glfwCloseChannel
    keys <- readAllKeys s'
    buttons <- readAllButtons s'
    mpos <- readMousePos s'
    wsize <- readWindowSize s'
    scroll <- readMouseScroll s'
    return (a, s' { 
        glfwKeys = keys
      , glfwMouseButtons = buttons
      , glfwMousePos = mpos
      , glfwNextState = nextState 
      , glfwWindowSize = wsize
      , glfwScroll = scroll
      , glfwClose = False
      })
    where 
      readAllKeys GLFWState{..} = liftIO $ do
        keys <- readAllChan glfwBufferSize glfwKeyChannel
        return $ M.fromList $ (\(k, ks, mds) -> (k, (ks, mds))) <$> reverse keys

      readAllButtons GLFWState{..} = liftIO $ do 
        btns <- readAllChan glfwBufferSize glfwMouseButtonChannel
        return $ M.fromList $ (\(b, bs, mds) -> (b, (bs, mds))) <$> reverse btns 

      readMousePos GLFWState{..} = liftIO $
        readIORef glfwMousePosChannel

      readWindowSize GLFWState{..} = liftIO $ 
        readIORef glfwWindowSizeChannel 

      readMouseScroll GLFWState{..} = liftIO $ 
        readAllChan glfwBufferSize glfwScrollChannel

      readCloseEvent GLFWState{..} = liftIO $ 
        readIORef glfwCloseChannel 

  newModuleState = do
    s <- newModuleState 
    kc <- liftIO $ newIORef []
    mbc <- liftIO $ newIORef []
    mpc <- liftIO $ newIORef (0, 0)
    wsc <- liftIO $ newIORef Nothing
    sch <- liftIO $ newIORef []
    cch <- liftIO $ newIORef False
    return $ GLFWState {
        glfwNextState = s
      , glfwKeyChannel = kc
      , glfwKeys = M.empty
      , glfwMouseButtonChannel = mbc 
      , glfwMouseButtons = M.empty
      , glfwMousePos = (0, 0)
      , glfwMousePosChannel = mpc
      , glfwWindow = Nothing
      , glfwPrevWindow = Nothing
      , glfwWindowSize = Nothing
      , glfwWindowSizeChannel = wsc
      , glfwScroll = []
      , glfwScrollChannel = sch
      , glfwClose = False
      , glfwCloseChannel = cch
      , glfwBufferSize = 100
      }

  withModule _ = withModule (Proxy :: Proxy m)
  cleanupModule _ = return ()
  
instance MonadTrans (GLFWT s) where
  lift = GLFWT . lift 

instance MonadIO m => MonadIO (GLFWT s m) where 
  liftIO = GLFWT . liftIO 

-- | Updates handlers when current window changes
bindWindow :: MonadIO m => Maybe Window -> Maybe Window 
  -> KeyChannel -> ButtonChannel -> MouseChannel -> WindowSizeChannel 
  -> ScrollChannel -> CloseChannel -> m ()
bindWindow prev cur kch mbch mpch wsch sch cch = unless (prev == cur) $ liftIO $ do 
  whenJust prev $ \w -> do
    setKeyCallback w Nothing
    setMouseButtonCallback w Nothing
    setCursorPosCallback w Nothing
    setWindowSizeCallback w Nothing >> atomicWriteIORef wsch Nothing
    setScrollCallback w Nothing
    setWindowCloseCallback w Nothing
  whenJust cur $ \w -> do
    bindKeyListener kch w
    bindMouseButtonListener mbch w
    bindMousePosListener mpch w

    bindWindowSizeListener wsch w
    -- update window size
    (!sx, !sy) <- getWindowSize w 
    atomicWriteIORef wsch $! Just (fromIntegral sx, fromIntegral sy)

    bindScrollListener sch w 
    bindCloseCallback cch w

atomicAppendIORef :: IORef [a] -> a -> IO ()
atomicAppendIORef ref a = atomicModifyIORef ref $ \as -> (a : as, ()) 

-- | Bind callback that passes keyboard info to channel
bindKeyListener :: KeyChannel -> Window -> IO ()
bindKeyListener kch w = setKeyCallback w (Just f)
  where
    f :: Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
    f _ k _ ks mds = atomicAppendIORef kch (k, ks, mds)

-- | Bind callback that passes mouse button info to channel
bindMouseButtonListener :: ButtonChannel -> Window -> IO ()
bindMouseButtonListener mbch w = setMouseButtonCallback w (Just f)
  where 
    f :: Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
    f _ b bs mds = atomicAppendIORef mbch (b, bs, mds)

-- | Bind callback that passes mouse position info to channel
bindMousePosListener :: MouseChannel -> Window -> IO ()
bindMousePosListener mpch w = setCursorPosCallback w (Just f)
  where 
    f :: Window -> Double -> Double -> IO ()
    f w' x y = do
      (sx, sy) <- getWindowSize w'
      let x' = 2 * (x / fromIntegral sx - 0.5)
          y' = 2 * (0.5 - y / fromIntegral sy)
      atomicWriteIORef mpch $! x' `seq` y' `seq` (x', y')

-- | Bind callback that passes window size info to channel
bindWindowSizeListener :: WindowSizeChannel -> Window -> IO ()
bindWindowSizeListener wsch w = setWindowSizeCallback w (Just f)
  where
    f :: Window -> Int -> Int -> IO ()
    f _ sx sy = do 
      let sx' = fromIntegral sx 
          sy' = fromIntegral sy
      atomicWriteIORef wsch . Just $! sx' `seq` sy' `seq` (sx', sy')

-- | Bind callback that passes scoll info to channel
bindScrollListener :: ScrollChannel -> Window -> IO ()
bindScrollListener sch w = setScrollCallback w (Just f)
  where 
    f :: Window -> Double -> Double -> IO ()
    f _ !sx !sy = atomicAppendIORef sch $! (sx, sy)

-- | Bind callback that passes close event to channel
bindCloseCallback :: CloseChannel -> Window -> IO ()
bindCloseCallback cch w = setWindowCloseCallback w (Just f)
  where 
    f :: Window -> IO ()
    f _ = atomicWriteIORef cch True 

-- | Helper function to read all elements from channel
readAllChan :: Int -> IORef [a] -> IO [a]
readAllChan mi chan = do 
  xs <- readIORef chan 
  atomicWriteIORef chan []
  return $ take mi xs