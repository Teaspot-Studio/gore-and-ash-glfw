{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.GLFW.API
Description : Monadic and arrow API for GLFW core module
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains monadic and arrow API of the core module.
-}
module Game.GoreAndAsh.GLFW.API(
    MonadGLFW(..)
  -- * Keyboard API
  , keyStatus
  , keyStatusDyn
  , keyPressed
  , keyPressedDyn
  , keyReleased
  , keyReleasedDyn
  , keyRepeating
  , keyRepeatingDyn
  , keyPressing
  , keyPressingDyn
  -- * Mouse buttons API
  , mouseButton
  , mouseButtonDyn
  , mouseButtonPressed
  , mouseButtonPressedDyn
  , mouseButtonReleased
  , mouseButtonReleasedDyn
  -- * Cursor position
  , mousePosition
  , mousePositionChange
  , mouseXChange
  , mouseYChange
  , mouseDelta
  , mouseDeltaChange
  , mouseDeltaXChange
  , mouseDeltaYChange
  -- * Mouse scroll
  , mouseScroll
  , mouseScrollX
  , mouseScrollY
  -- * Window API
  , windowSize
  -- * Reexports
  , Key(..)
  , KeyState(..)
  , MouseButton(..)
  , MouseButtonState(..)
  , ModifierKeys(..)
  ) where

import Prelude hiding (id, (.))
import Control.Wire 

import Control.Monad.State.Strict 
import Control.Wire.Unsafe.Event
import Graphics.UI.GLFW
import qualified Data.HashMap.Strict as M 

import Game.GoreAndAsh
import Game.GoreAndAsh.GLFW.State
import Game.GoreAndAsh.GLFW.Module 

-- | Module low-level API
class Monad m => MonadGLFW m where 
  -- | Returns state of given keyboard's key
  keyStatusM :: Key -> m (Maybe (KeyState, ModifierKeys))
  -- | Returns state of given mouse button
  mouseButtonM :: MouseButton -> m (Maybe (MouseButtonState, ModifierKeys))
  -- | Returns current position of mouse cursor
  mousePosM :: m (Double, Double)
  -- | Returns current scroll values of mouse
  mouseScrollM :: m [(Double, Double)]
  -- | Returns current size of window
  windowSizeM :: m (Maybe (Double, Double))
  -- | Setups current window for input catch
  setCurrentWindowM :: Maybe Window -> m ()
  -- | Setup maximum size of inner buffers for keys, mouse buttons
  setBufferSizeM :: Int -> m ()

instance {-# OVERLAPPING #-} Monad m => MonadGLFW (GLFWT s m) where 
  keyStatusM k = do 
    GLFWState{..} <- GLFWT get
    return $ M.lookup k glfwKeys

  mouseButtonM b = do 
    GLFWState{..} <- GLFWT get 
    return $ M.lookup b glfwMouseButtons

  mousePosM = GLFWT $ glfwMousePos <$> get 
  mouseScrollM = GLFWT $ glfwScroll <$> get 
  windowSizeM = GLFWT $ glfwWindowSize <$> get 

  setCurrentWindowM w = GLFWT $ do 
    s <- get 
    put $ s { 
        glfwWindow = w 
      , glfwPrevWindow = glfwWindow s 
      }

  setBufferSizeM i = GLFWT $ do 
    s <- get 
    put $ s {
        glfwBufferSize = i 
      }

instance {-# OVERLAPPABLE #-} (Monad (mt m), MonadGLFW m, MonadTrans mt) => MonadGLFW (mt m) where 
  keyStatusM = lift . keyStatusM
  mouseButtonM = lift . mouseButtonM
  mousePosM = lift mousePosM
  mouseScrollM = lift mouseScrollM
  windowSizeM = lift windowSizeM
  setCurrentWindowM = lift . setCurrentWindowM
  setBufferSizeM = lift . setBufferSizeM
  
-- | Produces event when key state changes
keyStatus :: MonadGLFW m => Key -> GameWire m a (Event (KeyState, ModifierKeys))
keyStatus k = liftGameMonad (maybe2Event <$> keyStatusM k)

-- | Produces event when key state changes, get key as arrow argument
keyStatusDyn :: MonadGLFW m => GameWire m Key (Event (KeyState, ModifierKeys))
keyStatusDyn = liftGameMonad1 $ \k -> do 
  ms <- keyStatusM k 
  return $ maybe2Event ms 

-- | Simple transform from maybe to event
maybe2Event :: Maybe a -> Event a 
maybe2Event Nothing = NoEvent 
maybe2Event (Just a) = Event a

keyStated :: MonadGLFW m => KeyState -> Key -> GameWire m a (Event ModifierKeys)
keyStated ks k = mapE snd . filterE (\(ks', _) -> ks' == ks) . keyStatus k

keyStatedDyn :: MonadGLFW m => KeyState -> GameWire m Key (Event ModifierKeys)
keyStatedDyn ks = mapE snd . filterE (\(ks', _) -> ks' == ks) . keyStatusDyn

-- | Fires when keyboard key is pressed
keyPressed :: MonadGLFW m => Key -> GameWire m a (Event ModifierKeys)
keyPressed = keyStated KeyState'Pressed

-- | Version of keyPressed that takes key at runtime
keyPressedDyn :: MonadGLFW m => GameWire m Key (Event ModifierKeys)
keyPressedDyn = keyStatedDyn KeyState'Pressed

-- | Fires when keyboard key is released
keyReleased :: MonadGLFW m => Key -> GameWire m a (Event ModifierKeys)
keyReleased = keyStated KeyState'Released

-- | Version of keyReleased that takes key at runtime
keyReleasedDyn :: MonadGLFW m => GameWire m Key (Event ModifierKeys)
keyReleasedDyn = keyStatedDyn KeyState'Released

-- | Fires when keyboard key is entered into repeating mode
keyRepeating :: MonadGLFW m => Key -> GameWire m a (Event ModifierKeys)
keyRepeating = keyStated KeyState'Repeating

-- | Version of keyRepeating that takes key at runtime
keyRepeatingDyn :: MonadGLFW m => GameWire m Key (Event ModifierKeys)
keyRepeatingDyn = keyStatedDyn KeyState'Repeating

-- | Fires event from moment of press until release of given key
keyPressing :: MonadGLFW m => Key -> GameWire m a (Event ModifierKeys)
keyPressing k = go NoEvent 
  where
    go !e = mkGen $ \_ _ -> do 
      !mks <- keyStatusM k
      return $! case mks of 
        Nothing -> (Right e, go e)
        Just (!ks, !mds) -> case ks of 
          KeyState'Pressed -> (Right $! Event mds, go $! Event mds)
          KeyState'Released -> (Right NoEvent, go NoEvent)
          _ -> (Right e, go e)

-- | Version of keyPressing that takes key at runtime
keyPressingDyn :: MonadGLFW m => GameWire m Key (Event ModifierKeys)
keyPressingDyn = go NoEvent 
  where
    go !e = mkGen $ \_ k -> do 
      !mks <- keyStatusM k
      return $! case mks of 
        Nothing -> (Right e, go e)
        Just (!ks, !mds) -> case ks of 
          KeyState'Pressed -> (Right $! Event mds, go $! Event mds)
          KeyState'Released -> (Right NoEvent, go NoEvent)
          _ -> (Right e, go e)

-- | Produces event when mouse button state changes
mouseButton :: MonadGLFW m => MouseButton -> GameWire m a (Event (MouseButtonState, ModifierKeys))
mouseButton k = liftGameMonad (maybe2Event <$> mouseButtonM k)

-- | Produces event when key state changes, get key as arrow argument
mouseButtonDyn :: MonadGLFW m => GameWire m MouseButton (Event (MouseButtonState, ModifierKeys))
mouseButtonDyn = liftGameMonad1 $ \k -> do 
  ms <- mouseButtonM k 
  return $ maybe2Event ms 

mouseButtonStated :: MonadGLFW m => MouseButtonState -> MouseButton -> GameWire m a (Event ModifierKeys)
mouseButtonStated bs b = mapE snd . filterE (\(bs', _) -> bs == bs') . mouseButton b

mouseButtonStatedDyn :: MonadGLFW m => MouseButtonState -> GameWire m MouseButton (Event ModifierKeys)
mouseButtonStatedDyn bs = mapE snd . filterE (\(bs', _) -> bs == bs') . mouseButtonDyn

-- | Fires when mouse button is pressed
mouseButtonPressed :: MonadGLFW m => MouseButton -> GameWire m a (Event ModifierKeys)
mouseButtonPressed = mouseButtonStated MouseButtonState'Pressed 

-- | Version of mouseButtonPressed that takes button at runtime
mouseButtonPressedDyn :: MonadGLFW m => GameWire m MouseButton (Event ModifierKeys)
mouseButtonPressedDyn = mouseButtonStatedDyn MouseButtonState'Pressed

-- | Fires when mouse button is released
mouseButtonReleased :: MonadGLFW m => MouseButton -> GameWire m a (Event ModifierKeys)
mouseButtonReleased = mouseButtonStated MouseButtonState'Released 

-- | Version of mouseButtonReleased that takes button at runtime
mouseButtonReleasedDyn :: MonadGLFW m => GameWire m MouseButton (Event ModifierKeys)
mouseButtonReleasedDyn = mouseButtonStatedDyn MouseButtonState'Released

-- | Returns current position of mouse
mousePosition :: MonadGLFW m => GameWire m a (Double, Double)
mousePosition = liftGameMonad mousePosM

-- | Fires event when mouse position changes
mousePositionChange :: MonadGLFW m => GameWire m a (Event (Double, Double))
mousePositionChange = go 0 0
  where
    go !x !y = mkGen $ \_ _-> do 
      (!x', !y') <- mousePosM
      return $ if x /= x' || y /= y' 
        then (Right $! Event (x', y'), go x' y')
        else (Right NoEvent, go x y)

-- | Fires event when mouse X axis changes
mouseXChange :: MonadGLFW m => GameWire m a (Event Double)
mouseXChange = go 0 
  where
    go !x = mkGen $ \_ _-> do 
      (!x', _) <- mousePosM
      return $ if x /= x'
        then (Right $! Event x', go x')
        else (Right NoEvent, go x)

-- | Fires event when mouse Y axis changes
mouseYChange :: MonadGLFW m => GameWire m a (Event Double)
mouseYChange = go 0 
  where
    go !y = mkGen $ \_ _-> do 
      (_, !y') <- mousePosM
      return $ if y /= y'
        then (Right $! Event y', go y')
        else (Right NoEvent, go y)

-- | Returns mouse delta moves
mouseDelta :: MonadGLFW m => GameWire m a (Double, Double)
mouseDelta = go 0 0
  where 
    go !x !y = mkGen $ \_ _ -> do 
      (!x', !y') <- mousePosM
      let dx = x' - x 
          dy = y' - y
          res = Right (dx, dy)
      return $ dx `seq` dy `seq` (res, go x' y')

-- | Fires when mouse moves, holds delta move
mouseDeltaChange :: MonadGLFW m => GameWire m a (Event (Double, Double))
mouseDeltaChange = go 0 0
  where 
    go !x !y = mkGen $ \_ _ -> do 
      (!x', !y') <- mousePosM
      let dx = x' - x 
          dy = y' - y
          res = Right $! Event (dx, dy)
      return $ if x /= x' || y /= y' 
        then dx `seq` dy `seq` (res, go x' y')
        else (Right NoEvent, go x y)

-- | Fires when mouse X axis changes, holds delta move
mouseDeltaXChange :: MonadGLFW m => GameWire m a (Event Double)
mouseDeltaXChange = go 0 
  where 
    go !x = mkGen $ \_ _ -> do 
      (!x', _) <- mousePosM
      let dx = x' - x 
          res = Right $! Event dx
      return $ if x /= x' 
        then dx `seq` (res, go x')
        else (Right NoEvent, go x)

-- | Fires when mouse Y axis changes, holds delta move
mouseDeltaYChange :: MonadGLFW m => GameWire m a (Event Double)
mouseDeltaYChange = go 0 
  where 
    go !y = mkGen $ \_ _ -> do 
      (_, !y') <- mousePosM
      let dy = y' - y 
          res = Right $! Event dy
      return $ if y /= y'
        then dy `seq` (res, go y')
        else (Right NoEvent, go y)

-- | Fires when windows size is changed
windowSize :: MonadGLFW m => GameWire m a (Event (Double, Double))
windowSize = go 0 0 
  where
    go !x !y = mkGen $ \_ _ -> do 
      ms <- windowSizeM
      return $! case ms of 
        Nothing -> (Right NoEvent, go x y)
        Just (!x', !y') -> if x /= x' || y /= y' 
          then x' `seq` y' `seq` (Right $! Event (x', y'), go x' y')
          else (Right NoEvent, go x y)

-- | Fires when user scrolls
mouseScroll :: MonadGLFW m => GameWire m a (Event (Double, Double))
mouseScroll = mkGen_ $ \_ -> do 
  ss <- mouseScrollM
  return . Right $! case ss of 
    [] -> NoEvent
    ((!x', !y'):_) -> Event (x', y')

-- | Fires when user scrolls X axis
mouseScrollX :: MonadGLFW m => GameWire m a (Event Double)
mouseScrollX = mapE fst . mouseScroll

-- | Fires when user scrolls Y axis
mouseScrollY :: MonadGLFW m => GameWire m a (Event Double)
mouseScrollY = mapE snd . mouseScroll 