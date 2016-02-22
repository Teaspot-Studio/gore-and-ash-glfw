{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-|
Module      : Game.GoreAndAsh.GLFW
Description : Module that contains GLFW integration for Gore&Ash
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The core module contains API for GLFW library integration. 
The module doesn't depends on others core modules and could be place in any place in 
game monad stack.

The module is NOT pure within first phase (see 'ModuleStack' docs), therefore currently only 'IO' end monad can handler the module.

Example of embedding:

@
-- | Application monad is monad stack build from given list of modules over base monad (IO)
type AppStack = ModuleStack [GLFWT, ... other modules ... ] IO
newtype AppState = AppState (ModuleState AppStack)
  deriving (Generic)

instance NFData AppState 

-- | Wrapper around type family
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadGLFW, ... other modules monads ... )
  
instance GameModule AppMonad AppState where 
  type ModuleState AppMonad = AppState
  runModule (AppMonad m) (AppState s) = do 
    (a, s') <- runModule m s 
    return (a, AppState s')
  newModuleState = AppState <$> newModuleState
  withModule _ = withModule (Proxy :: Proxy AppStack)
  cleanupModule (AppState s) = cleanupModule s 

-- | Arrow that is build over the monad stack
type AppWire a b = GameWire AppMonad a b
-- | Action that makes indexed app wire
type AppActor i a b = GameActor AppMonad i a b
@

-}
module Game.GoreAndAsh.GLFW(
  -- * Low-level API 
    GLFWState
  , GLFWT
  , MonadGLFW(..)
  -- * Arrow API
  -- ** Keyboard API
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
  -- ** Mouse buttons API
  , mouseButton
  , mouseButtonDyn
  , mouseButtonPressed
  , mouseButtonPressedDyn
  , mouseButtonReleased
  , mouseButtonReleasedDyn
  -- ** Cursor position
  , mousePosition
  , mousePositionChange
  , mouseXChange
  , mouseYChange
  , mouseDelta
  , mouseDeltaChange
  , mouseDeltaXChange
  , mouseDeltaYChange
  -- ** Mouse scroll
  , mouseScroll
  , mouseScrollX
  , mouseScrollY
  -- ** Window API
  , windowSize
  , windowClosing
  -- ** Reexports
  , Key(..)
  , KeyState(..)
  , MouseButton(..)
  , MouseButtonState(..)
  , ModifierKeys(..)
  ) where

import Control.Monad.Catch 
import Control.Wire 
import Game.GoreAndAsh
import Graphics.UI.GLFW

import Game.GoreAndAsh.GLFW.API 
import Game.GoreAndAsh.GLFW.Module
import Game.GoreAndAsh.GLFW.State 