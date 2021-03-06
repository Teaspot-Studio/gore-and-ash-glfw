{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.GLFW.State
Description : Internal core module state
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.GLFW.State(
    KeyChannel
  , ButtonChannel
  , MouseChannel
  , WindowSizeChannel
  , ScrollChannel
  , CloseChannel
  , GLFWState(..)
  ) where

import Control.DeepSeq
import Data.IORef
import Data.Hashable
import GHC.Generics (Generic)
import Graphics.UI.GLFW
import qualified Data.HashMap.Strict as M 

-- | Channel to connect core and callback with key states
type KeyChannel = IORef [(Key, KeyState, ModifierKeys)]
-- | Channel to connect core and callback with mouse button states
type ButtonChannel = IORef [(MouseButton, MouseButtonState, ModifierKeys)]
-- | Channel to connect core and callback with mouse position
type MouseChannel = IORef (Double, Double)
-- | Channel to connect core and callback with window resizing 
type WindowSizeChannel = IORef (Maybe (Double, Double))
-- | Channel to connect core and callback with mouse scrolling
type ScrollChannel = IORef [(Double, Double)]
-- | Channel to connect core and callback for window closing
type CloseChannel = IORef Bool 

-- | Module inner state
--
-- [@s@] - State of next module, the states are chained via nesting.
data GLFWState s = GLFWState {
  glfwNextState :: !s
, glfwKeys :: !(M.HashMap Key (KeyState, ModifierKeys))
, glfwKeyChannel :: !KeyChannel
, glfwMouseButtons :: !(M.HashMap MouseButton (MouseButtonState, ModifierKeys))
, glfwMouseButtonChannel :: !ButtonChannel
, glfwMousePos :: !(Double, Double)
, glfwMousePosChannel :: !MouseChannel
, glfwWindow :: !(Maybe Window)
, glfwPrevWindow :: !(Maybe Window)
, glfwWindowSize :: !(Maybe (Double, Double))
, glfwWindowSizeChannel :: !WindowSizeChannel
, glfwScroll :: ![(Double, Double)]
, glfwScrollChannel :: !ScrollChannel
, glfwClose :: !Bool
, glfwCloseChannel :: !CloseChannel 
, glfwBufferSize :: !Int
} deriving (Generic)

instance NFData s => NFData (GLFWState s) where 
  rnf GLFWState {..} = 
    glfwNextState `deepseq` 
    glfwKeys `deepseq` 
    glfwKeyChannel `seq` 
    glfwMouseButtons `deepseq` 
    glfwMouseButtonChannel `seq` 
    glfwMousePos `deepseq`
    glfwMousePosChannel `seq`
    glfwWindow `seq`
    glfwPrevWindow `seq` 
    glfwWindowSize `deepseq`
    glfwWindowSizeChannel `seq`
    glfwScroll `deepseq`
    glfwScrollChannel `seq` 
    glfwClose `seq` 
    glfwCloseChannel `seq`
    glfwBufferSize `seq` ()

instance Hashable Key 
instance Hashable MouseButton
instance NFData Key 
instance NFData KeyState 
instance NFData ModifierKeys
instance NFData MouseButton
instance NFData MouseButtonState