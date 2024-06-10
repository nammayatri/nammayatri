{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.CameraScreen.Controller where

import Data.Maybe
import Data.Function.Uncurried (runFn2)
import JBridge (stopCamera, cropImage)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude
import PrestoDOM (Eval, update, continue, exit, continueWithCmd)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (CameraScreenState)
import Debug


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data ScreenOutput = Exit

data Action = NoAction 
            | BackPressed 
            | PictureClick String String 
            | RetakeButtonPressed
            | ConfirmImage
            | CallBackImageUpload String String String

eval :: Action -> CameraScreenState -> Eval Action ScreenOutput CameraScreenState

eval BackPressed state = do
  void $ pure $ stopCamera "" 
  exit $ Exit

eval (PictureClick imageUri encodedImg)  state = do
    continue state {data {imageUri = imageUri, clickedImageUrl = encodedImg}, props{imageClicked = true}}

eval RetakeButtonPressed state = do
  continue state {data {imageUri = "", clickedImageUrl = ""}, props {imageClicked = false}}

eval ConfirmImage state = do
  let _ = runFn2 cropImage state.data.imageUri true
  void $ pure $ stopCamera "" 
  exit $ Exit

eval _ state = update state