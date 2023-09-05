{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.AddImagesModel.Controller where

import Prelude

import Components.PrimaryButton.Controller as PrimaryButton
import Styles.Colors (black700, black900, primaryButtonColor, white900) as Color
import Font.Size (a_18) as FontSize
import PrestoDOM.Types.DomAttributes (Length(..))
import Language.Strings (getString)
import Language.Types (STR(..))

data Action = OnClickDone PrimaryButton.Action
            | OnClickCancel PrimaryButton.Action
            | OnClickDelete Int
            | OnClickView String String
            | AddImage
            | BackPressed
            | NoAction

type AddImagesModelState = {
  images :: Array {image :: String, imageName :: String},
  stateChanged :: Boolean,
  isLoading :: Boolean,
  imageMediaIds :: Array String
}

doneButtonConfig :: AddImagesModelState -> PrimaryButton.Config
doneButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = (getString DONE)
      , color = Color.primaryButtonColor
      }
      , cornerRadius = 8.0
      , background = Color.black900
      , height = (V 60)
      , alpha = if (state.stateChanged) then 1.0 else 0.5
      , isClickable = state.stateChanged
      , enableLoader = state.isLoading
      , id = "add_images_model_done_button"
      }
  in primaryButtonConfig'

cancelButtonConfig :: AddImagesModelState -> PrimaryButton.Config
cancelButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = (getString CANCEL)
      , color = Color.black700}
      , cornerRadius = 8.0
      , background = Color.white900
      , stroke = "1,"<>Color.black700
      , height = (V 60)
      , isClickable = true
      , id = "AddImagesModelCancelButton"
      }
  in primaryButtonConfig'