{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.AddAudioModel.Controller where

import Prelude
import Components.PrimaryButton.Controller as PrimaryButton
import Common.Styles.Colors (black900, primaryButtonColor) as Color
import PrestoDOM.Types.DomAttributes (Length(..), Margin(..))
import Data.Maybe (Maybe(..))

data Action = OnClickDone PrimaryButton.Action
            | OnClickCross
            | OnClickDelete
            | AddAudio
            | BackPressed
            | NoAction

type AddAudioModelState = {
  audioFile :: Maybe String,
  stateChanged :: Boolean,
  addedVoiceNoteText :: String,
  noVoiceNoteAddedText :: String,
  addVoiceNoteText :: String,
  doneButtonText :: String,
  cancelButtonText :: String,
  deleteText:: String
}

config :: AddAudioModelState 
config = {
  audioFile : Nothing,
  stateChanged : false,
  addedVoiceNoteText : "",
  noVoiceNoteAddedText : "NO_VOICE_NOTE",
  addVoiceNoteText : "ADD_VOICE_NOTE",
  deleteText: "DELETE",
  doneButtonText : "DONE",
  cancelButtonText: "CANCEL"
}

donePrimaryButtonConfig :: AddAudioModelState -> PrimaryButton.Config
donePrimaryButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig { 
          text = state.doneButtonText
        , color = Color.primaryButtonColor
        }
      , cornerRadius = 8.0
      , background = Color.black900
      , height = V 48
      , alpha = if state.stateChanged then 1.0 else 0.5
      , isClickable = state.stateChanged
      , margin = Margin 0 0 0 0
      , id = "add_audio_model_done_button"
      }
  in primaryButtonConfig'
  
