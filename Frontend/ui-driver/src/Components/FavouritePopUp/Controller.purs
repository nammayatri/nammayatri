{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.FavouritePopUp.Controller where

import Components.PrimaryButton.Controller as PrimaryButton
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), LetterSpacing(..), Accessiblity(..), accessibility)
data Action = OnClickDone PrimaryButton.Action

type Config = {
  title :: String,
  message :: String
}

config :: Config
config = {
  title : "",
  message : ""
}

doneButtonConfig :: Config -> PrimaryButton.Config
doneButtonConfig config = let
    primaryButtonConfig' =  PrimaryButton.config
      { textConfig
        { text = getString GOT_IT
        , color = Color.primaryButtonColor
        }
      , cornerRadius = 8.0
      , background = Color.black900
      , height = V 48
      , alpha = 1.0 
      , isClickable = true 
      , margin = MarginTop 15
      }
  in primaryButtonConfig'