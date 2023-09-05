{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.WriteToUsScreen.ComponentConfig where

import Common.Types.App
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..), fromMaybe)
import Screens.WriteToUsScreen.ScreenData
import Screens.WriteToUsScreen.Controller
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings
import Language.Types (STR(..))
import Prelude
import PrestoDOM
import Styles.Colors as Color
import Screens.Types as ST

primaryEditTextConfig :: Listtype -> PrimaryEditText.Config
primaryEditTextConfig optionItem = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { singleLine = true
          , pattern = Just "[0-9]*,10"
          , text = if(optionItem.title == DescribeYourIssue) then "" else (optionItem.value)
          , placeholder = if(optionItem.title == DescribeYourIssue) then (getString YOU_CAN_DESCRIBE_ISSUE_THAT_YOU_FACED_HERE) else ""
        }
      , topLabel
        { text = (getTitle optionItem.title)
        , color = Color.greyTextColor
        }
      }
    in primaryEditTextConfig'

primaryButtonConfig :: ST.WriteToUsScreenState -> PrimaryButton.Config 
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig
      { text = if(state.props.isThankYouScreen) then (getString GO_TO_HOME) else (getString SUBMIT)
      , color = Color.primaryButtonColor
      }
      , margin = (Margin 0 0 0 0)
      , cornerRadius = 0.0
      , background = Color.black900
      , height = (V 60)
      , id = "WriteToUsScreenPrimaryButton"
      }
  in primaryButtonConfig'