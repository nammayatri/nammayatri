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
          , fontStyle = FontStyle.medium LanguageStyle
          , textSize = FontSize.a_16
          , text = if(optionItem.title == DescribeYourIssue) then "" else (optionItem.value)
          , placeholder = if(optionItem.title == DescribeYourIssue) then (getString YOU_CAN_DESCRIBE_ISSUE_THAT_YOU_FACED_HERE) else ""
        }
      , topLabel
        { textSize = FontSize.a_14
        , text = (getTitle optionItem.title)
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
      , textSize = FontSize.a_18}
      , margin = (Margin 0 0 0 0)
      , cornerRadius = 0.0
      , background = Color.black900
      , height = (V 60)
      }
  in primaryButtonConfig'