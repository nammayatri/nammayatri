module Screens.OnBoardingFlow.ChooseLanguageScreen.ComponentConfig where

import Components.MenuButton as MenuButton
import Components.PrimaryButton as PrimaryButton
import JBridge as JB 
import Prelude ((==))
import PrestoDOM (Margin(..))
import Screens.Types as ST

primaryButtonConfig :: ST.ChooseLanguageScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      {   textConfig
         {
          text = "Continue"
         } 
      , margin = (Margin 0 0 0 0)   
      , enableLoader = (JB.getBtnLoader "ChooseLanguageScreen")
      , id = "ChooseLanguageScreen"
      }
  in primaryButtonConfig'

menuButtonConfig :: ST.ChooseLanguageScreenState -> ST.Language -> MenuButton.Config
menuButtonConfig state language = let  
    config = MenuButton.config
    menuButtonConfig' = config {
      titleConfig{
          text = language.name
      }
      ,subTitleConfig
      {
        text = language.subTitle
      }
      , id = language.value
      , isSelected = (language.value == state.props.selectedLanguage)
    }
    in menuButtonConfig'