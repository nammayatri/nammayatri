module Screens.OnBoardingSubscriptionScreen.ComponentConfig where

import Screens.Types (OnBoardingSubscriptionScreenState)
import Components.PrimaryButton as PrimaryButton
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..))
import JBridge as JB


joinPlanButtonConfig :: OnBoardingSubscriptionScreenState -> PrimaryButton.Config
joinPlanButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = getString PAY_TO_JOIN_THIS_PLAN }
      , height = (V 48)
      , cornerRadius = 8.0
      , id = "StartTrialPrimaryButton"
      , enableLoader = (JB.getBtnLoader "StartTrialPrimaryButton")
      , margin = (MarginBottom 16)
      }
  in primaryButtonConfig'