module Screens.AddVehicleDetailsScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import Common.Types.App
import Data.Maybe
import Data.String
import Font.Size as FontSize
import Language.Strings
import Language.Types (STR(..))
import Prelude
import PrestoDOM
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonConfig :: ST.AddVehicleDetailsScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString NEXT), textSize = FontSize.a_16}
      , width = MATCH_PARENT
      , cornerRadius = 0.0
      , height = (V 60)
      , background = Color.black900 
      , alpha = if ((state.props.isValidState) && (toLower(state.data.vehicle_registration_number) == toLower(state.data.reEnterVehicleRegistrationNumber)) && (state.data.dateOfRegistration /= Just "") )  then 1.0 else 0.8
      , isClickable = ((state.props.isValidState) && (toLower(state.data.vehicle_registration_number) == toLower(state.data.reEnterVehicleRegistrationNumber)) && (state.data.dateOfRegistration /= Just "") )
      , margin = (Margin 0 0 0 0)
      }
  in primaryButtonConfig'