{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.BookingOptionsScreen.ComponentConfig where

import Language.Strings
import PrestoDOM
import Font.Style as FontStyle
import Language.Types (STR(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import Prelude
import Mobility.Prelude
import Prelude
import Components.PopUpModal as PopUpModal
import Screens.Types as ST
import Helpers.Utils as HU
import Data.Function.Uncurried (runFn3)
import DecodeUtil (getAnyFromWindow)
import Data.Maybe (Maybe(..), fromMaybe)
import Components.RateCard as RateCard
import Common.Types.App as CTA
import Data.Array as DA

topAcDriverPopUpConfig :: ST.BookingOptionsScreenState -> PopUpModal.Config
topAcDriverPopUpConfig state = let 
  appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
  config' = PopUpModal.config
    { gravity = CENTER,
      margin = MarginHorizontal 24 24 ,
      buttonLayoutMargin = Margin 16 0 16 20 ,
      optionButtonOrientation = "VERTICAL",
      primaryText {
        text = getString $ TOP_AC_DRIVER appName,
        margin = Margin 18 24 18 24
    },
      secondaryText { visibility = GONE },
      option1 {
        text = getString WATCH_VIDEO,
        margin = MarginHorizontal 16 16,
        color = Color.yellow900,
        background = Color.black900,
        strokeColor = Color.white900,
        width = MATCH_PARENT
      },
        option2 {
        text = getString MAYBE_LATER,
        margin = Margin 16 7 16 0,
        color = Color.black650,
        background = Color.white900,
        strokeColor = Color.white900,
        width = MATCH_PARENT
      },
      backgroundClickable = true,
      dismissPopup = true,
      cornerRadius = Corners 15.0 true true true true,
      coverImageConfig {
        visibility = VISIBLE,
        height = V 215,
        width = V 320,
        margin = Margin 17 20 17 0,
        imageUrl = HU.fetchImage HU.FF_ASSET "ny_ac_explanation"
      }
    }
  in config'

rateCardConfig :: CTA.RateCard -> Boolean -> Boolean -> RateCard.Config
rateCardConfig rateCard showTollCharges showDriverAdditions = 
  let
    config' = RateCard.config
    rateCardConfig' =
      config'
        { isNightShift = rateCard.isNightShift
        , currentRateCardType = rateCard.currentRateCardType
        , onFirstPage = rateCard.onFirstPage
        , showDetails = true 
        , description = if rateCard.isNightShift then (getString $ NIGHT_TIME_CHARGES rateCard.nightChargeFrom rateCard.nightChargeTill) else (getString $ DAY_TIME_CHARGES rateCard.nightChargeTill rateCard.nightChargeFrom)
        , buttonText = Just if rateCard.currentRateCardType == CTA.DefaultRateCard then (getString GOT_IT) else (getString GO_BACK)
        , title = getString RATE_CARD
        , fareList = rateCard.extraFare 
        , driverAdditions = rateCard.driverAdditions
        , otherOptions  = otherOptions $ (not DA.null rateCard.driverAdditions) && showDriverAdditions
        , fareInfoDescription = rateCard.fareInfoDescription
        , additionalStrings = if showDriverAdditions then 
          [{key : "DRIVER_ADDITIONS_OPTIONAL", val : (getString DRIVER_ADDITIONS_OPTIONAL)}] else []
          <> [{key : "THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC", val : (getString THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC)},
          {key : "DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE", val : (getString DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE)}
          ] <> if showTollCharges then [
          {key : "TOLL_OR_PARKING_CHARGES", val : (getString TOLL_OR_PARKING_CHARGES)},
          {key : "TOLL_CHARGES", val : (getString TOLL_CHARGES)},
          {key : "TOLL_CHARGES_DESC", val : (getString TOLL_CHARGES_DESC)}] else []
          <> [ {key : "PARKING_CHARGE", val : (getString PARKING_CHARGE)},
               {key : "PARKING_CHARGES_DESC", val : (getString PARKING_CHARGES_DESC)}]
        }
  in
    rateCardConfig'
  where     
    otherOptions :: Boolean -> Array CTA.FareList
    otherOptions showAdditions = (if showAdditions then 
                                    [ {key : "DRIVER_ADDITIONS", val : (getString DRIVER_ADDITIONS)}] 
                                    else [])
                                  <> (if showTollCharges then 
                                        [{key : "TOLL_OR_PARKING_CHARGES", val : getString TOLL_OR_PARKING_CHARGES }]
                                      else [])
