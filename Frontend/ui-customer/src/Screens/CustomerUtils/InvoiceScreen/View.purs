{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.InvoiceScreen.View where

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.String as DS
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, map, not, show, ($), (<<<), (<>), (==), (&&), (/=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Accessiblity(..), PrestoDOM, Screen, afterRender, alignParentRight, background, color, cornerRadius, fontStyle, gravity, height, layoutGravity, lineHeight, linearLayout, margin, onBackPressed, orientation, padding, text, textSize, textView, weight, width, accessibilityHint, accessibility)
import Screens.CustomerUtils.InvoiceScreen.ComponentConfig (genericHeaderConfig, primaryButtonConfig)
import Screens.InvoiceScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Helpers.Utils (isHaveFare)
import MerchantConfig.Utils (getValueFromConfig, getMerchant, Merchant (..))

screen :: ST.InvoiceScreenState -> Screen Action ST.InvoiceScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "InvoiceScreen"
  , globalEvents: []
  , eval
  }

view ::
  forall w.
  (Action -> Effect Unit) -> ST.InvoiceScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , padding $ Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 then 24 else EHC.safeMarginBottom)
        , onBackPressed push (const BackPressed)
        , afterRender push (const AfterRender)
        ]
        [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , padding (Padding 16 20 16 0)
            , background Color.blue600
            , margin (MarginBottom 12)
            ]
            [ rideDateAndTimeView state
            , totalAmountView state
            , linearLayout
                [ height $ V 1
                , width MATCH_PARENT
                , background Color.grey900
                , margin (Margin 0 20 0 20)
                ]
                []
            , amountBreakupView state
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ]
            ( map
                ( \item ->
                    linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , padding (Padding 16 6 16 6)
                      ]
                      [ localTextView item Color.black650 ]
                )
                (referenceList state)
            )
        , linearLayout
            [ width MATCH_PARENT
            , weight 1.0
            ]
            []
        , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
        ]

referenceList :: ST.InvoiceScreenState -> Array String
referenceList state =
  (if (state.data.selectedItem.nightCharges && (getMerchant FunctionCall) /= YATRI) then [ "1.5" <> (getString DAYTIME_CHARGES_APPLICABLE_AT_NIGHT) ] else [])
    <> (if (isHaveFare "DRIVER_SELECTED_FARE" state.data.selectedItem.faresList) then [(getString DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO) ] else [])
    <> (if (isHaveFare "WAITING_CHARGES" state.data.selectedItem.faresList) then [ (getString WAITING_CHARGE_DESCRIPTION) ] else [])
    <> (if (isHaveFare "EARLY_END_RIDE_PENALTY" state.data.selectedItem.faresList) then [ (getString EARLY_END_RIDE_CHARGES_DESCRIPTION) ] else [])
    <> (if (isHaveFare "CUSTOMER_SELECTED_FARE" state.data.selectedItem.faresList) then [ (getString CUSTOMER_TIP_DESCRIPTION) ] else [])

---------------------- amountBreakupView -------------------
amountBreakupView :: forall w. ST.InvoiceScreenState -> PrestoDOM (Effect Unit) w
amountBreakupView state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    ( map
        ( \(item) ->
            linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation HORIZONTAL
              , padding (Padding 0 5 0 5)
              , margin (MarginBottom 16)
              ]
              [ textView $
                  [ text (getFareText item.fareType state.data.selectedItem.baseDistance)
                  , color Color.black800
                  , accessibilityHint $ (getFareText item.fareType state.data.selectedItem.baseDistance) <> " : " <> (DS.replaceAll (DS.Pattern "₹") (DS.Replacement "") item.price) <> " Rupees"
                  , accessibility ENABLE
                  , layoutGravity "bottom"
                  ] <> FontStyle.paragraphText LanguageStyle
              , linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , gravity RIGHT
                  , orientation HORIZONTAL
                  ]
                  [ textView $
                      [ text item.price
                      , alignParentRight "true,-1"
                      , color Color.black800
                      , accessibility DISABLE
                      , height WRAP_CONTENT
                      , width WRAP_CONTENT
                      , lineHeight "18"
                      ] <> FontStyle.body1 LanguageStyle
                  ]
              ]
        )
        state.data.selectedItem.faresList
    )

--------------------------- TotalAmountView --------------------
totalAmountView :: forall w. ST.InvoiceScreenState -> PrestoDOM (Effect Unit) w
totalAmountView state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin (MarginTop 8)
    ]
    [ textView $
        [ text $ getString TOTAL_PAID
        , accessibilityHint $ "Total Paid : "<> (DS.replaceAll (DS.Pattern "₹") (DS.Replacement "") state.data.totalAmount) <> " Rupees"
        , accessibility ENABLE
        , color Color.black800
        , lineHeight "28"
        ] <> FontStyle.h1 LanguageStyle
    , linearLayout
        [ height WRAP_CONTENT
        , orientation HORIZONTAL
        , weight 1.0
        ]
        []
    , textView $
        [ text state.data.totalAmount
        , color Color.black800
        , accessibility DISABLE
        ] <> FontStyle.h1 LanguageStyle
    ]

--------------------------- RideDateAndTimeView --------------------
rideDateAndTimeView :: forall w. ST.InvoiceScreenState -> PrestoDOM (Effect Unit) w
rideDateAndTimeView state =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    ]
    [ localTextView state.data.date Color.black800
    , linearLayout
        [ height $ V 4
        , width $ V 4
        , gravity CENTER
        , background Color.black600
        , margin (Margin 8 1 8 0)
        , cornerRadius 2.0
        ]
        []
    , localTextView state.data.selectedItem.rideStartTime Color.black800
    ]

localTextView :: forall w. String -> String -> PrestoDOM (Effect Unit) w
localTextView textValue colorValue =
  textView $
    [ text textValue
    , color colorValue
    , width MATCH_PARENT
    ] <> FontStyle.tags LanguageStyle

getFareText :: String -> String -> String
getFareText fareType baseDistance = case fareType of
                      "BASE_FARE" -> (getString BASE_FARES) <> if baseDistance == "0 m" then "" else " (" <> baseDistance <> ")"
                      "EXTRA_DISTANCE_FARE" -> getString NOMINAL_FARE
                      "DRIVER_SELECTED_FARE" -> getString DRIVER_ADDITIONS
                      "TOTAL_FARE" -> getString TOTAL_PAID
                      "DEAD_KILOMETER_FARE" -> getString PICKUP_CHARGE
                      "PICKUP_CHARGES" -> getString PICKUP_CHARGE
                      "WAITING_CHARGES" -> getString WAITING_CHARGE
                      "EARLY_END_RIDE_PENALTY" -> getString EARLY_END_RIDE_CHARGES
                      "CUSTOMER_SELECTED_FARE" -> getString CUSTOMER_SELECTED_FARE
                      "SERVICE_CHARGE" -> getString SERVICE_CHARGES
                      "FIXED_GOVERNMENT_RATE" -> getString GOVERNMENT_CHAGRES
                      "WAITING_OR_PICKUP_CHARGES"  -> getString MISC_WAITING_CHARGE
                      "PLATFORM_FEE" -> getString PLATFORM_FEE
                      "SGST" -> getString PLATFORM_GST
                      _ -> "BASE_FARE"