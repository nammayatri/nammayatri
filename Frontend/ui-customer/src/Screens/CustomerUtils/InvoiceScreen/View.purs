{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.InvoiceScreen.View where

import Components.PrimaryButton as PrimaryButton
import Prelude (Unit, ($), (<>), show, (==), map, (<<<), const)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, afterRender, alignParentRight, background, color, cornerRadius, fontStyle,lineHeight, gravity, height, layoutGravity, linearLayout, margin, onBackPressed, orientation, padding, text, textSize, textView, width, weight, imageWithFallback)
import Screens.Types as ST
import Effect (Effect)
import Screens.InvoiceScreen.Controller (Action(..), ScreenOutput, eval)
import Font.Size as FontSize
import Font.Style as FontStyle
import Animation as Anim
import Language.Strings (getString)
import Language.Types (STR(..))
import Components.GenericHeader as GenericHeader
import Engineering.Helpers.Commons as EHC 
import Styles.Colors as Color
import Common.Types.App
import Screens.CustomerUtils.InvoiceScreen.ComponentConfig
import Constant.Test as Id

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
        , Id.testId $ Id.Screen Id.invoiceScreen
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
  [ "1.5" <> (getString DAYTIME_CHARGES_APPLICABLE_AT_NIGHT)
  , (getString DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO)
  , (getString WAITING_CHARGE_DESCRIPTION)
  ]

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
              [ textView
                  [ text case item of
                      ST.BASE_FARE -> ((getString BASE_FARES) <> " (" <> state.data.selectedItem.baseDistance <> ")")
                      ST.EXTRA_DISTANCE_FARE -> (getString NOMINAL_FARE)
                      ST.DRIVER_SELECTED_FARE -> (getString NOMINAL_FARE)
                      ST.TOTAL_FARE -> (getString TOTAL_PAID)
                      ST.PICKUP_CHARGES -> (getString PICKUP_CHARGE)
                      ST.WAITING_CHARGES -> (getString WAITING_CHARGE)
                  , textSize FontSize.a_14
                  , color Color.black800
                  , layoutGravity "bottom"
                  , fontStyle $ FontStyle.regular LanguageStyle
                  , lineHeight "18"
                  ]
              , linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , gravity RIGHT
                  , orientation HORIZONTAL
                  ]
                  [ textView
                      [ text case item of
                          ST.BASE_FARE -> state.data.selectedItem.fareBreakUpList.baseFare
                          ST.EXTRA_DISTANCE_FARE -> state.data.selectedItem.fareBreakUpList.nominalFare
                          ST.DRIVER_SELECTED_FARE -> state.data.selectedItem.fareBreakUpList.nominalFare
                          ST.TOTAL_FARE -> state.data.selectedItem.totalAmount
                          ST.PICKUP_CHARGES -> state.data.selectedItem.fareBreakUpList.pickupCharges
                          ST.WAITING_CHARGES -> state.data.selectedItem.fareBreakUpList.waitingCharges
                      , fontStyle $ FontStyle.medium LanguageStyle
                      , alignParentRight "true,-1"
                      , color Color.black800
                      , height WRAP_CONTENT
                      , width WRAP_CONTENT
                      , textSize FontSize.a_14
                      , lineHeight "18"
                      ]
                  ]
              ]
        )
        [ ST.BASE_FARE, ST.PICKUP_CHARGES, ST.DRIVER_SELECTED_FARE, ST.WAITING_CHARGES ]
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
    [ textView
        [ text $ getString TOTAL_PAID
        , textSize FontSize.a_22
        , color Color.black800
        , fontStyle $ FontStyle.bold LanguageStyle
        , lineHeight "28"
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , orientation HORIZONTAL
        , weight 1.0
        ]
        []
    , textView
        [ text state.data.totalAmount
        , textSize FontSize.a_22
        , fontStyle $ FontStyle.bold LanguageStyle
        , color Color.black800
        , lineHeight "28"
        ]
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
  textView
    [ text textValue
    , textSize FontSize.a_12
    , color colorValue
    , fontStyle $ FontStyle.medium LanguageStyle
    , width MATCH_PARENT
    , lineHeight "16"
    ]
