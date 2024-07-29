{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TripDetailsScreenV2.View where

import Common.Types.App
import Screens.TripDetailsScreenV2.ComponentConfig
import Animation as Anim
import Common.Types.App (LazyCheck(..), Price(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils (intPriceToBeDisplayed, distanceTobeDisplayed, priceToBeDisplayed)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getVehicleVariantImage, getCityConfig)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude 
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alignParentBottom, background, color, cornerRadius, editText, fontStyle, frameLayout, gravity, height, hint, horizontalScrollView, imageUrl, imageView, imageWithFallback, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, scrollBarX, scrollView, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Screens.TripDetailsScreenV2.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Styles.Colors as Colors
import Storage(getValueToLocalStore , KeyStore(..))
import ConfigProvider
import Data.Function.Uncurried (runFn2)
import Mobility.Prelude as MP
import JBridge as JB
import Data.Number as NUM
import Data.Int as INT
import Helpers.Utils as HU
import Resource.Constants as RC
import Constants.Configs (dummyDistance, dummyPrice)
import Debug

screen :: ST.TripDetailsScreenState -> Screen Action ST.TripDetailsScreenState ScreenOutput 
screen initialState = 
  { initialState
  , view
  , name : "TripDetailsScreenV2"
  , globalEvents : []
  , eval:
      ( \action state -> do
          let
            _ = spy "TripDetailsScreenV2 action" action
          let
            _ = spy "TripDetailsScreenV2 state" state
          eval action state
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w 
view push state =
  Anim.screenAnimation $
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , onBackPressed push (const BackPressed)
  , afterRender push (const AfterRender)
  , padding $ PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom
  ][linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][ headerView push state  
        , linearLayout
          [ height $ V 1
          , width MATCH_PARENT
          , background Color.grey900
          ][]
        , linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , weight 1.0
          ]
          [ scrollView
          [ height MATCH_PARENT
            , width MATCH_PARENT
          ][ 
            linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              ][ 
                linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  ][ linearLayout
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , stroke $ "1," <> Color.grey900
                        , margin $ Margin 12 16 12 12
                        , cornerRadius 6.0
                        ][ linearLayout
                            [ height WRAP_CONTENT
                            , width MATCH_PARENT
                            , orientation VERTICAL 
                            , padding (Padding 16 16 16 16)
                            ][ tripDetailsView state
                            , tagView state $ tagList state
                            , separatorView 
                            , SourceToDestination.view (push <<< SourceToDestinationActionController) (sourceToDestinationConfig state)
                            , separatorView 
                            , tripDataView push state
                            , separatorView
                            , youRatedView push state
                            ]
                        ]
                    , textView
                       ([ width WRAP_CONTENT
                        , height MATCH_PARENT
                        , text $ getString EARNINGS
                        , gravity LEFT
                        , color Color.black900
                        , margin $ MarginLeft 20
                        ] <> FontStyle.subHeading1 TypoGraphy)
                    , earningsDetailsView push state
                    , linearLayout 
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , orientation HORIZONTAL
                        , background "#F6F1FF"
                        , gravity CENTER
                        , padding $ Padding 12 12 12 12
                        , margin $ Margin 12 12 12 20
                        , cornerRadius 6.0
                        ][ imageView 
                            [ height $ V 32 
                            , width $ V 32
                            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_three_stars_purple"
                            ]
                         , textView $
                            [ height WRAP_CONTENT
                            , weight 1.0
                            , text "Bridge doesn't charge you any commission! We believe you deserve a fair price!"
                            , margin $ MarginLeft 12
                            ] <> FontStyle.body1 TypoGraphy
                        ]
                   ]
              ]
            ]
          ]
        ]
    ]


tagView :: forall w. ST.TripDetailsScreenState -> (Array ST.Tag) -> PrestoDOM (Effect Unit) w 
tagView state config =
  horizontalScrollView 
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginTop 15
  , visibility if anyTag then VISIBLE else GONE
  , scrollBarX false
  ][linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    ](map (\item ->
      linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , cornerRadius 13.0
      , background item.background
      , visibility if item.visibility then VISIBLE else GONE
      , padding $ Padding 12 5 12 5
      , margin $ MarginRight 5
      ][  imageView
          [ imageWithFallback item.image
          , height $ V 16
          , width $ V 16
          ]
        , textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text item.text
          , color item.textColor
          , margin $ MarginLeft 5
          ] <> FontStyle.tags TypoGraphy
      ]) config)
  ]
  where 
  anyTag = isJust state.data.customerExtraFee || state.data.purpleTagVisibility || state.data.gotoTagVisibility || state.data.spLocTagVisibility || state.data.specialZonePickup


tagList :: ST.TripDetailsScreenState -> Array ST.Tag
tagList state = [
  {background : Colors.yellow200, image : fetchImage FF_ASSET "ny_ic_tip_icon", visibility : isJust state.data.customerExtraFee, text : "₹" <> (show (fromMaybe 0 state.data.customerExtraFee)) <> " Tip" , textColor : Color.black900},
  {background : Colors.black200, image : fetchImage FF_ASSET "ny_ic_loc_black", visibility : state.data.gotoTagVisibility, text : getString GO_TO, textColor : Color.black900},
  {background : Colors.purple100, image : fetchImage FF_ASSET "ny_ic_disability_purple", visibility : state.data.purpleTagVisibility, text : getString PURPLE_RIDE, textColor : Color.purple},
  {background : Colors.blue100, image : fetchImage FF_ASSET "ny_ic_star", visibility : state.data.spLocTagVisibility, text : state.data.specialZoneText, textColor : Color.blue800},
  {background : Colors.green100, image : fetchImage COMMON_ASSET "ny_ic_sp_zone_green", visibility : state.data.specialZonePickup, text : getString ZONE_PICKUP, textColor : Color.green900}
]

---------------------- tripDetails ---------------------------

tripDetailsView ::  forall w . ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
tripDetailsView state =
  linearLayout
  [ height $ V 40
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  ][  imageView
      [ imageWithFallback $ getVehicleImage state
      , width $ V 36
      , height $ V 36
      ]
    , linearLayout 
      [ height MATCH_PARENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER_VERTICAL
      , margin (MarginLeft 10)
      ][ linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER_VERTICAL
          ][  textView $
              [ text state.data.date
              , color Color.black800
              ] <> FontStyle.body1 TypoGraphy
            , textView
              [ background Color.greyishBlue
              , cornerRadius 2.5
              , margin (Margin 5 0 5 0)
              , height (V 5)
              , width (V 5)
              ] 
            , textView $
              [ text state.data.time
              , color Color.black800
              ] <> FontStyle.body1 TypoGraphy
            ]
          , textView $
            [ text state.data.vehicleModel
            , color Color.black700
            ] <> FontStyle.body3 TypoGraphy
        ]
    , textView $
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity RIGHT
        , text $ intPriceToBeDisplayed state.data.totalAmountWithCurrency true
        , color Color.black
        ] <> FontStyle.body14 TypoGraphy
    ]

------------------- separator -------------------
separatorView ::  forall w . PrestoDOM (Effect Unit) w
separatorView =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , margin $ MarginTop 16
  , background Color.grey900
  ][]

type TripDetailsRow = {
  keyLeft :: String,
  valLeft :: String,
  keyRight :: String,
  valRight :: String,
  leftClick :: Action,
  rightClick :: Action,
  leftAsset :: String,
  rightAsset :: String,
  leftVisibility :: Boolean,
  rightVisibility :: Boolean,
  leftItemLeftAsset :: Maybe String
}

tripDataView ::  forall w . (Action -> Effect Unit) ->  ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
tripDataView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER_VERTICAL
  ][  tripDetailsRow push {  keyLeft : getString RIDE_TYPE, valLeft : rideType, keyRight : (getString TRIP_ID), valRight : state.data.tripId, leftClick : NoAction, rightClick : Copy, leftAsset : "", rightAsset : "ny_ic_copy", leftVisibility : true, rightVisibility : true, leftItemLeftAsset : if state.data.acRide == Just true then Just "ny_ic_ac" else Nothing},
      tripDetailsRow push {  keyLeft : getString RIDE_DISTANCE, valLeft : distanceTobeDisplayed state.data.distanceWithUnit true true , keyRight : "Pickup Distance", valRight : distanceTobeDisplayed dummyDistance true true, leftClick : NoAction, rightClick : NoAction, leftAsset : "", rightAsset : "", leftVisibility : true, rightVisibility : true, leftItemLeftAsset : Nothing},
      tripDetailsRow push {  keyLeft : getString RIDE_TIME, valLeft : tripTime, keyRight : (getString EARNINGS_PER_KM), valRight : earningPerKm, leftClick : NoAction, rightClick : NoAction, leftAsset : "", rightAsset : "", leftVisibility : true, rightVisibility : false, leftItemLeftAsset : Nothing}
  ]
  where 
    tripTime = case state.data.tripStartTime, state.data.tripEndTime of
                Just startTime, Just endTime -> (show $ runFn2 JB.differenceBetweenTwoUTCInMinutes endTime startTime) <> " Min"
                _, _ -> "NA"
    currency = getCurrency appConfig
    acText = if state.data.acRide == Just true then "AC ∙ " else ""
    rideType = acText <> RC.serviceTierMapping state.data.rideType state.data.acRide
    earningPerKm =
      let mbDist = NUM.fromString state.data.distance
      in case mbDist of
          Just dist | dist > 1.0 -> currency <> HU.parseFloat ((INT.toNumber state.data.totalAmount - state.data.tollCharge) / dist) 2 <> "/km"
          _ -> "NA"

tripDetailsRow :: forall w . (Action -> Effect Unit) -> TripDetailsRow -> PrestoDOM (Effect Unit) w
tripDetailsRow push tripDetailsRowItem =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ MarginTop 20
    ][ linearLayout
        [ height WRAP_CONTENT
        , width $ V $ (EHC.screenWidth unit) / 2
        , orientation VERTICAL
        , visibility $ MP.boolToVisibility tripDetailsRowItem.leftVisibility
        ][ textView $
            [ text tripDetailsRowItem.keyLeft
            , color Color.black700
            , margin $ MarginBottom 4
            ] <> FontStyle.body1 TypoGraphy
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation HORIZONTAL
            , onClick push $ const tripDetailsRowItem.leftClick
            , gravity CENTER_VERTICAL
            ][  imageView $
                [ height $ V 15
                , width $ V 13
                , margin $ MarginRight 5
                ] <> case tripDetailsRowItem.leftItemLeftAsset of
                        Just asset -> [imageWithFallback $ fetchImage FF_ASSET asset]
                        Nothing -> [visibility GONE]
              , textView $
                [ text tripDetailsRowItem.valLeft
                , width WRAP_CONTENT
                , color Color.black900
                ] <> FontStyle.body1 TypoGraphy
              , imageView
                [ imageWithFallback $ fetchImage FF_COMMON_ASSET tripDetailsRowItem.leftAsset
                , height $ V 15
                , width $ V 13
                , margin $ Margin 10 5 0 0
                ]
            ]
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width $ V $ (EHC.screenWidth unit) / 2
        , orientation VERTICAL
        , visibility $ MP.boolToVisibility tripDetailsRowItem.rightVisibility
        ][ textView $
            [ text tripDetailsRowItem.keyRight
            , color Color.black700
            , margin $ MarginBottom 4
            ] <> FontStyle.body1 TypoGraphy
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation HORIZONTAL
            , onClick push $ const tripDetailsRowItem.rightClick
            ][ textView $
                [ text tripDetailsRowItem.valRight
                , width WRAP_CONTENT
                , color Color.black900
                ] <> FontStyle.body1 TypoGraphy
              , imageView
                [ imageWithFallback $ fetchImage FF_COMMON_ASSET tripDetailsRowItem.rightAsset
                , height $ V 15
                , width $ V 13
                , margin $ Margin 10 5 0 0
                ]
            ]
        ]
    ]

headerView :: forall w . (Action -> Effect Unit) ->  ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
headerView push state = 
    linearLayout
        [ width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , padding $ PaddingRight 16
        , background Color.white900
        ]
        [ linearLayout
            [ weight 1.0
            ]
            [ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
            ]
        , textView
            ([ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ getString HELP
            , color Color.purple
            , onClick push (const HelpAndSupport)
            ] <> FontStyle.subHeading1 TypoGraphy)
        ]

youRatedView :: forall w . (Action -> Effect Unit) ->  ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
youRatedView push state = 
    linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity LEFT
    , orientation HORIZONTAL
    , margin $ MarginTop 16
    ][ textView
       ([ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text "You Rated:" 
        ] <> FontStyle.body1 TypoGraphy)
      , imageView 
        [ height $ V 18
        , width $ V 18
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_thumbs_up_black" 
        , gravity CENTER 
        , margin $ MarginLeft 20
        ]
    ]


earningsDetailsView :: forall w . (Action -> Effect Unit) ->  ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
earningsDetailsView push state = 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , stroke $ "1," <> Color.grey900
    , margin $ Margin 12 16 12 12
    , cornerRadius 6.0
    , orientation VERTICAL
    ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL 
        , padding $ Padding 12 12 12 0
        ](map (\item ->
            linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin $ MarginRight 5
            ][ linearLayout
                [ height WRAP_CONTENT
                , weight 1.0
                ][ textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text item.key
                    ] <> FontStyle.body1 TypoGraphy
                ]
             , linearLayout
                [ height WRAP_CONTENT
                , weight 1.0
                , gravity RIGHT
                ][ textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ priceToBeDisplayed item.value true
                    ] <> FontStyle.body1 TypoGraphy
                ]
            ]) getPriceBreakupList)
    , separatorView 
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ Padding 12 12 12 12
        , orientation HORIZONTAL
        ][ linearLayout
            [ height WRAP_CONTENT
            , weight 1.0
            ][ textView $
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text "Your Earnings"
                , color Color.black900
                ] <> FontStyle.body1 TypoGraphy
            ]
            , linearLayout
            [ height WRAP_CONTENT
            , weight 1.0
            , gravity RIGHT
            ][ textView $
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text $ priceToBeDisplayed (HU.dummyPriceForCity $ getValueToLocalStore DRIVER_LOCATION) true
                , color Color.black900
                ] <> FontStyle.body1 TypoGraphy
            ]
        ]
    ]

getVehicleImage :: ST.TripDetailsScreenState -> String
getVehicleImage state = 
  case getMerchant FunctionCall of
    YATRI      -> getVehicleVariantImage state.data.vehicleServiceTier
    YATRISATHI -> getVehicleVariantImage state.data.vehicleServiceTier
    NAMMAYATRI -> getVehicleVariantImage state.data.vehicleServiceTier
    BRIDGE     -> getVehicleVariantImage state.data.vehicleServiceTier
    _           -> mkAsset $ getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
  
  where
    mkAsset cityConfig =
      if cityConfig.cityCode == "std:040" 
        then fetchImage FF_ASSET "ny_ic_black_yellow_auto1"
        else fetchImage FF_ASSET "ic_vehicle_front"



getPriceBreakupList :: Array {key :: String, value :: Price}
getPriceBreakupList = 
    [ {key: "Passenger Payment", value: HU.dummyPriceForCity $ getValueToLocalStore DRIVER_LOCATION}
    , {key: "Passenger Payment", value: HU.dummyPriceForCity $ getValueToLocalStore DRIVER_LOCATION}
    , {key: "Passenger Payment", value: HU.dummyPriceForCity $ getValueToLocalStore DRIVER_LOCATION}
    ]