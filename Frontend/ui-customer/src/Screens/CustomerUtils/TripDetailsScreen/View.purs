{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TripDetailsScreen.View where

import Common.Types.App
import Screens.CustomerUtils.TripDetailsScreen.ComponentConfig

import Animation as Anim
import Animation as Anim
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.ServiceTierCard.View as ServiceTierCard
import Components.SourceToDestination as SourceToDestination
import Data.Array as DA
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getVehicleVariantImage, getVariantRideType, getCityConfig)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Mobility.Prelude (boolToVisibility, capitalize)
import Prelude ((<>), show)
import Prelude (Unit, const, map, unit, ($), (&&), (/=), (<<<), (<=), (<>), (==), (/), not, (-), (||), (>))
import PrestoDOM (Accessiblity(..), FlexWrap(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), accessibility, accessibilityHint, adjustViewWithKeyboard, afterRender, alignParentBottom, background, color, cornerRadius, disableClickFeedback, editText, fontStyle, frameLayout, gravity, height, hint, hintColor, imageUrl,textFromHtml, imageView, imageWithFallback, layoutGravity, linearLayout, margin, multiLineEditText, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, scrollView, stroke, text, textSize, textView, visibility, weight, width, onAnimationEnd, alpha)
import PrestoDOM.Animation as PrestoAnim
import Screens.TripDetailsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (PaymentMode(..), VehicleViewType(..), TripDetailsGoBackType(..), FareProductType(..), VehicleVariant(..))
import Screens.Types as ST
import Styles.Colors as Color
import Storage (getValueToLocalStore, KeyStore(..))
import Components.CommonComponentConfig as CommonComponentConfig
import Screens.RideSelectionScreen.Transformer as RSST
import Resources.LocalizableV2.Strings (getStringV2)
import Resources.LocalizableV2.Types

screen :: ST.TripDetailsScreenState -> Screen Action ST.TripDetailsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "TripDetailsScreen"
  , globalEvents : []
  , eval : \state  action -> do
      let _ = spy  "TripDetailsScreen action " state
      let _ = spy  "TripDetailsScreen state " action
      eval state action
  }

view :: forall w. (Action -> Effect Unit) -> ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  let 
    filteredTopics = DA.filter (\topic -> 
        topic.categoryType == "Category" &&
        RSST.findIfRideIsValid (Just topic) state.data.selectedItem.rideStatus state.data.selectedItem.rideCreatedAt state.data.selectedItem.status
      ) (topicsList state)
  in
  Anim.screenAnimation $
 relativeLayout
 [  height MATCH_PARENT
  , width MATCH_PARENT
 ]$[linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
    , onBackPressed push $ const BackPressed
    , afterRender push $ const AfterRender
    ][ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
    , scrollView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ PaddingVertical 16 16
        , gravity CENTER_VERTICAL
        ][tripDetailsLayout state push (DA.length filteredTopics > 0)
        , reportIssueView state push filteredTopics
        ]
      ]
    ] 
 ]<> (if state.props.isContactSupportPopUp then [PopUpModal.view (push <<< ContactSupportPopUpAction) (CommonComponentConfig.contactSupportPopUpConfig state.data.config)] else [])


providerDetails :: forall w. ST.TripDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
providerDetails state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginVertical 16 16
  , background Color.grey900
  , gravity CENTER
  , cornerRadius 14.0
  , padding $ Padding 5 5 5 5
  , visibility $ boolToVisibility $ state.data.selectedItem.providerType == OFFUS
  ][  imageView
        [ height $ V 20
        , width $ V 20
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info"
        , padding $ Padding 2 2 2 2
        , margin $ MarginHorizontal 5 5
        ]
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString $ RIDE_FULFILLED_BY state.data.selectedItem.providerName
        ] <> FontStyle.tags LanguageStyle
  ]

tripDetailsLayout :: forall w. ST.TripDetailsScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
tripDetailsLayout state push showDivider =
  linearLayout
  [height WRAP_CONTENT
  , width MATCH_PARENT
  ][  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , background Color.blue600
          , padding $ Padding 16 16 16 10
          , margin $ MarginBottom 24
          ][ tripDetailsView state
           , separatorView
           , providerDetails state push
           , tripIdView push state
           , distanceAndTimeTaken state
           , SourceToDestination.view (push <<< SourceToDestinationActionController) (sourceToDestinationConfig state)
           , ratingAndInvoiceView state push
           ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , visibility $ boolToVisibility $ not $ state.data.selectedItem.status == "CANCELLED" 
          , padding $ PaddingHorizontal 16 16 
          , orientation VERTICAL
          ][ invoiceView state push
            , linearLayout
             [ height $ V 1
             , width MATCH_PARENT
             , background Color.lightGreyShade
             , margin $ MarginVertical 16 16
             , visibility $ boolToVisibility showDivider
             ][]
           ]
        ]]

---------------------- tripIdView ---------------------------
tripIdView :: forall w . (Action -> Effect Unit) -> ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
tripIdView push state =
  let serviceTierName = fromMaybe "" state.data.selectedItem.serviceTierName
      cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore CUSTOMER_LOCATION)
      rideType = if cityConfig.enableAcViews 
                  then ServiceTierCard.parseName serviceTierName
                  else serviceTierName
      hasAirConditioned = ServiceTierCard.showACDetails rideType Nothing (if state.data.vehicleVariant == Just DELIVERY_BIKE then DELIVERY else ONE_WAY)
      rideTypeWithAc = if hasAirConditioned && rideType /= "" && cityConfig.enableAcViews then "AC • " <> rideType else rideType
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginBottom 16
  , orientation HORIZONTAL
  ][  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width $ V (EHC.screenWidth unit/ 2)
      , visibility $ boolToVisibility $ isJust state.data.selectedItem.serviceTierName && rideType /= ""
      ][  textView $
          [ text $ getString RIDE_TYPE
          , accessibilityHint $ "Ride Type :" <> rideType
          , accessibility ENABLE
          , color Color.black700
          ] <> FontStyle.body1 LanguageStyle
        , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER_VERTICAL
          ][ imageView
              [ imageWithFallback $ fetchImage FF_ASSET if state.data.vehicleVariant == Just ST.AMBULANCE_VENTILATOR then "ny_ic_ventilator_blue" else "ny_ic_ac"
              , height $ V 18
              , width $ V 18
              , margin $ Margin 0 1 3 0
              , visibility $ boolToVisibility $ hasAirConditioned && cityConfig.enableAcViews
              ]
            , textView $
              [ textFromHtml if state.data.vehicleVariant == Just ST.AMBULANCE_VENTILATOR then getStringV2 ventilator else rideTypeWithAc
              , width WRAP_CONTENT
              , color Color.black900
              , accessibility DISABLE_DESCENDANT
              ] <> FontStyle.paragraphText LanguageStyle
          ]
      ]
    , linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width $ V (EHC.screenWidth unit/ 2)
      , visibility if state.data.tripId == "" then GONE else VISIBLE
      ][  textView $
          [ text $ getString RIDE_ID
          , accessibilityHint $ "Ride I-D :" <> ( DS.replaceAll (DS.Pattern "") (DS.Replacement " ") state.data.tripId)
          , accessibility ENABLE
          , color Color.black700
          ] <> FontStyle.body1 LanguageStyle
        , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , onClick push $ const Copy
          , accessibility DISABLE_DESCENDANT
          , gravity CENTER_VERTICAL
          ][ textView $
              [ text state.data.tripId
              , width WRAP_CONTENT
              , color Color.black900
              ] <> FontStyle.paragraphText LanguageStyle
            , imageView
              [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_copy"
              , height $ V 13
              , width $ V 11
              , margin $ MarginLeft 10
              ]
          ]
      ]
   ]

distanceAndTimeTaken :: forall w . ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
distanceAndTimeTaken state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginBottom 16
  , orientation HORIZONTAL
  , visibility $ boolToVisibility $ state.data.selectedItem.status /= "CANCELLED"
  ][  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width $ V (EHC.screenWidth unit/ 2)
      , visibility $ boolToVisibility $ not $ DS.null state.data.selectedItem.baseDistance
      ][  textView $
          [ text $ getString TRIP_DISTANCE
          , accessibilityHint $ "Trip Distance :" <> state.data.selectedItem.baseDistance
          , accessibility ENABLE
          , color Color.black700
          ] <> FontStyle.body1 LanguageStyle
        , textView $
          [ text state.data.selectedItem.baseDistance
          , width WRAP_CONTENT
          , color Color.black900
          , accessibility DISABLE_DESCENDANT
          ] <> FontStyle.paragraphText LanguageStyle
      ]
    , linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width $ V (EHC.screenWidth unit/ 2)
      , visibility $ boolToVisibility $ not $ DS.null state.data.selectedItem.totalTime
      ][  textView $
          [ text $ getString TIME_TAKEN
          , accessibilityHint $ "Time Taken :" <> state.data.selectedItem.totalTime
          , accessibility ENABLE
          , color Color.black700
          ] <> FontStyle.body1 LanguageStyle
        , textView $
          [ text state.data.selectedItem.totalTime
          , width WRAP_CONTENT
          , color Color.black900
          , accessibility DISABLE_DESCENDANT
          ] <> FontStyle.paragraphText LanguageStyle
      ]
   ]

---------------------- tripDetails ---------------------------
tripDetailsView ::  forall w . ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
tripDetailsView state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  ][   imageView
        [ imageWithFallback $ case state.data.vehicleVariant of
                                Just variant -> getVehicleVariantImage (show variant) RIGHT_VIEW
                                Nothing      -> fetchImage FF_ASSET "ic_vehicle_side" 
        , width $ V 40
        , visibility if (isJust state.data.vehicleVariant) then VISIBLE else GONE
        , height $ V 40
        ]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , weight 1.0
      , orientation VERTICAL
      ][ linearLayout
         [ height WRAP_CONTENT
         , width MATCH_PARENT
         , margin $ MarginLeft 10
         ][ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity CENTER_VERTICAL
            , weight 1.0
            ][  textView $
                [ text state.data.date
                , accessibilityHint $ "date : " <> state.data.date
                , accessibility ENABLE
                , color Color.darkCharcoal
                ] <> FontStyle.body1 LanguageStyle
              , linearLayout
                [ height MATCH_PARENT
                , width WRAP_CONTENT
                , gravity CENTER
                , orientation VERTICAL
                ][  linearLayout
                    [ background Color.greyishBlue
                    , cornerRadius 2.5
                    , margin $ Margin 5 3 5 0
                    , height $ V 5
                    , width $ V 5
                    ][]
                ]
              , textView $
                [ text state.data.time
                , color Color.darkCharcoal
                ] <> FontStyle.body1 LanguageStyle
              ]
            , textView $
              [ text state.data.totalAmount
              , accessibilityHint $  ( DS.replaceAll (DS.Pattern "₹") (DS.Replacement "") state.data.totalAmount) <> "Rupees"
              , accessibility ENABLE
              , color Color.black
              ] <> FontStyle.h2 LanguageStyle
          ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginLeft 10
        ][  textView $
            [ text $ capitalize $ DS.toLower $ getProperVehicleModelName state.data.selectedItem.vehicleModel
            , accessibilityHint $ "date : " <> state.data.date
            , accessibility ENABLE
            , color Color.greyShade
            , weight 1.0
            ] <> FontStyle.body16 LanguageStyle
          , textView $
            [ text $ if state.data.selectedItem.status == "CANCELLED" then getString CANCELLED else getString PAID <> " " <> if state.data.paymentMode == CASH then getString BY_CASH else getString ONLINE_
            , color if state.data.selectedItem.status == "CANCELLED" then Color.red else Color.greyShade
            , accessibility DISABLE
            ] <> FontStyle.captions LanguageStyle
          ]
      ]
    ]
  where
    getProperVehicleModelName :: String -> String
    getProperVehicleModelName vehicleModel = 
      if vehicleModel == "Unkown" -- Fallback case when vehicle mapping fails during vehicle onboarding 
        then fromMaybe "" $ state.data.selectedItem.serviceTierName 
        else vehicleModel

------------------- separator -------------------
separatorView ::  forall w . PrestoDOM (Effect Unit) w
separatorView =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , margin $ MarginVertical 16  8
  , background Color.lightGreyShade
  ][]

-------------------- ratingAndInvoice ----------------
ratingAndInvoiceView ::  forall w . ST.TripDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
ratingAndInvoiceView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , orientation HORIZONTAL
  , visibility if state.data.selectedItem.status == "CANCELLED" || state.props.fromMyRides == ST.Home then GONE else VISIBLE
  ][  textView $ 
      [ text $ getString YOU_RATED
      , accessibilityHint $ "You Rated " <> (show state.data.rating) <> " Stars"
      , accessibility ENABLE
      , color Color.greyDavy
      ] <> FontStyle.tags LanguageStyle
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , padding $ PaddingVertical 10 10
        , gravity CENTER
        , margin $ MarginLeft 5
        ](map (\ item ->  linearLayout
                          [ height WRAP_CONTENT
                          , width WRAP_CONTENT
                          , margin $ MarginRight 4
                          ][imageView
                              [ height $ V 14
                              , width $ V 14
                              , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if item <= state.data.rating then "ny_ic_star_active" else "ny_ic_star_inactive" 
                              ]
                            ]) [1 ,2 ,3 ,4 ,5])
    ]

-------- invoice --------
invoiceView ::  forall w . ST.TripDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
invoiceView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , orientation HORIZONTAL
    , disableClickFeedback false
    , onClick push $ const ViewInvoice
    , alpha if state.data.selectedItem.providerType == ONUS then 1.0 else 0.5
    , visibility if state.data.selectedItem.status == "CANCELLED" then GONE else VISIBLE
    ][ imageView [
          imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_invoice_sheet_icon"
        , height $ V 20
        , width $ V 20
        , margin $ MarginRight 12
        ]
     , textView $
        [ text $ getString VIEW_DRIVER_RECEIPT
        , accessibilityHint "View Invoice : Button"
        , accessibility ENABLE
        , color Color.black800
        ] <> FontStyle.body1 LanguageStyle
     ,  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity RIGHT
        ][  imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right"
            , height $ V 20
            , width $ V 20
            ]
          ]
      ]

----------------- report Isssue ----------------
reportIssueView ::  forall w . ST.TripDetailsScreenState -> (Action -> Effect Unit) -> Array CategoryListType -> PrestoDOM (Effect Unit) w
reportIssueView state push filteredTopics =
  linearLayout
    [ orientation VERTICAL
    , width MATCH_PARENT
    , height WRAP_CONTENT
    , disableClickFeedback true
    , onClick push $ const ReportIssue
    , visibility $ boolToVisibility (state.props.fromMyRides /= ReportIssueChat && DA.length filteredTopics > 0) 
    ][  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , orientation HORIZONTAL
        , margin $ Margin 16 0 16 16
        ][  
          imageView
            [ width $ V 20
            , height $ V 20 
            , imageWithFallback $ fetchImage FF_COMMON_ASSET if state.props.reportIssue then "ny_ic_help_and_support_dark" else "ny_ic_help"
            , margin $ MarginRight 12
            ]
        , textView $
            [ text $ getString HELP_AND_SUPPORT
            , accessibilityHint "Report an Issue : Button"
            , accessibility ENABLE
            , color if state.props.reportIssue then Color.black900 else Color.black800
            ] <> (if state.props.reportIssue then FontStyle.body4 else FontStyle.body1) LanguageStyle 
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity RIGHT
            ][imageView[ 
                imageWithFallback $ fetchImage FF_COMMON_ASSET $ if state.props.reportIssue then "ny_ic_chevron_up_dark" else "ny_ic_chevron_down_light"
              , height $ V 20
              , width $ V 20
              ]
            ]
          ] 
        , allTopicsView state push filteredTopics
        ]


allTopicsView :: forall w . ST.TripDetailsScreenState -> (Action -> Effect Unit) -> Array CategoryListType -> PrestoDOM (Effect Unit) w
allTopicsView state push topicList = 
  PrestoAnim.animationSet ([] <>
    if EHC.os == "IOS" then
      [ Anim.fadeIn state.props.reportIssue 
      , Anim.fadeOut $ not state.props.reportIssue ]
    else
      [Anim.listExpandingAnimation $ listExpandingAnimationConfig state.props.reportIssue])
   $ 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , orientation VERTICAL
    , margin $ Margin 32 16 16 0 
    , visibility $ boolToVisibility $ state.props.reportIssue
    , onAnimationEnd push $ const ListExpandAinmationEnd
    ](DA.mapWithIndex (\index item ->
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginHorizontal 16 16
        , onClick push $ const $ OpenChat item
        , orientation VERTICAL
        ][  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            ][  imageView
                [ imageWithFallback (fromMaybe "" item.categoryImageUrl)
                , height $ V 20
                , width $ V 20
                ]
              , textView $
                [ accessibilityHint $ item.categoryName <> " : Button"
                , accessibility ENABLE
                , text item.categoryName
                , color Color.darkCharcoal
                , margin $ MarginLeft 13
                ] <> FontStyle.paragraphText LanguageStyle
              , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity RIGHT
                ][  imageView
                    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right"
                    , height $ V 20
                    , width $ V 20 
                    ]
                  ]
              ]
            , linearLayout
              [ height $ V 1
              , width MATCH_PARENT
              , margin $ MarginVertical 20 20
              , background Color.greyLight
              , visibility $ boolToVisibility $ not $ index == (DA.length (topicList)) - 1
              ][]
          ]) topicList)