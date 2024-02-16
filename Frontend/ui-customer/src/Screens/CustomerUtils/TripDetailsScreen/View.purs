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
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Array as DA
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Data.String as DS
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getVehicleVariantImage, getVariantRideType)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Mobility.Prelude (boolToVisibility)
import Prelude ((<>), show)
import Prelude (Unit, const, map, unit, ($), (&&), (/=), (<<<), (<=), (<>), (==), (/), not, (-), (||))
import PrestoDOM (Accessiblity(..), FlexWrap(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), accessibility, accessibilityHint, adjustViewWithKeyboard, afterRender, alignParentBottom, background, color, cornerRadius, disableClickFeedback, editText, fontStyle, frameLayout, gravity, height, hint, hintColor, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, multiLineEditText, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, scrollView, stroke, text, textSize, textView, visibility, weight, width, onAnimationEnd)
import Screens.TripDetailsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (PaymentMode(..))
import Screens.Types as ST
import Styles.Colors as Color
import Debug (spy)
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim

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
  Anim.screenAnimation $ linearLayout
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
      ][tripDetailsLayout state push
      , reportIssueView state push
      ]
    ]
  ]

tripDetailsLayout :: forall w. ST.TripDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
tripDetailsLayout state push =
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
           , tripIdView push state
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
             ][]
           ]
        ]]

---------------------- tripIdView ---------------------------
tripIdView :: forall w . (Action -> Effect Unit) -> ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
tripIdView push state =
  let rideType = case state.data.vehicleVariant of
                    Just variant -> getVariantRideType (show variant)
                    Nothing      -> getString AC_CAB
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginBottom 16
  , orientation HORIZONTAL
  ][  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width WRAP_CONTENT
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
    , linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      -- , weight 1.0
      , visibility if isJust state.data.vehicleVariant && (getMerchant FunctionCall) == YATRISATHI then VISIBLE else GONE
      ][  textView $
          [ text $ getString RIDE_TYPE
          , accessibilityHint $ "Ride Type :" <> rideType
          , accessibility ENABLE
          , color Color.black700
          ] <> FontStyle.body1 LanguageStyle
        , textView $
          [ text $ rideType
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
  ][   frameLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          ][  imageView
              [ margin $ MarginLeft $ if isJust state.data.vehicleVariant then 24 else 0
              , cornerRadius 18.0
              -- , background Color.grey800
              , width (V 36)
              , height (V 36)
              , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_profile_image"
              ]
            , imageView
              [ imageWithFallback $ case state.data.vehicleVariant of
                                      Just variant -> getVehicleVariantImage (show variant)
                                      Nothing      -> fetchImage FF_ASSET "ic_vehicle_side" 
              , width $ V 40
              , visibility if (isJust state.data.vehicleVariant) then VISIBLE else GONE
              , height $ V 40
              ]
            ]

    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      , margin $ MarginLeft 10
      ][  textView $
          [ text state.data.driverName
          , accessibilityHint $ "Driver : " <> state.data.driverName
          , accessibility ENABLE
          , color Color.darkCharcoal
          ] <> FontStyle.body1 LanguageStyle
        , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER_VERTICAL
          ][  textView $
              [ text state.data.date
              , color Color.greyShade
              ] <> FontStyle.body16 LanguageStyle
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
              , color Color.greyShade
              ] <> FontStyle.body16 LanguageStyle
            ]
        ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity RIGHT
      , orientation VERTICAL
      ][  textView $
          [ text state.data.totalAmount
          , accessibilityHint $  ( DS.replaceAll (DS.Pattern "₹") (DS.Replacement "") state.data.totalAmount) <> "Rupees"
          , accessibility ENABLE
          , color Color.black
          ] <> FontStyle.h2 LanguageStyle
        , textView $
          [ text $ if state.data.selectedItem.status == "CANCELLED" then getString CANCELLED else getString PAID <> " " <> if state.data.paymentMode == CASH then getString BY_CASH else getString ONLINE_
          , color if state.data.selectedItem.status == "CANCELLED" then Color.red else Color.greyShade
          , accessibility DISABLE
          ] <> FontStyle.captions LanguageStyle
        ]
    ]

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
  , visibility if state.data.selectedItem.status == "CANCELLED" then GONE else VISIBLE
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
reportIssueView ::  forall w . ST.TripDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
reportIssueView state push =
  linearLayout
    [ orientation VERTICAL
    , width MATCH_PARENT
    , height WRAP_CONTENT
    , disableClickFeedback true
    , onClick push $ const ReportIssue
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
        , allTopicsView state push $ topicsList state
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
                [ imageWithFallback item.categoryImageUrl
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