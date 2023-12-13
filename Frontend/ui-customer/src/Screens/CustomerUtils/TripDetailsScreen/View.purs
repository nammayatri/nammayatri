{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TripDetailsScreen.View where

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Components.PopUpModal as PopUpModal
import Effect (Effect)
import Language.Types (STR(..))
import Language.Strings (getString)
import Prelude (Unit, const, map, unit, ($), (&&), (/=), (<<<), (<=), (<>), (==), (/))
import PrestoDOM (Length(..), Margin(..), Orientation(..), Padding(..), Gravity(..), Visibility(..), Accessiblity(..), PrestoDOM, Screen, linearLayout, frameLayout, gravity, orientation, height, width, imageView, imageUrl, text, textSize, textView, padding, color, margin, fontStyle, background, cornerRadius, stroke, editText, weight, hint, onClick, visibility, pattern, onChange, scrollView, relativeLayout, alignParentBottom, onBackPressed, afterRender, multiLineEditText, disableClickFeedback, imageWithFallback, hintColor, adjustViewWithKeyboard, accessibilityHint, accessibility)
import Screens.Types as ST
import Screens.Types (PaymentMode(..))
import Screens.TripDetailsScreen.Controller (Action(..), ScreenOutput, eval)
import Font.Size as FontSize
import Font.Style as FontStyle
import Components.GenericHeader as GenericHeader
import Components.SourceToDestination as SourceToDestination
import Engineering.Helpers.Commons as EHC
import Styles.Colors as Color
import Debug (spy)
import Common.Types.App
import Screens.CustomerUtils.TripDetailsScreen.ComponentConfig
import Helpers.Utils (fetchImage, FetchImageFrom(..), getVehicleVariantImage, getVariantRideType)
import Prelude ((<>), show)
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Data.String as DS
import MerchantConfig.Utils (Merchant(..), getMerchant)

screen :: ST.TripDetailsScreenState -> Screen Action ST.TripDetailsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "TripDetailsScreen"
  , globalEvents: []
  , eval:
      \state action -> do
        let
          _ = spy "TripDetailsScreen action " state
        let
          _ = spy "TripDetailsScreen state " action
        eval state action
  }

view ::
  forall w.
  (Action -> Effect Unit) -> ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
        , onBackPressed push (const BackPressed)
        , afterRender push (const AfterRender)
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , accessibility if state.props.showConfirmationPopUp then DISABLE_DESCENDANT else DISABLE
            ]
            [ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
            , relativeLayout
                [ width MATCH_PARENT
                , height MATCH_PARENT
                , visibility if state.props.issueReported then GONE else VISIBLE
                , adjustViewWithKeyboard "true"
                ]
                [ tripDetailsLayout state push
                , linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , alignParentBottom "true,-1"
                    , background Color.white900
                    , visibility if state.props.reportIssue then VISIBLE else GONE
                    ]
                    [ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state) ]
                ]
            ]
        , issueReportedView state push
        , lostAndFoundPopUpView push state
        ]

tripDetailsLayout :: forall w. ST.TripDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
tripDetailsLayout state push =
  scrollView
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , background Color.blue600
            , visibility if state.props.issueReported then GONE else VISIBLE
            , padding (Padding 16 16 16 10)
            ]
            [ tripDetailsView state
            , separatorView
            , tripIdView push state
            , SourceToDestination.view (push <<< SourceToDestinationActionController) (sourceToDestinationConfig state)
            , ratingAndInvoiceView state push
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , padding (Padding 16 0 16 50)
            , margin (MarginBottom 40)
            , visibility if state.props.issueReported then GONE else VISIBLE
            , orientation VERTICAL
            ]
            [ invoiceView state push
            , linearLayout
                [ height (V 1)
                , width MATCH_PARENT
                , background Color.lightGreyShade
                , visibility if state.data.selectedItem.status == "CANCELLED" then GONE else VISIBLE
                ]
                []
            , lostAndFoundView push state
            , linearLayout
                [ height (V 1)
                , width MATCH_PARENT
                , background Color.lightGreyShade
                , visibility if (state.data.selectedItem.status /= "CANCELLED" && state.props.canConnectWithDriver) then VISIBLE else GONE
                ]
                []
            , reportIssueView state push
            ]
        ]
    ]

lostAndFoundView :: forall w. (Action -> Effect Unit) -> ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
lostAndFoundView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , orientation HORIZONTAL
    , padding (Padding 0 16 0 16)
    , disableClickFeedback false
    , visibility if (state.data.selectedItem.status /= "CANCELLED" && state.props.canConnectWithDriver) then VISIBLE else GONE
    , onClick push $ (const ShowPopUp)
    ]
    [ textView
        $ [ text (getString LOST_SOMETHING)
          , color Color.darkCharcoal
          ]
        <> FontStyle.body1 LanguageStyle
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity RIGHT
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right"
            , height (V 15)
            , width (V 15)
            ]
        ]
    ]

---------------------- tripIdView ---------------------------
tripIdView :: forall w. (Action -> Effect Unit) -> ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
tripIdView push state =
  let
    rideType = case state.data.vehicleVariant of
      Just variant -> getVariantRideType (show variant)
      Nothing -> getString AC_CAB
  in
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin (MarginBottom 16)
      , orientation HORIZONTAL
      ]
      [ linearLayout
          [ orientation VERTICAL
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          , visibility if state.data.tripId == "" then GONE else VISIBLE
          , weight 1.0
          ]
          [ textView
              $ [ text (getString RIDE_ID)
                , accessibilityHint $ "Ride I-D :" <> (DS.replaceAll (DS.Pattern "") (DS.Replacement " ") state.data.tripId)
                , accessibility ENABLE
                , color Color.black700
                ]
              <> FontStyle.body1 LanguageStyle
          , linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , orientation HORIZONTAL
              , onClick push (const Copy)
              , accessibility DISABLE_DESCENDANT
              , gravity CENTER_VERTICAL
              ]
              [ textView
                  $ [ text state.data.tripId
                    , width WRAP_CONTENT
                    , color Color.black900
                    ]
                  <> FontStyle.paragraphText LanguageStyle
              , imageView
                  [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_copy"
                  , height (V 13)
                  , width (V 11)
                  , margin (MarginLeft 10)
                  ]
              ]
          ]
      , linearLayout
          [ orientation VERTICAL
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          , weight 1.0
          , visibility if isJust state.data.vehicleVariant && (getMerchant FunctionCall) == YATRISATHI then VISIBLE else GONE
          ]
          [ textView
              $ [ text (getString RIDE_TYPE)
                , accessibilityHint $ "Ride Type :" <> rideType
                , accessibility ENABLE
                , color Color.black700
                ]
              <> FontStyle.body1 LanguageStyle
          , textView
              $ [ text $ rideType
                , width WRAP_CONTENT
                , color Color.black900
                , accessibility DISABLE_DESCENDANT
                ]
              <> FontStyle.paragraphText LanguageStyle
          ]
      ]

lostAndFoundPopUpView :: forall w. (Action -> Effect Unit) -> ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
lostAndFoundPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , visibility if state.props.showConfirmationPopUp then VISIBLE else GONE
    ]
    [ PopUpModal.view (push <<< PopUpModalAction) (confirmLostAndFoundConfig state) ]

---------------------- tripDetails ---------------------------
tripDetailsView :: forall w. ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
tripDetailsView state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    ]
    [ frameLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation HORIZONTAL
        ]
        [ imageView
            [ margin $ MarginLeft $ if isJust state.data.vehicleVariant then 24 else 0
            , cornerRadius 18.0
            -- , background Color.grey800
            , width (V 36)
            , height (V 36)
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_profile_image"
            ]
        , imageView
            [ imageWithFallback
                $ case state.data.vehicleVariant of
                    Just variant -> getVehicleVariantImage (show variant)
                    Nothing -> fetchImage FF_ASSET "ic_vehicle_side"
            , width (V 40)
            , visibility if (isJust state.data.vehicleVariant) then VISIBLE else GONE
            , height (V 40)
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
        , margin (MarginLeft 10)
        ]
        [ textView
            $ [ text state.data.driverName
              , accessibilityHint $ "Driver : " <> state.data.driverName
              , accessibility ENABLE
              , color Color.darkCharcoal
              ]
            <> FontStyle.body1 LanguageStyle
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation HORIZONTAL
            , gravity CENTER_VERTICAL
            ]
            [ textView
                $ [ text state.data.date
                  , color Color.greyShade
                  ]
                <> FontStyle.body16 LanguageStyle
            , linearLayout
                [ height MATCH_PARENT
                , width WRAP_CONTENT
                , gravity CENTER
                , orientation VERTICAL
                ]
                [ linearLayout
                    [ background Color.greyishBlue
                    , cornerRadius 2.5
                    , margin (Margin 5 3 5 0)
                    , height (V 5)
                    , width (V 5)
                    ]
                    []
                ]
            , textView
                $ [ text state.data.time
                  , color Color.greyShade
                  ]
                <> FontStyle.body16 LanguageStyle
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity RIGHT
        , orientation VERTICAL
        ]
        [ textView
            $ [ text (state.data.totalAmount)
              , accessibilityHint $ (DS.replaceAll (DS.Pattern "₹") (DS.Replacement "") state.data.totalAmount) <> "Rupees"
              , accessibility ENABLE
              , color Color.black
              ]
            <> FontStyle.h2 LanguageStyle
        , textView
            $ [ text $ if state.data.selectedItem.status == "CANCELLED" then (getString CANCELLED) else (getString PAID) <> " " <> if state.data.paymentMode == CASH then (getString BY_CASH) else (getString ONLINE_)
              , color if state.data.selectedItem.status == "CANCELLED" then Color.red else Color.greyShade
              , accessibility DISABLE
              ]
            <> FontStyle.captions LanguageStyle
        ]
    ]

------------------- separator -------------------
separatorView :: forall w. PrestoDOM (Effect Unit) w
separatorView =
  linearLayout
    [ height (V 1)
    , width MATCH_PARENT
    , margin (Margin 0 16 0 8)
    , background Color.lightGreyShade
    ]
    []

-------------------- ratingAndInvoice ----------------
ratingAndInvoiceView :: forall w. ST.TripDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
ratingAndInvoiceView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , orientation HORIZONTAL
    , visibility if state.data.selectedItem.status == "CANCELLED" then GONE else VISIBLE
    ]
    [ textView
        $ [ text $ (getString YOU_RATED)
          , accessibilityHint $ "You Rated " <> (show state.data.rating) <> " Stars"
          , accessibility ENABLE
          , color Color.greyDavy
          ]
        <> FontStyle.tags LanguageStyle
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , padding (Padding 0 10 0 10)
        , gravity CENTER
        , margin (MarginLeft 5)
        ]
        ( map
            ( \item ->
                linearLayout
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , margin (Margin 0 0 4 0)
                  ]
                  [ imageView
                      [ height $ V 14
                      , width $ V 14
                      , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if item <= state.data.rating then "ny_ic_star_active" else "ny_ic_star_inactive"
                      ]
                  ]
            )
            [ 1, 2, 3, 4, 5 ]
        )
    -- , linearLayout
    --   [ height WRAP_CONTENT
    --   , width MATCH_PARENT
    --   , gravity RIGHT
    --   , onClick push $ (const DownloadInvoice)
    --   ][  textView
    --       [ text (getString DOWNLOAD_DRIVER_RECEIPT)
    --       , textSize FontSize.a_12
    --       , color Color.blue900
    --       , visibility if not state.props.invoiceDownloaded then VISIBLE else GONE
    --       ]
    --     ]
    ]

-------- invoice --------
invoiceView :: forall w. ST.TripDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
invoiceView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , orientation HORIZONTAL
    , padding (Padding 0 16 0 16)
    , disableClickFeedback false
    , onClick push $ (const ViewInvoice)
    , visibility if state.data.selectedItem.status == "CANCELLED" then GONE else VISIBLE
    ]
    [ textView
        $ [ text (getString VIEW_DRIVER_RECEIPT)
          , accessibilityHint "View Invoice : Button"
          , accessibility ENABLE
          , color Color.darkCharcoal
          ]
        <> FontStyle.body1 LanguageStyle
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity RIGHT
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right"
            , height (V 15)
            , width (V 15)
            ]
        ]
    ]

----------------- report Isssue ----------------
reportIssueView :: forall w. ST.TripDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
reportIssueView state push =
  linearLayout
    [ orientation VERTICAL
    , width MATCH_PARENT
    , height WRAP_CONTENT
    , padding (Padding 0 16 0 16)
    , disableClickFeedback true
    , onClick push $ const ReportIssue
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , orientation HORIZONTAL
        , margin (MarginBottom 16)
        ]
        [ textView
            $ [ text (getString REPORT_AN_ISSUE)
              , accessibilityHint "Report an Issue : Button"
              , accessibility ENABLE
              , color Color.darkCharcoal
              ]
            <> FontStyle.body1 LanguageStyle
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity RIGHT
            ]
            [ imageView
                [ imageWithFallback $ fetchImage FF_COMMON_ASSET $ if state.props.reportIssue then "ny_ic_chevron_up" else "ny_ic_chevron_right"
                , height $ if state.props.reportIssue then V 6 else V 15
                , width $ if state.props.reportIssue then V 12 else V 15
                ]
            ]
        ]
    -- TODO add animations
    -- , PrestoAnim.animationSet[
    --   fadeIn state.props.reportIssue
    -- , fadeOut $ not state.props.reportIssue
    -- ] $
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , visibility if state.props.reportIssue then VISIBLE else GONE
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height $ V 120
            , orientation HORIZONTAL
            , cornerRadius 5.0
            , padding (Padding 2 2 2 2)
            , gravity LEFT
            , stroke ("1," <> Color.borderColorLight)
            ]
            [ (if EHC.os == "ANDROID" then editText else multiLineEditText)
                $ [ height MATCH_PARENT
                  , width WRAP_CONTENT
                  , weight 1.0
                  , padding (Padding 14 14 14 14)
                  , color Color.black800
                  , gravity LEFT
                  , background Color.white900
                  , text ""
                  , hint $ getString YOU_CAN_DESCRIBE_THE_ISSUE_YOU_FACED_HERE
                  , hintColor $ Color.blueGrey
                  , pattern "[^\n]*,255"
                  , onChange push $ MessageTextChanged
                  ]
                <> FontStyle.body6 LanguageStyle
            ]
        ]
    ]

-------------------------- issueReportedView -----------------------
issueReportedView :: forall w. ST.TripDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
issueReportedView state push =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , visibility if state.props.issueReported then VISIBLE else GONE
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
        , orientation VERTICAL
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_letter"
            , height $ V 149
            , width $ V 149
            , margin (MarginBottom 32)
            ]
        , textView
            $ [ text $ getString THANK_YOU_FOR_WRITING
              , gravity CENTER
              , width MATCH_PARENT
              , color Color.black900
              , margin (MarginBottom 12)
              ]
            <> FontStyle.h1 LanguageStyle
        , textView
            $ [ text $ getString WE_HAVE_RECEIVED_YOUR_ISSUE
              , margin (Margin 42 0 42 0)
              , gravity CENTER
              , width MATCH_PARENT
              , color Color.blackLightGrey
              ]
            <> FontStyle.body3 TypoGraphy
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , alignParentBottom "true,-1"
        , gravity BOTTOM
        , margin (MarginBottom 16)
        ]
        [ PrimaryButton.view (push <<< PrimaryButtonActionController) (goHomeButtonConfig state) ]
    ]
