module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.View where

import Prelude

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Common.Resources.Constants (zoomLevel)
import Components.ChooseVehicle.View as ChooseVehicle
import Components.GenericHeader.View as GenericHeader
import Animation.Config (removeYAnimFromTopConfig)
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination.View as SourceToDestinationView
import Components.PopUpModal as PopUpModal
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (isJust, isNothing, maybe, fromMaybe, Maybe(..))
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import DecodeUtil (getAnyFromWindow)
import Data.Function.Uncurried (runFn3)
import Common.Types.App as CT
import Debug (spy)
import Effect (Effect)
import Components.RateCard.View as RateCard
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Animation (fadeIn)
import Effect.Uncurried (runEffectFn2)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.CommonView (dummyView)
import Helpers.Utils (FetchImageFrom(..), fetchImage, decodeError, storeCallBackCustomer)
import JBridge as JB
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..) ,background, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, relativeLayout, scrollView, shimmerFrameLayout, onBackPressed, alignParentBottom, singleLine, accessibilityHint,accessibility,accessibilityHint, Accessiblity(..), id, afterRender, layoutGravity, rippleColor, maxLines, ellipsize, onAnimationEnd, scrollBarY, fillViewport)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Resources.Constants
import Resources.LocalizableV2.Strings (getEN)
import Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.Controller (Action(..), ScreenOutput, eval, validateInput)
import Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ComponentConfig (chooseVehicleConfig, deliveryPickupDetialsModalConfig, genericHeaderConfig, primaryButtonConfig, rateCardConfig, decodeAddress')
import Screens.HomeScreen.ScreenData (dummyRideBooking)
import Screens.Types as ST
import Services.API
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)

parcelDeliveryScreen :: ST.ParcelDeliveryScreenState -> Screen Action ST.ParcelDeliveryScreenState ScreenOutput
parcelDeliveryScreen initialState =
  { initialState
  , view
  , name: "ParcelDeliveryScreen"
  , globalEvents: [
    (\push -> do
      pure (pure unit)
    )
  ]
  , eval:
      \action state -> do
        let _ = spy "ParcelDeliveryScreen action " action
        let _ = spy "ParcelDeliveryScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , onBackPressed push $ const GoBack
    , padding $ PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom
    , onClick push $ const NoAction
    ]
    [ linearLayout 
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , background Color.white900
      , orientation VERTICAL
      ][
        GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
        , linearLayout
          [ height $ V 1 
          , width MATCH_PARENT
          , background Color.grey900
          ] []
        , linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , padding $ PaddingBottom 100
          ][ deliveryDetailsView push state
          , deliveryInstructionView push state
          ]
      ]
    , separatorView push state
    , footerView push state
    , (case state.data.currentStage of
        ST.SENDER_DETAILS -> deliveryDetailPopupView push state 
        ST.RECEIVER_DETAILS -> deliveryDetailPopupView push state 
        _ -> emptyTextView),
      (if state.props.showRateCard then (rateCardView push state) else emptyTextView)
    ]
    

mapViewLayout :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
mapViewLayout push state = 
  relativeLayout
  [ height $ V $ JB.getHeightFromPercent 20
  , width MATCH_PARENT
  , cornerRadius 24.0
  , margin $ MarginTop 16
  , id $ EHC.getNewIDWithTag idTag
  , afterRender (\action ->
                      if isNotInstructionsPage
                        then void $ JB.showMap (EHC.getNewIDWithTag idTag) false "satellite" zoomLevel state.data.sourceLat state.data.sourceLong push MapViewLoaded
                        else pure unit
                    ) $ const NoAction
  , visibility $ boolToVisibility isNotInstructionsPage
  ] []
  where
    idTag :: String
    idTag = "ParcelDetailsMapView"

    isNotInstructionsPage :: Boolean
    isNotInstructionsPage = state.data.currentStage /= ST.DELIVERY_INSTRUCTIONS

pickupView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
pickupView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 20
  ]
  [ textView $
    [ text $ getString PICKUP
    , color Color.black900
    ] <> FontStyle.subHeading1 TypoGraphy
  , pickupDropItemView push state true
  ]

dropView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
dropView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 20
  ]
  [ textView $
    [ text $ getString DROP
    , color Color.black900
    ] <> FontStyle.subHeading1 TypoGraphy
  , pickupDropItemView push state false
  ]

pickupDropItemView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> Boolean -> PrestoDOM (Effect Unit) w
pickupDropItemView push state isSource =
  let 
    personDetails = if isSource then state.data.senderDetails else state.data.receiverDetails
  in linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 8
    , cornerRadius 16.0
    , padding $ Padding 12 12 12 12
    , stroke $ "1," <> Color.borderGreyColor
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , gravity CENTER
      ]
      [ linearLayout
        [ width WRAP_CONTENT
        , orientation VERTICAL
        , gravity LEFT
        , weight 1.0
        ]
        [ textView $
          [ text personDetails.name
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
        , textView $
          [ text $ "+91 " <>  personDetails.phone
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
        ]
      , editButtonView push state isSource
      ]
    , sourceDestinationAddressView push state isSource
    ]

sourceDestinationAddressView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> Boolean -> PrestoDOM (Effect Unit) w
sourceDestinationAddressView push state isSource =
  let
    personDetails = if isSource then state.data.senderDetails else  state.data.receiverDetails
  in 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 8
    , background Color.blue600
    , cornerRadius 8.0
    , padding $ Padding 12 8 12 8
    ]
    [ linearLayout
      [ orientation HORIZONTAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      ]
      [ linearLayout
        [ orientation VERTICAL
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity LEFT
        , accessibility ENABLE
        , weight 1.0
        ]
        [ linearLayout
          [ width MATCH_PARENT
          , orientation HORIZONTAL
          ]
          [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET $ if isSource then "ny_ic_source_dot" else "ny_ic_dest_dot"
            , height $ V 14
            , width $ V 14
            , margin $ MarginRight 12
            , layoutGravity "center_vertical"
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            ]
            [ textView $
              [ text $ personDetails.extras
              , maxLines 1
              , ellipsize true
              , gravity LEFT
              , color Color.black900
              , margin $ MarginBottom 2
              ] <> FontStyle.tags TypoGraphy
            , textView $
              [ text $ decodeAddress' $ if isSource then state.data.sourceAddress else state.data.destinationAddress
              , color Color.black700
              , maxLines $ if (isJust personDetails.instructions) then 1 else 2
              , ellipsize true
              , margin $ MarginBottom 2
              ] <> FontStyle.body3 TypoGraphy
            , textView $
              [ textFromHtml $ "<em>" <> (if isSource then getString PICKUP_INSTRUCTION else  getString DROP_INSTRUCTION) <> ": " <> fromMaybe "" personDetails.instructions <> "</em>"
              , color Color.black700
              , visibility $ boolToVisibility $ isJust personDetails.instructions
              ] <> FontStyle.body3 TypoGraphy
            ]
          ]
        ]  
      ]
    ]

deliveryGuidelinesView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
deliveryGuidelinesView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ Padding 0 0 0 100
  , margin $ Margin 0 20 0 125
  ]
  [ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.blue600
    , padding $ Padding 16 16 16 16
    , cornerRadius 16.0
    ]
    [ textView $ 
      [ text $ getString DELIVERY_GUIDELINES
      , color Color.black800
      , margin $ MarginBottom 20
      ] 
      <> FontStyle.subHeading3 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      ( map (\item -> instructionItem item) instructionData)
    , textView $ 
      [ text $ getString VIEW_ALL_GUIDELINES
      , color Color.blue800
      , layoutGravity "center_horizontal"
      , margin $ Margin 16 16 16 0
      , onClick push $ const ExpandInstructions
      , visibility $ boolToVisibility false
      ]
      <> FontStyle.body1 TypoGraphy
    ]
]

footerView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
footerView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity BOTTOM
    , background Color.transparent
    ][ 
      linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingBottom 16
        , orientation VERTICAL
        , background Color.white900
        ]
        [ linearLayout  
          [ height $ V 1
          , width MATCH_PARENT
          , margin $ MarginBottom 16
          , background Color.grey900
          ][],
          linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , visibility $ boolToVisibility $ state.data.currentStage /= ST.DELIVERY_INSTRUCTIONS
          ][
            ChooseVehicle.view (push <<< ChooseVehicleAC) $ chooseVehicleConfig state
          , tipView push state
          ]
        , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
      ]
    ]
    

instructionItem :: forall w. { title :: String, image :: String } -> PrestoDOM (Effect Unit) w
instructionItem item =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginBottom 16
  ]
  [ imageView
    [ width $ V 20
    , height $ V 20
    , imageWithFallback $ fetchImage COMMON_ASSET item.image
    , margin $ MarginRight 8
    ]
  , textView
    $ [ text item.title
      , color Color.black800
      ]
    <> FontStyle.body20 TypoGraphy
  ]
  
editButtonView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> Boolean -> PrestoDOM (Effect Unit) w
editButtonView push state isSource =
  textView $ 
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text $ getString EDIT
  , cornerRadius if EHC.os == "IOS" then 20.0 else 32.0
  , stroke $ "1," <> Color.grey900
  , padding $ Padding 16 8 16 8
  , onClick push $ const $ EditAddress isSource
  , rippleColor Color.rippleShade
  , layoutGravity "right"
  , gravity CENTER_VERTICAL
  ] <> FontStyle.body1 TypoGraphy

primaryButtonView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
primaryButtonView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity BOTTOM
  , margin (MarginBottom 24)
  , alignParentBottom "true,-1"
  , stroke $ "1," <> Color.grey900
  ]
  [ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]

separatorView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
separatorView push state =
  linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , margin $ MarginVertical 5 5
  , background Color.grey900
  ]
  []

instructionData :: Array { title :: String, image :: String }
instructionData = 
  [ { title: getString $ ITEMS_SHOULD_FIT_IN_BACKPACK "15", image: "ny_ic_backpack" }
  , { title: getString AVOID_SENDING_HIGH_VALUE_ITEMS, image: "ny_ic_streamline_fragile_solid" }
  , { title: getString ILLEGAL_ITEMS_PROHIBITED, image: "ny_ic_prohibited" }
  , { title: getString $ MAX_PARCEL_SIZE "40" "40" "40", image: "ny_ic_backpack" }
  ]


deliveryDetailPopupView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
deliveryDetailPopupView push state =
    linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ PopUpModal.view (push <<< DeliveryDetailAction) (deliveryPickupDetialsModalConfig state) ]

emptyTextView :: forall w. PrestoDOM (Effect Unit) w
emptyTextView = textView [text "", width $ if EHC.os == "IOS" then V 1 else V 0]

rateCardView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
rateCardView push state =
  PrestoAnim.animationSet [ fadeIn true ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        ]
        [ RateCard.view (push <<< RateCardAction) (rateCardConfig state) ]

tipView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
tipView push state = 
  linearLayout
  [
     height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
  ][
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.ivory
    , cornerRadius 12.0
    , padding $ Padding 20 8 20 8
    , margin $ Margin 16 0 16 6
    , gravity CENTER
    , visibility $ boolToVisibility $ isJust state.data.tipForDriver
    ][
      textView $ 
      [ text $ "₹" <> (show $ fromMaybe 0 state.data.tipForDriver) <> " " <> getString TIP_ADDED
      , color Color.black900
      , width WRAP_CONTENT
      , height WRAP_CONTENT
      ] <> FontStyle.body4 LanguageStyle
    ]
  ]

deliveryDetailsView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
deliveryDetailsView push state = do
  scrollView
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , fillViewport true
    , visibility $ boolToVisibility $ state.data.currentStage /= ST.DELIVERY_INSTRUCTIONS
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ Margin 16 16 16 100
      ]
      [ mapViewLayout push state
      , pickupView push state
      , dropView push state
      , deliveryGuidelinesView push state
      ]
    ]

deliveryInstructionView :: forall w . (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
deliveryInstructionView push state = 
  let appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
  in scrollView
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , fillViewport true
    , visibility $ boolToVisibility $ state.data.currentStage == ST.DELIVERY_INSTRUCTIONS
    ][ 
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ Margin 16 16 16 100
      ]
      [
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , background Color.blue600
        , margin $ MarginHorizontal 16 16
        , padding $ Padding 16 16 16 16
        , cornerRadius 16.0
        ]
        [ imageView
          [ width $ V (EHC.screenWidth unit - 64)
          , height $ V (EHC.screenWidth unit - 64)
          , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_delivery_instructions"
          , margin $ MarginBottom 20
          , cornerRadius 8.0
          , layoutGravity "left"
          ]
        , textView
          $ [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ getString $ QUICK_DELIVERY_WITH appName
            , color Color.black800
            , margin $ MarginBottom 20
            ]
          <> FontStyle.subHeading3 CT.TypoGraphy
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ]
          ( map (\item -> instructionItem item) instructionData)
        , textView
          $ [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text $ getString VIEW_ALL_GUIDELINES
            , color Color.blue800
            , gravity CENTER_HORIZONTAL
            , margin $ Margin 16 16 16 0
            , visibility $ boolToVisibility false
            ]
          <> FontStyle.body1 CT.TypoGraphy
        ]
    ]
    ]