module Screens.BookingOptionsScreen.View where

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Ord (compare)
import Debug (spy)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getVehicleType, fetchImage, FetchImageFrom(..), getVariantRideType, getVehicleVariantImage, getDowngradeOptionsText, getUIDowngradeOptions)
import Language.Strings (getString)
import Engineering.Helpers.Utils as EHU
import Engineering.Helpers.Commons as EHC
import Language.Types (STR(..))
import Prelude (Unit, const, map, not, show, ($), (<<<), (<>), (==), (<>), (&&), (||), (-), bind, void, pure, unit, discard, negate, (/=)) 
import PrestoDOM (Gravity(..), Length(..), Margin(..), Gradient(..) ,Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alpha, background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, frameLayout, visibility, clickable, singleLine, imageUrl, rippleColor, scrollView, scrollBarY, fillViewport, relativeLayout, shimmerFrameLayout, gradient)
import Screens.BookingOptionsScreen.Controller (Action(..), ScreenOutput, eval, getVehicleCapacity)
import Screens.BookingOptionsScreen.ScreenData (defaultRidePreferenceOption)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Data.Array as DA
import Data.String as DS
import Mobility.Prelude as MP
import Services.API as API
import Data.Maybe as MB
import Components.PopUpModal as PopUpModal
import Resource.Constants as RC
import Storage (KeyStore(..), getValueToLocalStore)
import Screens.BookingOptionsScreen.ComponentConfig (topAcDriverPopUpConfig, rateCardConfig)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons as EHC
import Types.App (defaultGlobalState)
import Helpers.API as HelperAPI
import Presto.Core.Types.API (ErrorResponse(..))
import Data.Either (Either(..))
import Components.RateCard as RateCard
import PrestoDOM.Animation as PrestoAnim
import Common.Styles.Colors as Colors
import ConfigProvider as CP
import Constants as CS
import Helpers.Utils as HU

screen :: ST.BookingOptionsScreenState -> Screen Action ST.BookingOptionsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "BookingDetailsScreen"
  , globalEvents: 
        [( \push -> if not $ initialState.data.config.rateCardScreen.showRateCard then pure $ pure unit else do
            _ <-
              void $ launchAff $ EHC.flowRunner defaultGlobalState
                $ do
                    response <- HelperAPI.callApi $ API.GetDriverRateCardReq MB.Nothing MB.Nothing
                    case response of
                      Left _ -> pure unit
                      Right resp -> EHC.liftFlow $ push $ UpdateRateCard resp
                    pure unit
            pure $ pure unit
        )]
  , eval:
      ( \state action -> do
          let
            _ = spy "BookingOptionsScreenState -----" state
          let
            _ = spy "BookingOptionsScreenState--------action" action
          eval state action
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ frameLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        ]
    $ [ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , onBackPressed push $ const BackPressed
          , afterRender push $ const AfterRender
          , background Color.white900
          , padding $ PaddingBottom $ EHC.safeMarginBottomWithDefault 24
          ]
          $ [ headerLayout push state
            , scrollView
                [ width MATCH_PARENT
                , weight 1.0
                , scrollBarY false
                , fillViewport true
                ]
                [ linearLayout
                    [ height MATCH_PARENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    ]
                    [ defaultVehicleView push state
                    , rateCardBannerView push state
                    , acCheckForDriversView push state
                    , downgradeVehicleView push state
                    ]
                ]
            ]
      ]
    <> if state.props.acExplanationPopup then [ PopUpModal.view (push <<< TopAcDriverAction) (topAcDriverPopUpConfig state) ] else []
    <> if state.props.showRateCard then [ rateCardView push state ] else []


acCheckForDriversView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
acCheckForDriversView push state =
  let
    acCheckVisibility = MP.boolToVisibility $ (getMerchant FunctionCall) /= BRIDGE && MB.isJust state.data.airConditioned

    (API.AirConditionedTier airConditionedData) = MB.fromMaybe defaultAirConditionedData state.data.airConditioned

    backgroundColor = if airConditionedData.isWorking then state.data.config.bookingPreferencesConfig.primaryToggleBackground else Color.black600

    align = if airConditionedData.isWorking then RIGHT else LEFT

    messageColor = if airConditionedData.isWorking then Color.black600 else Color.red900

    callSupportVisibility = not airConditionedData.isWorking && airConditionedData.usageRestrictionType == API.ToggleNotAllowed

    -- TODO : enable when backend sends correct usageRestrictionType
    -- canUpgradeOrDowngradeVariant = (not airConditionedData.isWorking && airConditionedData.usageRestrictionType /= API.ToggleNotAllowed) || airConditionedData.isWorking
  in
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin (Margin 16 0 16 16)
      , padding $ Padding 16 0 16 0
      , visibility acCheckVisibility
      , stroke $ "1," <> Color.grey900
      , cornerRadius 8.0
      , gravity CENTER_VERTICAL
      , orientation VERTICAL
      ]
      [ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER_VERTICAL
          ]
          [ linearLayout
            [ weight 1.0
            , height WRAP_CONTENT
            , onClick push $ const $ ShowACVideoPopup
            , gravity CENTER_VERTICAL
            ][  textView $
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , color Color.black800
                , text $ getString AC_CHECK_TITILE
                , margin $ MarginRight 7
                ] <> FontStyle.subHeading1 TypoGraphy
              , imageView
                [ width $ V 32
                , height $ V 32
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_youtube"
                , rippleColor Color.rippleShade
                , visibility $ MP.boolToVisibility $ state.data.config.rateCardScreen.showYoutubeVideo
                , padding $ Padding 0 5 5 5
                ]
            ]
          , linearLayout 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , padding $ PaddingVertical 16 16
           ][
            linearLayout
              [ width $ V 40
              , height $ V 22
              , cornerRadius 100.0
              , background backgroundColor
              , stroke $ "1," <> backgroundColor
              -- , clickable canUpgradeOrDowngradeVariant -- TODO : enable when backend sends correct usageRestrictionType
              , onClick push $ const $ UpdateACAvailability airConditionedData.isWorking
              , gravity CENTER_VERTICAL
              ]
              [ linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , gravity align
                  ]
                  [ linearLayout
                      [ width $ V 16
                      , height $ V 16
                      , background Color.white900
                      , cornerRadius 100.0
                      , gravity CENTER_VERTICAL
                      , margin (MarginHorizontal 2 2)
                      ]
                      []
                  ]
              ]
            ]
          ]
      , textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , visibility $ MP.boolToVisibility $ MB.isJust airConditionedData.restrictionMessage
          , color messageColor
          , padding $ if callSupportVisibility then Padding 0 0 0 0 else PaddingBottom 12
          , text $ fromMaybe "" airConditionedData.restrictionMessage
          ] <> FontStyle.tags TypoGraphy
      , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , cornerRadius 4.0
          , padding $ Padding 6 8 12 10
          , gravity CENTER_VERTICAL
          , onClick push $ const $ CallSupport
          , visibility $ MP.boolToVisibility $ callSupportVisibility
          ]
          [ imageView
              [ width $ V 18
              , height $ V 18
              , margin $ MarginRight 7
              , imageUrl "ny_ic_phone_filled_blue"
              ]
          , textView
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString CONTACT_SUPPORT
              , color Color.blue900
              ]
          ]
      ]
  where
  defaultAirConditionedData :: API.AirConditionedTier
  defaultAirConditionedData =
    API.AirConditionedTier
      { isWorking: true
      , restrictionMessage: Nothing
      , usageRestrictionType: API.ToggleAllowed
      }

downgradeVehicleView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
downgradeVehicleView push state =
  let
    compareRidePreferences a b = compare a.priority b.priority
    defaultRidePreferences = DA.sortBy compareRidePreferences state.data.ridePreferences
  in
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin (MarginHorizontal 16 16)
      , padding $ Padding 16 16 16 16
      , stroke $ "1," <> Color.grey900
      , cornerRadius 8.0
      , orientation VERTICAL
      ]
      [ textView
          $ [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , color Color.black800
            , margin $ MarginBottom 16
            , text $ getString DOWNGRADE_VEHICLE
            ]
          <> FontStyle.body4 TypoGraphy
      , textView
          $ [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , color Color.black700
            , margin $ MarginBottom 16
            , text $ getString RIDE_TYPE_SELECT
            ] <> FontStyle.body1 TypoGraphy
      , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , margin $ MarginBottom 16
          ]
          [ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ]
              (( ridePreferencesView push state defaultRidePreferences) <> 
              [ 
                  rentalPreferenceView push state,
                  intercityPreferenceView push state
              ])
          ]
      ]

ridePreferencesView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> Array ST.RidePreference -> Array ( PrestoDOM (Effect Unit) w )
ridePreferencesView push state ridePreferences =
  DA.mapWithIndex
    ( \index item ->
        linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          ]
          [ serviceTierItem state push item state.props.downgraded false $ getActualIndex index]
    ) ridePreferences
  where
    getActualIndex index = DA.length ridePreferences - index - 1
  

serviceTierItem :: forall w. ST.BookingOptionsScreenState -> (Action -> Effect Unit) -> ST.RidePreference -> Boolean -> Boolean -> Int -> PrestoDOM (Effect Unit) w
serviceTierItem state push service enabled opacity index =
  frameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , weight 1.0
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding (Padding 12 4 12 4)
        , margin $ MarginVertical 5 5
        , orientation HORIZONTAL
        , stroke $ "1," <> Color.grey900
        , cornerRadius 8.0
        , gravity CENTER_VERTICAL
        ]
        [ imageView
            [ imageWithFallback $ getVehicleVariantImage $ HU.getVehicleMapping service.serviceTierType
            , width $ V 35
            , height $ V 35
            ]
        , linearLayout
        [ weight 1.0
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][ linearLayout
           [ width MATCH_PARENT
           , height WRAP_CONTENT
           , gravity CENTER_VERTICAL
           , onClick push $ const $ ShowRateCard service
           , clickable state.props.rateCardLoaded
           ][ textView
              [ height WRAP_CONTENT
              , text service.name
              , margin (MarginHorizontal 12 2)
              , color Color.black800
              , singleLine true
              ]
            , imageView
              [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_grey"
              , width $ V 12
              , height $ V 12
              , visibility $ MP.boolToVisibility $ state.props.rateCardLoaded && index /= -1 && state.data.config.rateCardScreen.showRateCard
              ]
           ]
          , textView $
            [ height WRAP_CONTENT
            , text $ fromMaybe "" service.shortDescription
            , margin (MarginHorizontal 12 2)
            , color Color.black650
            , singleLine true
            , textSize FontSize.a_12
            ] <> FontStyle.body3 TypoGraphy
        ]
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , padding $ PaddingVertical 12 12
            , onClick push $ const $ ToggleRidePreference service
            , gravity RIGHT
            ]
            [ toggleView push state service.isSelected service.isDefault service ]
        ]
    ]

rentalPreferenceView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
rentalPreferenceView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , visibility $ MP.boolToVisibility $ isJust state.props.canSwitchToRental
    ][serviceTierItem state push item (fromMaybe false state.props.canSwitchToRental) false (-1)]
  where 
    item :: ST.RidePreference
    item = defaultRidePreferenceOption {name = "Rentals", isSelected = fromMaybe false state.props.canSwitchToRental,  serviceTierType = API.RENTALS}

intercityPreferenceView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
intercityPreferenceView push state = do
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , visibility $ MP.boolToVisibility $ isJust state.props.canSwitchToInterCity
    ][serviceTierItem state push item (fromMaybe false state.props.canSwitchToInterCity) false (-1)]
  where 
    item :: ST.RidePreference
    item = defaultRidePreferenceOption {name = "Intercity", isSelected = fromMaybe false state.props.canSwitchToInterCity, serviceTierType = API.INTERCITY}

toggleView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> Boolean -> Boolean -> ST.RidePreference -> PrestoDOM (Effect Unit) w
toggleView push state enabled default service =
  let
    backgroundColor = if enabled && not service.isUsageRestricted then state.data.config.bookingPreferencesConfig.primaryToggleBackground else Color.black600

    align = if enabled && not service.isUsageRestricted then RIGHT else LEFT
  in
    linearLayout
      [ width $ V 40
      , height $ V 22
      , cornerRadius 100.0
      , alpha if default then 0.5 else 1.0
      , background backgroundColor
      , stroke $ "1," <> backgroundColor
      , gravity CENTER_VERTICAL
      , onClick push $ const $ getAction
      , clickable $ not default
      ]
      [ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity align
          ]
          [ linearLayout
              [ width $ V 16
              , height $ V 16
              , background Color.white900
              , cornerRadius 100.0
              , gravity CENTER_VERTICAL
              , margin (MarginHorizontal 2 2)
              ]
              []
          ]
      ]
  where
    getAction :: Action
    getAction = case service.serviceTierType of
      API.RENTALS -> ToggleRentalRide
      API.INTERCITY -> ToggleIntercityRide
      _ -> ToggleRidePreference service

defaultVehicleView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
defaultVehicleView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , cornerRadius 8.0
    , padding $ Padding 16 20 16 30
    , margin $ Margin 16 16 16 16
    , stroke $ "1," <> Color.grey900
    ]
    [ vehicleDetailsView push state
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.grey700
        , margin $ MarginVertical 23 20
        ]
        []
    , vehicleLogoAndType push state
    ]

vehicleDetailsView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
vehicleDetailsView push state =
  let
    vehicleViewLabel = if DS.null state.data.vehicleName then (getVariantRideType state.data.vehicleType) else state.data.vehicleName
    isBridgeApp = DS.contains (DS.Pattern "Bridge") (HU.appName false)  
  in
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER_VERTICAL
      ]
      [ linearLayout
          [ orientation VERTICAL
          , weight 1.0
          ]
          [ customTV (getString YOUR_VEHICLE) FontSize.a_12 FontStyle.body3 Color.black650
          , customTV vehicleViewLabel FontSize.a_20 FontStyle.h3 Color.black800
          ]
      , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , cornerRadius state.data.config.bookingPreferencesConfig.vehicleNumberRadius
          , background state.data.config.bookingPreferencesConfig.vehicleNumberBackground
          , padding $ Padding 3 3 3 3
          ]
          [ textView
              $ [ width MATCH_PARENT
                , height MATCH_PARENT
                , padding $ Padding 5 3 5 3
                , text state.data.vehicleNumber
                , color Color.black900
                , gravity CENTER
                , cornerRadius 3.0
                ] <> if isBridgeApp then [] else [stroke $ "2," <> Color.black800]
              <> FontStyle.body8 TypoGraphy
          ]
      ]

vehicleLogoAndType :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
vehicleLogoAndType push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        [ imageView
            [ imageWithFallback $ getVehicleVariantImage state.data.vehicleType
            , gravity LEFT
            , height $ V 48
            , width $ V 48
            ]
        , linearLayout
            [ height MATCH_PARENT
            , weight 1.0
            , orientation VERTICAL
            , gravity CENTER_VERTICAL
            , margin $ MarginLeft 7
            ]
            [ customTV (state.data.defaultRidePreference.name) FontSize.a_20 FontStyle.h3 Color.black800
            , customTV ((show $ fromMaybe 4 state.data.defaultRidePreference.seatingCapacity) <> " "<> getString PEOPLE) FontSize.a_12 FontStyle.body3 Color.black650
            ]
        ]
    ]

headerLayout :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
headerLayout push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , padding $ Padding 10 (EHC.safeMarginTopWithDefault 13) 10 13
        ]
        [ imageView
            [ width $ V 30
            , height $ V 30
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
            , onClick push $ const BackPressed
            , rippleColor Color.rippleShade
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString BOOKING_OPTIONS
              , margin $ MarginLeft 10
              , padding $ PaddingBottom 2
              , color Color.black900
              ]
            <> FontStyle.h3 TypoGraphy
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.greyLight
        ]
        []
    ]

customTV :: forall w. String -> Int -> (LazyCheck -> forall properties. (Array (Prop properties))) -> String -> PrestoDOM (Effect Unit) w
customTV text' textSize' fontStyle' color' =
  textView
    $ [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text text'
      , textSize textSize'
      , color color'
      ]
    <> fontStyle' TypoGraphy

rateCardView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
rateCardView push state =
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        ]
        [ RateCard.view (push <<< RateCardAction) (rateCardConfig state.data.rateCard state.data.config.rateCardScreen.showTollCharges state.data.config.rateCardScreen.showDriverAdditions) ]

rateCardBannerView :: forall w. (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
rateCardBannerView push state = 
  PrestoAnim.animationSet [ Anim.fadeIn state.props.rateCardLoaded ] $
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ Margin 16 0 16 16
  , padding $ Padding 16 16 16 16
  , stroke $ "1," <> Color.grey900
  , gravity CENTER
  , cornerRadius 8.0
  , rippleColor Color.rippleShade
  , gradient gradientColors
  , onClick push $ const $ RateCardBannerClick
  , visibility $ MP.boolToVisibility state.props.rateCardLoaded
  ][  relativeLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      ][  textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , color Color.white900
          , background bgColor
          , padding $ Padding 8 1 8 2
          , gravity CENTER
          , cornerRadius cornerRad
          , text txt
          ] <> FontStyle.body1 TypoGraphy
        , shimmerFrameLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , visibility $ MP.boolToVisibility $ peakTime
          , cornerRadius 8.0
          , alpha if EHC.os == "IOS" then 0.2 else 1.0
          ][  textView $
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , color Color.white900
              , background bgColor
              , padding $ Padding 8 1 8 2
              , gravity CENTER
              , cornerRadius cornerRad
              , text txt
              , visibility $ if EHC.os == "IOS" then VISIBLE else INVISIBLE
              ] <> FontStyle.body1 TypoGraphy
          ]
      ]
  , textView $
    [ height WRAP_CONTENT
    , color Color.black800
    , margin $ Margin 6 0 0 1
    , text $ (HU.appName true) <> " " <> getString RATE_CARD
    ]
    <> FontStyle.subHeading1 TypoGraphy
  , linearLayout[weight 1.0][]
  , imageView
    [ width $ V 18
    , height $ V 18
    , imageWithFallback $ fetchImage FF_ASSET "ny_ic_arrow_right"
    ]
  ]
  where peakTime = state.props.peakTime
        bgColor = if peakTime then Color.green900 else state.data.config.bookingPreferencesConfig.primaryToggleBackground
        cornerRad = if peakTime then 8.0 else 24.0
        txt = if peakTime then "↑  Peak" else CP.getCurrency CS.appConfig
        gradientColors = (Linear 90.0 state.data.config.bookingPreferencesConfig.rateCardGradient)