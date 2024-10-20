module Screens.MultiModalFlow.Components.VehicleCard where

import Common.Types.App (LazyCheck(..))
import Data.Array (take, (!!))
import Data.Maybe (fromMaybe, Maybe(..))
import Prelude
import PrestoDOM
import Screens.Types (NewContacts, VehicleViewType(..))
import Styles.Types (Color)
import Data.String as DS
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Screens.NammaSafetyFlow.Components.HelperViews as HV
import Font.Style as FontStyle
import Helpers.Utils as HU
import Mobility.Prelude (boolToVisibility, layoutWithWeight)
import Styles.Colors as Color
import Services.API as API
import Screens.EmergencyContactsScreen.ScreenData (neverShareRideOption)
import Data.Maybe (Maybe(..))
import Components.ChooseVehicle as ChooseVehicle
import Components.PrimaryButton as PrimaryButton
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.Animation as PrestoAnim
import Animation (translateOutXBackwardAnimY)
import Animation.Config as AnimConfig

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 12 12 12 12
    , orientation VERTICAL
    , cornerRadius 12.0
    , stroke $ "1," <> Color.grey900
    ]
    [ ChooseVehicle.view (push <<< ChooseSingleVehicleAction) (chooseVehicleConfig config)
    , separatorView Color.grey700 (MarginVertical 12 12)
    , specialAlertToggleView push config
    , requestRideButtonView push config
    ]

specialAlertToggleView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
specialAlertToggleView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
        , gravity CENTER_VERTICAL
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , weight 1.0
        , cornerRadius 25.0
        , stroke $ "1," <> Color.grey700
        , gravity CENTER_VERTICAL
        , padding $ Padding 16 8 8 8
        ]
        [ textView $ [ text "Smart Alert" ] <> FontStyle.tags TypoGraphy
        , imageView
            [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_info_black"
            , height $ V 12
            , width $ V 12
            , margin $ MarginLeft 4
            ]
        , layoutWithWeight
        , toggleSwitchView true ExpandStops push
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , cornerRadius 25.0
        , stroke $ "1," <> Color.grey700
        , margin $ MarginLeft 12
        ]
        [ imageView
            [ imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_setting_unfilled"
            , height $ V 20
            , width $ V 20
            ]
        ]
    ]

requestRideButtonView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
requestRideButtonView push state =
  let 
    animationDuration = 5 * 1000 - 100
    -- isRepeatRideTimerNonZero = state.props.repeatRideTimer /= "0"
  in
  relativeLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    ] 
    [ PrimaryButton.view (push <<< PrimaryButtonActionController) (confirmAndBookButtonConfig state)
    , PrestoAnim.animationSet
        [ translateOutXBackwardAnimY AnimConfig.animConfig
            { duration = animationDuration
            , toX = (EHC.screenWidth unit)
            , fromY = 0
            , ifAnim = true
            }
        ]  
        $ linearLayout
            [ height $ V 50
            , width MATCH_PARENT
            , alpha 0.5
            , background Color.white900
            -- , visibility $ boolToVisibility (not DS.null state.props.repeatRideTimerId)
            , margin $ MarginTop 16
            ][]
    ]

confirmAndBookButtonConfig :: Config -> PrimaryButton.Config
confirmAndBookButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = getBtnTextWithTimer "state"
      , color = Color.yellow900
      , accessibilityHint = "Confirm And Book Button"
      }
    , id = "ConfirmAndBookButton"
    , background = Color.black900
    , margin = MarginTop 16
    , enableRipple = true
    , rippleColor = Color.rippleShade
    }
  where
  getBtnTextWithTimer _ = (getString REQUESTING_RIDE_IN) <> " " <> "5" <> "s"
    -- if state.props.currentStage == RevisedEstimate then
    --   (getString REQUEST_CHANGE)
    -- else
    --   if state.props.repeatRideTimer /= "0" && not DS.null state.props.repeatRideTimerId then
    --     ((getString REQUESTING_RIDE_IN) <> " " <> state.props.repeatRideTimer <> "s")
    --   else if state.props.repeatRideTimer == "0" then
    --     (getString REQUESTING_RIDE) <> "..."
    --   else
    --     (getString REQUEST_RIDE)

chooseVehicleConfig :: Config -> ChooseVehicle.Config
chooseVehicleConfig state =
  let
    config = ChooseVehicle.config

    --   selectedEstimates = state.data.selectedEstimatesObject
    chooseVehicleConfig' =
      config
        { vehicleImage = HU.getVehicleVariantImage "AUTO_RICKSHAW" RIGHT_VIEW
        , isSelected = false
        , vehicleVariant = "AUTO_RICKSHAW"
        , vehicleType = "selectedEstimates.vehicleType"
        , capacity = "4" -- selectedEstimates.capacity
        , price = "40" --selectedEstimates.price
        , isCheckBox = false
        , isEnabled = true
        , index = 0 --selectedEstimates.index
        , activeIndex = 0 --selectedEstimates.activeIndex
        , singleVehicle = true
        -- , id = selectedEstimates.id
        -- , maxPrice = selectedEstimates.maxPrice
        -- , basePrice = selectedEstimates.basePrice
        -- , showInfo = selectedEstimates.showInfo
        -- , searchResultType = selectedEstimates.searchResultType
        -- , isBookingOption = false
        -- , pickUpCharges = selectedEstimates.pickUpCharges 
        -- , layoutMargin = Margin 0 0 0 0
        -- , tollCharge = selectedEstimates.tollCharge
        -- , serviceTierName = selectedEstimates.serviceTierName
        -- , serviceTierShortDesc = selectedEstimates.serviceTierShortDesc
        -- , airConditioned = selectedEstimates.airConditioned
        -- , extraFare = selectedEstimates.extraFare
        -- , fareInfoDescription = selectedEstimates.fareInfoDescription
        -- , isNightShift = selectedEstimates.isNightShift
        -- , nightChargeTill = selectedEstimates.nightChargeTill
        -- , nightChargeFrom = selectedEstimates.nightChargeFrom
        -- , driverAdditions = selectedEstimates.driverAdditions
        -- , showEditButton = true
        -- , editBtnText = getString CHANGE
        -- , validTill = selectedEstimates.validTill
        -- , hasTollCharges = selectedEstimates.hasTollCharges
        -- , hasParkingCharges = selectedEstimates.hasParkingCharges
        }
  in
    chooseVehicleConfig'

separatorView :: forall w. String -> Margin -> PrestoDOM (Effect Unit) w
separatorView color' margin' =
  linearLayout
    [ height (V 1)
    , width MATCH_PARENT
    , margin margin'
    , background color'
    ]
    []

--   let tagConfig = specialZoneTagConfig state.props.zoneType.priorityTag
--       showTag = any (_ == state.props.zoneType.priorityTag) [SPECIAL_PICKUP, METRO]
--   in
--   linearLayout
--   [ orientation VERTICAL
--   , height WRAP_CONTENT
--   , width MATCH_PARENT
--   , background tagConfig.backgroundColor
--   , clickable true
--   , stroke ("1," <> Color.grey900)
--   , gravity CENTER
--   , cornerRadii $ Corners 24.0 true true false false
-- --   , afterRender
-- --         ( \action -> do            
-- --             let fareEstimate = if state.data.rateCard.additionalFare == 0 then "₹" <> (show state.data.suggestedAmount) else  "₹" <> (show state.data.suggestedAmount) <> "-" <> "₹" <> (show $ (state.data.suggestedAmount + state.data.rateCard.additionalFare))
-- --             _ <- pure $  setValueToLocalStore FARE_ESTIMATE_DATA fareEstimate
-- --             pure unit
-- --         )
-- --         (const NoAction)
--   ][  
--      linearLayout
--       [ orientation VERTICAL
--       , height WRAP_CONTENT
--       , width MATCH_PARENT
--       , background Color.white900
--       , clickable true
--       , accessibility if state.props.showRateCard then DISABLE_DESCENDANT else DISABLE
--       , visibility if (state.props.currentStage == SettingPrice) then VISIBLE else GONE
--       , padding (Padding 16 7 16 24)
--       , stroke ("1," <> Color.grey900)
--       , gravity CENTER
--       , cornerRadii $ Corners 24.0 true true false false
--       ][ --estimateHeaderView push state
--         -- , 
--         linearLayout
--           [ width MATCH_PARENT
--           , height WRAP_CONTENT
--           , orientation VERTICAL
--           , gravity CENTER
--           , cornerRadius 8.0
--           , margin $ MarginTop 16
--           ][ rideDetailsViewV2 push state]
--         , sourceDestinationDetailsView push state
--         , requestRideButtonView push state
--         , linearLayout
--             [ width MATCH_PARENT
--             , height WRAP_CONTENT
--             , gravity CENTER
--             , margin $ MarginTop 24
--             , visibility if state.props.isRepeatRide && not DS.null state.props.repeatRideTimerId then VISIBLE else GONE
--             ][ textView $
--                 [ textFromHtml $ "<u>" <> (getString TAP_HERE_TO_STOP_AUTO_REQUESTING) <> "</u>" 
--                 , color Color.black700
--                 ] <> FontStyle.body1 LanguageStyle
--             ]
--       ]
--   ]
-- estimateHeaderView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
-- estimateHeaderView push state =
--   linearLayout
--     [ height WRAP_CONTENT
--     , width MATCH_PARENT
--     , orientation VERTICAL
--     , gravity CENTER_HORIZONTAL
--     ]
--     [ textView $
--         [ text$ getString CONFIRM_YOUR_RIDE
--         , color Color.black800
--         , gravity CENTER_HORIZONTAL
--         , height WRAP_CONTENT
--         , width MATCH_PARENT
--         , accessibility ENABLE
--         ] 
--         <> FontStyle.h1 TypoGraphy
--     -- , estimatedTimeDistanceView push state
--     -- , separatorView 
--     ]
-- estimatedTimeDistanceView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
-- estimatedTimeDistanceView push state =
--   linearLayout
--     [ width MATCH_PARENT
--     , height WRAP_CONTENT
--     , gravity CENTER
--     , margin $ MarginTop 4
--     , accessibility ENABLE
--     , accessibilityHint $ "Estimated distance is : " <> state.data.rideDistance <> "and Estimated time is : " <> state.data.rideDuration
--     ]
--     [ createTextView state.data.rideDistance
--     , linearLayout
--         [ height $ V 4
--         , width $ V 4
--         , cornerRadius 2.5
--         , background Color.black600
--         , margin (Margin 6 2 6 0)
--         ]
--         []
--     , createTextView state.data.rideDuration
--     ]
--   where
--     createTextView :: String -> PrestoDOM (Effect Unit) w
--     createTextView textContent =
--       textView $
--         [ height WRAP_CONTENT
--         , width WRAP_CONTENT
--         , text textContent
--         , color Color.black650
--         ]
--         <> FontStyle.paragraphText TypoGraphy
data Action
  = ExpandStops
  | ChooseSingleVehicleAction ChooseVehicle.Action
  | PrimaryButtonActionController PrimaryButton.Action

type Config
  = { routeTagColor :: String
    , routeName :: String
    , details :: String
    , time :: String
    , destinationDetails :: String
    , expandStops :: Boolean
    , stops :: Array String
    , price :: String
    , transportFrequency :: String
    , timeInterval :: String
    , duration :: String
    , numberOfStops :: Int
    }

dummyRouteInfo :: Config
dummyRouteInfo =
  { routeTagColor: "red"
  , routeName: "Bus 42"
  , details: "Fastest route with minimum stops"
  , time: "14:30"
  , destinationDetails: "Central Station"
  , expandStops: true
  , stops: [ "Main St" ]
  , price: "$2.50"
  , transportFrequency: "Every 15 minutes"
  , timeInterval: "14:30 - 15:00"
  , duration: "30 minutes"
  , numberOfStops: 5
  }

dotView :: forall w. PrestoDOM (Effect Unit) w
dotView =
  imageView
    [ imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_circle"
    , height $ V 4
    , width $ V 4
    , margin $ MarginHorizontal 4 4
    ]

toggleSwitchView :: Boolean -> Action -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
toggleSwitchView isActive action push =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , onClick push $ const action --ToggleSwitch stage
    , accessibilityHint $ if isActive then "Toggle Button Enabled" else "toggle button disabled"
    ]
    [ imageView
        [ imageUrl if isActive then "ny_ic_switch_filled_blue" else "ny_ic_switch_inactive"
        , width $ V 40
        , height $ V 24
        ]
    ]