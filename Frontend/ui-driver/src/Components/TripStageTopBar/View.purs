module Components.TripStageTopBar.View where


import Helpers.Utils as HU
import Font.Style as FontStyle
import Data.Maybe
import Services.API (BookingTypes(..))
import Mobility.Prelude (boolToVisibility)
import Components.TripStageTopBar.Controller (Action(..), Config)
import Data.Array as DA
import Screens.Types (HomeScreenStage(..))
import Storage (isLocalStageOn)
import Data.Tuple

import Common.Types.App
import ConfigProvider
import Effect (Effect)
import Language.Strings (getString)
import Language.Types (STR(..))

import Prelude (Unit, const, map, not, (||), ($), (-), unit, (*), (/), (+), (<>), (==), show, (&&), (/=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), alpha, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, padding, scrollView, text, textSize, textView, weight, width, visibility, stroke, imageWithFallback, horizontalScrollView, scrollBarX)
import Styles.Colors as Color



view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
    horizontalScrollView[
    width MATCH_PARENT,
    height WRAP_CONTENT,
    scrollBarX false,
    background Color.white900,
    visibility $ boolToVisibility $ DA.any (_ == config.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer] && config.data.cityConfig.enableAdvancedBooking && (isJust config.data.advancedRideData || not (isLocalStageOn RideAccepted && isJust config.data.activeRide.disabilityTag)) && not (HU.isAmbulance config.data.linkedVehicleVariant)
    ][
    linearLayout[
    width MATCH_PARENT,
    height WRAP_CONTENT,
    background Color.white900,
    margin $ MarginRight 16,
    gravity CENTER_VERTICAL
    ] $ [ advanceBookingSwitch] 
    <> ( 
        map (\(Tuple childs action) -> (tripStageTopBarPill action) childs) [
            -- [
            --   pillIcon  "ny_ic_blue_shield_white_plus",
            --   pillText  "Safety Center"
            -- ],
        ( Tuple [ pillIcon "ny_ic_red_triangle_warning",
            pillText $ getString REPORT_ISSUE
            ] (const HelpAndSupportScreen))
        ]
        )
    ]
    where 
        tripStageTopBarPill action = 
            linearLayout [
            width WRAP_CONTENT,
            height WRAP_CONTENT,
            padding $ Padding 16 16 16 16,
            margin $ Margin 12 12 0 12,
            cornerRadius 32.0,
            background Color.blue600,
            onClick push action
            ] 

        pillIcon imgStr = 
            imageView [
            width $ V 20,
            height $ V 20,
            imageWithFallback $ HU.fetchImage HU.FF_ASSET imgStr
            ]

        pillText str = 
            textView $ [
            text str,
            color Color.blue900,
            margin $ MarginLeft 8
            ] <> FontStyle.body6 TypoGraphy

        advanceBookingSwitch = 
            linearLayout [
            width WRAP_CONTENT,
            height WRAP_CONTENT,
            margin $ Margin 12 12 0 12,
            cornerRadius 32.0,
            background $ if isNothing config.data.advancedRideData then Color.grey700 else Color.blue600,
            padding $ Padding 4 4 4 4 
            ]$[ swichBtn (getString CURRENT_BUTTON_TEXT) CURRENT false $ config.props.bookingStage /= CURRENT
            , swichBtn (getString ADVANCE) ADVANCED (isNothing config.data.advancedRideData) (config.props.bookingStage /= ADVANCED)
            ]

        swichBtn txt stage isDisabled switchAllowed = 
            textView $ [
            text $ txt,
            color $ 
                case config.props.bookingStage == stage, isDisabled of
                true, _ -> Color.aliceBlueLight
                false, true -> Color.black700
                false, false -> Color.blue900,
            background $ 
                case config.props.bookingStage == stage, isDisabled of
                true, _ -> Color.blue900
                false, true -> Color.grey700
                false, false -> Color.blue600,
            padding $ Padding 12 12 12 12,
            cornerRadius 32.0,
            alpha $ if isDisabled then 0.5 else 1.0
            ] <> FontStyle.body6 TypoGraphy
            <> if isDisabled && switchAllowed then [] else [onClick push $ const $ SwitchBookingStage stage]
