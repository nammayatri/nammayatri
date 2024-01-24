module Screens.FollowRideScreen.Config where

import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Screens.FollowRideScreen.Controller
import Screens.FollowRideScreen.ScreenData
import Screens.Types
import Helpers.Utils
import JBridge
import Prelude
import Data.Function.Uncurried
import Engineering.Helpers.Commons
import Data.Maybe
import Components.DriverInfoCard.Common.Types
import Screens.HomeScreen.ScreenData
import Common.Types.App as Common
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color
import PrestoDOM
import Mobility.Prelude

primaryButtonConfig :: FollowRideScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = (getString DONE)
      , color = state.data.config.primaryTextColor
      }
    , id = "DonePrimaryButton"
    , margin = Margin 16 32 16 24
    , background = state.data.config.primaryBackground
    }

type SOSOverlayConfig
  = { title :: String
    , subTitle :: String
    , color :: String
    }

getSosOverlayConfig :: FollowRideScreenState -> Common.SosStatus -> SOSOverlayConfig
getSosOverlayConfig state status =
  let
    isSOS = status == Common.Pending
    isMockDrill = state.data.currentStage == MockFollowRide
    currentFollower = getCurrentFollower state.data.currentFollower

    name = getFollowerName currentFollower state
  in
    { title: if isSOS then if isMockDrill then getString $ THIS_IS_A_TEST_MOCK_DRILL name else getString $ IS_IN_SOS_SITUATION name else getString $ MARKED_RIDE_SAFE name
    , subTitle: if isMockDrill then getString THIS_IS_NOT_REAL_DRILL else if isSOS then getString $ STAY_CALM_KEEP_TRACKING name else getString YOU_WILL_BE_NOTIFIED
    , color: if isSOS then Color.red900 else Color.green900
    }


genericHeaderConfig :: FollowRideScreenState -> GenericHeader.Config
genericHeaderConfig state =
  let
    isFollowingRide = state.data.currentStage == FollowingRide

    currentFollower = getCurrentFollower state.data.currentFollower

    titleText = if isFollowingRide then getString $ FOLLOWING (getFollowerName currentFollower state) else getString CHOOSE_A_PERSON_TO_FOLLOW
  in
    GenericHeader.config
      { height = WRAP_CONTENT
      , suffixImageConfig
        { height = V 25
        , width = V 25
        , accessibilityHint = "Close : Button"
        , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_close"
        , visibility = boolToVisibility $ isFollowingRide
        , margin = (Margin 12 12 12 12)
        }
      , textConfig
        { text = titleText
        , color = Color.black900
        , margin = MarginLeft 16
        }
      , prefixImageConfig
        { height = V 25
        , width = V 25
        , accessibilityHint = "Back : Button"
        , imageUrl = GenericHeader.config.prefixImageConfig.imageUrl
        , visibility = boolToVisibility $ not isFollowingRide
        , margin = (Margin 12 12 12 12)
        }
      }

getDriverDetails :: FollowRideScreenState -> DriverDetailsType
getDriverDetails state =
  let
    ride = fromMaybe dummyDriverInfo state.data.driverInfoCardState
  in
    { searchType: ESTIMATES
    , rating: ride.rating
    , driverName: ride.driverName
    , vehicleDetails: ride.vehicleDetails
    , vehicleVariant: ride.vehicleVariant
    , merchantCity: state.props.city
    , registrationNumber: ride.registrationNumber
    , config: state.data.config
    , rideStarted: true
    }


getTripDetails :: FollowRideScreenState -> String -> TripDetails Action
getTripDetails state color =
  let
    ride = fromMaybe dummyDriverInfo state.data.driverInfoCardState
  in
    { rideStarted: true
    , source: ride.source
    , destination: ride.destination
    , onAnimationEnd: NoAction
    , backgroundColor: color
    }

getCurrentFollower :: Maybe Followers -> Followers
getCurrentFollower = fromMaybe dummyFollower

getFollowerName :: Followers -> FollowRideScreenState -> String
getFollowerName currentFollower state = if state.data.currentStage == MockFollowRide then getMockFollowerName "" else (fromMaybe currentFollower.mobileNumber currentFollower.name)

getPeekHeight :: FollowRideScreenState -> Int
getPeekHeight _ =
  let
    srcDestViewBounds = runFn1 getLayoutBounds $ getNewIDWithTag "FollowRideSourceDestinationView"

    headerViewBounds = runFn1 getLayoutBounds $ getNewIDWithTag "FollowRideHeaderView"

    finalHeight = srcDestViewBounds.height + headerViewBounds.height

    requiredPeekHeight = getDefaultPixelSize finalHeight
  in
    if requiredPeekHeight == 0 then 300 else requiredPeekHeight