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
import Data.Array as DA
import LocalStorage.Cache (getValueFromCache)
import Storage
import Engineering.Helpers.Suggestions
import Screens.Types as ST

primaryButtonConfig :: FollowRideScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = (getString DONE)
      , color = state.data.config.primaryTextColor
      }
    , id = "DonePrimaryButton"
    , margin = Margin 16 32 16 24
    , background = state.data.config.primaryButtonBackground
    }

type SOSOverlayConfig
  = { title :: String
    , subTitle :: String
    , color :: String
    }

getSosOverlayConfig :: FollowRideScreenState -> Common.SosStatus -> SOSOverlayConfig
getSosOverlayConfig state status =
  let
    isSOS = status == Common.Pending || status == Common.MockPending
    currentFollower = getCurrentFollower state.data.currentFollower
    name = getFollowerName currentFollower state
  in
    { title: if isSOS then if isMockDrill state then getString $ THIS_IS_A_TEST_MOCK_DRILL name else getString $ IS_IN_SOS_SITUATION name else getString $ MARKED_RIDE_SAFE name
    , subTitle: if isMockDrill state then getString THIS_IS_NOT_REAL_DRILL else if isSOS then getString $ STAY_CALM_KEEP_TRACKING name else getString YOU_WILL_BE_NOTIFIED
    , color: if isSOS then Color.red900 else Color.green900
    }


genericHeaderConfig :: FollowRideScreenState -> GenericHeader.Config
genericHeaderConfig state =
  let
    isFollowingRide = state.data.currentStage == FollowingRide

    currentFollower = getCurrentFollower state.data.currentFollower

    titleText = if state.data.currentStage == PersonList then getString CHOOSE_A_PERSON_TO_FOLLOW else getString $ FOLLOWING (getFollowerName currentFollower state)
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
    { fareProductType : ride.fareProductType
    , rating: ride.rating
    , driverName: ride.driverName
    , vehicleDetails: ride.vehicleDetails
    , vehicleVariant: ride.vehicleVariant
    , merchantCity: state.props.city
    , registrationNumber: ride.registrationNumber
    , config: state.data.config
    , rideStarted: true
    , enablePaddingBottom: true
    , vehicleModel : ride.vehicleModel
    , vehicleColor : ride.vehicleColor
    , serviceTierName : ride.serviceTierName
    , providerType : Common.ONUS
    , showAcView : false
    , isOtpRideFlow : false
    , isAirConditioned : ride.isAirConditioned
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
    , enablePaddingBottom : false
    , fareProductType : ride.fareProductType
    , enableEditDestination : false
    , editingDestinationLoc : NoAction
    , isOtpRideFlow : false
    , rideAccepted : false
    , editingPickupLocation : NoAction
    , isEditPickupEnabled : false
    , senderDetails : Nothing
    , receiverDetails : Nothing
    }

getCurrentFollower :: Maybe Followers -> Followers
getCurrentFollower = fromMaybe dummyFollower

getFollowerName :: Followers -> FollowRideScreenState -> String
getFollowerName currentFollower state = if state.data.currentStage == MockFollowRide then getMockFollowerName "" else (fromMaybe currentFollower.mobileNumber currentFollower.name)

getChatSuggestions :: FollowRideScreenState -> Array String
getChatSuggestions state = do 
  let lastMessage = DA.last state.data.messages
      didEmMessage = didReceiverMessage Common.FunctionCall
      canShowSuggestions = case lastMessage of 
                            Just value -> (value.sentBy /= (getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys))
                            Nothing -> true
  if (DA.null state.data.chatSuggestionsList) && state.props.canSendSuggestion 
    then do 
      let hideInitial = not $ DA.null state.data.messages
      if didEmMessage && hideInitial then
        getSuggestionsfromKey emChatSuggestion "31e3bbf96e4b4208f1328f5b0da57d2e"
      else if hideInitial 
        then state.data.chatSuggestionsList
        else getSuggestionsfromKey emChatSuggestion "31e3bbf96e4b4208f1328f5b0da57d2e"
    else state.data.chatSuggestionsList