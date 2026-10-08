{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ModifyScreenState where

import Accessor (_lat, _lon)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState)
import Presto.Core.Types.Language.Flow (Flow(..), modifyState)
import Prelude
import Resources.Constants (encodeAddress, getAddressFromBooking)
import Screens.HomeScreen.ScreenData (initData) as HomeScreen
import Screens.Types (MyRidesScreenState, Stage(..), Trip(..), LocationSelectType(..), HomeScreenState)
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Common.Types.App as CTA
import Control.Monad.Trans.Class (lift)

modifyScreenState :: ScreenType -> FlowBT String Unit
modifyScreenState st = void $ lift $ lift $ modifyScreenStateFlow st

modifyScreenStateFlow :: ScreenType -> Flow GlobalState GlobalState
modifyScreenStateFlow st =
  case st of
    TicketingScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state{ ticketingScreen = a state.ticketingScreen}) 
    ChooseLanguageScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { chooseLanguageScreen = a state.chooseLanguageScreen})
    EnterMobileNumberScreenType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { enterMobileNumberScreen = a state.enterMobileNumberScreen})
    AccountSetUpScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { accountSetUpScreen = a state.accountSetUpScreen})
    HomeScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {homeScreen = a state.homeScreen})
    TripDetailsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {tripDetailsScreen = a state.tripDetailsScreen})
    MyRideScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {myRidesScreen = a state.myRidesScreen})
    HelpAndSupportScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state { helpAndSupportScreen = a state.helpAndSupportScreen})
    InvoiceScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {invoiceScreen = a state.invoiceScreen})
    SelectLanguageScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {selectLanguageScreen = a state.selectLanguageScreen})
    AddNewAddressScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {addNewAddressScreen = a state.addNewAddressScreen})
    MyProfileScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {myProfileScreen = a state.myProfileScreen})
    ContactUsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {contactUsScreen = a state.contactUsScreen})
    SavedLocationScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {savedLocationScreen = a state.savedLocationScreen})
    ReferralScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {referralScreen = a state.referralScreen})
    EmergencyContactsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {emergencyContactsScreen = a state.emergencyContactsScreen})
    TicketBookingScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {ticketBookingScreen = a state.ticketBookingScreen})
    PermissionScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {permissionScreen = a state.permissionScreen})
    AboutUsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {aboutUsScreen = a state.aboutUsScreen})
    AppUpdatePopUpScreenType a->  modifyState (\(GlobalState state) -> GlobalState $ state { appUpdatePopUpScreen = a state.appUpdatePopUpScreen })
    TicketInfoScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {ticketInfoScreen = a state.ticketInfoScreen})
    FollowRideScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {followRideScreen = a state.followRideScreen})
    AppConfigType a->  modifyState (\(GlobalState state) -> GlobalState $ state { appConfig = a state.appConfig })
    RideScheduledScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {rideScheduledScreen = a state.rideScheduledScreen})
    SearchLocationScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {searchLocationScreen = a state.searchLocationScreen})
    GlobalPropsType a -> modifyState (\(GlobalState state) -> GlobalState $ state {globalProps = a state.globalProps})
    NammaSafetyScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {nammaSafetyScreen = a state.nammaSafetyScreen})
    RentalScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state { rentalScreen = a state.rentalScreen })
    RideSelectionScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {rideSelectionScreen = a state.rideSelectionScreen})
    ReportIssueChatScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {reportIssueChatScreen = a state.reportIssueChatScreen})
    SelectFaqScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {selectFaqScreen = a state.selectFaqScreen})
    FaqScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {faqScreen = a state.faqScreen})
    MetroTicketDetailsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {metroTicketDetailsScreen = a state.metroTicketDetailsScreen})
    MetroMyTicketsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {metroMyTicketsScreen = a state.metroMyTicketsScreen})
    MetroTicketBookingScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {metroTicketBookingScreen = a state.metroTicketBookingScreen})
    MetroTicketStatusScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {metroTicketStatusScreen = a state.metroTicketStatusScreen})
    GlobalFlowCacheType a -> modifyState (\(GlobalState state) -> GlobalState $ state {globalFlowCache = a state.globalFlowCache})
    PickupInstructionsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {pickupInstructionsScreen = a state.pickupInstructionsScreen})
    DriverProfileScreenCommonStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {riderDriverProfileScreen = a state.riderDriverProfileScreen})
    RiderRideCompletedScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {riderRideCompletedScreen = a state.riderRideCompletedScreen})
    DataFetchScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {dataExplainWithFetch = a state.dataExplainWithFetch})
    FavouriteDriverTripsStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {favouriteDriverListScreen = a state.favouriteDriverListScreen})
    ParcelDeliveryScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {parcelDeliveryScreen = a state.parcelDeliveryScreen})
    RideSummaryScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {rideSummaryScreen = a state.rideSummaryScreen})
    SelectContactsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {selectContactsScreen = a state.selectContactsScreen})
    BusTicketBookingScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {busTicketBookingScreen = a state.busTicketBookingScreen})
    BusTrackingScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {busTrackingScreen = a state.busTrackingScreen})
    AadhaarVerificationScreenType a -> modifyState (\(GlobalState state) -> GlobalState $ state { aadhaarVerificationScreen = a state.aadhaarVerificationScreen })
    SelectBusRouteScreenType a -> modifyState (\(GlobalState state) -> GlobalState $ state { selectBusRouteScreen = a state.selectBusRouteScreen })
    ReferralPayoutScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state { referralPayoutScreen = a state.referralPayoutScreen })

    
updateRepeatRideDetails :: Trip -> FlowBT String Unit
updateRepeatRideDetails state = do 
  (GlobalState globalState) <- getState
  modifyScreenState $ HomeScreenStateType 
    (\homeScreen -> HomeScreen.initData{ 
    data {
      source =  state.source
    , destination = state.destination
    , locationList = homeScreen.data.locationList
    , sourceAddress = state.sourceAddress
    , savedLocations = homeScreen.data.savedLocations
    , destinationAddress = state.destinationAddress 
    , famousDestinations = homeScreen.data.famousDestinations
    , manuallySharedFollowers = homeScreen.data.manuallySharedFollowers
    , settingSideBar {
        gender = globalState.homeScreen.data.settingSideBar.gender 
      , email = globalState.homeScreen.data.settingSideBar.email
      , hasCompletedSafetySetup = globalState.homeScreen.data.settingSideBar.hasCompletedSafetySetup
    }
    }
    , props{
      rideSearchProps { sourceSelectType = REPEAT_RIDE }
    , sourceLat = state.sourceLat
    , sourceLong = state.sourceLong
    , destinationLat = state.destLat
    , destinationLong = state.destLong
    , currentStage = FindingEstimate
    , rideRequestFlow = true
    , isSpecialZone = state.isSpecialZone
    , isBanner = globalState.homeScreen.props.isBanner
    , sosBannerType = globalState.homeScreen.props.sosBannerType 
    , followsRide = globalState.homeScreen.props.followsRide
    , suggestedRideFlow = globalState.homeScreen.props.suggestedRideFlow
    }
    })

updateSafetyScreenState :: HomeScreenState -> Int -> Boolean -> Boolean -> FlowBT String Unit
updateSafetyScreenState state defaultTimerValue showtestDrill triggerSos = do
  modifyScreenState
        $ NammaSafetyScreenStateType
            ( \nammaSafetyScreen ->
                nammaSafetyScreen
                  { props
                    { triggeringSos = false
                    , timerValue = defaultTimerValue
                    , showTestDrill = showtestDrill
                    , showShimmer = true
                    , confirmTestDrill = false
                    , isSafetyCenterDisabled = state.props.isSafetyCenterDisabled
                    , checkPastRide = state.props.currentStage == HomeScreen
                    , isAudioRecordingActive = false
                    , showCallPolice = if triggerSos then state.props.isSafetyCenterDisabled else false
                    , showMenu = false
                    , recordedAudioUrl = Nothing
                    , audioRecordingStatus = CTA.NOT_RECORDING
                    , recordingTimer = "00 : 00"
                    , defaultCallPopup = false
                    }
                  , data
                    { rideId = state.data.driverInfoCardState.rideId
                    , vehicleDetails = state.data.driverInfoCardState.registrationNumber
                    }
                  }
            )

data FlowState = HelpAndSupportScreenFlow 
               | IssueReportChatScreenFlow
               | RideSelectionScreenFlow
               | SelectFaqScreenFlow
               | FaqScreenFlow
               | HomeScreenFlow
               | ActivateSafetyScreenFlow
               | TripDetailsScreenFlow
               | ContactUsScreenFlow
               | MyRidesScreenFlow
               | GoToFavouritesScreenFlow
               | ChangeLanguageScreenFlow
               | RiderRideCompleted
               | RiderRideEndScreen
               | DeleteAccountFlow