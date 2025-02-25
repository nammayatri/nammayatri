{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Types.ModifyScreenState where

import Debug (spy)
import Engineering.Helpers.BackTrack (modifyState)
import Helpers.Utils (generateUniqueId)
import JBridge (removeAllPolylines, enableMyLocation)
import Prelude (Unit, ($), show, discard, unit, pure, bind)
import Screens.HomeScreen.ScreenData (initData) as HomeScreenData
import Screens.Types (HomeScreenStage(..))
import Storage (KeyStore(..), setValueToLocalStore, updateLocalStage)
import Types.App (FlowBT, GlobalState(..), ScreenType(..), ScreenStage(..))

modifyScreenState :: ScreenType -> FlowBT String Unit
modifyScreenState st =
  case st of
    DocumentCaptureScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state{ documentCaptureScreen = a state.documentCaptureScreen}) 
    SplashScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { splashScreen = a state.splashScreen})
    ChooseLanguageScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { chooseLanguageScreen = a state.chooseLanguageScreen})
    DriverProfileScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { driverProfileScreen = a state.driverProfileScreen})
    ApplicationStatusScreenType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { applicationStatusScreen = a state.applicationStatusScreen})
    EnterMobileNumberScreenType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { mobileNumberScreen = a state.mobileNumberScreen})
    EnterOTPScreenType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { enterOTPScreen = a state.enterOTPScreen})
    UploadDrivingLicenseScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { uploadDrivingLicenseScreen = a state.uploadDrivingLicenseScreen})
    RegisterScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { registrationScreen = a state.registrationScreen})
    UploadAdhaarScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { uploadAdhaarScreen = a state.uploadAdhaarScreen})
    AddVehicleDetailsScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { addVehicleDetailsScreen = a state.addVehicleDetailsScreen})
    RideHistoryScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { rideHistoryScreen = a state.rideHistoryScreen})
    ReportIssueChatScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { reportIssueChatScreen = a state.reportIssueChatScreen})
    RideSelectionScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {rideSelectionScreen = a state.rideSelectionScreen})
    BankDetailScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { bankDetailsScreen = a state.bankDetailsScreen})
    DriverDetailsScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { driverDetailsScreen = a state.driverDetailsScreen})
    VehicleDetailsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {vehicleDetailsScreen = a state.vehicleDetailsScreen})
    AboutUsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {aboutUsScreen = a state.aboutUsScreen})
    SelectLanguageScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {selectedLanguageScreen = a state.selectedLanguageScreen})
    HelpAndSupportScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {helpAndSupportScreen = a state.helpAndSupportScreen})
    WriteToUsScreenStateType  a -> modifyState (\(GlobalState state) -> GlobalState $ state {writeToUsScreen = a state.writeToUsScreen})
    PermissionsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {permissionsScreen = a state.permissionsScreen})
    HomeScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { homeScreen = a state.homeScreen})
    EditBankDetailsScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { editBankDetailsScreen = a state.editBankDetailsScreen})
    EditAadhaarDetailsScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { editAadhaarDetailsScreen = a state.editAadhaarDetailsScreen})
    TripDetailsScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { tripDetailsScreen = a state.tripDetailsScreen})
    PopUpScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state { popUpScreen = a state.popUpScreen })
    DriverRideRatingScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { driverRideRatingScreen = a state.driverRideRatingScreen})
    NotificationsScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { notificationScreen = a state.notificationScreen})
    ReferralScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state { referralScreen = a state.referralScreen })
    BookingOptionsScreenType a -> modifyState (\(GlobalState state) -> GlobalState $ state { bookingOptionsScreen = a state.bookingOptionsScreen })
    AppUpdatePopUpScreenType a->  modifyState (\(GlobalState state) -> GlobalState $ state { appUpdatePopUpScreen = a state.appUpdatePopUpScreen })
    AcknowledgementScreenType a -> modifyState (\(GlobalState state) -> GlobalState $ state { acknowledgementScreen = a state.acknowledgementScreen })
    AadhaarVerificationScreenType a -> modifyState (\(GlobalState state) -> GlobalState $ state { aadhaarVerificationScreen = a state.aadhaarVerificationScreen })
    GlobalPropsType a -> modifyState (\(GlobalState state) -> GlobalState $ state { globalProps = a state.globalProps })
    SubscriptionScreenStateType a ->  modifyState (\(GlobalState state) -> GlobalState $ state { subscriptionScreen = a state.subscriptionScreen })
    OnBoardingSubscriptionScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state { onBoardingSubscriptionScreen = a state.onBoardingSubscriptionScreen })
    PaymentHistoryScreenStateType a ->  modifyState (\(GlobalState state) -> GlobalState $ state { paymentHistoryScreen = a state.paymentHistoryScreen })
    DriverSavedLocationScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {driverSavedLocationScreen = a state.driverSavedLocationScreen})
    ChooseCityScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {chooseCityScreen = a state.chooseCityScreen})
    WelcomeScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {welcomeScreen = a state.welcomeScreen})
    BenefitsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {benefitsScreen = a state.benefitsScreen})
    RegistrationScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {registrationScreen = a state.registrationScreen})
    DriverEarningsScreenStateType a ->  modifyState (\(GlobalState state) -> GlobalState $ state { driverEarningsScreen = a state.driverEarningsScreen })
    LmsVideoScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {lmsVideoScreen = a state.lmsVideoScreen})
    LmsQuizScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {lmsQuizScreen = a state.lmsQuizScreen})
    DocumentDetailsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {documentDetailsScreen = a state.documentDetailsScreen})
    DriverCompleteProfileScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {driverCompleteProfileScreen = a state.driverCompleteProfileScreen})
    RateCardScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {rateCardScreen = a state.rateCardScreen})
    CustomerReferralTrackerScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {customerReferralTrackerScreen = a state.customerReferralTrackerScreen})
    CancellationRateScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {cancellationRateScreen = a state.cancellationRateScreen})
    HotspotScreenStateType a ->  modifyState (\(GlobalState state) -> GlobalState $ state { hotspotScreen = a state.hotspotScreen })
    RideRequestScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {rideRequestScreen = a state.rideRequestScreen})
    RideSummaryScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {rideSummaryScreen = a state.rideSummaryScreen})
    ScheduleRideAcceptedScreenStateType a  -> modifyState (\(GlobalState state) -> GlobalState $ state {scheduledRideAcceptedScreen = a state.scheduledRideAcceptedScreen})
    UploadParcelImageScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {uploadParcelImageScreen = a state.uploadParcelImageScreen})
    MetroWarriorsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {metroWarriorsScreen = a state.metroWarriorsScreen})
    MeterScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {meterScreen = a state.meterScreen})

updateStage :: ScreenStage -> FlowBT String Unit
updateStage stage = do
  case stage of
    HomeScreenStage stage' -> do
      _ <- pure $ enableMyLocation true
      _ <- pure $ updateLocalStage stage'
      _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING_ID (generateUniqueId unit)
      _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING "False"
      _ <- pure $ spy "UpdateStage" stage'
      case stage' of
        RideRequested ->
          modifyScreenState $
            HomeScreenStateType
              (\state ->
                state
                {
                  props
                  {
                    currentStage = stage'
                  }
                })
        RideAccepted -> do
          modifyScreenState $
            HomeScreenStateType
              (\state ->
                state
                { props
                  {
                    rideActionModal = true,
                    routeVisible = false,
                    currentStage = stage'
                  }
                })
        RideStarted -> do
          modifyScreenState $
            HomeScreenStateType
              (\state ->
                state
                { props
                  {
                    cancelRideModalShow = false,
                    rideActionModal = true,
                    enterOtpModal = false,
                    routeVisible = false,
                    currentStage = stage'
                  }
                })
        RideCompleted ->
          modifyScreenState $
            HomeScreenStateType
              (\state ->
                state
                { props
                  {
                    rideActionModal = false,
                    enterOtpModal = false,
                    routeVisible = false,
                    showRideCompleted = true,
                    currentStage = stage'
                  }
                })
        HomeScreen -> do
          _ <- pure $ removeAllPolylines ""
          _ <- pure $ spy "Inside HomeScreen" "removeAllPolyLines"
          modifyScreenState $ HomeScreenStateType (\state -> HomeScreenData.initData { props { showParcelIntroductionPopup = state.props.showParcelIntroductionPopup } })
        ChatWithCustomer -> do
          pure unit
        _ -> modifyScreenState $ HomeScreenStateType (\state -> state { props { currentStage = stage'} })
