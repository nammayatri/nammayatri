{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Types.App where

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Free (Free)
import Control.Transformers.Back.Trans (BackT)
import Data.Maybe (Maybe(..))
import LoaderOverlay.ScreenData as LoaderOverlayScreenData
import Presto.Core.Types.Language.Flow (FlowWrapper)
import Screens (ScreenName(..)) as ScreenNames
import Screens.ChooseLanguageScreen.ScreenData as ChooseLanguageScreenData
import Screens.EnterMobileNumberScreen.ScreenData as EnterMobileNumberScreenData
import Screens.AadhaarVerificationScreen.ScreenData as EnterAadhaarNumberScreenData
import Screens.AboutUsScreen.ScreenData as AboutUsScreenData
import Screens.AcknowledgementScreen.ScreenData as AcknowledgementScreenData
import Screens.AddVehicleDetailsScreen.ScreenData as AddVehicleDetailsScreenData
import Screens.AppUpdatePopUpScreen.ScreenData as AppUpdatePopUpScreenData
import Screens.ApplicationStatusScreen.ScreenData as ApplicationStatusScreenData
import Screens.BankDetailScreen.ScreenData as BankDetailScreenData
import Screens.BookingOptionsScreen.ScreenData as BookingOptionsScreenData
import Screens.ChooseCityScreen.ScreenData as ChooseCityScreenData
import Screens.ChooseLanguageScreen.ScreenData as ChooseLanguageScreenData
import Screens.DriverDetailsScreen.ScreenData as DriverDetailsScreenData
import Screens.DriverProfileScreen.ScreenData as DriverProfileScreenData
import Screens.DriverRideRatingScreen.ScreenData as DriverRideRatingScreenData
import Screens.DriverSavedLocationScreen.ScreenData as DriverSavedLocationScreenData
import Screens.EditAadhaarDetailsScreen.ScreenData as EditAadhaarDetailsScreenData
import Screens.EditBankDetailsScreen.ScreenData as EditBankDetailsScreenData
import Screens.EnterMobileNumberScreen.ScreenData as EnterMobileNumberScreenData
import Screens.EnterOTPScreen.ScreenData as EnterOTPScreenData
import Screens.HelpAndSupportScreen.ScreenData as HelpAndSupportScreenData
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.NotificationsScreen.ScreenData as NotificationsScreenData
import Screens.OnBoardingSubscriptionScreen.ScreenData as OnBoardingSubscriptionScreenData
import Screens.PaymentHistoryScreen.ScreenData as PaymentHistoryScreenData
import Screens.PermissionsScreen.ScreenData as PermissionsScreenData
import Screens.PopUpScreen.ScreenData as PopUpScreenData
import Screens.ReferralScreen.ScreenData as ReferralScreenData
import Screens.RegistrationScreen.ScreenData as RegistrationScreenData
import Screens.ReportIssueChatScreen.ScreenData as ReportIssueChatScreenData
import Screens.RideHistoryScreen.ScreenData as RideHistoryScreenData
import Screens.DriverEarningsScreen.ScreenData as DriverEarningsScreenData
import Screens.RideSelectionScreen.ScreenData as RideSelectionScreenData
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData
import Screens.SubscriptionScreen.ScreenData as SubscriptionScreenData
import Screens.TripDetailsScreen.ScreenData as TripDetailsScreenData
import Screens.Types 
import Screens.UploadAdhaarScreen.ScreenData as UploadAdhaarScreenData
import Screens.UploadDrivingLicenseScreen.ScreenData as UploadDrivingLicenseScreenData
import Screens.VehicleDetailsScreen.ScreenData as VehicleDetailsScreenData
import Screens.WelcomeScreen.ScreenData as WelcomeScreenData
import Screens.WriteToUsScreen.ScreenData as WriteToUsScreenData
import Data.Maybe (Maybe(..))
import MerchantConfig.Types (AppConfig(..))
import Screens.Benefits.BenefitsScreen.ScreenData as BenefitsScreenData
import Screens.Benefits.LmsVideoScreen.ScreenData as LmsVideoScreenData
import Screens.Benefits.LmsQuizScreen.ScreenData as LmsQuizScreenData
import Common.Types.App (CategoryListType)
import Services.API
import Screens.DocumentCaptureScreen.ScreenData as DocumentCaptureScreenData
import Screens.DocumentDetailsScreen.ScreenData as DocumentDetailsScreenData
import Screens.RateCardScreen.ScreenData as RateCardScreenData
import Screens.CustomerReferralTrackerScreen.ScreenData as CustomerReferralTrackerScreenData
import Screens.CustomerReferralTrackerScreen.Types as CustomerReferralScreenTypes
import Screens.DeleteAccountScreen.ScreenData as DeleteAccountScreenData
import Screens.CameraScreen.ScreenData as CameraScreenData
import Screens.EarningsScreen.ScreenData as EarningsScreenData
import Screens.RideCompletedScreen.ScreenData as RideCompletedScreenData

type FlowBT e a = BackT (ExceptT e (Free (FlowWrapper GlobalState))) a

newtype GlobalState = GlobalState {
    splashScreen :: SplashScreenState
  , documentCaptureScreen :: DocumentCaptureScreenState
  , chooseLanguageScreen :: ChooseLanguageScreenState
  , driverProfileScreen :: DriverProfileScreenState
  , applicationStatusScreen :: ApplicationStatusScreenState
  , mobileNumberScreen :: EnterMobileNumberScreenState
  , enterOTPScreen :: EnterOTPScreenState
  , uploadDrivingLicenseScreen :: UploadDrivingLicenseState
  , registrationScreen :: RegistrationScreenState
  , uploadAdhaarScreen :: UploadAdhaarScreenState
  , addVehicleDetailsScreen :: AddVehicleDetailsScreenData.AddVehicleDetailsScreenState
  , tripDetailsScreen :: TripDetailsScreenState
  , rideHistoryScreen :: RideHistoryScreenState
  , rideSelectionScreen :: RideSelectionScreenState
  , reportIssueChatScreen :: ReportIssueChatScreenState
  , bankDetailsScreen :: BankDetailScreenState
  , driverDetailsScreen :: DriverDetailsScreenState
  , vehicleDetailsScreen :: VehicleDetailsScreenState
  , aboutUsScreen :: AboutUsScreenState
  , selectedLanguageScreen :: SelectLanguageScreenState
  , helpAndSupportScreen :: HelpAndSupportScreenState
  , writeToUsScreen :: WriteToUsScreenState
  , permissionsScreen :: PermissionsScreenState
  , homeScreen :: HomeScreenState
  , editBankDetailsScreen :: EditBankDetailsScreenState
  , editAadhaarDetailsScreen :: EditAadhaarDetailsScreenState
  , noInternetScreen :: NoInternetScreenState
  , popUpScreen :: PopUpScreenState
  , driverRideRatingScreen :: DriverRideRatingScreenState
  , appUpdatePopUpScreen :: AppUpdatePopUpScreenState
  , notificationScreen :: NotificationsScreenState
  , referralScreen :: ReferralScreenState
  , bookingOptionsScreen :: BookingOptionsScreenState
  , loaderOverlay :: LoaderOverlayScreenData.LoaderOverlayState
  , acknowledgementScreen :: AcknowledgementScreenState
  , aadhaarVerificationScreen :: AadhaarVerificationScreenState
  , globalProps :: GlobalProps
  , subscriptionScreen :: SubscriptionScreenState
  , onBoardingSubscriptionScreen :: OnBoardingSubscriptionScreenState
  , paymentHistoryScreen :: PaymentHistoryScreenState
  , driverSavedLocationScreen :: DriverSavedLocationScreenState
  , chooseCityScreen :: ChooseCityScreenState
  , welcomeScreen :: WelcomeScreenState
  , driverEarningsScreen :: DriverEarningsScreenState
  , benefitsScreen :: BenefitsScreenState
  , lmsVideoScreen :: LmsVideoScreenState
  , lmsQuizScreen :: LmsQuizScreenState
  , documentDetailsScreen :: DocumentDetailsScreenState
  , rateCardScreen :: RateCardScreenState
  , customerReferralTrackerScreen :: CustomerReferralScreenTypes.CustomerReferralTrackerScreenState
  , deleteAccountScreen :: DeleteAccountScreenData.State
  , cameraScreen :: CameraScreenState
  , earningsScreenV2 :: EarningsScreenData.State
  , rideCompletedScreen :: RideCompletedScreenState
  }

defaultGlobalState :: GlobalState
defaultGlobalState = GlobalState {
  documentCaptureScreen : DocumentCaptureScreenData.initData
, splashScreen : {data : { message : "WeDontTalkAnymore"}}
, chooseLanguageScreen : ChooseLanguageScreenData.initData
, driverProfileScreen : DriverProfileScreenData.initData
, applicationStatusScreen : ApplicationStatusScreenData.initData
, mobileNumberScreen : EnterMobileNumberScreenData.initData
, enterOTPScreen : EnterOTPScreenData.initData
, uploadDrivingLicenseScreen : UploadDrivingLicenseScreenData.initData
, registrationScreen: RegistrationScreenData.initData
, uploadAdhaarScreen : UploadAdhaarScreenData.initData
, addVehicleDetailsScreen : AddVehicleDetailsScreenData.initData
, tripDetailsScreen : TripDetailsScreenData.initData
, rideHistoryScreen : RideHistoryScreenData.initData
, rideSelectionScreen : RideSelectionScreenData.initData
, reportIssueChatScreen : ReportIssueChatScreenData.initData
, bankDetailsScreen : BankDetailScreenData.initData
, driverDetailsScreen : DriverDetailsScreenData.initData
, vehicleDetailsScreen : VehicleDetailsScreenData.initData
, aboutUsScreen : AboutUsScreenData.initData
, selectedLanguageScreen : SelectLanguageScreenData.initData
, helpAndSupportScreen : HelpAndSupportScreenData.initData
, writeToUsScreen : WriteToUsScreenData.initData
, permissionsScreen : PermissionsScreenData.initData
, homeScreen : HomeScreenData.initData
, editBankDetailsScreen : EditBankDetailsScreenData.initData
, editAadhaarDetailsScreen : EditAadhaarDetailsScreenData.initData
, noInternetScreen : {}
, popUpScreen : PopUpScreenData.initData
, driverRideRatingScreen : DriverRideRatingScreenData.initData
, appUpdatePopUpScreen : AppUpdatePopUpScreenData.initData
, notificationScreen : NotificationsScreenData.initData
, referralScreen : ReferralScreenData.initData
, bookingOptionsScreen : BookingOptionsScreenData.initData
, loaderOverlay : LoaderOverlayScreenData.initData
, acknowledgementScreen : AcknowledgementScreenData.initData
, aadhaarVerificationScreen : EnterAadhaarNumberScreenData.initData
, globalProps : defaultGlobalProps
, subscriptionScreen : SubscriptionScreenData.initData
, onBoardingSubscriptionScreen : OnBoardingSubscriptionScreenData.initData
, paymentHistoryScreen : PaymentHistoryScreenData.initData
, driverSavedLocationScreen : DriverSavedLocationScreenData.initData
, chooseCityScreen : ChooseCityScreenData.initData
, welcomeScreen : WelcomeScreenData.initData
, driverEarningsScreen : DriverEarningsScreenData.initData
, benefitsScreen : BenefitsScreenData.initData
, lmsVideoScreen : LmsVideoScreenData.initData
, lmsQuizScreen : LmsQuizScreenData.initData
, documentDetailsScreen : DocumentDetailsScreenData.initData
, rateCardScreen : RateCardScreenData.initData
, customerReferralTrackerScreen : CustomerReferralTrackerScreenData.initData
, deleteAccountScreen : DeleteAccountScreenData.initData
, cameraScreen : CameraScreenData.initData
, earningsScreenV2 : EarningsScreenData.initialState
, rideCompletedScreen : RideCompletedScreenData.initData
}

defaultGlobalProps :: GlobalProps
defaultGlobalProps = {
  aadhaarVerificationRequired : false,
  driverInformation : Nothing,
  driverRideStats : Nothing,
  callScreen : ScreenNames.HOME_SCREEN,
  gotoPopupType : NO_POPUP_VIEW,
  addTimestamp : true,
  bgLocPopupShown : false,
  onBoardingDocs : Nothing
}

data ScreenType =
   SplashScreenStateType (SplashScreenState -> SplashScreenState)
  | ChooseLanguageScreenStateType (ChooseLanguageScreenState -> ChooseLanguageScreenState)
  | DriverProfileScreenStateType (DriverProfileScreenState -> DriverProfileScreenState)
  | ApplicationStatusScreenType (ApplicationStatusScreenState -> ApplicationStatusScreenState)
  | EnterMobileNumberScreenType (EnterMobileNumberScreenState -> EnterMobileNumberScreenState)
  | EnterOTPScreenType (EnterOTPScreenState -> EnterOTPScreenState)
  | UploadDrivingLicenseScreenStateType (UploadDrivingLicenseState -> UploadDrivingLicenseState)
  | RegisterScreenStateType (RegistrationScreenState -> RegistrationScreenState)
  | UploadAdhaarScreenStateType (UploadAdhaarScreenState -> UploadAdhaarScreenState)
  | AddVehicleDetailsScreenStateType (AddVehicleDetailsScreenData.AddVehicleDetailsScreenState -> AddVehicleDetailsScreenData.AddVehicleDetailsScreenState)
  | DriverDetailsScreenStateType (DriverDetailsScreenState -> DriverDetailsScreenState)
  | VehicleDetailsScreenStateType (VehicleDetailsScreenState -> VehicleDetailsScreenState)
  | AboutUsScreenStateType (AboutUsScreenState -> AboutUsScreenState)
  | SelectLanguageScreenStateType (SelectLanguageScreenState -> SelectLanguageScreenState)
  | HelpAndSupportScreenStateType (HelpAndSupportScreenState -> HelpAndSupportScreenState)
  | WriteToUsScreenStateType (WriteToUsScreenState -> WriteToUsScreenState)
  | BankDetailScreenStateType (BankDetailScreenState -> BankDetailScreenState)
  | HomeScreenStateType (HomeScreenState -> HomeScreenState)
  | RideHistoryScreenStateType (RideHistoryScreenState -> RideHistoryScreenState)
  | RideSelectionScreenStateType (RideSelectionScreenState -> RideSelectionScreenState)
  | ReportIssueChatScreenStateType (ReportIssueChatScreenState -> ReportIssueChatScreenState)
  | PermissionsScreenStateType (PermissionsScreenState -> PermissionsScreenState)
  | EditBankDetailsScreenStateType (EditBankDetailsScreenState -> EditBankDetailsScreenState)
  | EditAadhaarDetailsScreenStateType (EditAadhaarDetailsScreenState -> EditAadhaarDetailsScreenState)
  | TripDetailsScreenStateType (TripDetailsScreenState -> TripDetailsScreenState)
  | PopUpScreenStateType (PopUpScreenState -> PopUpScreenState)
  | DriverRideRatingScreenStateType (DriverRideRatingScreenState -> DriverRideRatingScreenState)
  | NotificationsScreenStateType (NotificationsScreenState -> NotificationsScreenState)
  | ReferralScreenStateType (ReferralScreenState -> ReferralScreenState)
  | BookingOptionsScreenType (BookingOptionsScreenState -> BookingOptionsScreenState)
  | AppUpdatePopUpScreenType (AppUpdatePopUpScreenState -> AppUpdatePopUpScreenState)
  | AcknowledgementScreenType (AcknowledgementScreenState -> AcknowledgementScreenState)
  | AadhaarVerificationScreenType (AadhaarVerificationScreenState -> AadhaarVerificationScreenState)
  | GlobalPropsType (GlobalProps -> GlobalProps)
  | SubscriptionScreenStateType (SubscriptionScreenState -> SubscriptionScreenState)
  | OnBoardingSubscriptionScreenStateType (OnBoardingSubscriptionScreenState -> OnBoardingSubscriptionScreenState)
  | PaymentHistoryScreenStateType (PaymentHistoryScreenState -> PaymentHistoryScreenState)
  | DriverSavedLocationScreenStateType (DriverSavedLocationScreenState -> DriverSavedLocationScreenState)
  | ChooseCityScreenStateType (ChooseCityScreenState -> ChooseCityScreenState)
  | WelcomeScreenStateType (WelcomeScreenState -> WelcomeScreenState)
  | DriverEarningsScreenStateType (DriverEarningsScreenState -> DriverEarningsScreenState)
  | BenefitsScreenStateType (BenefitsScreenState -> BenefitsScreenState)
  | RegistrationScreenStateType (RegistrationScreenState -> RegistrationScreenState)
  | LmsVideoScreenStateType (LmsVideoScreenState -> LmsVideoScreenState)
  | LmsQuizScreenStateType (LmsQuizScreenState -> LmsQuizScreenState)
  | DocumentCaptureScreenStateType (DocumentCaptureScreenState -> DocumentCaptureScreenState)
  | DocumentDetailsScreenStateType (DocumentDetailsScreenState -> DocumentDetailsScreenState)
  | RateCardScreenStateType (RateCardScreenState -> RateCardScreenState)
  | CustomerReferralTrackerScreenStateType (CustomerReferralScreenTypes.CustomerReferralTrackerScreenState -> CustomerReferralScreenTypes.CustomerReferralTrackerScreenState)
  | CameraScreenStateType (CameraScreenState -> CameraScreenState)
  | EarningsScreenV2 (EarningsScreenData.State -> EarningsScreenData.State)
  | RideCompletedScreenStateType (RideCompletedScreenState -> RideCompletedScreenState)

data ScreenStage = HomeScreenStage HomeScreenStage

data DOCUMENT_DETAILS_SCREEN_OUTPUT = GO_TO_HOME_FROM_DOCUMENT_DETAILS DocumentDetailsScreenState

data DeleteAccountScreenOutput = SubmitRequest | Back 

data MY_RIDES_SCREEN_OUTPUT = HOME_SCREEN 
                            | PROFILE_SCREEN
                            | GO_TO_REFERRAL_SCREEN
                            | REFRESH RideHistoryScreenState
                            | LOADER_OUTPUT RideHistoryScreenState
                            | FILTER String
                            | GO_TO_TRIP_DETAILS IndividualRideCardState
                            | NOTIFICATION_FLOW
                            | SELECTED_TAB RideHistoryScreenState
                            | OPEN_PAYMENT_HISTORY RideHistoryScreenState
                            | RIDE_HISTORY_NAV NAVIGATION_ACTIONS

data DRIVER_EARNINGS_SCREEN_OUTPUT = EARNINGS_NAV NAVIGATION_ACTIONS DriverEarningsScreenState
                                   | CHANGE_SUB_VIEW DriverEarningsSubView DriverEarningsScreenState
                                   | CONVERT_COIN_TO_CASH DriverEarningsScreenState
                                   | REFRESH_EARNINGS_SCREEN DriverEarningsScreenState
                                   | EARNINGS_HISTORY DriverEarningsScreenState
                                   | GOTO_PAYMENT_HISTORY_FROM_COINS
                                   | GOTO_MY_PLAN_FROM_COINS
                                   | GOTO_TRIP_DETAILS IndividualRideCardState
                                   | LOAD_MORE_HISTORY DriverEarningsScreenState

data REFERRAL_SCREEN_OUTPUT = GO_TO_HOME_SCREEN_FROM_REFERRAL_SCREEN
                            | GO_TO_RIDES_SCREEN_FROM_REFERRAL_SCREEN
                            | GO_TO_PROFILE_SCREEN_FROM_REFERRAL_SCREEN
                            | GO_TO_NOTIFICATION_SCREEN_FROM_REFERRAL_SCREEN
                            | GO_TO_FLOW_AND_COME_BACK ReferralScreenState
                            | REFRESH_LEADERBOARD
                            | REFERRAL_SCREEN_NAV NAVIGATION_ACTIONS

data RIDES_SELECTION_SCREEN_OUTPUT = REFRESH_RIDES RideSelectionScreenState
                                   | LOADER_RIDES_OUTPUT RideSelectionScreenState
                                   | SELECT_RIDE RideSelectionScreenState

data DRIVER_PROFILE_SCREEN_OUTPUT = DRIVER_DETAILS_SCREEN
                                    | VEHICLE_DETAILS_SCREEN
                                    | ABOUT_US_SCREEN
                                    | GO_TO_LOGOUT
                                    | SELECT_LANGUAGE_SCREEN
                                    | HELP_AND_SUPPORT_SCREEN
                                    | GO_TO_HOME_FROM_PROFILE
                                    | GO_TO_DRIVER_HISTORY_SCREEN
                                    | GO_TO_EDIT_BANK_DETAIL_SCREEN
                                    | ON_BOARDING_FLOW
                                    | DOCUMENTS_FLOW
                                    | DELETE_ACCOUNT_FLOW
                                    | NOTIFICATIONS_SCREEN
                                    | GO_TO_REFERRAL_SCREEN_FROM_DRIVER_PROFILE_SCREEN
                                    | GO_TO_BOOKING_OPTIONS_SCREEN DriverProfileScreenState
                                    | VERIFY_OTP1 DriverProfileScreenState
                                    | DRIVER_ALTERNATE_CALL_API1 DriverProfileScreenState
                                    | RESEND_ALTERNATE_OTP1 DriverProfileScreenState
                                    | ALTERNATE_NUMBER_REMOVE1 DriverProfileScreenState
                                    | DRIVER_GENDER1 DriverProfileScreenState
                                    | GO_TO_ACTIVATE_OR_DEACTIVATE_RC DriverProfileScreenState
                                    | GO_TO_DELETE_RC DriverProfileScreenState
                                    | GO_TO_CALL_DRIVER DriverProfileScreenState
                                    | ADD_RC DriverProfileScreenState
                                    | UPDATE_LANGUAGES (Array String)
                                    | SUBCRIPTION 
                                    | SAVED_LOCATIONS_SCREEN
                                    | GO_HOME DriverProfileScreenState
                                    | VIEW_PENDING_VEHICLE String VehicleCategory


data DRIVER_DETAILS_SCREEN_OUTPUT = VERIFY_OTP DriverDetailsScreenState
                                  | DRIVER_ALTERNATE_CALL_API DriverDetailsScreenState
                                  | RESEND_ALTERNATE_OTP DriverDetailsScreenState
                                  | ALTERNATE_NUMBER_REMOVE DriverDetailsScreenState
                                  | GO_TO_HOMESCREEN DriverDetailsScreenState
                                  | DRIVER_GENDER DriverDetailsScreenState


data VEHICLE_DETAILS_SCREEN_OUTPUT = UPDATE_VEHICLE_INFO VehicleDetailsScreenState
data ABOUT_US_SCREEN_OUTPUT = GO_TO_DRIVER_HOME_SCREEN
data SELECT_LANGUAGE_SCREEN_OUTPUT = CHANGE_LANGUAGE SelectLanguageScreenState | LANGUAGE_CONFIRMED SelectLanguageScreenState
data HELP_AND_SUPPORT_SCREEN_OUTPUT = WRITE_TO_US_SCREEN
                                    | REPORT_ISSUE_CHAT_SCREEN CategoryListType
                                    | RIDE_SELECTION_SCREEN CategoryListType
                                    | REMOVE_ISSUE_SCREEN String HelpAndSupportScreenState
                                    | RESOLVED_ISSUE_SCREEN HelpAndSupportScreenState
                                    | ON_GOING_ISSUE_SCREEN HelpAndSupportScreenState
                                    | ISSUE_LIST_GO_BACK_SCREEN HelpAndSupportScreenState
                                    | DUMMY_RIDE_REQUEST HelpAndSupportScreenState
                                    | GO_BACK_TO_PROFILE_SCREEN HelpAndSupportScreenState
                                    | GO_BACK_TO_HELP_AND_SUPPORT HelpAndSupportScreenState
                                    | GO_BACK_TO_HOME_SCREEN_FROM_HELP HelpAndSupportScreenState
                                    | GO_BACK_TO_TRIP_DETAILS HelpAndSupportScreenState
                                    | GO_BACK_TO_EARNINGS HelpAndSupportScreenState


data WRITE_TO_US_SCREEN_OUTPUT = GO_TO_HOME_SCREEN_FLOW
data REGISTRATION_SCREEN_OUTPUT = UPLOAD_DRIVER_LICENSE RegistrationScreenState StepProgress
                                | UPLOAD_VEHICLE_DETAILS RegistrationScreenState StepProgress
                                | PERMISSION_SCREEN RegistrationScreenState
                                | LOGOUT_FROM_REGISTERATION_SCREEN
                                | GO_TO_ONBOARD_SUBSCRIPTION RegistrationScreenState
                                | GO_TO_HOME_SCREEN_FROM_REGISTERATION_SCREEN RegistrationScreenState
                                | REFRESH_REGISTERATION_SCREEN
                                | REFERRAL_CODE_SUBMIT RegistrationScreenState
                                | DOCUMENT_CAPTURE_FLOW RegistrationScreenState RegisterationStep
                                | SELECT_LANG_FROM_REGISTRATION
                                | AADHAAR_PAN_SELFIE_UPLOAD RegistrationScreenState HyperVergeKycResult
                                | GO_TO_APP_UPDATE_POPUP_SCREEN RegistrationScreenState
                                | SSN_FROM_REGISTRATION RegistrationScreenState
                                | PROFILE_DETAILS_FROM_REGISTRATION RegistrationScreenState
                                | HANDLE_CHECKR_WEBVIEW_EXIT
                                | GET_BGV_URL RegistrationScreenState

data UPLOAD_DRIVER_LICENSE_SCREENOUTPUT = VALIDATE_DL_DETAILS UploadDrivingLicenseState 
                                          | VALIDATE_DATA_API UploadDrivingLicenseState 
                                          | GOTO_VEHICLE_DETAILS_SCREEN 
                                          | LOGOUT_ACCOUNT 
                                          | GOTO_ONBOARDING_FLOW
                                          | CHANGE_VEHICLE_FROM_DL_SCREEN
                                          | CHANGE_LANG_FROM_DL_SCREEN

data UPLOAD_ADHAAR_CARD_SCREENOUTPUT = GO_TO_ADD_BANK_DETAILS

data BANK_DETAILS_SCREENOUTPUT = GO_TO_ADD_VEHICLE_DETAILS

data ADD_VEHICLE_DETAILS_SCREENOUTPUT = VALIDATE_DETAILS AddVehicleDetailsScreenData.AddVehicleDetailsScreenState 
                                        | VALIDATE_RC_DATA_API_CALL AddVehicleDetailsScreenData.AddVehicleDetailsScreenState 
                                        | REFER_API_CALL AddVehicleDetailsScreenData.AddVehicleDetailsScreenState 
                                        | APPLICATION_STATUS_SCREEN 
                                        | LOGOUT_USER 
                                        | ONBOARDING_FLOW 
                                        | DRIVER_PROFILE_SCREEN 
                                        | RC_ACTIVATION AddVehicleDetailsScreenData.AddVehicleDetailsScreenState
                                        | CHANGE_VEHICLE_FROM_RC_SCREEN
                                        | CHANGE_LANG_FROM_RC_SCREEN

data TRIP_DETAILS_SCREEN_OUTPUT = ON_SUBMIT | GO_TO_EARINING | OPEN_HELP_AND_SUPPORT | GO_TO_HOME_SCREEN | GO_TO_RIDE_HISTORY_SCREEN 

data PERMISSIONS_SCREEN_OUTPUT = DRIVER_HOME_SCREEN | LOGOUT_FROM_PERMISSIONS_SCREEN | GO_TO_REGISTERATION_SCREEN PermissionsScreenState

data ONBOARDING_SUBSCRIPTION_SCREENOUTPUT =  MAKE_PAYMENT_FROM_ONBOARDING OnBoardingSubscriptionScreenState | REGISTERATION_ONBOARDING OnBoardingSubscriptionScreenState

data HOME_SCREENOUTPUT = GO_TO_PROFILE_SCREEN HomeScreenState
                          | GO_TO_RIDES_SCREEN
                          | GO_TO_REFERRAL_SCREEN_FROM_HOME_SCREEN
                          | GO_TO_HELP_AND_SUPPORT_SCREEN
                          | GO_TO_EDIT_GENDER_SCREEN
                          | GO_TO_START_RIDE {id :: String, otp :: String, startOdometerReading :: Maybe String, startOdometerImage :: Maybe String, lat :: String, lon :: String, ts :: String} HomeScreenState
                          | GO_TO_CANCEL_RIDE {id :: String, info :: String , reason :: String} HomeScreenState
                          | GO_TO_ARRIVED_AT_STOP {id :: String, lat :: String , lon :: String, ts :: String } HomeScreenState
                          | GO_TO_END_RIDE {id :: String, endOtp :: String, endOdometerReading :: Maybe String, endOdometerImage :: Maybe String, lat :: String , lon :: String, ts :: String } HomeScreenState
                          | GO_TO_NEW_STOP HomeScreenState
                          | DRIVER_AVAILABILITY_STATUS HomeScreenState DriverStatus
                          | REFRESH_HOME_SCREEN_FLOW
                          | RELOAD HomeScreenState
                          | UPDATE_ROUTE HomeScreenState
                          | FCM_NOTIFICATION String HomeScreenState
                          | NOTIFY_CUSTOMER HomeScreenState
                          | UPDATE_STAGE HomeScreenStage
                          | GO_TO_NOTIFICATIONS
                          | ADD_ALTERNATE_HOME
                          | GO_TO_AADHAAR_VERIFICATION
                          | GO_TO_START_ZONE_RIDE {otp :: String, lat :: String, lon :: String, ts :: String}
                          | ON_CALL HomeScreenState String
                          | OPEN_PAYMENT_PAGE HomeScreenState
                          | HOMESCREEN_NAV NAVIGATION_ACTIONS
                          | GO_TO_VEHICLE_DETAILS_SCREEN
                          | GO_TO_RIDE_DETAILS_SCREEN 
                          | POST_RIDE_FEEDBACK HomeScreenState
                          | CLEAR_PENDING_DUES
                          | ENABLE_GOTO_API HomeScreenState String String
                          | LOAD_GOTO_LOCATIONS HomeScreenState
                          | DISABLE_GOTO HomeScreenState
                          | GOTO_LOCATION_FLOW HomeScreenState Boolean
                          | REFRESH_GOTO HomeScreenState
                          | GO_TO_EARNINGS_SCREEN
                          | GOT_DRIVER_STATS DriverProfileStatsResp
                          | UPDATE_SPECIAL_LOCATION_LIST
                          | UPDATE_AIR_CONDITIONED Boolean
                          | GO_TO_BOOKING_PREFERENCES
                          | UPDATE_ROUTE_ON_STAGE_SWITCH HomeScreenState
                          | GO_TO_CUSTOMER_REFERRAL_TRACKER HomeScreenState
                          | GO_TO_BENEFITS_SCREEN_FROM_HOME
                          | GO_TO_ADD_UPI_SCREEN
                          | VERIFY_MANUAL_UPI HomeScreenState

data RIDE_COMPLETED_SCREEN_OUTPUT = GO_TO_HOME_SCREEN_FROM_RIDE_COMPLETED_SCREEN RideCompletedScreenState
                                  -- | GO_TO_HELP_AND_SUPPORT_FROM_RIDE_COMPLETED_SCREEN
                                  | GO_TO_RIDE_DETAILS_SCREEN_FROM_RIDE_COMPLETED_SCREEN

data REPORT_ISSUE_CHAT_SCREEN_OUTPUT = GO_TO_HELP_AND_SUPPORT | SUBMIT_ISSUE ReportIssueChatScreenState | CALL_CUSTOMER ReportIssueChatScreenState

data RIDE_DETAIL_SCREENOUTPUT = GO_TO_HOME_FROM_RIDE_DETAIL | SHOW_ROUTE_IN_RIDE_DETAIL
data APPLICATION_STATUS_SCREENOUTPUT = GO_TO_HOME_FROM_APPLICATION_STATUS
                                      | LOGOUT_ACCOUT
                                      | GO_TO_UPLOAD_DL_SCREEN
                                      | GO_TO_VEHICLE_DETAIL_SCREEN
                                      | VALIDATE_NUMBER ApplicationStatusScreenState
                                      | VALIDATE_OTP ApplicationStatusScreenState
                                      | RESEND_OTP_TO_ALTERNATE_NUMBER ApplicationStatusScreenState
data EDIT_BANK_DETAILS_SCREEN_OUTPUT = EDIT_BANK_DETAILS
data EDIT_AADHAAR_DETAILS_SCREEN_OUTPUT = EDIT_AADHAAR_DETAILS
data ENTER_MOBILE_NUMBER_SCREEN_OUTPUT
  = GO_TO_ENTER_OTP EnterMobileNumberScreenState
  | GO_TO_DRIVER_INFO EnterMobileNumberScreenState
data ENTER_OTP_SCREEN_OUTPUT = RETRY EnterOTPScreenState | DRIVER_INFO_API_CALL EnterOTPScreenState | CHANGE_LOGIN_METHOD EnterOTPScreenState
data NO_INTERNET_SCREEN_OUTPUT = REFRESH_INTERNET | TURN_ON_GPS | CHECK_INTERNET
data POPUP_SCREEN_OUTPUT = POPUP_REQUEST_RIDE String Number
data DRIVER_RIDE_RATING_SCREEN_OUTPUT = CloseScreen | SendCustomerFeedBack DriverRideRatingScreenState
data NOTIFICATIONS_SCREEN_OUTPUT = REFRESH_SCREEN NotificationsScreenState
                                    | LOAD_NOTIFICATIONS NotificationsScreenState
                                    | GO_HOME_SCREEN
                                    | GO_REFERRAL_SCREEN
                                    | GO_RIDE_HISTORY_SCREEN
                                    | GO_PROFILE_SCREEN
                                    | GO_EARNINGS_SCREEN
                                    | CHECK_RIDE_FLOW_STATUS
                                    | NOTIFICATION_SCREEN_NAV NAVIGATION_ACTIONS

data BOOKING_OPTIONS_SCREEN_OUTPUT = 
  SELECT_CAB BookingOptionsScreenState Boolean 
  | ENABLE_RENTAL_INTERCITY_RIDE BookingOptionsScreenState 
  | GO_TO_PROFILE 
  | CHANGE_RIDE_PREFERENCE BookingOptionsScreenState RidePreference 
  | UPDATE_AC_AVAILABILITY BookingOptionsScreenState Boolean 
  | HOME_SCREEN_FROM_BOOKING_PREFS 
  | EXIT_TO_RATE_CARD_SCREEN BookingOptionsScreenState

data ACKNOWLEDGEMENT_SCREEN_OUTPUT = EXIT_TO_HOME_SCREEN | RETRY_PAYMENT

data AADHAAR_VERIFICATION_SCREEN_OUTPUT = ENTER_AADHAAR_OTP AadhaarVerificationScreenState 
  | VERIFY_AADHAAR_OTP AadhaarVerificationScreenState
  | RESEND_AADHAAR_OTP AadhaarVerificationScreenState
  | SEND_UNVERIFIED_AADHAAR_DATA AadhaarVerificationScreenState
  | GO_TO_HOME_FROM_AADHAAR
  | LOGOUT_FROM_AADHAAR

data SUBSCRIPTION_SCREEN_OUTPUT = GOTO_HOMESCREEN
                                  | NAV NAVIGATION_ACTIONS
                                  | MAKE_PAYMENT SubscriptionScreenState
                                  | GOTO_PAYMENT_HISTORY SubscriptionScreenState
                                  | CANCEL_AUTOPAY SubscriptionScreenState
                                  | PAUSE_AUTOPAY SubscriptionScreenState
                                  | SWITCH_PLAN SubscriptionScreenState String
                                  | RESUME_AUTOPAY SubscriptionScreenState
                                  | CHECK_ORDER_STATUS SubscriptionScreenState String
                                  | GO_TO_MANAGE_PLAN SubscriptionScreenState
                                  | GO_TO_FIND_HELP_CENTRE SubscriptionScreenState
                                  | GO_TO_OPEN_GOOGLE_MAPS SubscriptionScreenState
                                  | REFRESH_SUSCRIPTION
                                  | RETRY_PAYMENT_AC SubscriptionScreenState String
                                  | REFRESH_HELP_CENTRE SubscriptionScreenState
                                  | CLEAR_DUES_ACT
                                  | SUBSCRIBE_API SubscriptionScreenState
                                  | REMOVE_REELS_VIEW_MY_PLANS SubscriptionScreenState

data NAVIGATION_ACTIONS = HomeScreenNav
                          | GoToRideHistory
                          | GoToSubscription
                          | GoToContest
                          | GoToAlerts
                          | GoToEarningsScreen Boolean

data PAYMENT_HISTORY_SCREEN_OUTPUT = GoToSetupAutoPay PaymentHistoryScreenState
                                    | EntityDetailsAPI PaymentHistoryScreenState String
                                    | SWITCH_TAB
                                    | LOAD_MORE_ITEMS PaymentHistoryScreenState

data APP_UPDATE_POPUP = Later | UpdateNow

data DRIVE_SAVED_LOCATION_OUTPUT = EXIT_FROM_SCREEN 
                                  | AUTO_COMPLETE DriverSavedLocationScreenState String String String
                                  | GET_LOCATION_NAME DriverSavedLocationScreenState
                                  | SAVE_LOCATION DriverSavedLocationScreenState
                                  | GET_PLACE_NAME DriverSavedLocationScreenState String
                                  | DELETE_PLACE DriverSavedLocationScreenState String
                                  | CHANGE_VIEW 
                                  | UPDATE_HOME_LOCATION DriverSavedLocationScreenState
                      
data WELCOME_SCREEN_OUTPUT = GoToMobileNumberScreen

data CHOOSE_CITY_SCREEN_OUTPUT = GoToWelcomeScreen 
                                  | REFRESH_SCREEN_CHOOSE_CITY ChooseCityScreenState
                                  | DETECT_CITY Number Number ChooseCityScreenState

data CHOOSE_LANG_SCREEN_OUTPUT = LOGIN_FLOW
data BENEFITS_SCREEN_OUTPUT = DRIVER_REFERRAL_SCREEN_NAV NAVIGATION_ACTIONS
                              | DRIVER_CONTEST_SCREEN
                              | GO_TO_LMS_VIDEO_SCREEN BenefitsScreenState
                              | CUSTOMER_REFERRAL_TRACKER_NAV

data LMS_VIDEO_SCREEN_OUTPUT = GO_TO_QUIZ_SCREEN LmsVideoScreenState | REFRESH_LMS_VIDEO_SCREEN LmsVideoScreenState | GO_TO_BENEFITS_SCREEN | SELECT_LANGUAGE_FOR_VIDEOS LmsVideoScreenState

data LMS_QUIZ_SCREEN_OUTPUT = GO_TO_NEXT_QUESTION LmsQuizScreenState
                            | CONFIRM_QUESTION LmsQuizScreenState
                            | RETRY_QUESTION LmsQuizScreenState
                            | RETAKE_QUIZ_SO LmsQuizScreenState
                            | SELECT_LANGUAGE_FOR_QUESTION LmsQuizScreenState
                            | GO_TO_LMS_VIDEOS_SCREEN_FROM_QUIZ LmsQuizScreenState
                            | GO_TO_BENEFITS_SCREEN_FROM_QUIZ LmsQuizScreenState

data DOCUMENT_CAPTURE_SCREEN_OUTPUT = UPLOAD_DOC_API DocumentCaptureScreenState String 
                                      | LOGOUT_FROM_DOC_CAPTURE 
                                      | CHANGE_LANG_FROM_DOCUMENT_CAPTURE
                                      | CHANGE_VEHICLE_FROM_DOCUMENT_CAPTURE
                                      | UPDATE_SSN DocumentCaptureScreenState
                                      | UPDATE_SOCIAL_PROFILE DocumentCaptureScreenState
                                      | OPEN_CAMERA_FOR_PROFILE_PIC DocumentCaptureScreenState

data RATE_CARD_SCREEN_OUTPUT = REFRESH_RATE_CARD RateCardScreenState | RATE_CARD_API RateCardScreenState Int

data CUSTOMER_REFERRAL_TRACKER_SCREEN_OUTPUT = ADD_UPI_FLOW CustomerReferralScreenTypes.CustomerReferralTrackerScreenState
                                               | DELETE_UPI_FLOW CustomerReferralScreenTypes.CustomerReferralTrackerScreenState
                                               | REFRESH_ORDER_STATUS CustomerReferralScreenTypes.CustomerReferralTrackerScreenState
data CAMERA_SCREEN_OUTPUT = OPEN_CAMERA


data FlowState = EarningsV2Daily Boolean
  | EarningsV2Weekly
  | EarningsV2RideHistory
  | EarningsV2PayoutHistory
  | HomeScreen
  | Benefits
  | Notifications
  | Subscription
  | Profile
  | OpenRideDetails IndividualRideCardState
  | OpenHelpAndSuportScreen
