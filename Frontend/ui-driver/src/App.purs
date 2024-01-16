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
import Screens.RideSelectionScreen.ScreenData as RideSelectionScreenData
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData
import Screens.SubscriptionScreen.ScreenData as SubscriptionScreenData
import Screens.TripDetailsScreen.ScreenData as TripDetailsScreenData
import Screens.Types (AadhaarVerificationScreenState, AboutUsScreenState, AcknowledgementScreenState, ActiveRide, AddVehicleDetailsScreenState, AppUpdatePopUpScreenState, ApplicationStatusScreenState, BankDetailScreenState, BookingOptionsScreenState, CategoryListType, ChooseLanguageScreenState, DriverDetailsScreenState, DriverProfileScreenState, DriverRideRatingScreenState, DriverSavedLocationScreenState, DriverStatus, EditAadhaarDetailsScreenState, EditBankDetailsScreenState, EnterMobileNumberScreenState, EnterOTPScreenState, GlobalProps, GoToPopUpType(..), HelpAndSupportScreenState, HomeScreenStage(..), HomeScreenState, IndividualRideCardState, NoInternetScreenState, NotificationsScreenState, OnBoardingSubscriptionScreenState, PaymentHistoryScreenState, PermissionsScreenState, PopUpScreenState, ReferralScreenState, RegistrationScreenState, ReportIssueChatScreenState, RideDetailScreenState, RideHistoryScreenState, RideSelectionScreenState, SelectLanguageScreenState, SplashScreenState, SubscriptionScreenState, TripDetailsScreenState, UpdatePopupType(..), UploadAdhaarScreenState, UploadDrivingLicenseState, VehicleDetailsScreenState, WelcomeScreenState, WriteToUsScreenState, ChooseCityScreenState, DriverReferralScreenState)
import Screens.UploadAdhaarScreen.ScreenData as UploadAdhaarScreenData
import Screens.UploadDrivingLicenseScreen.ScreenData as UploadDrivingLicenseScreenData
import Screens.VehicleDetailsScreen.ScreenData as VehicleDetailsScreenData
import Screens.WelcomeScreen.ScreenData as WelcomeScreenData
import Screens.WriteToUsScreen.ScreenData as WriteToUsScreenData
import Data.Maybe (Maybe(..))
import MerchantConfig.Types (AppConfig(..))
import Screens.DriverReferralScreen.ScreenData as DriverReferralScreenData

type FlowBT e a = BackT (ExceptT e (Free (FlowWrapper GlobalState))) a

newtype GlobalState = GlobalState {
    splashScreen :: SplashScreenState
  , chooseLanguageScreen :: ChooseLanguageScreenState
  , driverProfileScreen :: DriverProfileScreenState
  , applicationStatusScreen :: ApplicationStatusScreenState
  , mobileNumberScreen :: EnterMobileNumberScreenState
  , enterOTPScreen :: EnterOTPScreenState
  , uploadDrivingLicenseScreen :: UploadDrivingLicenseState
  , registrationScreen :: RegistrationScreenState
  , uploadAdhaarScreen :: UploadAdhaarScreenState
  , addVehicleDetailsScreen :: AddVehicleDetailsScreenState
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
  , driverReferralScreen :: DriverReferralScreenState
  }

defaultGlobalState :: GlobalState
defaultGlobalState = GlobalState {
  splashScreen : {data : { message : "WeDontTalkAnymore"}}
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
, driverReferralScreen : DriverReferralScreenData.initData
}

defaultGlobalProps :: GlobalProps
defaultGlobalProps = {
  aadhaarVerificationRequired : false,
  driverInformation : Nothing,
  driverRideStats : Nothing,
  callScreen : ScreenNames.HOME_SCREEN,
  gotoPopupType : NO_POPUP_VIEW,
  addTimestamp : true
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
  | AddVehicleDetailsScreenStateType (AddVehicleDetailsScreenState -> AddVehicleDetailsScreenState)
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
  | DriverReferralScreenStateType (DriverReferralScreenState -> DriverReferralScreenState)
  | RegistrationScreenStateType (RegistrationScreenState -> RegistrationScreenState)
  

data ScreenStage = HomeScreenStage HomeScreenStage

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


data DRIVER_DETAILS_SCREEN_OUTPUT = VERIFY_OTP DriverDetailsScreenState
                                  | DRIVER_ALTERNATE_CALL_API DriverDetailsScreenState
                                  | RESEND_ALTERNATE_OTP DriverDetailsScreenState
                                  | ALTERNATE_NUMBER_REMOVE DriverDetailsScreenState
                                  | GO_TO_HOMESCREEN DriverDetailsScreenState
                                  | DRIVER_GENDER DriverDetailsScreenState


data VEHICLE_DETAILS_SCREEN_OUTPUT = UPDATE_VEHICLE_INFO VehicleDetailsScreenState
data ABOUT_US_SCREEN_OUTPUT = GO_TO_DRIVER_HOME_SCREEN
data SELECT_LANGUAGE_SCREEN_OUTPUT = CHANGE_LANGUAGE
data HELP_AND_SUPPORT_SCREEN_OUTPUT = WRITE_TO_US_SCREEN
                                    | REPORT_ISSUE_CHAT_SCREEN CategoryListType
                                    | RIDE_SELECTION_SCREEN CategoryListType
                                    | REMOVE_ISSUE_SCREEN String HelpAndSupportScreenState
                                    | RESOLVED_ISSUE_SCREEN HelpAndSupportScreenState
                                    | ON_GOING_ISSUE_SCREEN HelpAndSupportScreenState
                                    | ISSUE_LIST_GO_BACK_SCREEN HelpAndSupportScreenState


data WRITE_TO_US_SCREEN_OUTPUT = GO_TO_HOME_SCREEN_FLOW
data REGISTRATION_SCREEN_OUTPUT = UPLOAD_DRIVER_LICENSE RegistrationScreenState
                                | UPLOAD_VEHICLE_DETAILS RegistrationScreenState
                                | PERMISSION_SCREEN RegistrationScreenState
                                | LOGOUT_FROM_REGISTERATION_SCREEN
                                | GO_TO_ONBOARD_SUBSCRIPTION
                                | GO_TO_HOME_SCREEN_FROM_REGISTERATION_SCREEN
                                | REFRESH_REGISTERATION_SCREEN
                                | REFERRAL_CODE_SUBMIT RegistrationScreenState

data UPLOAD_DRIVER_LICENSE_SCREENOUTPUT = VALIDATE_DL_DETAILS UploadDrivingLicenseState | VALIDATE_DATA_API UploadDrivingLicenseState | GOTO_VEHICLE_DETAILS_SCREEN | LOGOUT_ACCOUNT | GOTO_ONBOARDING_FLOW

data UPLOAD_ADHAAR_CARD_SCREENOUTPUT = GO_TO_ADD_BANK_DETAILS

data BANK_DETAILS_SCREENOUTPUT = GO_TO_ADD_VEHICLE_DETAILS

data ADD_VEHICLE_DETAILS_SCREENOUTPUT = VALIDATE_DETAILS AddVehicleDetailsScreenState | VALIDATE_RC_DATA_API_CALL AddVehicleDetailsScreenState | REFER_API_CALL AddVehicleDetailsScreenState | APPLICATION_STATUS_SCREEN | LOGOUT_USER | ONBOARDING_FLOW | DRIVER_PROFILE_SCREEN | RC_ACTIVATION AddVehicleDetailsScreenState

data TRIP_DETAILS_SCREEN_OUTPUT = ON_SUBMIT | GO_TO_HOME_SCREEN | OPEN_HELP_AND_SUPPORT

data PERMISSIONS_SCREEN_OUTPUT = DRIVER_HOME_SCREEN | LOGOUT_FROM_PERMISSIONS_SCREEN | GO_TO_REGISTERATION_SCREEN PermissionsScreenState

data ONBOARDING_SUBSCRIPTION_SCREENOUTPUT =  MAKE_PAYMENT_FROM_ONBOARDING OnBoardingSubscriptionScreenState | REGISTERATION_ONBOARDING OnBoardingSubscriptionScreenState

data HOME_SCREENOUTPUT = GO_TO_PROFILE_SCREEN
                          | GO_TO_RIDES_SCREEN
                          | GO_TO_REFERRAL_SCREEN_FROM_HOME_SCREEN
                          | GO_TO_HELP_AND_SUPPORT_SCREEN
                          | GO_TO_EDIT_GENDER_SCREEN
                          | GO_TO_START_RIDE {id :: String, otp :: String, lat :: String, lon :: String, ts :: String} HomeScreenState
                          | GO_TO_CANCEL_RIDE {id :: String, info :: String , reason :: String} HomeScreenState
                          | GO_TO_END_RIDE {id :: String, lat :: String , lon :: String, ts :: String } HomeScreenState
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
data ENTER_MOBILE_NUMBER_SCREEN_OUTPUT = GO_TO_ENTER_OTP EnterMobileNumberScreenState
data ENTER_OTP_SCREEN_OUTPUT = RETRY EnterOTPScreenState | DRIVER_INFO_API_CALL EnterOTPScreenState
data NO_INTERNET_SCREEN_OUTPUT = REFRESH_INTERNET | TURN_ON_GPS | CHECK_INTERNET
data POPUP_SCREEN_OUTPUT = POPUP_REQUEST_RIDE String Number
data DRIVER_RIDE_RATING_SCREEN_OUTPUT = CloseScreen | SendCustomerFeedBack DriverRideRatingScreenState
data NOTIFICATIONS_SCREEN_OUTPUT = REFRESH_SCREEN NotificationsScreenState
                                    | LOAD_NOTIFICATIONS NotificationsScreenState
                                    | GO_HOME_SCREEN
                                    | GO_REFERRAL_SCREEN
                                    | GO_RIDE_HISTORY_SCREEN
                                    | GO_PROFILE_SCREEN
                                    | CHECK_RIDE_FLOW_STATUS
                                    | NOTIFICATION_SCREEN_NAV NAVIGATION_ACTIONS

data BOOKING_OPTIONS_SCREEN_OUTPUT = SELECT_CAB BookingOptionsScreenState Boolean | GO_TO_PROFILE

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

data NAVIGATION_ACTIONS = HomeScreenNav
                          | GoToRideHistory
                          | GoToSubscription
                          | GoToContest
                          | GoToAlerts

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
data DRIVER_REFERRAL_SCREEN_OUTPUT = DRIVER_REFERRAL_SCREEN_NAV NAVIGATION_ACTIONS
                                   | DRIVER_CONTEST_SCREEN
