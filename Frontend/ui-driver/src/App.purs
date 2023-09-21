{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Types.App where

import Control.Transformers.Back.Trans (BackT)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Free (Free)
import Presto.Core.Types.Language.Flow (FlowWrapper)
import Screens.Types (AadhaarVerificationScreenState, AboutUsScreenState, ActiveRide,BookingOptionsScreenState, AddVehicleDetailsScreenState, AppUpdatePopUpScreenState, ApplicationStatusScreenState, BankDetailScreenState, CategoryListType, ChooseLanguageScreenState, DriverDetailsScreenState, DriverProfileScreenState, DriverRideRatingScreenState, DriverStatus, EditAadhaarDetailsScreenState, EditBankDetailsScreenState, EnterMobileNumberScreenState, EnterOTPScreenState, HelpAndSupportScreenState, HomeScreenState, IndividualRideCardState, NoInternetScreenState, NotificationsScreenState, PermissionsScreenState, PopUpScreenState, ReferralScreenState, RegistrationScreenState, ReportIssueChatScreenState, RideDetailScreenState, RideHistoryScreenState, RideSelectionScreenState, SelectLanguageScreenState, SplashScreenState, TripDetailsScreenState, UploadAdhaarScreenState, UploadDrivingLicenseState, VehicleDetailsScreenState, WriteToUsScreenState, AcknowledgementScreenState, UpdatePopupType(..), SubscriptionScreenState, OnBoardingSubscriptionScreenState, PaymentHistoryScreenState, HomeScreenStage(..), GlobalProps)
import Screens.ChooseLanguageScreen.ScreenData as ChooseLanguageScreenData
import Screens.EnterMobileNumberScreen.ScreenData as EnterMobileNumberScreenData
import Screens.AadhaarVerificationScreen.ScreenData as EnterAadhaarNumberScreenData
import Screens.EnterOTPScreen.ScreenData as  EnterOTPScreenData
import Screens.AddVehicleDetailsScreen.ScreenData as AddVehicleDetailsScreenData
import Screens.UploadDrivingLicenseScreen.ScreenData as UploadDrivingLicenseScreenData
import Screens.RegistrationScreen.ScreenData as RegistrationScreenData
import Screens.UploadAdhaarScreen.ScreenData as UploadAdhaarScreenData
import Screens.ApplicationStatusScreen.ScreenData as ApplicationStatusScreenData
import Screens.TripDetailsScreen.ScreenData as TripDetailsScreenData
import Screens.RideHistoryScreen.ScreenData as RideHistoryScreenData
import Screens.RideSelectionScreen.ScreenData as RideSelectionScreenData
import Screens.BankDetailScreen.ScreenData as BankDetailScreenData
import Screens.DriverProfileScreen.ScreenData as DriverProfileScreenData
import Screens.DriverDetailsScreen.ScreenData as DriverDetailsScreenData
import Screens.VehicleDetailsScreen.ScreenData as VehicleDetailsScreenData
import Screens.AboutUsScreen.ScreenData as AboutUsScreenData
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData
import Screens.HelpAndSupportScreen.ScreenData as HelpAndSupportScreenData
import Screens.WriteToUsScreen.ScreenData as WriteToUsScreenData
import Screens.PermissionsScreen.ScreenData as PermissionsScreenData
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.ReportIssueChatScreen.ScreenData as ReportIssueChatScreenData
import Screens.EditBankDetailsScreen.ScreenData as EditBankDetailsScreenData
import Screens.EditAadhaarDetailsScreen.ScreenData as EditAadhaarDetailsScreenData
import Screens.PopUpScreen.ScreenData as PopUpScreenData
import Screens.DriverRideRatingScreen.ScreenData as DriverRideRatingScreenData
import Screens.NotificationsScreen.ScreenData as NotificationsScreenData
import Screens.ReferralScreen.ScreenData as ReferralScreenData
import Screens.BookingOptionsScreen.ScreenData as BookingOptionsScreenData
import LoaderOverlay.ScreenData as LoaderOverlayScreenData
import Screens.AcknowledgementScreen.ScreenData as AcknowledgementScreenData
import Screens.SubscriptionScreen.ScreenData as SubscriptionScreenData
import Screens.PaymentHistoryScreen.ScreenData as PaymentHistoryScreenData
import Screens.OnBoardingSubscriptionScreen.ScreenData as OnBoardingSubscriptionScreenData
import Screens (ScreenName(..)) as ScreenNames

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
  }

defaultGlobalState :: GlobalState
defaultGlobalState = GlobalState{
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
, appUpdatePopUpScreen : {version : 1, updatePopup : NoUpdatePopup}
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
}

defaultGlobalProps :: GlobalProps
defaultGlobalProps = {
  aadhaarVerificationRequired : false,
  driverInformation : DriverProfileScreenData.dummyDriverInfo,
  callScreen : ScreenNames.HOME_SCREEN
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

data ScreenStage = HomeScreenStage HomeScreenStage

data MY_RIDES_SCREEN_OUTPUT = MY_RIDE RideHistoryScreenState
                            | HOME_SCREEN
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
data REGISTRATION_SCREENOUTPUT = UPLOAD_DRIVER_LICENSE

data UPLOAD_DRIVER_LICENSE_SCREENOUTPUT = ADD_VEHICLE_DETAILS_SCREEN UploadDrivingLicenseState | VALIDATE_IMAGE_API UploadDrivingLicenseState | GOTO_VEHICLE_DETAILS_SCREEN | LOGOUT_ACCOUNT | GOTO_ONBOARDING_FLOW

data UPLOAD_ADHAAR_CARD_SCREENOUTPUT = GO_TO_ADD_BANK_DETAILS

data BANK_DETAILS_SCREENOUTPUT = GO_TO_ADD_VEHICLE_DETAILS

data ADD_VEHICLE_DETAILS_SCREENOUTPUT = GO_TO_APPLICATION_SCREEN AddVehicleDetailsScreenState | VALIDATE_IMAGE_API_CALL AddVehicleDetailsScreenState | REFER_API_CALL AddVehicleDetailsScreenState | APPLICATION_STATUS_SCREEN | LOGOUT_USER | ONBOARDING_FLOW | DRIVER_PROFILE_SCREEN

data TRIP_DETAILS_SCREEN_OUTPUT = ON_SUBMIT | GO_TO_HOME_SCREEN | OPEN_HELP_AND_SUPPORT

data PERMISSIONS_SCREEN_OUTPUT = DRIVER_HOME_SCREEN

data ONBOARDING_SUBSCRIPTION_SCREENOUTPUT = GOTO_HOME_SCREEN_FROM_ONBOARDING_SUBSCRIPTION_SCREEN | MAKE_PAYMENT_FROM_ONBOARDING OnBoardingSubscriptionScreenState

data HOME_SCREENOUTPUT = GO_TO_PROFILE_SCREEN
                          | GO_TO_RIDES_SCREEN
                          | GO_TO_REFERRAL_SCREEN_FROM_HOME_SCREEN
                          | GO_TO_HELP_AND_SUPPORT_SCREEN
                          | GO_TO_EDIT_GENDER_SCREEN
                          | GO_TO_START_RIDE {id :: String, otp :: String, lat :: String, lon :: String} HomeScreenState
                          | GO_TO_CANCEL_RIDE {id :: String, info :: String , reason :: String}
                          | GO_TO_END_RIDE {id :: String, lat :: String , lon :: String }
                          | DRIVER_AVAILABILITY_STATUS HomeScreenState DriverStatus
                          | REFRESH_HOME_SCREEN_FLOW
                          | RELOAD HomeScreenState
                          | UPDATE_ROUTE HomeScreenState
                          | FCM_NOTIFICATION String
                          | NOTIFY_CUSTOMER HomeScreenState
                          | UPDATE_STAGE HomeScreenStage
                          | GO_TO_NOTIFICATIONS
                          | ADD_ALTERNATE_HOME
                          | GO_TO_AADHAAR_VERIFICATION
                          | GO_TO_START_ZONE_RIDE {otp :: String, lat :: String, lon :: String}
                          | ON_CALL HomeScreenState
                          | OPEN_PAYMENT_PAGE HomeScreenState
                          | HOMESCREEN_NAV NAVIGATION_ACTIONS
                          | GO_TO_VEHICLE_DETAILS_SCREEN
                          | GO_TO_RIDE_DETAILS_SCREEN 
                          | POST_RIDE_FEEDBACK HomeScreenState

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

data BOOKING_OPTIONS_SCREEN_OUTPUT = SELECT_CAB BookingOptionsScreenState | GO_TO_PROFILE

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

data NAVIGATION_ACTIONS = HomeScreenNav
                          | GoToRideHistory
                          | GoToSubscription
                          | GoToContest
                          | GoToAlerts

data PAYMENT_HISTORY_SCREEN_OUTPUT = ViewDetails PaymentHistoryScreenState

data APP_UPDATE_POPUP = Later | UpdateNow
