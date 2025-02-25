{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Handlers (module UI) where

import Screens.AboutUsScreen.Handler (aboutUsScreen) as UI
import Screens.AddVehicleDetailsScreen.Handler (addVehicleDetails) as UI
import Screens.AppUpdatePopUpScreen.Handler (handleAppUpdatePopUp) as UI
import Screens.ApplicationStatusScreen.Handler (applicationStatus) as UI
import Screens.BankDetailScreen.Handler (bankDetail) as UI
import Screens.ChooseLanguageScreen.Handler (chooseLanguage) as UI
import Screens.DriverDetailsScreen.Handler (driverDetailsScreen) as UI
import Screens.DriverProfileScreen.Handler (driverProfileScreen) as UI
import Screens.DriverRideRatingScreen.Handler (driverRideRatingScreen) as UI
import Screens.EditAadhaarDetailsScreen.Handler (editAadhaarDetailsScreen) as UI
import Screens.EditBankDetailsScreen.Handler (editBankDetailsScreen) as UI
import Screens.EnterMobileNumberScreen.Handler (enterMobileNumber) as UI
import Screens.EnterOTPScreen.Handler (enterOTP) as UI
import Screens.HelpAndSupportScreen.Handler (helpAndSupportScreen) as UI
import Screens.HomeScreen.Handler (homeScreen) as UI
import Screens.NoInternetScreen.Handler (noInternetScreen) as UI
import Screens.PermissionsScreen.Handler (permissions) as UI
import Screens.PopUpScreen.Handler (popUpScreen) as UI
import Screens.RegistrationScreen.Handler (registration) as UI
import Screens.RideHistoryScreen.Handler (rideHistory) as UI
import Screens.ReferralScreen.Handler (referralScreen) as UI
import Screens.SelectLanguageScreen.Handler (selectLanguageScreen) as UI
import Screens.SplashScreen.Handler (splashScreen) as UI
import Screens.TripDetailsScreen.Handler (tripDetailsScreen) as UI
import Screens.UploadAdhaarScreen.Handler (uploadAdhaar) as UI
import Screens.UploadDrivingLicenseScreen.Handler (uploadDrivingLicense) as UI
import Screens.VehicleDetailsScreen.Handler (vehicleDetailsScreen) as UI
import Screens.WriteToUsScreen.Handler (writeToUsScreen) as UI
import Screens.NotificationsScreen.Handler (notifications) as UI
import Screens.BookingOptionsScreen.Handler (bookingOptions) as UI
import Screens.RideSelectionScreen.Handler (rideSelection) as UI
import Screens.ReportIssueChatScreen.Handler (reportIssueChatScreen) as UI
import Screens.AcknowledgementScreen.Handler (acknowledgementScreen) as UI
import Screens.AadhaarVerificationScreen.Handler (aadhaarVerificationScreen) as UI
import Screens.SubscriptionScreen.Handler (subscriptionScreen) as UI
import Screens.PaymentHistoryScreen.Handler (paymentHistory) as UI
import Screens.OnBoardingSubscriptionScreen.Handler (onBoardingSubscriptionScreen) as UI
import Screens.DriverSavedLocationScreen.Handler (driverSavedLocationScreen) as UI
import Screens.ChooseCityScreen.Handler (chooseCityScreen) as UI
import Screens.WelcomeScreen.Handler (welcomeScreen) as UI
import Screens.DriverEarningsScreen.Handler (driverEarningsScreen) as UI
import Screens.Benefits.BenefitsScreen.Handler (benefitsScreen) as UI
import Screens.Benefits.LmsVideoScreen.Handler (lmsVideoScreen) as UI
import Screens.Benefits.LmsQuizScreen.Handler (lmsQuizScreen) as UI
import Screens.DocumentCaptureScreen.Handler (documentCaptureScreen) as UI
import Screens.DocumentDetailsScreen.Handler (documentDetailsScreen) as UI
import Screens.DriverCompleteProfileScreen.Handler (driverCompleteProfileScreen) as UI
import Screens.RateCardScreen.Handler (rateCardScreen) as UI
import Screens.CustomerReferralTrackerScreen.Handler (customerReferralTrackerScreen) as UI
import Screens.CancellationRateScreen.Handler (cancellationRateScreen) as UI
import Screens.HotspotScreen.Handler (hotspotScreen) as UI
import Screens.RideRequestScreen.Handler (rideRequestScreen) as UI
import Screens.RideSummaryScreen.Handler (rideSummaryScreen) as UI
import Screens.ScheduledRideAcceptedScreen.Handler (scheduledRideAcceptedScreen) as UI
import Screens.UploadParcelImageScreen.Handler (uploadParcelImageScreen) as UI
import Screens.MetroWarriorsScreen.Handler (metroWarriorsScreen) as UI
import Screens.MeterScreen.Handler (meterScreen) as UI
