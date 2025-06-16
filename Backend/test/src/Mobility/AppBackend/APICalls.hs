{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.AppBackend.APICalls where

import qualified "rider-app" API.UI.Booking as AppBooking
import qualified "rider-app" API.UI.Cancel as CancelAPI
import qualified "rider-app" API.UI.Confirm as ConfirmAPI
import qualified "rider-app" API.UI.Rating as AppFeedback
import qualified "rider-app" API.UI.Registration as Reg
import qualified "rider-app" API.UI.Select as AppSelect
import qualified "rider-app" API.UI.Serviceability as AppServ
import qualified "rider-app" Domain.Action.UI.Cancel as CancelAPI
import qualified "rider-app" Domain.Types.Booking as BRB
import qualified "rider-app" Domain.Types.Booking.API as AbeBooking
import qualified "rider-app" Domain.Types.CancellationReason as AbeCRC
import qualified "rider-app" Domain.Types.Client as DC
import qualified "rider-app" Domain.Types.Estimate as AbeEstimate
import qualified "rider-app" Domain.Types.Quote as AbeQuote
import qualified "rider-app" Domain.Types.RegistrationToken as AppSRT
import qualified "rider-app" Domain.Types.Ride as BRide
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Whatsapp.Interface.Types (OptApiMethods (..))
import Kernel.Types.APISuccess
import Kernel.Types.App
import Kernel.Types.Id
import Kernel.Types.Version
import Mobility.AppBackend.Fixtures
import Servant hiding (Context)
import Servant.Client

selectQuote2 :: RegToken -> Id AbeEstimate.Estimate -> AppSelect.DSelectReq -> ClientM APISuccess
selectList :: RegToken -> Id AbeEstimate.Estimate -> ClientM AppSelect.SelectListRes
selectResult :: RegToken -> Id AbeEstimate.Estimate -> ClientM AppSelect.QuotesResultResponse
cancelSearch :: RegToken -> Id AbeEstimate.Estimate -> ClientM AppSelect.CancelAPIResponse
selectEstimate :: RegToken -> Id AbeEstimate.Estimate -> AppSelect.DSelectReq -> ClientM AppSelect.DSelectResultRes
selectEstimate :<|> selectQuote2 :<|> selectList :<|> selectResult :<|> cancelSearch :<|> _ = client (Proxy :: Proxy AppSelect.API)

cancelRide :: Id BRB.Booking -> Text -> CancelAPI.CancelReq -> ClientM APISuccess
cancelRide = client (Proxy :: Proxy CancelAPI.CancelAPI)

mkAppCancelReq :: AbeCRC.CancellationStage -> CancelAPI.CancelReq
mkAppCancelReq stage =
  CancelAPI.CancelReq (AbeCRC.CancellationReasonCode "OTHER") stage Nothing Nothing Nothing

appConfirmRide :: Text -> Id AbeQuote.Quote -> Maybe Payment.PaymentMethodId -> Maybe Bool -> ClientM ConfirmAPI.ConfirmRes
appConfirmRide = client (Proxy :: Proxy ConfirmAPI.API)

knowYourDriver :: Text -> Id BRide.Ride -> Maybe Bool -> ClientM AppFeedback.DriverProfileResponse
knowYourFavDriver :: Text -> Text -> Maybe Bool -> ClientM AppFeedback.DriverProfileResponse
appFeedback :: Text -> AppFeedback.FeedbackReq -> ClientM APISuccess
appFeedback :<|> knowYourDriver :<|> knowYourFavDriver = client (Proxy :: Proxy AppFeedback.API)

callAppFeedback :: Int -> Id BRide.Ride -> ClientM APISuccess
callAppFeedback ratingValue rideId =
  let request =
        AppFeedback.FeedbackReq
          { rideId = rideId,
            rating = ratingValue,
            feedbackDetails = Just "driver was well behaved!",
            wasRideSafe = Nothing,
            wasOfferedAssistance = Nothing,
            shouldFavDriver = Nothing,
            mbAudio = Nothing
          }
   in appFeedback appRegistrationToken request

appBookingStatus :: Id BRB.Booking -> Text -> ClientM AbeBooking.BookingAPIEntity
appBookingList :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe BRB.BookingStatus -> Maybe (Id DC.Client) -> Maybe Integer -> Maybe Integer -> [BRB.BookingStatus] -> ClientM AppBooking.BookingListRes
appBookingStatus :<|> _ :<|> appBookingList :<|> _ :<|> _ = client (Proxy :: Proxy AppBooking.API)

originServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
originServiceability regToken = origin
  where
    origin :<|> _ = client (Proxy :: Proxy AppServ.API) regToken

destinationServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
destinationServiceability regToken = destination
  where
    _ :<|> destination :<|> _ = client (Proxy :: Proxy AppServ.API) regToken

appAuth :: Reg.AuthReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM Reg.AuthRes
appSignatureAuth :: Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> ClientM Reg.AuthRes
appVerify :: Id AppSRT.RegistrationToken -> Reg.AuthVerifyReq -> ClientM Reg.AuthVerifyRes
appReInitiateLogin :: Id AppSRT.RegistrationToken -> ClientM Reg.ResendAuthRes
logout :: RegToken -> ClientM APISuccess
appAuth
  :<|> appSignatureAuth
  :<|> appVerify
  :<|> appReInitiateLogin
  :<|> logout =
    client (Proxy :: Proxy Reg.API)

mkAuthReq :: Reg.AuthReq
mkAuthReq =
  Reg.AuthReq
    { mobileNumber = Just "9000090000",
      mobileCountryCode = Just "+91",
      identifierType = Nothing,
      merchantId = "FIXME",
      deviceToken = Nothing,
      notificationToken = Nothing,
      whatsappNotificationEnroll = Nothing,
      firstName = Nothing,
      middleName = Nothing,
      lastName = Nothing,
      email = Nothing,
      language = Nothing,
      gender = Nothing,
      otpChannel = Nothing,
      registrationLat = Nothing,
      registrationLon = Nothing,
      enableOtpLessRide = Nothing,
      allowBlockedUserLogin = Nothing
    }

mkAuthVerifyReq :: Reg.AuthVerifyReq
mkAuthVerifyReq =
  Reg.AuthVerifyReq
    { otp = "7891",
      deviceToken = "AN_DEV_TOKEN",
      whatsappNotificationEnroll = Just OPT_IN
    }

initiateAuth :: ClientM Reg.AuthRes
initiateAuth = appAuth mkAuthReq (Just defaultVersion) (Just defaultVersion) Nothing Nothing Nothing Nothing

verifyAuth :: Id AppSRT.RegistrationToken -> ClientM Reg.AuthVerifyRes
verifyAuth tokenId = appVerify tokenId mkAuthVerifyReq
