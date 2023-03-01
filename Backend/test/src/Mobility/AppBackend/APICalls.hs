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
import qualified "rider-app" API.UI.Feedback as AppFeedback
import qualified "rider-app" API.UI.Registration as Reg
import qualified "rider-app" API.UI.Select as AppSelect
import qualified "rider-app" API.UI.Serviceability as AppServ
import qualified "rider-app" Domain.Action.UI.Cancel as CancelAPI
import qualified "rider-app" Domain.Action.UI.Select as DSelect
import qualified "rider-app" Domain.Types.Booking as AbeBooking
import qualified "rider-app" Domain.Types.Booking as BRB
import qualified "rider-app" Domain.Types.CancellationReason as AbeCRC
import qualified "rider-app" Domain.Types.Estimate as AbeEstimate
import qualified "rider-app" Domain.Types.Quote as AbeQuote
import qualified "rider-app" Domain.Types.RegistrationToken as AppSRT
import qualified "rider-app" Domain.Types.Ride as BRide
import EulerHS.Prelude
import Kernel.External.FCM.Types
import Kernel.External.Whatsapp.Interface.Types (OptApiMethods (..))
import Kernel.Types.APISuccess
import Kernel.Types.App
import Kernel.Types.Id
import Kernel.Types.Version
import Mobility.AppBackend.Fixtures
import Servant hiding (Context)
import Servant.Client

selectQuote :: RegToken -> Id AbeEstimate.Estimate -> AppSelect.DSelectReq -> ClientM APISuccess
selectQuote2 :: RegToken -> Id AbeEstimate.Estimate -> DSelect.DEstimateSelectReq -> ClientM APISuccess
selectList :: RegToken -> Id AbeEstimate.Estimate -> ClientM AppSelect.SelectListRes
cancelSearch :: RegToken -> Id AbeEstimate.Estimate -> ClientM APISuccess
selectQuote :<|> selectQuote2 :<|> selectList :<|> cancelSearch = client (Proxy :: Proxy AppSelect.API)

cancelRide :: Id BRB.Booking -> Text -> CancelAPI.CancelReq -> ClientM APISuccess
cancelRide = client (Proxy :: Proxy CancelAPI.API)

mkAppCancelReq :: AbeCRC.CancellationStage -> CancelAPI.CancelReq
mkAppCancelReq stage =
  CancelAPI.CancelReq (AbeCRC.CancellationReasonCode "OTHER") stage Nothing

appConfirmRide :: Text -> Id AbeQuote.Quote -> ClientM ConfirmAPI.ConfirmRes
appConfirmRide = client (Proxy :: Proxy ConfirmAPI.API)

appFeedback :: Text -> AppFeedback.FeedbackReq -> ClientM APISuccess
appFeedback = client (Proxy :: Proxy AppFeedback.API)

callAppFeedback :: Int -> Id BRide.Ride -> ClientM APISuccess
callAppFeedback ratingValue rideId =
  let request =
        AppFeedback.FeedbackReq
          { rideId = rideId,
            rating = ratingValue,
            feedbackDetails = Just "driver was well behaved!"
          }
   in appFeedback appRegistrationToken request

appBookingStatus :: Id BRB.Booking -> Text -> ClientM AbeBooking.BookingAPIEntity
appBookingList :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe BRB.BookingStatus -> ClientM AppBooking.BookingListRes
appBookingStatus :<|> appBookingList = client (Proxy :: Proxy AppBooking.API)

originServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
originServiceability regToken = origin
  where
    origin :<|> _ = client (Proxy :: Proxy AppServ.API) regToken

destinationServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
destinationServiceability regToken = destination
  where
    _ :<|> destination = client (Proxy :: Proxy AppServ.API) regToken

appAuth :: Reg.AuthReq -> Maybe Version -> Maybe Version -> ClientM Reg.AuthRes
appVerify :: Id AppSRT.RegistrationToken -> Reg.AuthVerifyReq -> ClientM Reg.AuthVerifyRes
appReInitiateLogin :: Id AppSRT.RegistrationToken -> ClientM Reg.ResendAuthRes
logout :: RegToken -> ClientM APISuccess
appAuth
  :<|> appVerify
  :<|> appReInitiateLogin
  :<|> logout =
    client (Proxy :: Proxy Reg.API)

mkAuthReq :: Reg.AuthReq
mkAuthReq =
  Reg.AuthReq
    { mobileNumber = "9000090000",
      mobileCountryCode = "+91",
      merchantId = "FIXME" --,
      -- otpChannel = Nothing
    }

mkAuthVerifyReq :: Reg.AuthVerifyReq
mkAuthVerifyReq =
  Reg.AuthVerifyReq
    { otp = "7891",
      deviceToken = FCMRecipientToken "AN_DEV_TOKEN",
      whatsappNotificationEnroll = Just OPT_IN
    }

initiateAuth :: ClientM Reg.AuthRes
initiateAuth = appAuth mkAuthReq (Just defaultVersion) (Just defaultVersion)

verifyAuth :: Id AppSRT.RegistrationToken -> ClientM Reg.AuthVerifyRes
verifyAuth tokenId = appVerify tokenId mkAuthVerifyReq
