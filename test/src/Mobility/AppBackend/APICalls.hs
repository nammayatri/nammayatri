module Mobility.AppBackend.APICalls where

import qualified "app-backend" API.UI.Booking as AppBooking
import qualified "app-backend" API.UI.Cancel as CancelAPI
import qualified "app-backend" API.UI.Confirm as ConfirmAPI
import qualified "app-backend" API.UI.Feedback as AppFeedback
import qualified "app-backend" API.UI.Registration as Reg
import qualified "app-backend" API.UI.Select as AppSelect
import qualified "app-backend" API.UI.Serviceability as AppServ
import Beckn.External.FCM.Types
import Beckn.Types.APISuccess
import Beckn.Types.App
import Beckn.Types.Id
import qualified "app-backend" Domain.Action.UI.Cancel as CancelAPI
import qualified "app-backend" Domain.Types.Booking as AbeBooking
import qualified "app-backend" Domain.Types.Booking as BRB
import qualified "app-backend" Domain.Types.CancellationReason as AbeCRC
import qualified "app-backend" Domain.Types.Estimate as AbeEstimate
import qualified "app-backend" Domain.Types.Quote as AbeQuote
import qualified "app-backend" Domain.Types.RegistrationToken as AppSRT
import qualified "app-backend" Domain.Types.Ride as BRide
import EulerHS.Prelude
import Mobility.AppBackend.Fixtures
import Servant hiding (Context)
import Servant.Client

selectQuote :: RegToken -> Id AbeEstimate.Estimate -> ClientM APISuccess
selectList :: RegToken -> Id AbeEstimate.Estimate -> ClientM AppSelect.SelectListRes
selectQuote :<|> selectList = client (Proxy :: Proxy AppSelect.API)

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
            feedbackDetails = Just "driver is so qt!"
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

appAuth :: Reg.AuthReq -> Maybe Text -> Maybe Text -> ClientM Reg.AuthRes
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
      merchantId = "FIXME"
    }

mkAuthVerifyReq :: Reg.AuthVerifyReq
mkAuthVerifyReq =
  Reg.AuthVerifyReq
    { otp = "7891",
      deviceToken = FCMRecipientToken "AN_DEV_TOKEN"
    }

initiateAuth :: ClientM Reg.AuthRes
initiateAuth = appAuth mkAuthReq (Just "0.0.0") (Just "0.0.0")

verifyAuth :: Id AppSRT.RegistrationToken -> ClientM Reg.AuthVerifyRes
verifyAuth tokenId = appVerify tokenId mkAuthVerifyReq
