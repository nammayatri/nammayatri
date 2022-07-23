module Mobility.AppBackend.APICalls where

import "app-backend" App.Routes as AbeRoutes
import Beckn.External.FCM.Types
import Beckn.Types.APISuccess
import Beckn.Types.App
import Beckn.Types.Id
import qualified "app-backend" Domain.Action.UI.Cancel as CancelAPI
import qualified "app-backend" Domain.Action.UI.Confirm as DConfirm
import qualified "app-backend" Domain.Action.UI.Init as DInit
import qualified "app-backend" Domain.Types.CancellationReason as AbeCRC
import qualified "app-backend" Domain.Types.Quote as AbeQuote
import qualified "app-backend" Domain.Types.RegistrationToken as AppSRT
import qualified "app-backend" Domain.Types.Ride as BRide
import qualified "app-backend" Domain.Types.RideBooking as BRB
import qualified "app-backend" Domain.Types.SelectedQuote as AbeSelQuote
import EulerHS.Prelude
import Mobility.AppBackend.Fixtures
import qualified "app-backend" Product.Cancel as CancelAPI
import qualified "app-backend" Product.Confirm as ConfirmAPI
import qualified "app-backend" Product.Init as InitAPI
import Servant hiding (Context)
import Servant.Client
import qualified "app-backend" Types.API.Feedback as AppFeedback
import qualified "app-backend" Types.API.Registration as Reg
import qualified "app-backend" Types.API.RideBooking as AppRideBooking
import qualified "app-backend" Types.API.Select as AppSelect
import qualified "app-backend" Types.API.Serviceability as AppServ

selectQuote :: RegToken -> Id AbeQuote.Quote -> ClientM APISuccess
selectList :: RegToken -> Id AbeQuote.Quote -> ClientM AppSelect.SelectListRes
selectQuote :<|> selectList = client (Proxy :: Proxy AbeRoutes.SelectAPI)

cancelRide :: Id BRB.RideBooking -> Text -> CancelAPI.CancelReq -> ClientM APISuccess
cancelRide = client (Proxy :: Proxy CancelAPI.CancelAPI)

mkAppCancelReq :: AbeCRC.CancellationStage -> CancelAPI.CancelReq
mkAppCancelReq stage =
  CancelAPI.CancelReq (AbeCRC.CancellationReasonCode "OTHER") stage Nothing

appInitRide :: Text -> DInit.InitReq -> ClientM InitAPI.InitRes
appInitRide = client (Proxy :: Proxy InitAPI.InitAPI)

mkAppInitReq :: Id AbeQuote.Quote -> DInit.InitReq
mkAppInitReq =
  flip DInit.InitReq False

mkAppInitReqSelected :: Id AbeSelQuote.SelectedQuote -> DInit.InitReq
mkAppInitReqSelected = flip DInit.InitReq True . cast

appConfirmRide :: Text -> DConfirm.ConfirmReq -> ClientM APISuccess
appConfirmRide = client (Proxy :: Proxy ConfirmAPI.ConfirmAPI)

confirmAddress :: DConfirm.ConfirmLocationReq
confirmAddress =
  DConfirm.ConfirmLocationReq
    { door = Just "#817",
      building = Just "Juspay Apartments",
      street = Just "27th Main",
      area = Just "8th Block Koramangala",
      city = Just "Bangalore",
      country = Just "India",
      areaCode = Just "560047",
      state = Just "Karnataka"
    }

mkAppConfirmReq :: Id BRB.RideBooking -> DConfirm.ConfirmReq
mkAppConfirmReq bookingId =
  DConfirm.ConfirmReq
    { bookingId = bookingId,
      fromLocation = confirmAddress,
      toLocation = Just confirmAddress
    }

appFeedback :: Text -> AppFeedback.FeedbackReq -> ClientM APISuccess
appFeedback = client (Proxy :: Proxy AbeRoutes.FeedbackAPI)

callAppFeedback :: Int -> Id BRide.Ride -> ClientM APISuccess
callAppFeedback ratingValue rideId =
  let request =
        AppFeedback.FeedbackReq
          { rideId = rideId,
            rating = ratingValue
          }
   in appFeedback appRegistrationToken request

appRideBookingStatus :: Id BRB.RideBooking -> Text -> ClientM AppRideBooking.RideBookingStatusRes
appRideBookingList :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> ClientM AppRideBooking.RideBookingListRes
appRideBookingStatus :<|> appRideBookingList = client (Proxy :: Proxy AbeRoutes.RideBookingAPI)

originServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
originServiceability regToken = origin
  where
    origin :<|> _ = client (Proxy :: Proxy AbeRoutes.ServiceabilityAPI) regToken

destinationServiceability :: RegToken -> AppServ.ServiceabilityReq -> ClientM AppServ.ServiceabilityRes
destinationServiceability regToken = destination
  where
    _ :<|> destination = client (Proxy :: Proxy AbeRoutes.ServiceabilityAPI) regToken

appAuth :: Reg.AuthReq -> ClientM Reg.AuthRes
appVerify :: Id AppSRT.RegistrationToken -> Reg.AuthVerifyReq -> ClientM Reg.AuthVerifyRes
appReInitiateLogin :: Id AppSRT.RegistrationToken -> ClientM Reg.ResendAuthRes
logout :: RegToken -> ClientM APISuccess
appAuth
  :<|> appVerify
  :<|> appReInitiateLogin
  :<|> logout =
    client (Proxy :: Proxy AbeRoutes.RegistrationAPI)

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
initiateAuth = appAuth mkAuthReq

verifyAuth :: Id AppSRT.RegistrationToken -> ClientM Reg.AuthVerifyRes
verifyAuth tokenId = appVerify tokenId mkAuthVerifyReq
