module API.Internal
  ( API,
    handler,
  )
where

import qualified API.Action.UI.AlertWebhook as AlertWebhook
import qualified API.Action.UI.FRFSInternal as FRFSInternal
import qualified API.Action.UI.InsuranceInternal as InsuranceInternal
import qualified API.Action.UI.MeterRideInternal as MeterRideInternal
import qualified API.Action.UI.ZendeskWebhook as ZendeskWebhook
import qualified API.Internal.Auth as Auth
import qualified API.Internal.Cac as Cac
import qualified API.Internal.DriverArrivalNotf as DriverArrivalNotf
import qualified API.Internal.EKDLiveCallFeedback as EKDLiveCallFeedback
import qualified API.Internal.FRFS as FRFS
import qualified API.Internal.FRFSBooking as FRFSBooking
import qualified API.Internal.FrequentLocUser as FrequentLocUser
import qualified API.Internal.GetPickupInstructions as GetPickupInstructions
import qualified API.Internal.InMemManagement as InMemManagement
import qualified API.Internal.NotificationWebhook as NotificationWebhook
import qualified API.Internal.OfferDiscount as OfferDiscount
import qualified API.Internal.Rating as Rating
import qualified API.Internal.RideSearchExpired as RideSearchExpired
import qualified API.Internal.SendEmailOTP as SendEmailOTP
import qualified API.Internal.SendSMS as SendSMS
import qualified API.Internal.Sos as Sos
import qualified API.Internal.StopEvents as StopEvents
import qualified API.Internal.UpdateCancellationFeeStatus as UpdateCancellationFeeStatus
import qualified API.Internal.VerifyEmailUpdate as VerifyEmailUpdate
import qualified API.Internal.ViolationDetection as ViolationDetection
import qualified API.Internal.XyneWebhook as XyneWebhook
import Environment
import Servant
import Tools.Auth ()

type API =
  "internal"
    :> ( Auth.API
           :<|> Rating.API
           :<|> FRFS.API
           :<|> FRFSBooking.API
           :<|> Cac.API
           :<|> StopEvents.API
           :<|> FrequentLocUser.API
           :<|> DriverArrivalNotf.API
           :<|> MeterRideInternal.API
           :<|> InsuranceInternal.API
           :<|> ViolationDetection.API
           :<|> RideSearchExpired.API
           :<|> GetPickupInstructions.API
           :<|> AlertWebhook.API
           :<|> EKDLiveCallFeedback.API
           :<|> Sos.API
           :<|> UpdateCancellationFeeStatus.API
           :<|> OfferDiscount.API
           :<|> SendSMS.API
           :<|> SendEmailOTP.API
           :<|> VerifyEmailUpdate.API
           :<|> InMemManagement.API
           :<|> FRFSInternal.API
           :<|> ZendeskWebhook.API
           :<|> XyneWebhook.API
           :<|> NotificationWebhook.API
           :<|> XyneWebhook.BearerAPI
       )

handler :: FlowServer API
handler =
  Auth.handler
    :<|> Rating.handler
    :<|> FRFS.handler
    :<|> FRFSBooking.handler
    :<|> Cac.handler
    :<|> StopEvents.handler
    :<|> FrequentLocUser.handler
    :<|> DriverArrivalNotf.handler
    :<|> MeterRideInternal.handler
    :<|> InsuranceInternal.handler
    :<|> ViolationDetection.handler
    :<|> RideSearchExpired.handler
    :<|> GetPickupInstructions.handler
    :<|> AlertWebhook.handler
    :<|> EKDLiveCallFeedback.handler
    :<|> Sos.handler
    :<|> UpdateCancellationFeeStatus.handler
    :<|> OfferDiscount.handler
    :<|> SendSMS.handler
    :<|> SendEmailOTP.handler
    :<|> VerifyEmailUpdate.handler
    :<|> InMemManagement.handler
    :<|> FRFSInternal.handler
    :<|> ZendeskWebhook.handler
    :<|> XyneWebhook.handler
    :<|> NotificationWebhook.handler
    :<|> XyneWebhook.bearerHandler
