module API.Beckn.Confirm (API, handler) where

import qualified Beckn.ACL.Confirm as ACL
import qualified Beckn.ACL.OnConfirm as ACL
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
import Servant
import qualified SharedLogic.CallBAP as BP
import SharedLogic.DriverPool (incrementTotalRidesCount)
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Person as QPerson

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> Confirm.ConfirmAPI

handler :: FlowServer API
handler = confirm

confirm ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Confirm.ConfirmReq ->
  FlowHandler AckResponse
confirm transporterId (SignatureAuthResult _ subscriber _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Confirm API Flow" "Reached"
    dConfirmReq <- ACL.buildConfirmReq req
    Redis.whenWithLockRedis (confirmLockKey dConfirmReq.bookingId.getId) 60 $ do
      let context = req.context
      dConfirmRes <- DConfirm.handler subscriber transporterId dConfirmReq
      now <- getCurrentTime
      transporter <- QM.findById transporterId >>= fromMaybeM (MerchantNotFound transporterId.getId)
      driverQuote <- runInReplica $ QDQ.findById dConfirmRes.booking.quoteId >>= fromMaybeM (QuoteNotFound dConfirmRes.booking.quoteId.getId)
      driver <- runInReplica $ QPerson.findById driverQuote.driverId >>= fromMaybeM (PersonNotFound driverQuote.driverId.getId)
      fork "on_confirm/on_update" $ do
        handle (errHandler dConfirmRes transporter driver) $ do
          void $
            BP.callOnConfirm dConfirmRes.transporter context $ ACL.mkOnConfirmMessage now dConfirmRes
          void $
            BP.sendRideAssignedUpdateToBAP dConfirmRes.booking dConfirmRes.ride
      incrementTotalRidesCount transporterId driverQuote.driverId
    pure Ack
  where
    errHandler dConfirmRes transporter driver exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DConfirm.cancelBooking dConfirmRes.booking driver transporter
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DConfirm.cancelBooking dConfirmRes.booking driver transporter
      | otherwise = throwM exc

confirmLockKey :: Text -> Text
confirmLockKey id = "Driver:Confirm:BookingId-" <> id
