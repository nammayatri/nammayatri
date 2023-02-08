module Domain.Action.Beckn.Cancel
  ( cancel,
    CancelReq (..),
  )
where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.RideRequest as SRideRequest
import EulerHS.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.RideRequest as RideRequest
import Tools.Error

newtype CancelReq = CancelReq
  { bookingId :: Id SRB.Booking
  }

cancel ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  SignatureAuthResult ->
  CancelReq ->
  m ()
cancel transporterId _ req = do
  merchant <-
    QM.findById transporterId
      >>= fromMaybeM (MerchantNotFound transporterId.getId)
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  let transporterId' = booking.providerId
  unless (transporterId' == transporterId) $ throwError AccessDenied
  rideReq <- buildRideReq booking.id merchant.subscriberId
  Esq.runTransaction $ RideRequest.create rideReq
  where
    buildRideReq bookingId subscriberId = do
      guid <- generateGUID
      now <- getCurrentTime
      pure
        SRideRequest.RideRequest
          { id = Id guid,
            bookingId = bookingId,
            subscriberId = subscriberId,
            createdAt = now,
            _type = SRideRequest.CANCELLATION,
            info = Nothing
          }
