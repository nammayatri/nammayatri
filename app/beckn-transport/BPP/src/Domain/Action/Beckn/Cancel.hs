module Domain.Action.Beckn.Cancel
  ( cancel,
    CancelReq (..),
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.RideRequest as SRideRequest
import EulerHS.Prelude
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.RideRequest as RideRequest
import Tools.Error

newtype CancelReq = CancelReq
  { bookingId :: Id SRB.Booking
  }

cancel ::
  (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) =>
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
