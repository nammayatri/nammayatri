module Domain.Action.UI.Ride.EndRide.Internal
  ( endRideTransaction,
    putDiffMetric,
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as SRB
import Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as Ride
import EulerHS.Prelude hiding (id)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Metrics as Metrics

endRideTransaction :: EsqDBFlow m r => Id SRB.Booking -> Ride.Ride -> Id DP.Driver -> m ()
endRideTransaction bookingId ride driverId = Esq.runTransaction $ do
  QRide.updateAll ride.id ride
  QRide.updateStatus ride.id Ride.COMPLETED
  QRB.updateStatus bookingId SRB.COMPLETED
  DriverInformation.updateOnRide driverId False
  DriverStats.updateIdleTime driverId

putDiffMetric :: (Metrics.HasBPPMetrics m r, HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id Merchant -> Money -> Meters -> m ()
putDiffMetric merchantId money mtrs = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  Metrics.putFareAndDistanceDeviations org.name money mtrs
