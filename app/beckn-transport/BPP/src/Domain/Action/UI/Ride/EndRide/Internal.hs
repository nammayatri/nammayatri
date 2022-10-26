module Domain.Action.UI.Ride.EndRide.Internal where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import Domain.Types.Organization
import Domain.Types.Person (Driver)
import qualified Domain.Types.Ride as SRide
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Organization as CQOrg
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.FarePolicy.FareBreakup as QFareBreakup
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Metrics as Metrics

endRideTransaction :: EsqDBFlow m r => Id SRB.Booking -> SRide.Ride -> Id Driver -> [DFareBreakup.FareBreakup] -> m ()
endRideTransaction bookingId ride driverId fareBreakups = Esq.runTransaction $ do
  QRide.updateAll ride.id ride
  QRide.updateStatus ride.id SRide.COMPLETED
  QRB.updateStatus bookingId SRB.COMPLETED
  DriverInformation.updateOnRide driverId False
  DriverStats.updateIdleTime driverId
  traverse_ QFareBreakup.create fareBreakups

putDiffMetric :: (Metrics.HasBPPMetrics m r, HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id Organization -> Money -> Meters -> m ()
putDiffMetric orgId money mtrs = do
  org <- CQOrg.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId)
  Metrics.putFareAndDistanceDeviations org.name money mtrs
