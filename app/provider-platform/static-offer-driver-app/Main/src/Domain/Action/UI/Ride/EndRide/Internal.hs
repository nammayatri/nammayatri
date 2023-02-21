module Domain.Action.UI.Ride.EndRide.Internal where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import Domain.Types.Merchant
import Domain.Types.Person (Driver)
import qualified Domain.Types.Ride as SRide
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverLocation as SDrLoc
import qualified SharedLogic.Ride as SRide
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.FarePolicy.FareBreakup as QFareBreakup
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Metrics as Metrics

endRideTransaction :: (CacheFlow m r, EsqDBFlow m r) => Id Driver -> Id SRB.Booking -> SRide.Ride -> [DFareBreakup.FareBreakup] -> m ()
endRideTransaction driverId bookingId ride fareBreakups = do
  Esq.runTransaction $ do
    QRide.updateAll ride.id ride
    QRide.updateStatus ride.id SRide.COMPLETED
    QRB.updateStatus bookingId SRB.COMPLETED
    DriverInformation.updateOnRide driverId False
    DriverStats.updateIdleTime driverId
    traverse_ QFareBreakup.create fareBreakups
  SRide.clearCache $ cast driverId
  SDrLoc.clearDriverInfoCache driverId

-- DLoc.updateOnRide driverId False -- FIXME it should be the same as in dynamic-offer-driver-app

putDiffMetric :: (Metrics.HasBPPMetrics m r, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Money -> Meters -> m ()
putDiffMetric merchantId money mtrs = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  Metrics.putFareAndDistanceDeviations org.name money mtrs
