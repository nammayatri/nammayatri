module Domain.Action.UI.Ride.EndRide.Internal
  ( endRideTransaction,
    putDiffMetric,
  )
where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.FareParameters as DFare
import Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RiderDetails as RD
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverLocation as DLoc
import qualified SharedLogic.Ride as SRide
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.DriverInformation as CDI
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.FareParameters as QFare
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error
import qualified Tools.Metrics as Metrics

endRideTransaction ::
  (CacheFlow m r, EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)) =>
  Id DP.Driver ->
  Id SRB.Booking ->
  Ride.Ride ->
  Maybe DFare.FareParameters ->
  Maybe (Id RD.RiderDetails) ->
  m ()
endRideTransaction driverId bookingId ride mbFareParams mbRiderDetailsId = do
  driverInfo <- CDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  mbRiderDetails <- join <$> QRD.findById `mapM` mbRiderDetailsId
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  let shouldUpdateRideComplete =
        case minTripDistanceForReferralCfg of
          Just distance -> ride.traveledDistance >= distance
          Nothing -> True
  Esq.runTransaction $ do
    whenJust mbRiderDetails $ \riderDetails ->
      when (not riderDetails.hasTakenRide && shouldUpdateRideComplete) do
        QRD.updateHasTakenRide riderDetails.id
    whenJust mbFareParams QFare.create
    QRide.updateAll ride.id ride
    QRide.updateStatus ride.id Ride.COMPLETED
    QRB.updateStatus bookingId SRB.COMPLETED
    DriverStats.updateIdleTime driverId
    if driverInfo.active
      then QDFS.updateStatus ride.driverId DDFS.ACTIVE
      else QDFS.updateStatus ride.driverId DDFS.IDLE
  DLoc.updateOnRide driverId False
  SRide.clearCache $ cast driverId

putDiffMetric :: (Metrics.HasBPPMetrics m r, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Money -> Meters -> m ()
putDiffMetric merchantId money mtrs = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  Metrics.putFareAndDistanceDeviations org.name money mtrs
