module Domain.Action.UI.Performance where

import Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.LeaderBoardConfig as LConfig
import qualified Domain.Types.Person as SP
import Domain.Types.RiderDetails ()
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBFlow, EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error (PersonError (PersonNotFound))
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, fromMaybeM, getCurrentTime)
import qualified Storage.CachedQueries.DriverInformation as CQDI
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RiderDetails as QRD

data Results = Results
  { totalReferredCustomers :: Int,
    totalActivatedCustomers :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data PerformanceRes = PerformanceRes
  { referrals :: Results,
    referralCode :: Text,
    totalDriver :: Int,
    currRideRank :: Integer,
    currReferralRank :: Integer
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

getDriverPerformance :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => (Id SP.Person, Id DM.Merchant) -> m PerformanceRes
getDriverPerformance (driverId, merchantId) = do
  _ <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  allRefferedCustomers <- QRD.findAllReferredByDriverId driverId
  let ridesTakenList = filter (.hasTakenValidRide) allRefferedCustomers
  driverReferralCode <- fmap (.referralCode) <$> QDR.findById (cast driverId)
  nowUtc <- getCurrentTime
  let today = RideEndInt.getCurrentDate nowUtc

  mbCurrPersonRideRank <- Redis.withNonCriticalRedis $ Redis.zRevRank (RideEndInt.makeDailyDriverLeaderBoardKey merchantId today LConfig.RIDE) driverId.getId
  currPersonRideRank <- case mbCurrPersonRideRank of
    Nothing -> Redis.withNonCriticalRedis $ Redis.zCard (RideEndInt.makeDailyDriverLeaderBoardKey merchantId today LConfig.RIDE)
    Just rank -> pure rank

  mbCurrPersonReferralRank <- Redis.withNonCriticalRedis $ Redis.zRevRank (RideEndInt.makeDailyDriverLeaderBoardKey merchantId today LConfig.REFERRAL) driverId.getId
  currPersonReferralRank <- case mbCurrPersonReferralRank of
    Nothing -> Redis.withNonCriticalRedis $ Redis.zCard (RideEndInt.makeDailyDriverLeaderBoardKey merchantId today LConfig.REFERRAL)
    Just rank -> pure rank

  (active, inactive) <- CQDI.totalDrivers merchantId

  case driverReferralCode of
    Just code -> pure $ PerformanceRes (Results (length allRefferedCustomers) (length ridesTakenList)) code.getId (active + inactive) currPersonRideRank currPersonReferralRank
    Nothing -> pure $ PerformanceRes (Results (length allRefferedCustomers) (length ridesTakenList)) "" (active + inactive) currPersonRideRank currPersonReferralRank
