module Domain.Action.UI.Performance where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Domain.Types.RiderDetails ()
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBFlow, EsqDBReplicaFlow)
import Kernel.Types.Error (PersonError (PersonNotFound))
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, fromMaybeM)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RiderDetails as QRD

data Results = Results
  { totalReferredCustomers :: Int,
    totalActivatedCustomers :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype PerformanceRes = PerformanceRes
  { referrals :: Results
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

getDriverPerformance :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> m PerformanceRes
getDriverPerformance (driverId, _, _) = do
  _ <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  allRefferedCustomers <- QRD.findAllReferredByDriverId driverId
  let ridesTakenList = filter (.hasTakenValidRide) allRefferedCustomers
  pure $ PerformanceRes (Results (length allRefferedCustomers) (length ridesTakenList))
