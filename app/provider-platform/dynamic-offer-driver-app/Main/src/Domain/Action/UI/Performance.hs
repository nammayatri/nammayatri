module Domain.Action.UI.Performance where

import qualified Domain.Types.Person as SP
import Domain.Types.RiderDetails ()
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBFlow, EsqDBReplicaFlow)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error (PersonError (PersonNotFound))
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import Storage.CachedQueries.CacheConfig (CacheFlow)
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

getDriverPerformance :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SP.Person -> m PerformanceRes
getDriverPerformance driverId = do
  _ <- Esq.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  allRefferedCustomers <- QRD.findAllReferredByDriverId driverId
  let ridesTakenList = filter (.hasTakenRide) allRefferedCustomers
  pure $ PerformanceRes (Results (length allRefferedCustomers) (length ridesTakenList))