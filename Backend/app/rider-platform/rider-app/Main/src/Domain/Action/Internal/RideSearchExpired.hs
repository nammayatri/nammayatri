module Domain.Action.Internal.RideSearchExpired where

import Data.Foldable ()
import Data.OpenApi (ToSchema)
import qualified Domain.Types.SearchRequest as DSearchRequest
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Types (ServiceFlow)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.CacheFlow
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Storage.Queries.SearchRequest as SQ
import Tools.Notifications

newtype RideSearchExpiredReq = RideSearchExpiredReq
  { transactionId :: Id DSearchRequest.SearchRequest
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

rideSearchExpired ::
  ( MonadFlow m,
    ServiceFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["internalAPIKey" ::: Text]
  ) =>
  Maybe Text ->
  RideSearchExpiredReq ->
  m APISuccess
rideSearchExpired apiKey RideSearchExpiredReq {..} = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  searchReq <- B.runInReplica $ SQ.findById transactionId
  whenJust searchReq notifyOnRideSearchExpired
  pure Success
