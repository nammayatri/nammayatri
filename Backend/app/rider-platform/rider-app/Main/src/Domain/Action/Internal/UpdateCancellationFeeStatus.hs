module Domain.Action.Internal.UpdateCancellationFeeStatus where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Storage.Queries.Ride as QRide

newtype UpdateCancellationFeeStatusReq = UpdateCancellationFeeStatusReq
  { bppRideIds :: [Text]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

updateCancellationFeeStatus ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["internalAPIKey" ::: Text]
  ) =>
  Maybe Text ->
  UpdateCancellationFeeStatusReq ->
  m APISuccess
updateCancellationFeeStatus apiKey UpdateCancellationFeeStatusReq {..} = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  forM_ bppRideIds $ \bppRideId -> do
    mbRide <- B.runInReplica $ QRide.findByBPPRideId (Id bppRideId)
    case mbRide of
      Just ride -> QRide.updateCancellationFeeStatus (Just DRide.CLEARED) ride.id
      Nothing -> logWarning $ "UpdateCancellationFeeStatus: Ride not found for bppRideId: " <> bppRideId
  pure Success
