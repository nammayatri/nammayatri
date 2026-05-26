module Lib.Communication.Domain.Action.Dispatch
  ( processCommunicationDeliveryJob,
  )
where

import qualified Lib.Communication.Domain.Types.CommunicationDelivery as DDelivery
import qualified Lib.Communication.Storage.Queries.CommunicationDelivery as QDelivery
import Lib.Communication.Scheduler.JobData (CommunicationDeliveryDispatchJobData (..))
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common

-- | Runs the given dispatch action, then updates the delivery status to DS_SENT
-- on success or DS_FAILED on any exception. Both driver and rider job handlers
-- call this so the try/catch and status-update logic is not duplicated.
processCommunicationDeliveryJob ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  CommunicationDeliveryDispatchJobData ->
  (CommunicationDeliveryDispatchJobData -> m ()) ->
  m ()
processCommunicationDeliveryJob jobData dispatchFn = do
  let deliveryId = Kernel.Types.Id.Id jobData.deliveryId
  result <- try @_ @SomeException (dispatchFn jobData)
  case result of
    Right _ -> QDelivery.updateStatusById DDelivery.DS_SENT deliveryId
    Left err -> do
      logError $
        "Communication dispatch failed for delivery "
          <> jobData.deliveryId
          <> ": "
          <> show err
      QDelivery.updateStatusById DDelivery.DS_FAILED deliveryId
