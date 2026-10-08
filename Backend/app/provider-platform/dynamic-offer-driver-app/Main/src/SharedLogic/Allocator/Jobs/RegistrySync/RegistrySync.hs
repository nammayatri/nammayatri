module SharedLogic.Allocator.Jobs.RegistrySync.RegistrySync
  ( runRegistrySyncJob,
    registrySyncSeedKey,
  )
where

import qualified Data.Aeson as Aeson
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Beckn.Country (Country (..))
import Kernel.Types.Beckn.Domain (Domain (..))
import Kernel.Types.Error (MerchantError (MerchantDoesNotExist))
import Kernel.Types.Id (ShortId (..))
import qualified Kernel.Types.Registry.API as API
import Kernel.Utils.Common
import Kernel.Utils.Registry (registryFetch)
import Lib.Scheduler
import qualified Registry.Beckn.Nammayatri.Flow as NyRegistry
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified Storage.CachedQueries.Merchant as CQM

registrySyncSeedKey :: Text
registrySyncSeedKey = "registry-sync:seeded"

-- | Signs outgoing ONDC/NY registry calls. This job is not scoped to any one merchant,
-- so it borrows the already-registered Namma Yatri (driver-app) identity's signing key.
registrySyncMerchantShortId :: ShortId DM.Merchant
registrySyncMerchantShortId = ShortId "NAMMA_YATRI_PARTNER"

registrySyncInterval :: NominalDiffTime
registrySyncInterval = 6 * 60 * 60

runRegistrySyncJob ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    HasRequestId r,
    MonadReader r m,
    HasShortDurationRetryCfg r c,
    HasField "ondcRegistryUrl" r BaseUrl,
    HasField "nyRegistryUrl" r BaseUrl
  ) =>
  Job 'RegistrySync ->
  m ExecutionResult
runRegistrySyncJob Job {id} = withLogTag ("JobId-" <> id.getId) do
  ondcUrl <- asks (.ondcRegistryUrl)
  nyUrl <- asks (.nyRegistryUrl)
  merchant <- CQM.findByShortId registrySyncMerchantShortId >>= fromMaybeM (MerchantDoesNotExist registrySyncMerchantShortId.getShortId)
  let selfId = merchant.subscriberId.getShortId
  let lookupReq = API.emptyLookupRequest {API.domain = Just MOBILITY, API.country = Just India}
  subs <- withShortRetry $ registryFetch ondcUrl lookupReq selfId
  results <- forM subs \sub -> case Aeson.fromJSON (Aeson.toJSON sub) of
    Aeson.Error err -> do
      logError $ "Skipping subscriber " <> sub.subscriber_id <> " (" <> show sub._type <> "): invalid payload: " <> show err
      pure False
    Aeson.Success nySub ->
      try @_ @SomeException (NyRegistry.createSubscriber nyUrl nySub) >>= \case
        Left err -> do
          logError $ "Failed to create subscriber " <> sub.subscriber_id <> " (" <> show sub._type <> "): " <> show err
          pure False
        Right _ -> pure True
  let failedCount = length (filter not results)
      summary =
        "Registry sync completed: " <> show (length results - failedCount) <> " succeeded, "
          <> show failedCount
          <> " failed, out of "
          <> show (length subs)
          <> " subscribers fetched"
  if failedCount > 0 then logError summary else logInfo summary
  now <- getCurrentTime
  pure $ ReSchedule (addUTCTime registrySyncInterval now)
