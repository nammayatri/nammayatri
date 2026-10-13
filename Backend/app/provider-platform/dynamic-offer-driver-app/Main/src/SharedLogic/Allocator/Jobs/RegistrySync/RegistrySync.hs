module SharedLogic.Allocator.Jobs.RegistrySync.RegistrySync
  ( runRegistrySyncJob,
  )
where

import qualified Data.Aeson as Aeson
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Beckn.Country (Country (..))
import Kernel.Types.Beckn.Domain (Domain (..))
import qualified Kernel.Types.Registry.API as API
import Kernel.Utils.Common
import Kernel.Utils.Registry (registryFetchPublic)
import Lib.Scheduler
import qualified Registry.Beckn.Nammayatri.Flow as NyRegistry
import SharedLogic.Allocator (AllocatorJobType (..))

registrySyncInterval :: NominalDiffTime
registrySyncInterval = 24 * 60 * 60

runRegistrySyncJob ::
  ( MonadFlow m,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m,
    HasShortDurationRetryCfg r c,
    HasField "ondcRegistryUrl" r BaseUrl,
    HasField "nyRegistryUrl" r BaseUrl
  ) =>
  Job 'RegistrySync ->
  m ExecutionResult
runRegistrySyncJob Job {id} = withLogTag ("JobId-" <> id.getId) do
  -- A failure anywhere below (a domain fetch exhausting its retries) must still
  -- leave the job rescheduled, not permanently dead.
  runResult <- try @_ @SomeException $ do
    ondcUrl <- asks (.ondcRegistryUrl)
    nyUrl <- asks (.nyRegistryUrl)
    perDomain <- forM [MOBILITY, PUBLIC_TRANSPORT] \domain -> do
      let lookupReq = API.emptyLookupRequest {API.domain = Just domain, API.country = Just India}
      -- ONDC's /lookup is a public, unauthenticated read (verified directly against
      -- prod.registry.ondc.org); no signing identity is needed or used to fetch it.
      subs <- withShortRetry $ registryFetchPublic ondcUrl lookupReq
      results <- forM subs \sub -> case Aeson.fromJSON (Aeson.toJSON sub) of
        Aeson.Error err -> do
          logError $ "Skipping subscriber " <> sub.subscriber_id <> " (" <> show sub._type <> "): invalid payload: " <> show err
          pure False
        Aeson.Success nySub ->
          -- The registry's own /create writes only the fields that differ, so
          -- calling it every cycle is safe.
          try @_ @SomeException (NyRegistry.createSubscriber nyUrl nySub) >>= \case
            Left err -> do
              logError $ "Failed to create/update subscriber " <> sub.subscriber_id <> " (" <> show sub._type <> "): " <> show err
              pure False
            Right _ -> pure True
      pure (length subs, results)
    let totalFetched = sum (map fst perDomain)
        allResults = concatMap snd perDomain
        failedCount = length (filter not allResults)
        summary =
          "Registry sync completed: " <> show (length allResults - failedCount) <> " succeeded, "
            <> show failedCount
            <> " failed, out of "
            <> show totalFetched
            <> " subscribers fetched"
    if failedCount > 0 then logError summary else logInfo summary
  case runResult of
    Left err -> logError $ "Registry sync run aborted before completion: " <> show err
    Right () -> pure ()
  now <- getCurrentTime
  pure $ ReSchedule (addUTCTime registrySyncInterval now)
