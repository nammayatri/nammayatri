module Beckn.Utils.Registry where

import Beckn.Prelude
import Beckn.Types.Cache
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Types.Registry
import qualified Beckn.Types.Registry.API as API
import qualified Beckn.Types.Registry.Routes as Registry
import Beckn.Utils.Common
import qualified EulerHS.Types as T

registryLookup ::
  ( MonadReader r m,
    MonadFlow m,
    CoreMetrics m,
    HasInConfig r c "registryUrl" BaseUrl
  ) =>
  SimpleLookupRequest ->
  m (Maybe Subscriber)
registryLookup request = do
  registryUrl <- askConfig (.registryUrl)
  callAPI registryUrl (T.client Registry.lookupAPI (toLookupReq request)) "lookup"
    >>= fromEitherM (ExternalAPICallError (Just "REGISTRY_CALL_ERROR") registryUrl)
    >>= \case
      [subscriber] ->
        pure $ Just subscriber
      _subscriber : _subscribers ->
        throwError $ InternalError "Multiple subscribers returned for a unique key."
      [] -> pure Nothing
  where
    toLookupReq SimpleLookupRequest {..} =
      API.emptyLookupRequest
        { API.unique_key_id = Just unique_key_id,
          API.subscriber_id = Just subscriber_id
        }

whitelisting ::
  (MonadThrow m, Log m) =>
  (Text -> m Bool) ->
  Maybe Subscriber ->
  m (Maybe Subscriber)
whitelisting p = maybe (pure Nothing) \sub -> do
  unlessM (p sub.unique_key_id) . throwError . InvalidRequest $
    "Not whitelisted subscriber " <> sub.unique_key_id
  pure (Just sub)

withSubscriberCache ::
  ( MonadTime m,
    CacheEx Subscriber m
  ) =>
  (CacheKey Subscriber -> m (Maybe Subscriber)) ->
  CacheKey Subscriber ->
  m (Maybe Subscriber)
withSubscriberCache getData key = do
  now <- getCurrentTime
  caching (getTtl now) getData key
  where
    getTtl now Subscriber {..} =
      nominalDiffTimeToSeconds . fromMaybe (5 * 60) $ valid_until <&> (`diffUTCTime` now)
