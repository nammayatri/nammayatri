module Beckn.Context where

import qualified Beckn.Spec.Common.Context as Context
import qualified Beckn.Spec.Common.Domain as Domain
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Predicate
import Kernel.Types.TimeRFC339
import Kernel.Utils.Validation

coreConfig :: CoreConfig
coreConfig =
  CoreConfig
    { version = "0.9.3",
      domain = Domain.PUBLIC_TRANSPORT,
      country = "IND",
      city = "Kochi"
    }

buildContext ::
  (MonadTime m, MonadGuid m) =>
  Context.Action ->
  Text ->
  Text ->
  BaseUrl ->
  Maybe Text ->
  Maybe BaseUrl ->
  m Context.Context
buildContext = buildContext' coreConfig

validateContext :: (MonadThrow m, Log m) => Context.Action -> Context.Context -> m ()
validateContext = validateContext' coreConfig

-- TODO We can move this common code to the lib

data CoreConfig = CoreConfig
  { version :: Text,
    domain :: Domain.Domain,
    country :: Text,
    city :: Text
  }

buildContext' ::
  (MonadTime m, MonadGuid m) =>
  CoreConfig ->
  Context.Action ->
  Text ->
  Text ->
  BaseUrl ->
  Maybe Text ->
  Maybe BaseUrl ->
  m Context.Context
buildContext' config action txnId bapId bapUri bppId bppUri = do
  timestamp <- UTCTimeRFC3339 <$> getCurrentTime
  message_id <- generateGUIDText
  return
    Context.Context
      { domain = config.domain,
        country = config.country,
        city = config.city,
        action = action,
        core_version = config.version,
        bap_id = bapId,
        bap_uri = bapUri,
        bpp_id = bppId,
        bpp_uri = bppUri,
        transaction_id = Just txnId,
        message_id = message_id,
        timestamp = timestamp
      }

validateContext' :: (MonadThrow m, Log m) => CoreConfig -> Context.Action -> Context.Context -> m ()
validateContext' config action' = runRequestValidation validator
  where
    validator Context.Context {..} =
      sequenceA_
        [ validateField "domain" domain $ Exact config.domain,
          validateField "action" action $ Exact action',
          validateField "core_version" core_version $ Exact config.version,
          validateField "country" country $ Exact config.country
        ]
