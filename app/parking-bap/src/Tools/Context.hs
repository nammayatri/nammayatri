module Tools.Context where

import Beckn.Prelude
import qualified Beckn.Product.Validation.Context as Validation
import Beckn.Types.Common
import Beckn.Utils.Common
import Beckn.Utils.Servant.BaseUrl (showBaseUrlText)
import qualified Core.Common.Context as Context
import qualified Core.Common.Domain as Domain

buildContext ::
  (MonadTime m, MonadGuid m) =>
  Context.Action ->
  Text ->
  BaseUrl ->
  Maybe BaseUrl ->
  m Context.Context
buildContext action txnId bapUri bppUri = do
  timestamp <- getCurrentTime
  message_id <- generateGUIDText
  return
    Context.Context
      { domain = Domain.PARKING,
        country = "IND",
        city = "Kochi",
        action = action,
        core_version = "0.9.3-draft",
        bap_id = showBaseUrlText bapUri, -- maybe selfId?
        bap_uri = bapUri,
        bpp_id = show <$> bppUri,
        bpp_uri = bppUri,
        transaction_id = txnId,
        message_id = message_id,
        timestamp = timestamp,
        key = Nothing,
        ttl = Nothing
      }

validateContext :: (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) => Context.Action -> Context.Context -> m ()
validateContext action context = do
  Validation.validateDomainMig Domain.PARKING context
  Validation.validateContextCommonsMig action context
