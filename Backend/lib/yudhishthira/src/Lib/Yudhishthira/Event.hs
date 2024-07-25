module Lib.Yudhishthira.Event where

import EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Yudhishthira.Types
import Servant hiding (Application, throwError)

-- Should we add auth?
type YudhishthiraDecideAPI =
  "v1"
    :> "yudhishthira"
    :> "decide"
    :> ReqBody '[JSON] YudhishthiraDecideReq
    :> Post '[JSON] YudhishthiraDecideResp

yudhishthiraDecide ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasFlowEnv m r '["yudhishthiraUrl" ::: BaseUrl]
  ) =>
  YudhishthiraDecideReq ->
  m YudhishthiraDecideResp
yudhishthiraDecide req = do
  let proxy = Proxy @YudhishthiraDecideAPI
      eulerClient = Euler.client proxy req
  url <- asks (.yudhishthiraUrl)
  callAPI url eulerClient "yudhishthira-decide" proxy
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_YUDHISHTHIRA_DECIDE") url)

data Handle m = Handle
  { updateTags :: (Text -> m ()),
    getData :: m ApplicationData
  }

addEvent ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    HasFlowEnv m r '["yudhishthiraUrl" ::: BaseUrl]
  ) =>
  ApplicationEvent ->
  Handle m ->
  m ()
addEvent event Handle {..} = do
  sourceData <- getData
  let req = YudhishthiraDecideReq {source = Application event, sourceData = ApplicationD sourceData}
  resp <- yudhishthiraDecide req
  resp.tags `forM_` \tag -> do
    let tagText = tag.tagName <> "#" <> tag.tagValue
    updateTags tagText
