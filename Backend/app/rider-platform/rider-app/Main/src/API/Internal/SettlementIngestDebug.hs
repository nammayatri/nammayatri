-- | __Dev only__: exercises SFTP fetch + 'ingestPaymentSettlementReport'.
module API.Internal.SettlementIngestDebug (API, handler) where

import qualified Domain.Action.Internal.SettlementIngestDebug as Domain
import Environment
import Kernel.Prelude
import Kernel.Utils.Common (withFlowHandlerAPI)
import Lib.Finance.Settlement.Ingestion (IngestionResult)
import Servant

type API =
  "settlementIngestDebug"
    :> ReqBody '[JSON] Domain.DebugSettlementIngestReq
    :> Post '[JSON] IngestionResult

handler :: FlowServer API
handler = postHandler

postHandler :: Domain.DebugSettlementIngestReq -> FlowHandler IngestionResult
postHandler req = withFlowHandlerAPI $ Domain.debugSettlementIngest req
