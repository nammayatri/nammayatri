module API.Internal.SettlementReportIngestion
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.SettlementReportIngestion as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "settlementReportIngestion"
      :> "trigger"
      :> ReqBody '[JSON] Domain.TriggerSettlementReportIngestionReq
      :> Post '[JSON] Domain.TriggerSettlementReportIngestionRes
  )

handler :: FlowServer API
handler = triggerSettlementReportIngestion

triggerSettlementReportIngestion :: Domain.TriggerSettlementReportIngestionReq -> FlowHandler Domain.TriggerSettlementReportIngestionRes
triggerSettlementReportIngestion = withFlowHandlerAPI . Domain.triggerSettlementReportIngestion
