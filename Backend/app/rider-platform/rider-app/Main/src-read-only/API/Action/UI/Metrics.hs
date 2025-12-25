{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Metrics
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Metrics
import qualified Control.Lens
import qualified Domain.Action.UI.Metrics
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "metrics" :> "increment" :> ReqBody ('[JSON]) API.Types.UI.Metrics.MetricCounterReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

handler :: Environment.FlowServer API
handler = postMetricsIncrement

postMetricsIncrement ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.Metrics.MetricCounterReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postMetricsIncrement a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Metrics.postMetricsIncrement (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
