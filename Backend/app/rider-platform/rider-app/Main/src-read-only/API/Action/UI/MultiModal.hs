{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.MultiModal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.MultiModal
import qualified Control.Lens
import qualified Domain.Action.UI.MultiModal as Domain.Action.UI.MultiModal
import qualified Domain.Types.Estimate
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "multiModal" :> Capture "estimateId" (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate) :> "routeDetails" :> Post '[JSON] API.Types.UI.MultiModal.MultiModalRouteDetails)

handler :: Environment.FlowServer API
handler = postMultiModalRouteDetails

postMultiModalRouteDetails ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Estimate.Estimate ->
    Environment.FlowHandler API.Types.UI.MultiModal.MultiModalRouteDetails
  )
postMultiModalRouteDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultiModal.postMultiModalRouteDetails (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
