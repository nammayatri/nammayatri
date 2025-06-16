{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Insurance
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Insurance
import qualified Control.Lens
import qualified Domain.Action.UI.Insurance as Domain.Action.UI.Insurance
import qualified Domain.Types.Insurance
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

type API = (TokenAuth :> "insurance" :> Capture "referenceId" (Kernel.Types.Id.Id Domain.Types.Insurance.Insurance) :> Get '[JSON] API.Types.UI.Insurance.InsuranceAPIEntity)

handler :: Environment.FlowServer API
handler = getInsurance

getInsurance ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Insurance.Insurance ->
    Environment.FlowHandler API.Types.UI.Insurance.InsuranceAPIEntity
  )
getInsurance a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Insurance.getInsurance (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
