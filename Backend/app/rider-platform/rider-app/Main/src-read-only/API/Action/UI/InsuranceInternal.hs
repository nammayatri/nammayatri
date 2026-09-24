{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.InsuranceInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Insurance
import qualified Domain.Action.UI.InsuranceInternal
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = ("insurance" :> Capture "bppRideId" Kernel.Prelude.Text :> Header "token" Kernel.Prelude.Text :> Get '[JSON] API.Types.UI.Insurance.InsuranceAPIEntity)

handler :: Environment.FlowServer API
handler = getInsurance

getInsurance :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.Insurance.InsuranceAPIEntity)
getInsurance a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.InsuranceInternal.getInsurance a2 a1
