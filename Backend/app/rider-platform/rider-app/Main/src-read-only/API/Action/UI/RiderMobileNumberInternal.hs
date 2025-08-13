{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.RiderMobileNumberInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.RiderMobileNumberInternal
import qualified Domain.Action.UI.RiderMobileNumberInternal as Domain.Action.UI.RiderMobileNumberInternal
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = ("riderMobileNumber" :> Capture "bppRideId" Kernel.Prelude.Text :> Header "token" Kernel.Prelude.Text :> Get ('[JSON]) API.Types.UI.RiderMobileNumberInternal.RiderMobileAPIEntity)

handler :: Environment.FlowServer API
handler = getRiderMobileNumber

getRiderMobileNumber :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.UI.RiderMobileNumberInternal.RiderMobileAPIEntity)
getRiderMobileNumber a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.RiderMobileNumberInternal.getRiderMobileNumber a2 a1
