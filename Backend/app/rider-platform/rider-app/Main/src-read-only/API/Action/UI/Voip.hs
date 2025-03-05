{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Voip
  ( API,
    handler,
  )
where

import qualified Control.Lens
import qualified Domain.Action.UI.Voip as Domain.Action.UI.Voip
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
import qualified Utils.Common.Voip.Types.VoipApiType

type API = (TokenAuth :> "call" :> "voip" :> ReqBody '[JSON] Utils.Common.Voip.Types.VoipApiType.VoipReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

handler :: Environment.FlowServer API
handler = postCallVoip

postCallVoip ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Utils.Common.Voip.Types.VoipApiType.VoipReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postCallVoip a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Voip.postCallVoip (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
