{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Sos where

import qualified API.Types.UI.Sos
import qualified Control.Lens
import qualified Domain.Action.UI.Sos as Domain.Action.UI.Sos
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "sos" :> "create" :> ReqBody '[JSON] API.Types.UI.Sos.SosReq :> Post '[JSON] API.Types.UI.Sos.SosRes)

handler :: Environment.FlowServer API
handler = postSosCreate

postSosCreate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.Sos.SosReq ->
    Environment.FlowHandler API.Types.UI.Sos.SosRes
  )
postSosCreate a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.postSosCreate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
