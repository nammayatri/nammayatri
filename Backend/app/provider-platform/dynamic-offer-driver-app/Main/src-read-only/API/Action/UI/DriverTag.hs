{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverTag
  ( API,
    handler,
  )
where

import qualified API.Types.UI.DriverTag
import qualified Control.Lens
import qualified Domain.Action.UI.DriverTag
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "driver" :> "tag" :> "update" :> ReqBody '[JSON] API.Types.UI.DriverTag.DriverTagUpdateReq :> Post '[JSON] API.Types.UI.DriverTag.DriverTagRes)

handler :: Environment.FlowServer API
handler = postDriverTagUpdate

postDriverTagUpdate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverTag.DriverTagUpdateReq ->
    Environment.FlowHandler API.Types.UI.DriverTag.DriverTagRes
  )
postDriverTagUpdate a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverTag.postDriverTagUpdate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
