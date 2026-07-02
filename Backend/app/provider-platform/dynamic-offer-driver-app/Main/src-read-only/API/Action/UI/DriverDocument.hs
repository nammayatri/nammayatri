{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverDocument
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.DriverRegistration
import qualified Control.Lens
import qualified Domain.Action.UI.DriverDocument
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
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

type API =
  ( TokenAuth :> "driver" :> "document" :> "register" :> ReqBody ('[JSON]) API.Types.ProviderPlatform.Management.DriverRegistration.DocumentRegisterReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postDriverDocumentRegister

postDriverDocumentRegister ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.ProviderPlatform.Management.DriverRegistration.DocumentRegisterReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverDocumentRegister a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverDocument.postDriverDocumentRegister (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
