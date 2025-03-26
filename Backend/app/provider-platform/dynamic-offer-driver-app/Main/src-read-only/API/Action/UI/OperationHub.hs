{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.OperationHub
  ( API,
    handler,
  )
where

import qualified API.Types.UI.OperationHub
import qualified Control.Lens
import qualified Domain.Action.UI.OperationHub as Domain.Action.UI.OperationHub
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.OperationHub
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
  ( TokenAuth :> "operation" :> "getAllHubs" :> Get ('[JSON]) [Domain.Types.OperationHub.OperationHub] :<|> TokenAuth :> "operation" :> "createRequest"
      :> ReqBody
           ('[JSON])
           API.Types.UI.OperationHub.DriverOperationHubRequest
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getOperationGetAllHubs :<|> postOperationCreateRequest

getOperationGetAllHubs ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler [Domain.Types.OperationHub.OperationHub]
  )
getOperationGetAllHubs a1 = withFlowHandlerAPI $ Domain.Action.UI.OperationHub.getOperationGetAllHubs (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postOperationCreateRequest ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.OperationHub.DriverOperationHubRequest ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postOperationCreateRequest a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.OperationHub.postOperationCreateRequest (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
