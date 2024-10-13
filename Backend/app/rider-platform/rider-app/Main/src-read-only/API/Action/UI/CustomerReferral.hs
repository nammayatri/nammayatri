{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.CustomerReferral
  ( API,
    handler,
  )
where

import qualified API.Types.UI.CustomerReferral
import qualified Control.Lens
import qualified Domain.Action.UI.CustomerReferral as Domain.Action.UI.CustomerReferral
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

type API =
  ( TokenAuth :> "CustomerRefferal" :> "count" :> Get '[JSON] API.Types.UI.CustomerReferral.ReferredCustomers :<|> TokenAuth :> "person" :> "applyReferral"
      :> ReqBody
           '[JSON]
           API.Types.UI.CustomerReferral.ApplyCodeReq
      :> Post '[JSON] API.Types.UI.CustomerReferral.ReferrerInfo
  )

handler :: Environment.FlowServer API
handler = getCustomerRefferalCount :<|> postPersonApplyReferral

getCustomerRefferalCount ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler API.Types.UI.CustomerReferral.ReferredCustomers
  )
getCustomerRefferalCount a1 = withFlowHandlerAPI $ Domain.Action.UI.CustomerReferral.getCustomerRefferalCount (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postPersonApplyReferral ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.CustomerReferral.ApplyCodeReq ->
    Environment.FlowHandler API.Types.UI.CustomerReferral.ReferrerInfo
  )
postPersonApplyReferral a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CustomerReferral.postPersonApplyReferral (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
