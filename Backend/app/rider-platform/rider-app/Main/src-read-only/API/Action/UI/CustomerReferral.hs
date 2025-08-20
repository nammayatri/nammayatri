{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.CustomerReferral
  ( API,
    handler,
  )
where

import qualified API.Types.UI.CustomerReferral
import qualified Control.Lens
import qualified Domain.Action.UI.CustomerReferral
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

type API =
  ( TokenAuth :> "CustomerRefferal" :> "count" :> Get '[JSON] API.Types.UI.CustomerReferral.ReferredCustomers :<|> TokenAuth :> "person" :> "applyReferral"
      :> ReqBody
           '[JSON]
           API.Types.UI.CustomerReferral.ApplyCodeReq
      :> Post
           '[JSON]
           API.Types.UI.CustomerReferral.ReferrerInfo
      :<|> TokenAuth
      :> "referral"
      :> "verifyVpa"
      :> MandatoryQueryParam
           "vpa"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.UI.CustomerReferral.VpaResp
      :<|> TokenAuth
      :> "referralPayout"
      :> "history"
      :> Get
           '[JSON]
           API.Types.UI.CustomerReferral.PayoutHistory
      :<|> TokenAuth
      :> "payoutVpa"
      :> "upsert"
      :> ReqBody
           '[JSON]
           API.Types.UI.CustomerReferral.UpdatePayoutVpaReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getCustomerRefferalCount :<|> postPersonApplyReferral :<|> getReferralVerifyVpa :<|> getReferralPayoutHistory :<|> postPayoutVpaUpsert

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

getReferralVerifyVpa ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.CustomerReferral.VpaResp
  )
getReferralVerifyVpa a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CustomerReferral.getReferralVerifyVpa (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getReferralPayoutHistory :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler API.Types.UI.CustomerReferral.PayoutHistory)
getReferralPayoutHistory a1 = withFlowHandlerAPI $ Domain.Action.UI.CustomerReferral.getReferralPayoutHistory (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postPayoutVpaUpsert ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.CustomerReferral.UpdatePayoutVpaReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postPayoutVpaUpsert a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CustomerReferral.postPayoutVpaUpsert (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
