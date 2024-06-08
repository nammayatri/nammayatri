{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.ReferalPayout where

import qualified API.Types.UI.ReferalPayout
import qualified Control.Lens
import qualified Domain.Action.UI.ReferalPayout as Domain.Action.UI.ReferalPayout
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
  ( TokenAuth :> "referral" :> "earnings" :> MandatoryQueryParam "personId" Kernel.Types.Id.Id Domain.Types.Person.Person
      :> ReqBody
           ('[JSON])
           API.Types.UI.ReferalPayout.ReferralEarningsReq
      :> Get
           ('[JSON])
           API.Types.UI.ReferalPayout.ReferralEarningsRes
      :<|> TokenAuth
      :> "delete"
      :> "vpa"
      :> MandatoryQueryParam
           "personId"
           Kernel.Types.Id.Id
           Domain.Types.Person.Person
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getReferralEarnings :<|> postDeleteVpa

getReferralEarnings ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Person.Person ->
    API.Types.UI.ReferalPayout.ReferralEarningsReq ->
    Environment.FlowHandler API.Types.UI.ReferalPayout.ReferralEarningsRes
  )
getReferralEarnings a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ReferalPayout.getReferralEarnings (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postDeleteVpa ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Person.Person ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDeleteVpa a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ReferalPayout.postDeleteVpa (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
