{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.MinimumGuarantee
  ( API,
    handler,
  )
where

import qualified API.Types.UI.MinimumGuarantee
import qualified Control.Lens
import qualified Domain.Action.UI.MinimumGuarantee
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
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( TokenAuth :> "driver" :> "mg" :> "invitations" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> Get
           '[JSON]
           API.Types.UI.MinimumGuarantee.MgInvitationsRes
      :<|> TokenAuth
      :> "driver"
      :> "mg"
      :> "optIn"
      :> ReqBody
           '[JSON]
           API.Types.UI.MinimumGuarantee.MgOptInReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "mg"
      :> "optOut"
      :> ReqBody
           '[JSON]
           API.Types.UI.MinimumGuarantee.MgOptOutReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "mg"
      :> "progress"
      :> Get
           '[JSON]
           API.Types.UI.MinimumGuarantee.MgProgressRes
  )

handler :: Environment.FlowServer API
handler = getDriverMgInvitations :<|> postDriverMgOptIn :<|> postDriverMgOptOut :<|> getDriverMgProgress

getDriverMgInvitations ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.FlowHandler API.Types.UI.MinimumGuarantee.MgInvitationsRes
  )
getDriverMgInvitations a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.MinimumGuarantee.getDriverMgInvitations (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postDriverMgOptIn ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.MinimumGuarantee.MgOptInReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverMgOptIn a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.MinimumGuarantee.postDriverMgOptIn (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDriverMgOptOut ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.MinimumGuarantee.MgOptOutReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverMgOptOut a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.MinimumGuarantee.postDriverMgOptOut (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getDriverMgProgress ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.MinimumGuarantee.MgProgressRes
  )
getDriverMgProgress a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.MinimumGuarantee.getDriverMgProgress (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
