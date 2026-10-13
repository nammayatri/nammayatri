{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.TicketVerify
  ( API,
    handler,
  )
where

import qualified API.Types.UI.TicketVerify
import qualified Control.Lens
import qualified Domain.Action.UI.TicketVerify
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MultimodalTicketVerify
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( TokenAuth :> "multimodal" :> "ticket" :> "verify" :> MandatoryQueryParam "city" Kernel.Types.Beckn.Context.City
      :> ReqBody
           '[JSON]
           API.Types.UI.TicketVerify.MultimodalTicketVerifyReq
      :> Post '[JSON] Domain.Types.MultimodalTicketVerify.MultimodalTicketVerifyResp
  )

handler :: Environment.FlowServer API
handler = postMultimodalTicketVerify

postMultimodalTicketVerify ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Beckn.Context.City ->
    API.Types.UI.TicketVerify.MultimodalTicketVerifyReq ->
    Environment.FlowHandler Domain.Types.MultimodalTicketVerify.MultimodalTicketVerifyResp
  )
postMultimodalTicketVerify a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.TicketVerify.postMultimodalTicketVerify (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
