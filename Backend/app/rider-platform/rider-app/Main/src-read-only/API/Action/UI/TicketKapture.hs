{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.TicketKapture
  ( API,
    handler,
  )
where

import qualified API.Types.UI.TicketKapture
import qualified Control.Lens
import qualified Domain.Action.UI.TicketKapture as Domain.Action.UI.TicketKapture
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Ticket.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "kaptureCustomerLogin" :> MandatoryQueryParam "ticketType" Kernel.External.Ticket.Interface.Types.TicketType
      :> Post
           '[JSON]
           API.Types.UI.TicketKapture.TicketKaptureResp
      :<|> TokenAuth
      :> "kaptureCloseTicket"
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postKaptureCustomerLogin :<|> postKaptureCloseTicket

postKaptureCustomerLogin ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.External.Ticket.Interface.Types.TicketType ->
    Environment.FlowHandler API.Types.UI.TicketKapture.TicketKaptureResp
  )
postKaptureCustomerLogin a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketKapture.postKaptureCustomerLogin (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postKaptureCloseTicket :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postKaptureCloseTicket a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketKapture.postKaptureCloseTicket (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
