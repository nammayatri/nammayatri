{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.TicketKapture
  ( API,
    handler,
  )
where

import qualified API.Types.UI.TicketKapture
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.TicketKapture
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
      :> MandatoryQueryParam "ticketId" Data.Text.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "getAllActiveTickets"
      :> Get
           '[JSON]
           API.Types.UI.TicketKapture.GetAllActiveTicketsRes
      :<|> TokenAuth
      :> "getClosedTicketIds"
      :> Get
           '[JSON]
           API.Types.UI.TicketKapture.GetClosedTicketIdsRes
      :<|> TokenAuth
      :> "getClosedTicketDetails"
      :> MandatoryQueryParam
           "ticketId"
           Data.Text.Text
      :> Get
           '[JSON]
           API.Types.UI.TicketKapture.GetClosedTicketDetailsRes
  )

handler :: Environment.FlowServer API
handler = postKaptureCustomerLogin :<|> postKaptureCloseTicket :<|> getGetAllActiveTickets :<|> getGetClosedTicketIds :<|> getGetClosedTicketDetails

postKaptureCustomerLogin ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.External.Ticket.Interface.Types.TicketType ->
    Environment.FlowHandler API.Types.UI.TicketKapture.TicketKaptureResp
  )
postKaptureCustomerLogin a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketKapture.postKaptureCustomerLogin (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postKaptureCloseTicket ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postKaptureCloseTicket a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketKapture.postKaptureCloseTicket (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getGetAllActiveTickets ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler API.Types.UI.TicketKapture.GetAllActiveTicketsRes
  )
getGetAllActiveTickets a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketKapture.getGetAllActiveTickets (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getGetClosedTicketIds ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler API.Types.UI.TicketKapture.GetClosedTicketIdsRes
  )
getGetClosedTicketIds a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketKapture.getGetClosedTicketIds (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getGetClosedTicketDetails ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler API.Types.UI.TicketKapture.GetClosedTicketDetailsRes
  )
getGetClosedTicketDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketKapture.getGetClosedTicketDetails (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
