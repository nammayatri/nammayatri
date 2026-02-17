{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.TicketKapture
  ( API,
    handler,
  )
where

import qualified API.Types.UI.TicketKapture
import qualified Data.Text
import qualified Domain.Action.UI.TicketKapture
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Ticket.Interface.Types
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth
      :> "kaptureCustomerLogin"
      :> MandatoryQueryParam "ticketType" Kernel.External.Ticket.Interface.Types.TicketType
      :> Post '[JSON] API.Types.UI.TicketKapture.TicketKaptureResp
      :<|> TokenAuth
      :> "kaptureCloseTicket"
      :> MandatoryQueryParam "ticketId" Data.Text.Text
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "getAllActiveTickets"
      :> Get '[JSON] API.Types.UI.TicketKapture.GetAllActiveTicketsRes
      :<|> TokenAuth
      :> "getClosedTicketIds"
      :> Get '[JSON] API.Types.UI.TicketKapture.GetClosedTicketIdsRes
      :<|> TokenAuth
      :> "getClosedTicketDetails"
      :> MandatoryQueryParam "ticketId" Data.Text.Text
      :> Get '[JSON] API.Types.UI.TicketKapture.GetClosedTicketDetailsRes
  )

handler :: Environment.FlowServer API
handler =
  postKaptureCustomerLogin
    :<|> postKaptureCloseTicket
    :<|> getGetAllActiveTickets
    :<|> getGetClosedTicketIds
    :<|> getGetClosedTicketDetails

postKaptureCustomerLogin ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Kernel.External.Ticket.Interface.Types.TicketType ->
  Environment.FlowHandler API.Types.UI.TicketKapture.TicketKaptureResp
postKaptureCustomerLogin auth ticketType = withFlowHandlerAPI $ Domain.Action.UI.TicketKapture.postKaptureCustomerLogin auth ticketType

postKaptureCloseTicket ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Data.Text.Text ->
  Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
postKaptureCloseTicket auth ticketId = withFlowHandlerAPI $ Domain.Action.UI.TicketKapture.postKaptureCloseTicket auth ticketId

getGetAllActiveTickets ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Environment.FlowHandler API.Types.UI.TicketKapture.GetAllActiveTicketsRes
getGetAllActiveTickets auth = withFlowHandlerAPI $ Domain.Action.UI.TicketKapture.getGetAllActiveTickets auth

getGetClosedTicketIds ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Environment.FlowHandler API.Types.UI.TicketKapture.GetClosedTicketIdsRes
getGetClosedTicketIds auth = withFlowHandlerAPI $ Domain.Action.UI.TicketKapture.getGetClosedTicketIds auth

getGetClosedTicketDetails ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Data.Text.Text ->
  Environment.FlowHandler API.Types.UI.TicketKapture.GetClosedTicketDetailsRes
getGetClosedTicketDetails auth ticketId = withFlowHandlerAPI $ Domain.Action.UI.TicketKapture.getGetClosedTicketDetails auth ticketId
