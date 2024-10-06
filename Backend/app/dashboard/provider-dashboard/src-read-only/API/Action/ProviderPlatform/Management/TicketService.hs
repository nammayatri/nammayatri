{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.TicketService
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.TicketService
import qualified Domain.Action.ProviderPlatform.Management.TicketService
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.External.Ticket.Interface.Types
import qualified Kernel.External.Ticket.Kapture.Types
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("ticketService" :> PostTicketServiceCreateKaptureTicket)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postTicketServiceCreateKaptureTicket merchantId city

type PostTicketServiceCreateKaptureTicket =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DRIVERS)
      ('TICKET_SERVICE)
      :> API.Types.ProviderPlatform.Management.TicketService.PostTicketServiceCreateKaptureTicket
  )

postTicketServiceCreateKaptureTicket :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.External.Ticket.Interface.Types.CreateTicketReq -> Environment.FlowHandler Kernel.External.Ticket.Kapture.Types.CreateTicketResp)
postTicketServiceCreateKaptureTicket merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TicketService.postTicketServiceCreateKaptureTicket merchantShortId opCity apiTokenInfo req
