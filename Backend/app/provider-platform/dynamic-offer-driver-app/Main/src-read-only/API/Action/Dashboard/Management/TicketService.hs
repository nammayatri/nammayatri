{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.TicketService
  ( API.Types.ProviderPlatform.Management.TicketService.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.TicketService
import qualified Domain.Action.Dashboard.Management.TicketService as Domain.Action.Dashboard.Management.TicketService
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Ticket.Interface.Types
import qualified Kernel.External.Ticket.Kapture.Types
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.TicketService.API)
handler merchantId city = postTicketServiceCreateKaptureTicket merchantId city

postTicketServiceCreateKaptureTicket :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.External.Ticket.Interface.Types.CreateTicketReq -> Environment.FlowHandler Kernel.External.Ticket.Kapture.Types.CreateTicketResp)
postTicketServiceCreateKaptureTicket a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.TicketService.postTicketServiceCreateKaptureTicket a3 a2 a1
