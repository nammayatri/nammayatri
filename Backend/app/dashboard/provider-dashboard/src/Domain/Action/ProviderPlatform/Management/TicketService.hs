{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.ProviderPlatform.Management.TicketService (postTicketServiceCreateKaptureTicket) where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.TicketService
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Ticket.Interface.Types
import qualified Kernel.External.Ticket.Kapture.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postTicketServiceCreateKaptureTicket :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.External.Ticket.Interface.Types.CreateTicketReq -> Environment.Flow Kernel.External.Ticket.Kapture.Types.CreateTicketResp)
postTicketServiceCreateKaptureTicket merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ProviderManagementAPI (API.Types.ProviderPlatform.Management.TicketServiceAPI API.Types.ProviderPlatform.Management.TicketService.PostTicketServiceCreateKaptureTicketEndpoint)) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.ticketServiceDSL.postTicketServiceCreateKaptureTicket) req)
