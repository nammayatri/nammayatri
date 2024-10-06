{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.Management.TicketService (postTicketServiceCreateKaptureTicket) where

import qualified API.Types.ProviderPlatform.Management.TicketService
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id, state)
import Kernel.External.Ticket.Interface.Types (CreateTicketReq)
import qualified Kernel.External.Ticket.Interface.Types
import Kernel.External.Ticket.Kapture.Types (CreateTicketResp)
import qualified Kernel.External.Ticket.Kapture.Types
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Auth
import Tools.Error
import qualified Tools.Ticket as Ticket

postTicketServiceCreateKaptureTicket :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> CreateTicketReq -> Environment.Flow CreateTicketResp)
postTicketServiceCreateKaptureTicket merchantShortId opCity createTicketReq = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  Ticket.createTicket merchant.id merchantOpCity.id createTicketReq
