module Domain.Action.Dashboard.AppManagement.FrfsFleetOperator
  ( postFrfsFleetOperatorCurrentOperation,
    postFrfsFleetOperatorTripAction,
  )
where

import qualified API.Types.UI.FRFSTicketService
import qualified Domain.Action.UI.FRFSTicketService as UIFRFSTicketService
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

postFrfsFleetOperatorCurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationReq -> Environment.Flow API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationResp)
postFrfsFleetOperatorCurrentOperation merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  UIFRFSTicketService.postFrfsFleetOperatorCurrentOperation' (merchant.id, merchantOpCityId) req

postFrfsFleetOperatorTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.UI.FRFSTicketService.FleetOperatorTripActionReq -> Environment.Flow API.Types.UI.FRFSTicketService.FleetOperatorTripActionResp)
postFrfsFleetOperatorTripAction merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just opCity)
  UIFRFSTicketService.postFrfsFleetOperatorTripAction' (merchant.id, merchantOpCityId) req
