{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.FRFSTicket
  ( getFRFSTicketFrfsRoutes,
    postFRFSTicketFrfsRouteAdd,
    postFRFSTicketFrfsRouteDelete,
    getFRFSTicketFrfsRouteFareList,
    putFRFSTicketFrfsRouteFareUpsert,
    getFRFSTicketFrfsRouteStations,
    postFRFSTicketFrfsStationAdd,
    postFRFSTicketFrfsStationDelete,
  )
where

import qualified API.Types.RiderPlatform.Management.FRFSTicket
import qualified BecknV2.FRFS.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getFRFSTicketFrfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteAPI])
getFRFSTicketFrfsRoutes _merchantShortId _opCity limit offset vehicleType = do error "Logic yet to be decided" limit offset vehicleType

postFRFSTicketFrfsRouteAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsRouteAdd _merchantShortId _opCity code vehicleType req = do error "Logic yet to be decided" code vehicleType req

postFRFSTicketFrfsRouteDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsRouteDelete _merchantShortId _opCity code vehicleType = do error "Logic yet to be decided" code vehicleType

getFRFSTicketFrfsRouteFareList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteFareAPI])
getFRFSTicketFrfsRouteFareList _merchantShortId _opCity routeCode vehicleType = do error "Logic yet to be decided" routeCode vehicleType

putFRFSTicketFrfsRouteFareUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putFRFSTicketFrfsRouteFareUpsert _merchantShortId _opCity routeCode vehicleType req = do error "Logic yet to be decided" routeCode vehicleType req

getFRFSTicketFrfsRouteStations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationAPI])
getFRFSTicketFrfsRouteStations _merchantShortId _opCity searchStr limit offset vehicleType = do error "Logic yet to be decided" searchStr limit offset vehicleType

postFRFSTicketFrfsStationAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsStationAdd _merchantShortId _opCity code vehicleType req = do error "Logic yet to be decided" code vehicleType req

postFRFSTicketFrfsStationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsStationDelete _merchantShortId _opCity code vehicleType = do error "Logic yet to be decided" code vehicleType
