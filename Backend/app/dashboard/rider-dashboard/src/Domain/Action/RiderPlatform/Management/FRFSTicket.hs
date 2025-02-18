{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.RiderPlatform.Management.FRFSTicket
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

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.FRFSTicket
import qualified BecknV2.FRFS.Enums
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getFRFSTicketFrfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.RiderPlatform.Management.FRFSTicket.FRFSDashboardRouteAPI])
getFRFSTicketFrfsRoutes merchantShortId opCity apiTokenInfo searchStr limit offset vehicleType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.fRFSTicketDSL.getFRFSTicketFrfsRoutes) searchStr limit offset vehicleType

postFRFSTicketFrfsRouteAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsRouteAdd merchantShortId opCity apiTokenInfo code vehicleType req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.fRFSTicketDSL.postFRFSTicketFrfsRouteAdd) code vehicleType req)

postFRFSTicketFrfsRouteDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsRouteDelete merchantShortId opCity apiTokenInfo code vehicleType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.fRFSTicketDSL.postFRFSTicketFrfsRouteDelete) code vehicleType)

getFRFSTicketFrfsRouteFareList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow API.Types.RiderPlatform.Management.FRFSTicket.FRFSRouteFareAPI)
getFRFSTicketFrfsRouteFareList merchantShortId opCity apiTokenInfo routeCode vehicleType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.fRFSTicketDSL.getFRFSTicketFrfsRouteFareList) routeCode vehicleType

putFRFSTicketFrfsRouteFareUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareReq -> Environment.Flow API.Types.RiderPlatform.Management.FRFSTicket.UpsertRouteFareResp)
putFRFSTicketFrfsRouteFareUpsert merchantShortId opCity apiTokenInfo routeCode vehicleType req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (addMultipartBoundary "XXX00XXX" . (.fRFSTicketDSL.putFRFSTicketFrfsRouteFareUpsert)) routeCode vehicleType req)
  where
    addMultipartBoundary :: LBS.ByteString -> (Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> (LBS.ByteString, req) -> res) -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> req -> res
    addMultipartBoundary boundary clientFn routeCode_ vehicleType_ reqBody = clientFn routeCode_ vehicleType_ (boundary, reqBody)

getFRFSTicketFrfsRouteStations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationAPI])
getFRFSTicketFrfsRouteStations merchantShortId opCity apiTokenInfo searchStr limit offset vehicleType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.fRFSTicketDSL.getFRFSTicketFrfsRouteStations) searchStr limit offset vehicleType

postFRFSTicketFrfsStationAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.RiderPlatform.Management.FRFSTicket.FRFSStationReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsStationAdd merchantShortId opCity apiTokenInfo code vehicleType req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.fRFSTicketDSL.postFRFSTicketFrfsStationAdd) code vehicleType req)

postFRFSTicketFrfsStationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFRFSTicketFrfsStationDelete merchantShortId opCity apiTokenInfo code vehicleType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.fRFSTicketDSL.postFRFSTicketFrfsStationDelete) code vehicleType)
