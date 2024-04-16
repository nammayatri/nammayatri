{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.Merchant
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Merchant as Common
import qualified Data.Text as T
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, fromMaybeM, throwError, withFlowHandlerAPI')
import Kernel.Utils.Geometry (getGeomFromKML)
import Kernel.Utils.Validation (runRequestValidation)
import qualified Lib.Types.SpecialLocation as SL
import qualified RiderPlatformClient.RiderApp.Operations as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Merchant as SQM
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "merchant"
    :> ( MerchantUpdateAPI
           :<|> ServiceUsageConfigAPI
           :<|> MapsServiceConfigUpdateAPI
           :<|> MapsServiceUsageConfigUpdateAPI
           :<|> SmsServiceConfigUpdateAPI
           :<|> SmsServiceUsageConfigUpdateAPI
           :<|> CreateMerchantOperatingCityAPI
           :<|> UpsertSpecialLocationAPI
           :<|> DeleteSpecialLocationAPI
           :<|> UpsertSpecialLocationGateAPI
           :<|> DeleteSpecialLocationGateAPI
       )

type MerchantUpdateAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'MERCHANT_UPDATE
    :> Common.MerchantUpdateAPI

type ServiceUsageConfigAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'SERVICE_USAGE_CONFIG
    :> Common.ServiceUsageConfigAPI

type MapsServiceConfigUpdateAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'MAPS_SERVICE_CONFIG_UPDATE
    :> Common.MapsServiceConfigUpdateAPI

type MapsServiceUsageConfigUpdateAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'MAPS_SERVICE_USAGE_CONFIG_UPDATE
    :> Common.MapsServiceUsageConfigUpdateAPI

type SmsServiceConfigUpdateAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'SMS_SERVICE_CONFIG_UPDATE
    :> Common.SmsServiceConfigUpdateAPI

type SmsServiceUsageConfigUpdateAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'SMS_SERVICE_USAGE_CONFIG_UPDATE
    :> Common.SmsServiceUsageConfigUpdateAPI

type CreateMerchantOperatingCityAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'CREATE_MERCHANT_OPERATING_CITY
    :> Common.CreateMerchantOperatingCityAPI

type UpsertSpecialLocationAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'UPSERT_SPECIAL_LOCATION
    :> Common.UpsertSpecialLocationAPI

type DeleteSpecialLocationAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'DELETE_SPECIAL_LOCATION
    :> Common.DeleteSpecialLocationAPI

type UpsertSpecialLocationGateAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'UPSERT_SPECIAL_LOCATION_GATE
    :> Common.UpsertSpecialLocationGateAPI

type DeleteSpecialLocationGateAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'DELETE_SPECIAL_LOCATION_GATE
    :> Common.DeleteSpecialLocationGateAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  merchantUpdate merchantId city
    :<|> serviceUsageConfig merchantId city
    :<|> mapsServiceConfigUpdate merchantId city
    :<|> mapsServiceUsageConfigUpdate merchantId city
    :<|> smsServiceConfigUpdate merchantId city
    :<|> smsServiceUsageConfigUpdate merchantId city
    :<|> createMerchantOperatingCity merchantId city
    :<|> upsertSpecialLocation merchantId city
    :<|> deleteSpecialLocation merchantId city
    :<|> upsertSpecialLocationGate merchantId city
    :<|> deleteSpecialLocationGate merchantId city

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.MerchantEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.MerchantAPI endpoint) (Just APP_BACKEND_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

merchantUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MerchantUpdateReq ->
  FlowHandler APISuccess
merchantUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateMerchantUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MerchantUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.merchant.merchantUpdate) req

serviceUsageConfig ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  FlowHandler Common.ServiceUsageConfigRes
serviceUsageConfig merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.merchant.serviceUsageConfig)

mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MapsServiceConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MapsServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.merchant.mapsServiceConfigUpdate) req

mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MapsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceUsageConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  whenJust req.getEstimatedPickupDistances $ \_ ->
    throwError (InvalidRequest "getEstimatedPickupDistances is not allowed for bap")
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MapsServiceConfigUsageUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.merchant.mapsServiceUsageConfigUpdate) req

smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.SmsServiceConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.SmsServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.merchant.smsServiceConfigUpdate) req

smsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.SmsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceUsageConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateSmsServiceUsageConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.SmsServiceConfigUsageUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.merchant.smsServiceUsageConfigUpdate) req

createMerchantOperatingCity :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.CreateMerchantOperatingCityReq -> FlowHandler Common.CreateMerchantOperatingCityRes
createMerchantOperatingCity merchantShortId opCity apiTokenInfo req@Common.CreateMerchantOperatingCityReq {..} = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.CreateMerchantOperatingCityEndpoint apiTokenInfo (Just req)
  -- update entry in dashboard
  merchant <- SQM.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest $ "Merchant not found with shortId " <> show merchantShortId)
  geom <- getGeomFromKML req.file >>= fromMaybeM (InvalidRequest "Cannot convert KML to Geom")
  unless (req.city `elem` merchant.supportedOperatingCities) $
    SQM.updateSupportedOperatingCities merchantShortId (merchant.supportedOperatingCities <> [req.city])
  T.withTransactionStoring transaction $ Client.callRiderAppOperations checkedMerchantId opCity (.merchant.createMerchantOperatingCity) Common.CreateMerchantOperatingCityReqT {geom = T.pack geom, ..}

upsertSpecialLocation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe (Id SL.SpecialLocation) -> Common.UpsertSpecialLocationReq -> FlowHandler APISuccess
upsertSpecialLocation merchantShortId opCity apiTokenInfo specialLocationId req@Common.UpsertSpecialLocationReq {..} = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.UpsertSpecialLocationEndpoint apiTokenInfo (Just req)
  geom <- maybe (return Nothing) mkGeom (req.file)
  T.withTransactionStoring transaction $ Client.callRiderAppOperations checkedMerchantId opCity (.merchant.upsertSpecialLocation) specialLocationId Common.UpsertSpecialLocationReqT {geom = geom, ..}

deleteSpecialLocation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id SL.SpecialLocation -> FlowHandler APISuccess
deleteSpecialLocation merchantShortId opCity apiTokenInfo specialLocationId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.DeleteSpecialLocationEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $ Client.callRiderAppOperations checkedMerchantId opCity (.merchant.deleteSpecialLocation) specialLocationId

upsertSpecialLocationGate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id SL.SpecialLocation -> Common.UpsertSpecialLocationGateReq -> FlowHandler APISuccess
upsertSpecialLocationGate merchantShortId opCity apiTokenInfo specialLocationId req@Common.UpsertSpecialLocationGateReq {..} = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.UpsertSpecialLocationGateEndpoint apiTokenInfo (Just req)
  geom <- maybe (return Nothing) mkGeom (req.file)
  T.withTransactionStoring transaction $ Client.callRiderAppOperations checkedMerchantId opCity (.merchant.upsertSpecialLocationGate) specialLocationId Common.UpsertSpecialLocationGateReqT {geom = geom, ..}

deleteSpecialLocationGate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id SL.SpecialLocation -> Text -> FlowHandler APISuccess
deleteSpecialLocationGate merchantShortId opCity apiTokenInfo specialLocationId gateName = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.DeleteSpecialLocationGateEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $ Client.callRiderAppOperations checkedMerchantId opCity (.merchant.deleteSpecialLocationGate) specialLocationId gateName

mkGeom :: FilePath -> Flow (Maybe Text)
mkGeom kmlFile = do
  result <- getGeomFromKML kmlFile >>= fromMaybeM (InvalidRequest "Cannot convert KML to Geom.")
  return $ Just $ T.pack result
