{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Merchant
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as Common
import qualified Data.Text as T
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error (GenericError (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Geometry (getGeomFromKML)
import Kernel.Utils.Validation (runRequestValidation)
import Lib.Types.SpecialLocation as SL
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Merchant as SQM
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "merchant"
    :> ( MerchantUpdateAPI
           :<|> MerchantCommonConfigAPI
           :<|> MerchantCommonConfigUpdateAPI
           :<|> DriverPoolConfigAPI
           :<|> DriverPoolConfigUpdateAPI
           :<|> DriverPoolConfigCreateAPI
           :<|> DriverIntelligentPoolConfigAPI
           :<|> DriverIntelligentPoolConfigUpdateAPI
           :<|> DocumentVerificationConfigAPI
           :<|> DocumentVerificationConfigUpdateAPI
           :<|> DocumentVerificationConfigCreateAPI
           :<|> ServiceUsageConfigAPI
           :<|> MapsServiceConfigUpdateAPI
           :<|> MapsServiceUsageConfigUpdateAPI
           :<|> SmsServiceConfigUpdateAPI
           :<|> SmsServiceUsageConfigUpdateAPI
           :<|> VerificationServiceConfigUpdateAPI
           :<|> CreateFPDriverExtraFee
           :<|> UpdateFPDriverExtraFee
           :<|> UpdateFPPerExtraKmRate
           :<|> UpdateFarePolicy
           :<|> CreateMerchantOperatingCityAPI
           :<|> SchedulerTriggerAPI
           :<|> UpdateOnboardingVehicleVariantMapping
           :<|> UpsertSpecialLocationAPI
           :<|> DeleteSpecialLocationAPI
           :<|> UpsertSpecialLocationGateAPI
           :<|> DeleteSpecialLocationGatesAPI
       )

type MerchantUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'MERCHANT_UPDATE
    :> Common.MerchantUpdateAPI

type MerchantCommonConfigAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'MERCHANT_COMMON_CONFIG
    :> Common.MerchantCommonConfigAPI

type MerchantCommonConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'MERCHANT_COMMON_CONFIG_UPDATE
    :> Common.MerchantCommonConfigUpdateAPI

type DriverPoolConfigAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'DRIVER_POOL_CONFIG
    :> Common.DriverPoolConfigAPI

type DriverPoolConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'DRIVER_POOL_CONFIG_UPDATE
    :> Common.DriverPoolConfigUpdateAPI

type DriverPoolConfigCreateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'DRIVER_POOL_CONFIG_CREATE
    :> Common.DriverPoolConfigCreateAPI

type DriverIntelligentPoolConfigAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'DRIVER_INTELLIGENT_POOL_CONFIG
    :> Common.DriverIntelligentPoolConfigAPI

type DriverIntelligentPoolConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'DRIVER_INTELLIGENT_POOL_CONFIG_UPDATE
    :> Common.DriverIntelligentPoolConfigUpdateAPI

type DocumentVerificationConfigAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'ONBOARDING_DOCUMENT_CONFIG
    :> Common.DocumentVerificationConfigAPI

type DocumentVerificationConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'ONBOARDING_DOCUMENT_CONFIG_UPDATE
    :> Common.DocumentVerificationConfigUpdateAPI

type DocumentVerificationConfigCreateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'ONBOARDING_DOCUMENT_CONFIG_CREATE
    :> Common.DocumentVerificationConfigCreateAPI

type ServiceUsageConfigAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'SERVICE_USAGE_CONFIG
    :> Common.ServiceUsageConfigAPI

type MapsServiceConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'MAPS_SERVICE_CONFIG_UPDATE
    :> Common.MapsServiceConfigUpdateAPI

type MapsServiceUsageConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'MAPS_SERVICE_USAGE_CONFIG_UPDATE
    :> Common.MapsServiceUsageConfigUpdateAPI

type SmsServiceConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'SMS_SERVICE_CONFIG_UPDATE
    :> Common.SmsServiceConfigUpdateAPI

type SmsServiceUsageConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'SMS_SERVICE_USAGE_CONFIG_UPDATE
    :> Common.SmsServiceUsageConfigUpdateAPI

type VerificationServiceConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'VERIFICATION_SERVICE_CONFIG_UPDATE
    :> Common.VerificationServiceConfigUpdateAPI

type CreateFPDriverExtraFee =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'CREATE_FP_DRIVER_EXTRA_FEE
    :> Common.CreateFPDriverExtraFee

type UpdateFPDriverExtraFee =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'UPDATE_FP_DRIVER_EXTRA_FEE
    :> Common.UpdateFPDriverExtraFee

type UpdateFPPerExtraKmRate =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'UPDATE_FP_PER_EXTRA_KM_RATE
    :> Common.UpdateFPPerExtraKmRate

type UpdateFarePolicy =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'UPDATE_FARE_POLICY
    :> Common.UpdateFarePolicy

type CreateMerchantOperatingCityAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'CREATE_MERCHANT_OPERATING_CITY
    :> Common.CreateMerchantOperatingCityAPI

type SchedulerTriggerAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'SCHEDULER_TRIGGER
    :> Common.SchedulerTriggerAPI

type UpdateOnboardingVehicleVariantMapping =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'UPDATE_ONBOARDING_VEHICLE_VARIANT_MAPPING
    :> Common.UpdateOnboardingVehicleVariantMappingAPI

type UpsertSpecialLocationAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'UPSERT_SPECIAL_LOCATION
    :> Common.UpsertSpecialLocationAPI

type DeleteSpecialLocationAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'DELETE_SPECIAL_LOCATION
    :> Common.DeleteSpecialLocationAPI

type UpsertSpecialLocationGateAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'UPSERT_SPECIAL_LOCATION_GATE
    :> Common.UpsertSpecialLocationGateAPI

type DeleteSpecialLocationGatesAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'DELETE_SPECIAL_LOCATION_GATE
    :> Common.DeleteSpecialLocationGateAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  merchantUpdate merchantId city
    :<|> merchantCommonConfig merchantId city
    :<|> merchantCommonConfigUpdate merchantId city
    :<|> driverPoolConfig merchantId city
    :<|> driverPoolConfigUpdate merchantId city
    :<|> driverPoolConfigCreate merchantId city
    :<|> driverIntelligentPoolConfig merchantId city
    :<|> driverIntelligentPoolConfigUpdate merchantId city
    :<|> documentVerificationConfig merchantId city
    :<|> documentVerificationConfigUpdate merchantId city
    :<|> documentVerificationConfigCreate merchantId city
    :<|> serviceUsageConfig merchantId city
    :<|> mapsServiceConfigUpdate merchantId city
    :<|> mapsServiceUsageConfigUpdate merchantId city
    :<|> smsServiceConfigUpdate merchantId city
    :<|> smsServiceUsageConfigUpdate merchantId city
    :<|> verificationServiceConfigUpdate merchantId city
    :<|> createFPDriverExtraFee merchantId city
    :<|> updateFPDriverExtraFee merchantId city
    :<|> updateFPPerExtraKmRate merchantId city
    :<|> updateFarePolicy merchantId city
    :<|> createMerchantOperatingCity merchantId city
    :<|> schedulerTrigger merchantId city
    :<|> updateOnboardingVehicleVariantMapping merchantId city
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
  T.buildTransaction (DT.MerchantAPI endpoint) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

merchantUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MerchantUpdateReq ->
  FlowHandler Common.MerchantUpdateRes
merchantUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateMerchantUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MerchantUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.merchantUpdate) req

merchantCommonConfig ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  FlowHandler Common.MerchantCommonConfigRes
merchantCommonConfig merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.merchantCommonConfig)

merchantCommonConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MerchantCommonConfigUpdateReq ->
  FlowHandler APISuccess
merchantCommonConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateMerchantCommonConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MerchantCommonConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.merchantCommonConfigUpdate) req

schedulerTrigger ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.SchedulerTriggerReq ->
  FlowHandler APISuccess
schedulerTrigger merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.schedulerTrigger) req

driverPoolConfig ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Meters ->
  FlowHandler Common.DriverPoolConfigRes
driverPoolConfig merchantShortId opCity apiTokenInfo tripDistance = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.driverPoolConfig) tripDistance

driverPoolConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Meters ->
  SL.Area ->
  Maybe Common.Variant ->
  Maybe Text ->
  Common.DriverPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverPoolConfigUpdate merchantShortId opCity apiTokenInfo tripDistance area variant tripCategory req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateDriverPoolConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.DriverPoolConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.driverPoolConfigUpdate) tripDistance area variant tripCategory req

driverPoolConfigCreate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Meters ->
  SL.Area ->
  Maybe Common.Variant ->
  Maybe Text ->
  Common.DriverPoolConfigCreateReq ->
  FlowHandler APISuccess
driverPoolConfigCreate merchantShortId opCity apiTokenInfo tripDistance area variant tripCategory req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateDriverPoolConfigCreateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.DriverPoolConfigCreateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.driverPoolConfigCreate) tripDistance area variant tripCategory req

driverIntelligentPoolConfig ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  FlowHandler Common.DriverIntelligentPoolConfigRes
driverIntelligentPoolConfig merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.driverIntelligentPoolConfig)

driverIntelligentPoolConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.DriverIntelligentPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverIntelligentPoolConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateDriverIntelligentPoolConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.DriverIntelligentPoolConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.driverIntelligentPoolConfigUpdate) req

documentVerificationConfig ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Common.DocumentType ->
  Maybe Common.Category ->
  FlowHandler Common.DocumentVerificationConfigRes
documentVerificationConfig merchantShortId opCity apiTokenInfo documentType category = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.documentVerificationConfig) documentType category

documentVerificationConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.DocumentType ->
  Common.Category ->
  Common.DocumentVerificationConfigUpdateReq ->
  FlowHandler APISuccess
documentVerificationConfigUpdate merchantShortId opCity apiTokenInfo documentType category req = withFlowHandlerAPI' $ do
  -- runRequestValidation Common.validateDocumentVerificationConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.DocumentVerificationConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.documentVerificationConfigUpdate) documentType category req

documentVerificationConfigCreate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.DocumentType ->
  Common.Category ->
  Common.DocumentVerificationConfigCreateReq ->
  FlowHandler APISuccess
documentVerificationConfigCreate merchantShortId opCity apiTokenInfo documentType category req = withFlowHandlerAPI' $ do
  -- runRequestValidation Common.validateDocumentVerificationConfigCreateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.DocumentVerificationConfigCreateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.documentVerificationConfigCreate) documentType category req

serviceUsageConfig ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  FlowHandler Common.ServiceUsageConfigRes
serviceUsageConfig merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.serviceUsageConfig)

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
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.mapsServiceConfigUpdate) req

mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MapsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceUsageConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MapsServiceConfigUsageUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.mapsServiceUsageConfigUpdate) req

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
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.smsServiceConfigUpdate) req

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
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.smsServiceUsageConfigUpdate) req

verificationServiceConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.VerificationServiceConfigUpdateReq ->
  FlowHandler APISuccess
verificationServiceConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.VerificationServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.verificationServiceConfigUpdate) req

createFPDriverExtraFee :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.FarePolicy -> Meters -> Common.CreateFPDriverExtraFeeReq -> FlowHandler APISuccess
createFPDriverExtraFee merchantShortId opCity apiTokenInfo farePolicyId startDistance req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.CreateFPDriverExtraFeeEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.createFPDriverExtraFee) farePolicyId startDistance req

updateFPDriverExtraFee :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.FarePolicy -> Meters -> Common.CreateFPDriverExtraFeeReq -> FlowHandler APISuccess
updateFPDriverExtraFee merchantShortId opCity apiTokenInfo farePolicyId startDistance req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.UpdateFPDriverExtraFeeEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.updateFPDriverExtraFee) farePolicyId startDistance req

updateFPPerExtraKmRate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.FarePolicy -> Meters -> Common.UpdateFPPerExtraKmRateReq -> FlowHandler APISuccess
updateFPPerExtraKmRate merchantShortId opCity apiTokenInfo farePolicyId startDistance req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.UpdateFPPerExtraKmRate apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.updateFPPerExtraKmRate) farePolicyId startDistance req

updateFarePolicy :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.FarePolicy -> Common.UpdateFarePolicyReq -> FlowHandler APISuccess
updateFarePolicy merchantShortId opCity apiTokenInfo farePolicyId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.UpdateFarePolicy apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.updateFarePolicy) farePolicyId req

createMerchantOperatingCity :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.CreateMerchantOperatingCityReq -> FlowHandler Common.CreateMerchantOperatingCityRes
createMerchantOperatingCity merchantShortId opCity apiTokenInfo req@Common.CreateMerchantOperatingCityReq {..} = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.CreateMerchantOperatingCityEndpoint apiTokenInfo (Just req)
  -- update entry in dashboard
  merchant <- SQM.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest $ "Merchant not found with shortId " <> show merchantShortId)
  geom <- getGeomFromKML req.file >>= fromMaybeM (InvalidRequest "Cannot convert KML to Geom.")
  unless (req.city `elem` merchant.supportedOperatingCities) $
    SQM.updateSupportedOperatingCities merchantShortId (merchant.supportedOperatingCities <> [req.city])
  T.withTransactionStoring transaction $ Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.createMerchantOperatingCity) Common.CreateMerchantOperatingCityReqT {geom = T.pack geom, ..}

updateOnboardingVehicleVariantMapping :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.UpdateOnboardingVehicleVariantMappingReq -> FlowHandler APISuccess
updateOnboardingVehicleVariantMapping merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.UpdateOnboardingVehicleVariantMappingEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (addMultipartBoundary . (.merchant.updateOnboardingVehicleVariantMapping)) req
  where
    addMultipartBoundary clientFn reqBody = clientFn ("XXX00XXX", reqBody)

upsertSpecialLocation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe (Id SL.SpecialLocation) -> Common.UpsertSpecialLocationReq -> FlowHandler APISuccess
upsertSpecialLocation merchantShortId opCity apiTokenInfo mbSpecialLocationId req@Common.UpsertSpecialLocationReq {..} = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.UpsertSpecialLocationEndpoint apiTokenInfo (Just req)
  geom <- maybe (return Nothing) mkGeom (req.file)
  T.withTransactionStoring transaction $ Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.upsertSpecialLocation) mbSpecialLocationId Common.UpsertSpecialLocationReqT {geom = geom, ..}

deleteSpecialLocation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id SL.SpecialLocation -> FlowHandler APISuccess
deleteSpecialLocation merchantShortId opCity apiTokenInfo specialLocationId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.DeleteSpecialLocationEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $ Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.deleteSpecialLocation) specialLocationId

upsertSpecialLocationGate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id SL.SpecialLocation -> Common.UpsertSpecialLocationGateReq -> FlowHandler APISuccess
upsertSpecialLocationGate merchantShortId opCity apiTokenInfo specialLocationId req@Common.UpsertSpecialLocationGateReq {..} = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.UpsertSpecialLocationGateEndpoint apiTokenInfo (Just req)
  geom <- maybe (return Nothing) mkGeom (req.file)
  T.withTransactionStoring transaction $ Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.upsertSpecialLocationGate) specialLocationId Common.UpsertSpecialLocationGateReqT {geom = geom, ..}

deleteSpecialLocationGate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id SL.SpecialLocation -> Text -> FlowHandler APISuccess
deleteSpecialLocationGate merchantShortId opCity apiTokenInfo specialLocationId gateName = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.DeleteSpecialLocationGateEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $ Client.callDriverOfferBPPOperations checkedMerchantId opCity (.merchant.deleteSpecialLocationGate) specialLocationId gateName

mkGeom :: FilePath -> Flow (Maybe Text)
mkGeom kmlFile = do
  result <- getGeomFromKML kmlFile >>= fromMaybeM (InvalidRequest "Cannot convert KML to Geom.")
  return $ Just $ T.pack result
