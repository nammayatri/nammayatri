{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.ProviderPlatform.Management.Merchant
  ( postMerchantUpdate,
    getMerchantConfigCommon,
    postMerchantConfigCommonUpdate,
    getMerchantConfigDriverPool,
    postMerchantConfigDriverPoolUpdate,
    postMerchantConfigDriverPoolCreate,
    getMerchantConfigDriverIntelligentPool,
    postMerchantConfigDriverIntelligentPoolUpdate,
    getMerchantConfigOnboardingDocument,
    postMerchantConfigOnboardingDocumentUpdate,
    postMerchantConfigOnboardingDocumentCreate,
    getMerchantServiceUsageConfig,
    postMerchantServiceConfigMapsUpdate,
    postMerchantServiceUsageConfigMapsUpdate,
    postMerchantServiceConfigSmsUpdate,
    postMerchantServiceUsageConfigSmsUpdate,
    postMerchantServiceConfigVerificationUpdate,
    postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate,
    postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate,
    postMerchantConfigFarePolicyPerExtraKmRateUpdate,
    postMerchantConfigFarePolicyUpdate,
    postMerchantConfigFarePolicyUpsert,
    postMerchantConfigOperatingCityCreate,
    postMerchantSchedulerTrigger,
    postMerchantUpdateOnboardingVehicleVariantMapping,
    postMerchantSpecialLocationUpsert,
    deleteMerchantSpecialLocationDelete,
    postMerchantSpecialLocationGatesUpsert,
    deleteMerchantSpecialLocationGatesDelete,
    postMerchantConfigClearCacheSubscription,
    postMerchantConfigFailover,
    postMerchantPayoutConfigUpdate,
    postMerchantConfigSpecialLocationUpsert,
    postMerchantConfigUpsertPlanAndConfigSubscription,
    postMerchantConfigOperatingCityWhiteList,
    postMerchantConfigMerchantCreate,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant as Common
import qualified Dashboard.Common
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
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Merchant as SQM
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction apiTokenInfo =
  T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

postMerchantUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MerchantUpdateReq ->
  Flow Common.MerchantUpdateRes
postMerchantUpdate merchantShortId opCity apiTokenInfo req = do
  runRequestValidation Common.validateMerchantUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantUpdate) req

getMerchantConfigCommon ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Flow Common.MerchantCommonConfigRes
getMerchantConfigCommon merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.getMerchantConfigCommon)

postMerchantConfigCommonUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MerchantCommonConfigUpdateReq ->
  Flow APISuccess
postMerchantConfigCommonUpdate merchantShortId opCity apiTokenInfo req = do
  runRequestValidation Common.validateMerchantCommonConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigCommonUpdate) req

getMerchantConfigDriverPool ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Meters ->
  Maybe HighPrecDistance ->
  Maybe DistanceUnit ->
  Flow Common.DriverPoolConfigRes
getMerchantConfigDriverPool merchantShortId opCity apiTokenInfo tripDistance tripDistanceValue distanceUnit = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.getMerchantConfigDriverPool) tripDistance tripDistanceValue distanceUnit

postMerchantConfigDriverPoolUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe HighPrecDistance ->
  Maybe DistanceUnit ->
  Maybe Common.VehicleVariant ->
  Maybe Text ->
  Meters ->
  SL.Area ->
  Common.DriverPoolConfigUpdateReq ->
  Flow APISuccess
postMerchantConfigDriverPoolUpdate merchantShortId opCity apiTokenInfo tripDistanceValue distanceUnit variant tripCategory tripDistance area req = do
  runRequestValidation Common.validateDriverPoolConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigDriverPoolUpdate) tripDistanceValue distanceUnit variant tripCategory tripDistance area req

postMerchantConfigDriverPoolCreate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe HighPrecDistance ->
  Maybe DistanceUnit ->
  Maybe Common.VehicleVariant ->
  Maybe Text ->
  Meters ->
  SL.Area ->
  Common.DriverPoolConfigCreateReq ->
  Flow APISuccess
postMerchantConfigDriverPoolCreate merchantShortId opCity apiTokenInfo tripDistanceValue distanceUnit variant tripCategory tripDistance area req = do
  runRequestValidation Common.validateDriverPoolConfigCreateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigDriverPoolCreate) tripDistanceValue distanceUnit variant tripCategory tripDistance area req

getMerchantConfigDriverIntelligentPool ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Flow Common.DriverIntelligentPoolConfigRes
getMerchantConfigDriverIntelligentPool merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.getMerchantConfigDriverIntelligentPool)

postMerchantConfigDriverIntelligentPoolUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.DriverIntelligentPoolConfigUpdateReq ->
  Flow APISuccess
postMerchantConfigDriverIntelligentPoolUpdate merchantShortId opCity apiTokenInfo req = do
  runRequestValidation Common.validateDriverIntelligentPoolConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigDriverIntelligentPoolUpdate) req

getMerchantConfigOnboardingDocument ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Common.DocumentType ->
  Maybe Common.VehicleCategory ->
  Flow Common.DocumentVerificationConfigRes
getMerchantConfigOnboardingDocument merchantShortId opCity apiTokenInfo documentType category = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.getMerchantConfigOnboardingDocument) documentType category

postMerchantConfigOnboardingDocumentUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.DocumentType ->
  Common.VehicleCategory ->
  Common.DocumentVerificationConfigUpdateReq ->
  Flow APISuccess
postMerchantConfigOnboardingDocumentUpdate merchantShortId opCity apiTokenInfo documentType category req = do
  -- runRequestValidation Common.validateDocumentVerificationConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigOnboardingDocumentUpdate) documentType category req

-- FIXME does validation required?
postMerchantConfigOnboardingDocumentCreate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.DocumentType ->
  Common.VehicleCategory ->
  Common.DocumentVerificationConfigCreateReq ->
  Flow APISuccess
postMerchantConfigOnboardingDocumentCreate merchantShortId opCity apiTokenInfo documentType category req = do
  -- runRequestValidation Common.validateDocumentVerificationConfigCreateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigOnboardingDocumentCreate) documentType category req

getMerchantServiceUsageConfig ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Flow Common.ServiceUsageConfigRes
getMerchantServiceUsageConfig merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.getMerchantServiceUsageConfig)

postMerchantServiceConfigMapsUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MapsServiceConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceConfigMapsUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantServiceConfigMapsUpdate) req

postMerchantServiceUsageConfigMapsUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MapsServiceUsageConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceUsageConfigMapsUpdate merchantShortId opCity apiTokenInfo req = do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantServiceUsageConfigMapsUpdate) req

postMerchantServiceConfigSmsUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.SmsServiceConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceConfigSmsUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantServiceConfigSmsUpdate) req

postMerchantServiceUsageConfigSmsUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.SmsServiceUsageConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceUsageConfigSmsUpdate merchantShortId opCity apiTokenInfo req = do
  runRequestValidation Common.validateSmsServiceUsageConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantServiceUsageConfigSmsUpdate) req

postMerchantServiceConfigVerificationUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.VerificationServiceConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceConfigVerificationUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantServiceConfigVerificationUpdate) req

postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.FarePolicy -> Maybe HighPrecDistance -> Maybe DistanceUnit -> Meters -> Common.CreateFPDriverExtraFeeReq -> Flow APISuccess
postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate merchantShortId opCity apiTokenInfo farePolicyId startDistanceValue distanceUnit startDistance req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate) farePolicyId startDistanceValue distanceUnit startDistance req

postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.FarePolicy -> Maybe HighPrecDistance -> Maybe DistanceUnit -> Meters -> Common.CreateFPDriverExtraFeeReq -> Flow APISuccess
postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate merchantShortId opCity apiTokenInfo farePolicyId startDistanceValue distanceUnit startDistance req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate) farePolicyId startDistanceValue distanceUnit startDistance req

postMerchantConfigFarePolicyPerExtraKmRateUpdate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.FarePolicy -> Meters -> Common.UpdateFPPerExtraKmRateReq -> Flow APISuccess
postMerchantConfigFarePolicyPerExtraKmRateUpdate merchantShortId opCity apiTokenInfo farePolicyId startDistance req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigFarePolicyPerExtraKmRateUpdate) farePolicyId startDistance req

postMerchantConfigFarePolicyUpdate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.FarePolicy -> Common.UpdateFarePolicyReq -> Flow APISuccess
postMerchantConfigFarePolicyUpdate merchantShortId opCity apiTokenInfo farePolicyId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigFarePolicyUpdate) farePolicyId req

postMerchantConfigFarePolicyUpsert :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.UpsertFarePolicyReq -> Flow Common.UpsertFarePolicyResp
postMerchantConfigFarePolicyUpsert merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.merchantDSL.postMerchantConfigFarePolicyUpsert)) req

postMerchantConfigOperatingCityCreate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.CreateMerchantOperatingCityReq -> Flow Common.CreateMerchantOperatingCityRes
postMerchantConfigOperatingCityCreate merchantShortId opCity apiTokenInfo req = do
  processMerchantCreateRequest merchantShortId opCity apiTokenInfo False req

postMerchantSchedulerTrigger ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.SchedulerTriggerReq ->
  Flow APISuccess
postMerchantSchedulerTrigger merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantSchedulerTrigger) req

postMerchantUpdateOnboardingVehicleVariantMapping :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.UpdateOnboardingVehicleVariantMappingReq -> Flow APISuccess
postMerchantUpdateOnboardingVehicleVariantMapping merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (addMultipartBoundary . (.merchantDSL.postMerchantUpdateOnboardingVehicleVariantMapping)) req
  where
    addMultipartBoundary clientFn reqBody = clientFn ("XXX00XXX", reqBody)

postMerchantSpecialLocationUpsert :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe (Id SL.SpecialLocation) -> Common.UpsertSpecialLocationReq -> Flow APISuccess
postMerchantSpecialLocationUpsert merchantShortId opCity apiTokenInfo mbSpecialLocationId req@Common.UpsertSpecialLocationReq {..} = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  geom <- maybe (return Nothing) mkGeom (req.file)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantSpecialLocationUpsert) mbSpecialLocationId Common.UpsertSpecialLocationReqT {geom = geom, ..}

deleteMerchantSpecialLocationDelete :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id SL.SpecialLocation -> Flow APISuccess
deleteMerchantSpecialLocationDelete merchantShortId opCity apiTokenInfo specialLocationId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.deleteMerchantSpecialLocationDelete) specialLocationId

postMerchantSpecialLocationGatesUpsert :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id SL.SpecialLocation -> Common.UpsertSpecialLocationGateReq -> Flow APISuccess
postMerchantSpecialLocationGatesUpsert merchantShortId opCity apiTokenInfo specialLocationId req@Common.UpsertSpecialLocationGateReq {..} = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  geom <- maybe (return Nothing) mkGeom (req.file)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantSpecialLocationGatesUpsert) specialLocationId Common.UpsertSpecialLocationGateReqT {geom = geom, ..}

deleteMerchantSpecialLocationGatesDelete :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id SL.SpecialLocation -> Text -> Flow APISuccess
deleteMerchantSpecialLocationGatesDelete merchantShortId opCity apiTokenInfo specialLocationId gateName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.deleteMerchantSpecialLocationGatesDelete) specialLocationId gateName

mkGeom :: FilePath -> Flow (Maybe Text)
mkGeom kmlFile = do
  result <- getGeomFromKML kmlFile >>= fromMaybeM (InvalidRequest "Cannot convert KML to Geom.")
  return $ Just $ T.pack result

postMerchantConfigClearCacheSubscription :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.ClearCacheSubscriptionReq -> Flow APISuccess
postMerchantConfigClearCacheSubscription merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigClearCacheSubscription) req

postMerchantConfigFailover :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.ConfigNames -> Common.ConfigFailoverReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postMerchantConfigFailover merchantShortId opCity apiTokenInfo configName req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigFailover) configName req

postMerchantPayoutConfigUpdate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.PayoutConfigReq -> Flow APISuccess
postMerchantPayoutConfigUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantPayoutConfigUpdate) req

postMerchantConfigSpecialLocationUpsert :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.UpsertSpecialLocationCsvReq -> Flow Common.APISuccessWithUnprocessedEntities
postMerchantConfigSpecialLocationUpsert merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.merchantDSL.postMerchantConfigSpecialLocationUpsert)) req

postMerchantConfigUpsertPlanAndConfigSubscription :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.UpsertPlanAndConfigReq -> Environment.Flow Common.UpsertPlanAndConfigResp
postMerchantConfigUpsertPlanAndConfigSubscription merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigUpsertPlanAndConfigSubscription) req

postMerchantConfigOperatingCityWhiteList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.WhiteListOperatingCityReq -> Environment.Flow Common.WhiteListOperatingCityRes
postMerchantConfigOperatingCityWhiteList merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigOperatingCityWhiteList) req

processMerchantCreateRequest ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Bool ->
  Common.CreateMerchantOperatingCityReq ->
  Flow Common.CreateMerchantOperatingCityRes
processMerchantCreateRequest merchantShortId opCity apiTokenInfo canCreateMerchant req@Common.CreateMerchantOperatingCityReq {..} = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  -- update entry in dashboard
  baseMerchant <- SQM.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest $ "Merchant not found with shortId " <> show merchantShortId)
  geom <- getGeomFromKML req.file >>= fromMaybeM (InvalidRequest "Cannot convert KML to Geom.")
  now <- getCurrentTime
  merchant <-
    case (merchantData, canCreateMerchant) of
      (Just merchantD, True) -> do
        SQM.findByShortId (ShortId merchantD.shortId) >>= \case
          Nothing -> do
            let newMerchant = buildMerchant now merchantD baseMerchant
            SQM.create newMerchant
            return newMerchant
          Just newMerchant -> return newMerchant
      (Just merchantD, False) -> throwError (InvalidRequest $ "Merchant Cannot be created using city/create: " <> merchantD.shortId)
      (Nothing, _) -> return baseMerchant
  unless (req.city `elem` merchant.supportedOperatingCities) $
    SQM.updateSupportedOperatingCities merchant.shortId (merchant.supportedOperatingCities <> [req.city])
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigOperatingCityCreate) Common.CreateMerchantOperatingCityReqT {geom = T.pack geom, ..}
  where
    buildMerchant now merchantD DM.Merchant {..} =
      DM.Merchant
        { id = Id merchantD.subscriberId,
          shortId = ShortId merchantD.shortId,
          defaultOperatingCity = req.city,
          supportedOperatingCities = [req.city],
          createdAt = now,
          enabled = Just enableForMerchant,
          ..
        }

postMerchantConfigMerchantCreate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.CreateMerchantOperatingCityReq -> Flow Common.CreateMerchantOperatingCityRes
postMerchantConfigMerchantCreate merchantShortId opCity apiTokenInfo req = processMerchantCreateRequest merchantShortId opCity apiTokenInfo True req
