{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.RiderPlatform.Management.Merchant
  ( postMerchantUpdate,
    getMerchantServiceUsageConfig,
    postMerchantServiceConfigMapsUpdate,
    postMerchantServiceUsageConfigMapsUpdate,
    postMerchantServiceConfigSmsUpdate,
    postMerchantServiceUsageConfigSmsUpdate,
    postMerchantConfigOperatingCityCreate,
    postMerchantSpecialLocationUpsert,
    deleteMerchantSpecialLocationDelete,
    postMerchantSpecialLocationGatesUpsert,
    deleteMerchantSpecialLocationGatesDelete,
    postMerchantConfigFailover,
    postMerchantTicketConfigUpsert,
    postMerchantConfigSpecialLocationUpsert,
    postMerchantSchedulerTrigger,
    postMerchantConfigOperatingCityWhiteList,
    postMerchantConfigMerchantCreate,
  )
where

import qualified API.Client.RiderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management.Merchant as Common
import Dashboard.Common.Merchant
import qualified Data.Text as T
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Geometry (getGeomFromKML)
import Kernel.Utils.Validation (runRequestValidation)
import qualified Lib.Types.SpecialLocation as SL
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
  T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just APP_BACKEND_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

postMerchantUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MerchantUpdateReq ->
  Flow APISuccess
postMerchantUpdate merchantShortId opCity apiTokenInfo req = do
  runRequestValidation Common.validateMerchantUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantUpdate) req

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
  whenJust req.getEstimatedPickupDistances $ \_ ->
    throwError (InvalidRequest "getEstimatedPickupDistances is not allowed for bap")
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

postMerchantConfigOperatingCityCreate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.CreateMerchantOperatingCityReq -> Flow Common.CreateMerchantOperatingCityRes
postMerchantConfigOperatingCityCreate merchantShortId opCity apiTokenInfo req = do
  processMerchantCreateRequest merchantShortId opCity apiTokenInfo False req

postMerchantSpecialLocationUpsert :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe (Id SL.SpecialLocation) -> Common.UpsertSpecialLocationReq -> Flow APISuccess
postMerchantSpecialLocationUpsert merchantShortId opCity apiTokenInfo specialLocationId req@Common.UpsertSpecialLocationReq {..} = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  geom <- maybe (return Nothing) mkGeom (req.file)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantSpecialLocationUpsert) specialLocationId Common.UpsertSpecialLocationReqT {geom = geom, ..}

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

postMerchantConfigFailover :: (Kernel.Types.Id.ShortId DM.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Common.ConfigNames -> Common.ConfigFailoverReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFailover merchantShortId opCity apiTokenInfo configName req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigFailover) configName req

postMerchantTicketConfigUpsert :: (Kernel.Types.Id.ShortId DM.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Common.UpsertTicketConfigReq -> Environment.Flow Common.UpsertTicketConfigResp)
postMerchantTicketConfigUpsert merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.merchantDSL.postMerchantTicketConfigUpsert)) req

postMerchantConfigSpecialLocationUpsert :: (Kernel.Types.Id.ShortId DM.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.UpsertSpecialLocationCsvReq -> Environment.Flow Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities)
postMerchantConfigSpecialLocationUpsert merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.merchantDSL.postMerchantConfigSpecialLocationUpsert)) req

postMerchantSchedulerTrigger :: (Kernel.Types.Id.ShortId DM.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Common.SchedulerTriggerReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantSchedulerTrigger merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantSchedulerTrigger) req

postMerchantConfigOperatingCityWhiteList :: (Kernel.Types.Id.ShortId DM.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.WhiteListOperatingCityReq -> Environment.Flow Dashboard.Common.Merchant.WhiteListOperatingCityRes)
postMerchantConfigOperatingCityWhiteList merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.merchantDSL.postMerchantConfigOperatingCityWhiteList) req

postMerchantConfigMerchantCreate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.CreateMerchantOperatingCityReq -> Flow Common.CreateMerchantOperatingCityRes
postMerchantConfigMerchantCreate merchantShortId opCity apiTokenInfo req = do
  processMerchantCreateRequest merchantShortId opCity apiTokenInfo True req

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
  geom <- getGeomFromKML req.file >>= fromMaybeM (InvalidRequest "Cannot convert KML to Geom")
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
    buildMerchant now merchantD baseMerchant =
      DM.Merchant
        { id = Id merchantD.subscriberId,
          shortId = ShortId merchantD.shortId,
          defaultOperatingCity = req.city,
          supportedOperatingCities = [req.city],
          serverNames = baseMerchant.serverNames,
          is2faMandatory = baseMerchant.is2faMandatory,
          domain = baseMerchant.domain,
          website = baseMerchant.website,
          authToken = baseMerchant.authToken,
          createdAt = now,
          enabled = Just enableForMerchant,
          requireAdminApprovalForFleetOnboarding = baseMerchant.requireAdminApprovalForFleetOnboarding,
          verifyFleetWhileLogin = baseMerchant.verifyFleetWhileLogin,
          hasFleetMemberHierarchy = baseMerchant.hasFleetMemberHierarchy,
          isStrongNameCheckRequired = baseMerchant.isStrongNameCheckRequired,
          singleActiveSessionOnly = baseMerchant.singleActiveSessionOnly
        }
