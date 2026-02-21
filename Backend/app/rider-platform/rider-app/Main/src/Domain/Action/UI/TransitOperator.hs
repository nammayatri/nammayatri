{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Domain.Action.UI.TransitOperator where

import qualified BecknV2.OnDemand.Enums as BecknSpec
import qualified Kernel.Types.Beckn.Context as Context
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant (Merchant)
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id (ShortId (..))
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.Flow as NandiFlow
import SharedLogic.External.Nandi.Types
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error
import qualified Tools.MultiModal as MM

-- | Resolve the OTPRest base URL and gtfsId (feedKey) from merchant details.
--   Finds MerchantOperatingCity by shortId and city.
--   Then finds IntegratedBPPConfig by merchantOperatingCityId, vehicleCategory and MULTIMODAL.
--   Returns (BaseUrl, gtfsId)
resolveBaseUrlAndGtfsId :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow (BaseUrl, Text)
resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory = do
  merchantOpCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)

  let opCityId = merchantOpCity.id
  bppConfig <-
    SIBC.findIntegratedBPPConfig Nothing opCityId vehicleCategory DIBC.MULTIMODAL

  baseUrl <- MM.getOTPRestServiceReq bppConfig.merchantId opCityId
  pure (baseUrl, bppConfig.feedKey)

transitOperatorGetRowUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Maybe Text -> Flow Value
transitOperatorGetRowUtil merchantShortId city vehicleCategory table column = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorGetRow baseUrl gtfsId table column

transitOperatorGetAllRowsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Maybe Int -> Maybe Int -> Flow [Value]
transitOperatorGetAllRowsUtil merchantShortId city vehicleCategory table limit offset = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorGetAllRows baseUrl gtfsId table limit offset

transitOperatorDeleteRowUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Value -> Flow RowsAffectedResp
transitOperatorDeleteRowUtil merchantShortId city vehicleCategory table pkValue = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorDeleteRow baseUrl gtfsId table pkValue

transitOperatorUpsertRowUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Value -> Flow Value
transitOperatorUpsertRowUtil merchantShortId city vehicleCategory table body = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorUpsertRow baseUrl gtfsId table body

transitOperatorGetServiceTypesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [ServiceType]
transitOperatorGetServiceTypesUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorServiceTypes baseUrl gtfsId

transitOperatorGetRoutesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [NandiRoute]
transitOperatorGetRoutesUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorRoutes baseUrl gtfsId

transitOperatorGetDepotsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [Depot]
transitOperatorGetDepotsUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorDepots baseUrl gtfsId

transitOperatorGetShiftTypesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [Text]
transitOperatorGetShiftTypesUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorShiftTypes baseUrl gtfsId

transitOperatorGetScheduleNumbersUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [ScheduleNumber]
transitOperatorGetScheduleNumbersUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorScheduleNumbers baseUrl gtfsId

transitOperatorGetDayTypesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [Text]
transitOperatorGetDayTypesUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorDayTypes baseUrl gtfsId

transitOperatorGetTripTypesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [Text]
transitOperatorGetTripTypesUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorTripTypes baseUrl gtfsId

transitOperatorGetBreakTypesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [Text]
transitOperatorGetBreakTypesUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorBreakTypes baseUrl gtfsId

transitOperatorGetTripDetailsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Flow [NandiTripDetail]
transitOperatorGetTripDetailsUtil merchantShortId city vehicleCategory scheduleNumber = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorTripDetails baseUrl gtfsId scheduleNumber

transitOperatorGetFleetsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [Fleet]
transitOperatorGetFleetsUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorFleets baseUrl gtfsId

transitOperatorGetConductorUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Flow Employee
transitOperatorGetConductorUtil merchantShortId city vehicleCategory token = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorConductors baseUrl gtfsId token

transitOperatorGetDriverUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Flow Employee
transitOperatorGetDriverUtil merchantShortId city vehicleCategory token = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorDrivers baseUrl gtfsId token

transitOperatorGetDeviceIdsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [Text]
transitOperatorGetDeviceIdsUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorDeviceIds baseUrl gtfsId

transitOperatorGetTabletIdsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [Text]
transitOperatorGetTabletIdsUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorTabletIds baseUrl gtfsId

transitOperatorGetOperatorsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Flow [Employee]
transitOperatorGetOperatorsUtil merchantShortId city vehicleCategory role = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorOperators baseUrl gtfsId role

transitOperatorUpdateWaybillStatusUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> UpdateWaybillStatusReq -> Flow RowsAffectedResp
transitOperatorUpdateWaybillStatusUtil merchantShortId city vehicleCategory req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorWaybillStatus baseUrl gtfsId req

transitOperatorUpdateWaybillFleetUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> UpdateWaybillFleetReq -> Flow RowsAffectedResp
transitOperatorUpdateWaybillFleetUtil merchantShortId city vehicleCategory req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorWaybillFleet baseUrl gtfsId req

transitOperatorUpdateWaybillTabletUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> UpdateWaybillTabletReq -> Flow RowsAffectedResp
transitOperatorUpdateWaybillTabletUtil merchantShortId city vehicleCategory req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorWaybillTablet baseUrl gtfsId req

transitOperatorGetWaybillsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Int -> Maybe Int -> Flow [Value]
transitOperatorGetWaybillsUtil merchantShortId city vehicleCategory limit offset = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorWaybills baseUrl gtfsId limit offset
