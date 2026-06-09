{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.StopRouteDetails
  ( stopRouteDetailsGetStops,
    stopRouteDetailsGetStop,
    stopRouteDetailsGetRouteStopMappingByStop,
    stopRouteDetailsGetRouteStopMappingByRoute,
    clearStopRouteDetailsGetStopsCache,
  )
where

import qualified API.Types.Dashboard.AppManagement.Endpoints.StopRouteDetails as StopRouteDetailsAPI
import qualified "beckn-spec" BecknV2.OnDemand.Enums as Enums
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.RouteStopMapping
import qualified "this" Domain.Types.Station
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.RoutePolylines as QRoutePolylines
import Tools.Error

resolveBppConfig ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Enums.VehicleCategory ->
  Environment.Flow DIBC.IntegratedBPPConfig
resolveBppConfig merchantShortId opCity vehicleCategory = do
  moc <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM
        ( MerchantOperatingCityNotFound $
            "merchantShortId: " <> merchantShortId.getShortId <> ", city: " <> show opCity
        )
  SIBC.findIntegratedBPPConfig Nothing moc.id vehicleCategory DIBC.MULTIMODAL

stopRouteDetailsGetStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Enums.VehicleCategory -> Environment.Flow [Domain.Types.Station.Station])
stopRouteDetailsGetStops merchantShortId opCity mbIncludeClusterId vehicleCategory = do
  bppConfig <- resolveBppConfig merchantShortId opCity vehicleCategory
  gtfsVersion <- OTPRest.getGtfsVersion bppConfig
  let includeClusterId = fromMaybe True mbIncludeClusterId
      cacheKey = makeStopRouteDetailsGetStopsKey merchantShortId opCity vehicleCategory includeClusterId gtfsVersion
  IM.withInMemCache [cacheKey] 86400 $ do
    Hedis.safeGet cacheKey >>= \case
      Just stations | not (null stations) -> pure stations
      _ -> do
        stations <- OTPRest.getStationsByGtfsId (Just includeClusterId) bppConfig
        unless (null stations) $ Hedis.setExp cacheKey stations 86400 -- 1 day
        pure stations

makeStopRouteDetailsGetStopsKey :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Enums.VehicleCategory -> Bool -> Text -> Text
makeStopRouteDetailsGetStopsKey merchantShortId opCity vehicleCategory includeClusterId gtfsVersion =
  "CachedQueries:StopRouteDetails:GetStops:Merchant-" <> merchantShortId.getShortId
    <> ":City-"
    <> show opCity
    <> ":VehicleCategory-"
    <> show vehicleCategory
    <> ":IncludeClusterId-"
    <> show includeClusterId
    <> ":GtfsVersion-"
    <> gtfsVersion

clearStopRouteDetailsGetStopsCache :: (CacheFlow m r) => Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Enums.VehicleCategory -> Text -> m ()
clearStopRouteDetailsGetStopsCache merchantShortId opCity vehicleCategory gtfsVersion = do
  Hedis.del (makeStopRouteDetailsGetStopsKey merchantShortId opCity vehicleCategory True gtfsVersion)
  Hedis.del (makeStopRouteDetailsGetStopsKey merchantShortId opCity vehicleCategory False gtfsVersion)

stopRouteDetailsGetStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Enums.VehicleCategory -> Environment.Flow StopRouteDetailsAPI.StationResp)
stopRouteDetailsGetStop merchantShortId opCity stopCode vehicleCategory = do
  bppConfig <- resolveBppConfig merchantShortId opCity vehicleCategory
  mbStation <- OTPRest.getStationByGtfsIdAndStopCodeWithClusterId (Just True) stopCode bppConfig
  pure $ StopRouteDetailsAPI.StationResp {station = mbStation}

stopRouteDetailsGetRouteStopMappingByStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Enums.VehicleCategory -> Environment.Flow [Domain.Types.RouteStopMapping.RouteStopMapping])
stopRouteDetailsGetRouteStopMappingByStop merchantShortId opCity stopCode vehicleCategory = do
  bppConfig <- resolveBppConfig merchantShortId opCity vehicleCategory
  OTPRest.getRouteStopMappingByStopCode stopCode bppConfig

stopRouteDetailsGetRouteStopMappingByRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Enums.VehicleCategory -> Environment.Flow StopRouteDetailsAPI.RouteStopMappingWithPolyline)
stopRouteDetailsGetRouteStopMappingByRoute merchantShortId opCity routeCode vehicleCategory = do
  bppConfig <- resolveBppConfig merchantShortId opCity vehicleCategory
  routeStopMappings <- OTPRest.getRouteStopMappingByRouteCode routeCode bppConfig
  mbPolyline <- QRoutePolylines.getByRouteIdAndCity routeCode bppConfig.merchantOperatingCityId
  pure $
    StopRouteDetailsAPI.RouteStopMappingWithPolyline
      { routeStopMappings = routeStopMappings,
        polyline = mbPolyline >>= (.polyline)
      }
