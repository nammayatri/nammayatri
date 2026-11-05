{-# LANGUAGE TypeApplications #-}

-- | Reads of the LTS route hash (`route:{routeCode}`, one field per plate): the only place that knows its layout.
module SharedLogic.SharedCab.Lts
  ( readRoutePositions,
    readRoutePosition,
  )
where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB

-- | Read straight from the LTS Redis, the hot path (the tick every few seconds, every rider poll) as bus reads partner buses
-- (FRFSUtils.trackVehicles uses bus next-stop logic, wrong for cabs). Fields decode one by one: a bad one is dropped, not the route.
readRoutePositions ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, Log m) =>
  Text ->
  m [LT.VehicleTrackingOnRouteResp]
readRoutePositions routeCode = do
  pairs <- Redis.runInMultiCloudLTSRedisForListFromReplica $ Redis.hGetAll @A.Value (routeKey routeCode)
  fmap catMaybes . forM pairs $ \(plate, raw) -> case A.fromJSON raw of
    A.Success info -> pure (Just (LT.VehicleTrackingOnRouteResp plate info))
    A.Error err -> Nothing <$ logWarning ("shared-cab tick: dropping LTS field " <> plate <> " on route " <> routeCode <> ": " <> T.pack err)

-- | One cab's position; an LTS read failure is Nothing, like a cab with no position.
readRoutePosition ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Text ->
  Text ->
  m (Maybe LT.VehicleTrackingOnRouteResp)
readRoutePosition routeCode plate = do
  fields <- either (const []) identity <$> withTryCatch "sharedCab:readRoutePosition" (Redis.runInMultiCloudLTSRedisForListFromReplica $ maybeToList <$> Redis.hGet @A.Value (routeKey routeCode) plate)
  pure $ case listToMaybe fields of
    Just raw | A.Success info <- A.fromJSON raw -> Just (LT.VehicleTrackingOnRouteResp plate info)
    _ -> Nothing

-- | LTS and gps-processor write cab positions under the unprefixed key; a bus feed's redisPrefix names a partner feed's own hash.
routeKey :: Text -> Text
routeKey = CQMMB.mkRouteKey Nothing
