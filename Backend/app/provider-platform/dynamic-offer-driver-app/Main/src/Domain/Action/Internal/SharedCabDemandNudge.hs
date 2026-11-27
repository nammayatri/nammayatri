module Domain.Action.Internal.SharedCabDemandNudge
  ( sharedCabDemandNudge,
  )
where

import API.Types.UI.SharedCabInternal
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Domain.Action.Internal.SharedCabTrip (authMerchant)
import qualified Domain.Types.Common as DCommon
import Domain.Types.DriverInformation (DriverInformationE (..))
import Domain.Types.DriverLocation (DriverLocation (..))
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person (Person, PersonE (..))
import qualified Domain.Types.Vehicle as DVeh
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Types (Language (..))
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Notifications

sharedCabDemandNudge :: Id DM.Merchant -> Maybe Text -> SharedCabDemandNudgeReq -> Flow APISuccess
sharedCabDemandNudge merchantId apiKey req = do
  authMerchant merchantId apiKey
  fork "sharedCabDemandNudge" $ nudgeDrivers merchantId req
  pure Success

nudgeDrivers :: Id DM.Merchant -> SharedCabDemandNudgeReq -> Flow ()
nudgeDrivers merchantId req = do
  let tunables = req.tunables
      onRoutePlates = Set.fromList req.onRoutePlates
  onRouteVehicles <- filter isSharedCab <$> QVehicle.findAllByRegistrationNos (ordNub [cab.vehicleNumber | route <- req.routes, cab <- route.onRoute])
  onlineDrivers <- Set.fromList . map (.driverId) . filter ((== Just DCommon.ONLINE) . (.mode)) <$> QDriverInformation.findAllByDriverIds (map (.driverId.getId) onRouteVehicles)
  let driverOfPlate = Map.fromList [(v.registrationNo, v.driverId) | v <- onRouteVehicles, Set.member v.driverId onlineDrivers]
      onRouteCards =
        Map.fromList
          [ (driverId, [option route cab.nextIdx 0])
            | route <- req.routes,
              cab <- route.onRoute,
              Just driverId <- [Map.lookup cab.vehicleNumber driverOfPlate]
          ]
  found <- forM (lookupPoints tunables req.routes) $ \point ->
    withTryCatch "sharedCabDemandNudge:nearBy" (LF.nearBy point.lat point.lon (Just False) Nothing tunables.idleRadiusM merchantId Nothing Nothing Nothing)
      >>= either (\err -> [] <$ logError ("shared-cab demand nudge: nearBy failed: " <> show err)) pure
  let located = Map.fromList [(loc.driverId, LatLong loc.lat loc.lon) | loc <- concat found, Map.notMember loc.driverId onRouteCards]
  idleVehicles <- filter (\v -> isSharedCab v && Set.notMember v.registrationNo onRoutePlates) <$> QVehicle.findAllByDriverIds (Map.keys located)
  let idleOptions =
        [ (v.driverId, opts)
          | v <- idleVehicles,
            Just position <- [Map.lookup v.driverId located],
            let opts = driverOptions tunables req.routes position,
            not (null opts)
        ]
      cards = onRouteCards <> Map.fromList (selectDrivers tunables idleOptions)
  persons <- QPerson.getDriversByIdIn (Map.keys cards)
  forM_ persons $ \person -> whenJust (Map.lookup person.id cards) $ \options ->
    notifySharedCabDemandNudge
      person.merchantOperatingCityId
      person.id
      person.deviceToken
      (fromMaybe ENGLISH person.language)
      SharedCabDemandNudgeEntityData {notificationType = "SHARED_CAB_DEMAND_NUDGE", options, expiresAt = tunables.expiresAt}
  logInfo $ "shared-cab demand nudge: " <> show (Map.size onRouteCards) <> " on-route, " <> show (Map.size cards - Map.size onRouteCards) <> " idle drivers"

isSharedCab :: DVeh.Vehicle -> Bool
isSharedCab v = DCommon.SHARED_CAB `elem` v.selectedServiceTiers

stopCounts :: [Int] -> [Int]
stopCounts ahead = zipWith (-) ahead (drop 1 ahead <> [0])

aheadAt :: [Int] -> Int -> Int
aheadAt counts i = fromMaybe 0 (listToMaybe (drop i counts))

metresBetween :: LatLong -> LatLong -> Int
metresBetween a b = round (distanceBetweenInMeters a b)

option :: SharedCabNudgeRoute -> Int -> Int -> SharedCabDemandOption
option route k distance =
  SharedCabDemandOption
    { routeCode = route.routeCode,
      routeName = route.routeName,
      nearStopCode = maybe "" (.code) nearStop,
      nearStopName = maybe "" (.name) nearStop,
      distanceM = distance,
      waiting = aheadAt route.ahead k,
      stopsAhead =
        [ SharedCabDemandStopCount {stopCode = stop.code, stopName = stop.name, count}
          | (stop, count) <- drop k (zip route.stops (stopCounts route.ahead)),
            count > 0
        ]
    }
  where
    nearStop = listToMaybe (drop k route.stops)

lookupPoints :: SharedCabNudgeTunables -> [SharedCabNudgeRoute] -> [LatLong]
lookupPoints tunables routes = take tunables.maxLookups (foldl' keep [] ranked)
  where
    byStop = Map.fromListWith max [(stop.code, (count, stop.point)) | route <- routes, (stop, count) <- zip route.stops (stopCounts route.ahead), count > 0]
    ranked = map snd (sortOn (Down . fst) (Map.elems byStop))
    keep kept point = if any (\p -> metresBetween p point <= tunables.mergeRadiusM) kept then kept else kept <> [point]

driverOptions :: SharedCabNudgeTunables -> [SharedCabNudgeRoute] -> LatLong -> [SharedCabDemandOption]
driverOptions tunables routes position =
  take tunables.maxOptions . sortOn (\o -> (Down o.waiting, o.distanceM)) $
    [ option route k distance
      | route <- routes,
        Just (distance, k) <- [nearest route],
        distance <= tunables.idleRadiusM,
        aheadAt route.ahead k >= tunables.minRiders
    ]
  where
    nearest route = listToMaybe (sortOn fst [(metresBetween position stop.point, k) | (stop, k) <- zip route.stops [0 ..]])

selectDrivers :: SharedCabNudgeTunables -> [(Id Person, [SharedCabDemandOption])] -> [(Id Person, [SharedCabDemandOption])]
selectDrivers tunables candidates = snd (foldl' assign (Map.empty, []) ranked)
  where
    ranked = sortOn (\(_, opts) -> fmap (\o -> (Down o.waiting, o.distanceM)) (listToMaybe opts)) candidates
    assign (taken, picked) (driverId, opts) =
      let hasRoom o = Map.findWithDefault 0 o.routeCode taken < tunables.maxIdlePerRoute
       in case find hasRoom opts <|> listToMaybe opts of
            Nothing -> (taken, picked)
            Just primary -> (Map.insertWith (+) primary.routeCode (1 :: Int) taken, (driverId, primary : filter (/= primary) opts) : picked)
