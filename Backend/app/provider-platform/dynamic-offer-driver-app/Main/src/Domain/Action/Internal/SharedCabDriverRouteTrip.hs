module Domain.Action.Internal.SharedCabDriverRouteTrip
  ( recordDriverRouteTrip,
    driverRouteTrips,
  )
where

import API.Types.UI.SharedCabInternal (SharedCabDriverRouteTripCount (..), SharedCabDriverRouteTripReq (..))
import qualified Data.Map.Strict as Map
import Domain.Action.Internal.SharedCabAllocation (verifiedDriver)
import qualified Domain.Types.DriverRouteTrip as DDRT
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverRouteTrip as QDRT

defaultDays, maxDays :: Int
defaultDays = 14
maxDays = 30

confirmedTripWeight :: Int
confirmedTripWeight = 3

recordDriverRouteTrip :: Maybe Text -> SharedCabDriverRouteTripReq -> Flow APISuccess
recordDriverRouteTrip apiKey req = do
  person <- verifiedDriver req.driverId apiKey
  seen <- maybe (pure Nothing) QDRT.findByIdempotencyKey req.idempotencyKey
  when (isNothing seen) $ do
    id <- generateGUID
    now <- getCurrentTime
    QDRT.create
      DDRT.DriverRouteTrip
        { id,
          driverId = person.id,
          routeCode = req.routeCode,
          source = fromMaybe DDRT.INFERRED req.source,
          idempotencyKey = fromMaybe id.getId req.idempotencyKey,
          rideId = req.rideId,
          vehicleNumber = req.vehicleNumber,
          merchantId = person.merchantId,
          merchantOperatingCityId = person.merchantOperatingCityId,
          createdAt = now,
          updatedAt = now
        }
  pure Success

driverRouteTrips :: Text -> Maybe Int -> Maybe Text -> Flow [SharedCabDriverRouteTripCount]
driverRouteTrips driverId mbDays apiKey = do
  person <- verifiedDriver driverId apiKey
  now <- getCurrentTime
  let days = max 1 . min maxDays $ fromMaybe defaultDays mbDays
      since = addUTCTime (negate . fromIntegral $ days * 86400) now
  trips <- QDRT.findAllByDriverIdAndCityCreatedSince Nothing Nothing person.id person.merchantOperatingCityId since
  let weight trip = if trip.source == DDRT.CONFIRMED then confirmedTripWeight else 1
      counts = Map.fromListWith (+) [(trip.routeCode, weight trip) | trip <- trips]
  pure [SharedCabDriverRouteTripCount {routeCode, tripCount} | (routeCode, tripCount) <- Map.toList counts]
