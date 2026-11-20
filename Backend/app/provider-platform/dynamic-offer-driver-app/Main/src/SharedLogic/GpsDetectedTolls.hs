{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Tolls crossed on the route the driver has actually driven so far, recorded mid-ride.
--
-- Why: while a driver stays on the planned route, the on-ride pipeline does not snap the GPS
-- points or record tolls on the ride -- the final snap-to-road and toll detection run only at
-- end ride. So mid-ride @ride.tollCharges@ is empty even after a toll has been crossed, and the
-- collect-payment preview (which must show the fare as if the ride ended now) would leave a
-- crossed toll out.
--
-- The per-batch toll-route check ('Lib.LocationUpdates.updateTollRouteDeviation') already runs
-- the toll detector on every batch of GPS points, but only keeps a yes/no flag
-- (@ride.driverDeviatedToTollRoute@). It now also records the tolls it finds here, so the preview
-- can bill them. End ride does not read this; it keeps doing its own final detection.
--
-- Stored per ride (never reused) with a TTL, so it needs no cleanup.
module SharedLogic.GpsDetectedTolls
  ( GpsDetectedTolls (..),
    recordGpsDetectedTolls,
    getGpsDetectedTolls,
  )
where

import qualified Data.List as List
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.LocationUpdates.TollTypes (TollInfo (..))

-- | Tolls crossed so far on the driven route, at the regular (car) charge. Toll-exempt tiers
-- (autos, bikes) never bill tolls, so the per-tier charge details are not needed here.
data GpsDetectedTolls = GpsDetectedTolls
  { tollCharges :: HighPrecMoney,
    tollNames :: [Text],
    tollIds :: [Text]
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

gpsDetectedTollsKey :: Id DRide.Ride -> Text
gpsDetectedTollsKey rideId = "GpsDetectedTolls:RideId-" <> rideId.getId

-- | Longest a ride can reasonably stay in progress.
gpsDetectedTollsTtlSec :: Int
gpsDetectedTollsTtlSec = 86400

-- | Adds the tolls found in one batch to the ride's record. A toll is counted once even if a
-- later batch reports it again (e.g. GPS noise around a gate), so re-reports never double-bill.
recordGpsDetectedTolls :: (CacheFlow m r) => Id DRide.Ride -> TollInfo -> m ()
recordGpsDetectedTolls rideId tollInfo = do
  existing <- fromMaybe (GpsDetectedTolls 0 [] []) <$> getGpsDetectedTolls rideId
  let newTolls =
        [ (tollId, name, charge)
          | (tollId, name, charge) <- zip3 tollInfo.tollIds tollInfo.tollNames (perTollCharges tollInfo),
            tollId `notElem` existing.tollIds
        ]
  unless (null newTolls) $ do
    let updated =
          GpsDetectedTolls
            { tollCharges = existing.tollCharges + sum (map (\(_, _, c) -> c) newTolls),
              tollNames = existing.tollNames <> map (\(_, n, _) -> n) newTolls,
              tollIds = existing.tollIds <> map (\(i, _, _) -> i) newTolls
            }
    logInfo $ "GpsDetectedTolls: ride " <> rideId.getId <> " crossed " <> show (map (\(_, n, _) -> n) newTolls) <> ", total so far " <> show updated.tollCharges
    Redis.setExp (gpsDetectedTollsKey rideId) updated gpsDetectedTollsTtlSec
  where
    -- TollInfo carries one aggregate charge for the batch. Split it evenly only when the batch
    -- found several tolls; the usual case is a single toll per batch, which gets its full charge.
    perTollCharges info = case List.length info.tollIds of
      0 -> []
      n -> replicate n (info.tollCharges / fromIntegral n)

getGpsDetectedTolls :: (CacheFlow m r) => Id DRide.Ride -> m (Maybe GpsDetectedTolls)
getGpsDetectedTolls rideId = Redis.safeGet (gpsDetectedTollsKey rideId)
