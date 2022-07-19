module SharedLogic.MissingLocationUpdatesMarker
  ( isMarketAsMissingLocationUpdates,
    markAsMissingLocationUpdates,
  )
where

import Beckn.Storage.Hedis (HedisFlow)
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Id
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (pi)

isMarketAsMissingLocationUpdates :: (HedisFlow m r) => Id DRide.Ride -> m Bool
isMarketAsMissingLocationUpdates rideId = Redis.get @() (missingLocationUpdatesKey rideId) <&> isJust

markAsMissingLocationUpdates :: (HedisFlow m r) => Id DRide.Ride -> m ()
markAsMissingLocationUpdates rideId = Redis.setExp (missingLocationUpdatesKey rideId) () (60 * 60 * 24)

missingLocationUpdatesKey :: Id DRide.Ride -> Text
missingLocationUpdatesKey (Id rideId) = "BPP:missingLocationUpdates:" <> rideId
