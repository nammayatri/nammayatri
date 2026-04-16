{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnTrack (mkOnTrackMessage, mkOnTrackMessageV2) where

import qualified Beckn.Types.Core.Taxi.OnTrack as OnTrack
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Track as DTrack
import Kernel.Prelude
import Text.Printf (printf)

mkOnTrackMessage :: DTrack.DTrackRes -> OnTrack.OnTrackMessage
mkOnTrackMessage res = do
  OnTrack.OnTrackMessage
    { tracking =
        OnTrack.Tracking
          { url = res.url,
            status = if res.isRideCompleted then OnTrack.INACTIVE else OnTrack.ACTIVE
          }
    }

mkOnTrackMessageV2 :: DTrack.DTrackRes -> Spec.OnTrackReqMessage
mkOnTrackMessageV2 res@DTrack.TrackRes {..} = do
  let trackingLocation =
        case driverLocation of
          Just loc ->
            Just $
              Spec.Location
                { locationGps = Just $ T.pack (printf "%.6f" loc.lat) <> ", " <> T.pack (printf "%.6f" loc.lon),
                  locationUpdatedAt = Just loc.coordinatesCalculatedAt,
                  locationAddress = Nothing,
                  locationAreaCode = Nothing,
                  locationCity = Nothing,
                  locationCountry = Nothing,
                  locationId = Nothing,
                  locationState = Nothing
                }
          Nothing -> Nothing
  Spec.OnTrackReqMessage
    { onTrackReqMessageTracking =
        Spec.Tracking
          { trackingUrl = Nothing, -- ONDC spec: send location, not URL
            trackingStatus = if res.isRideCompleted then Just "inactive" else Just "active",
            trackingLocation
          }
    }
