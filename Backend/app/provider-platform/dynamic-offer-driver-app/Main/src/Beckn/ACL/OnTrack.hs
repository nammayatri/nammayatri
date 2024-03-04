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
import qualified Domain.Action.Beckn.Track as DTrack
import Kernel.Prelude

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
  let trackingUrl = if isValueAddNP then Just $ showBaseUrl res.url else Nothing
  let trackingLocation =
        ( \loc ->
            Just $
              Spec.Location
                { locationGps = Just $ (show loc.lat) <> ", " <> show loc.lon,
                  locationUpdatedAt = Just loc.coordinatesCalculatedAt,
                  locationAddress = Nothing,
                  locationAreaCode = Nothing,
                  locationCity = Nothing,
                  locationCountry = Nothing,
                  locationId = Nothing,
                  locationState = Nothing
                }
        )
          =<< driverLocation
  Spec.OnTrackReqMessage
    { onTrackReqMessageTracking =
        Spec.Tracking
          { trackingUrl,
            trackingStatus = if res.isRideCompleted then Just "INACTIVE" else Just "ACTIVE",
            trackingLocation
          }
    }
