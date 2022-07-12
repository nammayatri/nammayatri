module Core.ACL.OnTrack (mkOnTrackMessage) where

import qualified Beckn.Types.Core.Taxi.OnTrack as OnTrack
import qualified Domain.Action.Beckn.Track as DTrack

mkOnTrackMessage :: DTrack.DTrackRes -> OnTrack.OnTrackMessage
mkOnTrackMessage res = do
  OnTrack.OnTrackMessage
    { tracking =
        OnTrack.Tracking
          { url = res.url,
            content_type = "application/json"
          }
    }
