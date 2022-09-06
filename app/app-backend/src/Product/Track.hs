module Product.Track
  ( track,
  )
where

import Beckn.Prelude
import Beckn.Storage.Hedis.Config (HedisFlow)
import Beckn.Types.Id
import qualified Core.ACL.Track as ACL
import qualified Domain.Action.UI.Track as DTrack
import qualified Domain.Types.Ride as DRide
import qualified ExternalAPI.Flow as ExternalAPI
import Tools.Metrics
import Utils.Common

--TODO: technically it is not an EP, so we should move it somewhere i think.
track ::
  ( EsqDBFlow m r,
    CoreMetrics m,
    ExternalAPI.HasBapInfo r m,
    HasFlowEnv
      m
      r
      '["bapSelfIds" ::: ExternalAPI.BAPs Text, "bapSelfURIs" ::: ExternalAPI.BAPs BaseUrl],
    HedisFlow m r
  ) =>
  Id DRide.Ride ->
  m ()
track rideId = do
  dTrackRes <- DTrack.track rideId
  void . ExternalAPI.track dTrackRes.bppUrl =<< ACL.buildTrackReq dTrackRes
  return ()
