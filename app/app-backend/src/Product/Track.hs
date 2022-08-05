module Product.Track
  ( track,
    onTrack,
  )
where

import App.Types
import Beckn.Prelude
import Beckn.Storage.Hedis.Config (HedisFlow)
import qualified Beckn.Types.Core.Taxi.API.OnTrack as API
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnTrack as ACL
import qualified Core.ACL.Track as ACL
import qualified Domain.Action.Beckn.OnTrack as DOnTrack
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

onTrack ::
  SignatureAuthResult ->
  API.OnTrackReq ->
  FlowHandler AckResponse
onTrack (SignatureAuthResult _ _ registryUrl) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnTrackReq <- ACL.buildOnTrackReq req
  whenJust mbDOnTrackReq (DOnTrack.onTrack registryUrl)
  pure Ack
