module Core.ACL.Track (buildTrackReq) where

import Beckn.Prelude
import Beckn.Storage.Hedis as Redis
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.Track as Track
import qualified Domain.Action.UI.Track as DTrack
import ExternalAPI.Flow
import Utils.Common

buildTrackReq ::
  ( HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HedisFlow m r
  ) =>
  DTrack.DTrackRes ->
  m (BecknReq Track.TrackMessage)
buildTrackReq res = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  messageId <- generateGUID
  Redis.setExp (key messageId) res.bppRideId 1800 --30 mins
  context <- buildTaxiContext Context.TRACK messageId Nothing bapIDs.cabs bapURIs.cabs (Just res.bppId) (Just res.bppUrl)
  pure $ BecknReq context $ mkTrackMessage res
  where
    key messageId = "Track:bppRideId:" <> messageId

mkTrackMessage :: DTrack.DTrackRes -> Track.TrackMessage
mkTrackMessage res = Track.TrackMessage $ Track.Order res.bppRideId.getId
