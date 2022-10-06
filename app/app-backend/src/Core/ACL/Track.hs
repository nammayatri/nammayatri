module Core.ACL.Track
  ( TrackBuildReq (..),
    buildTrackReq,
  )
where

import Beckn.Prelude
import Beckn.Storage.Hedis as Redis
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.Track as Track
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Ride as DRide
import Environment

data TrackBuildReq = TrackBuildReq
  { bppRideId :: Id DRide.BPPRide,
    bppId :: Text,
    bppUrl :: BaseUrl
  }

buildTrackReq ::
  ( MonadFlow m,
    HasBapInfo r m,
    HedisFlow m r
  ) =>
  TrackBuildReq ->
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

mkTrackMessage :: TrackBuildReq -> Track.TrackMessage
mkTrackMessage res = Track.TrackMessage $ Track.Order res.bppRideId.getId
