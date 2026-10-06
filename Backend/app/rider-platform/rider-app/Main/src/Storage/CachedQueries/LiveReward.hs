{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Per-city cache of live reward card candidates for GET /rewards/live,
-- which the home screen calls on every visit. Campaigns and cohorts only change
-- from the dashboard, which calls 'clearCache' on every campaign/cohort edit and
-- status change; the short TTL bounds staleness if a clear is missed. Campaign
-- time windows are filtered per request ('LiveReward.isLiveAt'), so a campaign
-- starting or ending needs no clear.
module Storage.CachedQueries.LiveReward
  ( findCandidatesByCity,
    clearCache,
  )
where

import qualified Domain.Action.Rewards.LiveReward as LiveReward
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RewardCampaign as DRCmp
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.RewardCampaignExtra as QRCmpE
import qualified Storage.Queries.RewardCohort as QRC

findCandidatesByCity ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [LiveReward.LiveRewardCandidate]
findCandidatesByCity moCityId =
  Hedis.safeGet (makeCityKey moCityId) >>= \case
    Just candidates -> pure candidates
    Nothing -> do
      campaigns <- filter (\c -> c.status == DRCmp.Active) <$> QRCmpE.findAllByCity moCityId
      withCohorts <- forM campaigns $ \campaign -> (campaign,) <$> QRC.findAllByCampaign campaign.id
      let candidates = LiveReward.mkLiveRewardCandidates withCohorts
      Hedis.setExp (makeCityKey moCityId) candidates cacheTtlSeconds
      pure candidates

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m ()
clearCache moCityId = Hedis.runInMultiCloudRedisWrite $ Hedis.del (makeCityKey moCityId)

cacheTtlSeconds :: Int
cacheTtlSeconds = 300

makeCityKey :: Id DMOC.MerchantOperatingCity -> Text
makeCityKey moCityId = "CachedQueries:LiveReward:MerchantOperatingCityId-" <> moCityId.getId
