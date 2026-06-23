{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Action.Rewards.Coupon
  ( renderTemplate,
    claimCoupon,
    finalizeClaim,
    reconcileInflight,
    staleThresholdSeconds,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardCohort as DRC
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.RewardUnlockExtra as QRUE
import qualified Tools.Rewards.RedisPool as Pool

staleThresholdSeconds :: Int
staleThresholdSeconds = 300

renderTemplate :: Text -> Id DP.Person -> Id DRCmp.RewardCampaign -> Text
renderTemplate template (Id pid) (Id cid) =
  T.replace "{personId}" pid $
    T.replace "{campaignId}" cid template

claimCoupon ::
  ( MonadFlow m,
    HedisFlow m env
  ) =>
  DRCmp.RewardCampaign ->
  DRC.RewardCohort ->
  Id DP.Person ->
  m (Maybe Text)
claimCoupon campaign cohort personId =
  case campaign.couponSourceType of
    DRCmp.Templated ->
      pure $
        case campaign.couponTemplate of
          Just tmpl -> Just (renderTemplate tmpl personId campaign.id)
          Nothing -> Nothing
    DRCmp.Pool ->
      Pool.claimFromPool campaign.id cohort.id

finalizeClaim ::
  (HedisFlow m env) =>
  DRCmp.RewardCampaign ->
  DRC.RewardCohort ->
  Text ->
  m ()
finalizeClaim campaign cohort code =
  case campaign.couponSourceType of
    DRCmp.Templated -> pure ()
    DRCmp.Pool -> Pool.removeFromInflight campaign.id cohort.id code

reconcileInflight ::
  ( MonadFlow m,
    HedisFlow m env,
    MonadTime m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DRCmp.RewardCampaign ->
  DRC.RewardCohort ->
  m ()
reconcileInflight campaign cohort = do
  let cId = campaign.id
      coId = cohort.id
  staleCodes <- Pool.staleInflightCodes cId coId staleThresholdSeconds
  logInfo $
    "rewards.reconcile cId="
      <> cId.getId
      <> " coId="
      <> coId.getId
      <> " staleCount="
      <> show (length staleCodes)
  forM_ staleCodes $ \code -> do
    mUnlock <- QRUE.findByCampaignCohortAndCode cId coId code
    case mUnlock of
      Just _ ->
        Pool.removeFromInflight cId coId code
      Nothing -> do
        Pool.pushBackToPool cId coId code
        Pool.removeFromInflight cId coId code
