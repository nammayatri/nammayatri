{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

-- | The rider home-screen live reward card (GET /rewards/live), as pure
-- steps so the selection is unit testable without a DB, like
-- 'Domain.Action.Rewards.Evaluator.evaluateCohortsPure'.
--
-- Ops store the card on a cohort's @presentation.homeCard@: the card fields
-- plus an optional @targetingJsonLogic@ — "who should see this card", evaluated
-- against the rider's current context. That is deliberately not the cohort's
-- @eligibilityJsonLogic@, which answers "did this ride earn it" and would hide
-- e.g. a "take 3 rides" card from exactly the riders it should motivate.
module Domain.Action.Rewards.LiveReward
  ( LiveRewardCandidate (..),
    parseHomeCard,
    mkLiveRewardCandidates,
    isLiveAt,
    eligibleLiveRewards,
  )
where

import qualified API.Types.UI.Rewards as API
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Domain.Action.Rewards.Evaluator as Eval
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardCohort as DRC
import Domain.Types.RewardContext (rewardContextKeys)
import qualified Domain.Types.RewardUnlock as DRU
import Kernel.Prelude
import Kernel.Types.Id
import qualified Storage.Queries.RewardUnlockExtra as QRUE

-- | One cohort's home card with what the request path needs to filter it,
-- flattened so the per-city list can be cached as a whole.
data LiveRewardCandidate = LiveRewardCandidate
  { campaignId :: Id DRCmp.RewardCampaign,
    cohortId :: Id DRC.RewardCohort,
    startsAt :: UTCTime,
    endsAt :: Maybe UTCTime,
    isPoolSourced :: Bool,
    maxUnlocksPerCohort :: Maybe Int,
    card :: API.LiveRewardCard,
    targetingJsonLogic :: Maybe A.Value
  }
  deriving (Generic, ToJSON, FromJSON)

-- | Read @presentation.homeCard@: the card (the extra @targetingJsonLogic@ key
-- is ignored by its decoder) and the optional audience rule. The rule may only
-- reference rider-context fields known at home-screen time; @isValidRide@
-- describes a just-finished ride, so it is rejected here.
parseHomeCard :: A.Value -> Either Text (API.LiveRewardCard, Maybe A.Value)
parseHomeCard v = do
  parsedCard <- case A.fromJSON v of
    A.Success c -> Right c
    A.Error e -> Left (T.pack e)
  audience <- case v of
    A.Object o -> case KM.lookup "targetingJsonLogic" o of
      Nothing -> Right Nothing
      Just A.Null -> Right Nothing
      Just logic -> Just logic <$ validateTarget logic
    _ -> Right Nothing
  pure (parsedCard, audience)
  where
    allowedFields = filter (/= "isValidRide") rewardContextKeys
    validateTarget logic =
      let referenced = filter (not . T.null) (Eval.collectVarNames logic)
          unknown = filter (\f -> T.takeWhile (/= '.') f `notElem` allowedFields) referenced
       in unless (null unknown) $
            Left $
              "targetingJsonLogic references unknown field(s): " <> T.intercalate ", " unknown
                <> ". Allowed fields: "
                <> T.intercalate ", " allowedFields

-- | Every cohort home card across the city's Active campaigns, in display
-- priority: campaigns, then cohorts, by (displayOrder, createdAt, id) so ties
-- (displayOrder defaults to 0) can't swap between calls. Time windows are left
-- to 'isLiveAt' so the result stays valid to cache.
mkLiveRewardCandidates :: [(DRCmp.RewardCampaign, [DRC.RewardCohort])] -> [LiveRewardCandidate]
mkLiveRewardCandidates campaigns =
  [ LiveRewardCandidate
      { campaignId = campaign.id,
        cohortId = cohort.id,
        startsAt = campaign.startsAt,
        endsAt = campaign.endsAt,
        isPoolSourced = campaign.couponSourceType == DRCmp.Pool,
        maxUnlocksPerCohort = cohort.maxUnlocksPerCohort,
        card = homeCardValue,
        targetingJsonLogic = homeCardAudience
      }
    | (campaign, cohorts) <- sortOn (\(c, _) -> (c.displayOrder, c.createdAt, c.id.getId)) campaigns,
      campaign.status == DRCmp.Active,
      cohort <- sortOn (\c -> (c.displayOrder, c.createdAt, c.id.getId)) cohorts,
      Just (A.Object presentation) <- [cohort.presentation],
      Just homeCard <- [KM.lookup "homeCard" presentation],
      Right (homeCardValue, homeCardAudience) <- [parseHomeCard homeCard]
  ]

-- | The campaign's window covers @now@ (same bounds as findAllActiveInCityAtTime).
isLiveAt :: UTCTime -> LiveRewardCandidate -> Bool
isLiveAt now c = c.startsAt <= now && maybe True (> now) c.endsAt

-- | Candidates this rider should see, in priority order: still unlockable under
-- the cohort's repeat cap ('QRUE.hasUnlockCapacity', the rule the unlock engine
-- uses) and matching the audience rule, if any, against the rider's current
-- context. A rule that fails to evaluate hides the card. Pool stock is checked
-- by the caller, since it needs Redis.
eligibleLiveRewards :: A.Value -> [DRU.RewardUnlock] -> [LiveRewardCandidate] -> [LiveRewardCandidate]
eligibleLiveRewards riderContext unlocks = filter (\c -> canStillUnlock c && inAudience c)
  where
    canStillUnlock c = QRUE.hasUnlockCapacity c.maxUnlocksPerCohort c.cohortId unlocks
    inAudience c = case c.targetingJsonLogic of
      Nothing -> True
      Just logic -> either (const False) snd (Eval.evalCohortLogic logic riderContext)
