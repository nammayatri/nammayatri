{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Daily / weekly blocks counted from a campaign's start, so a campaign
-- starting 11 Nov 00:00 IST has days 11, 12, … and weeks 11–18, 18–25, …
-- regardless of how long it runs. Used both for the valid-ride counts in the
-- eligibility context and for cohorts whose unlock cap resets per block.
module Domain.Action.Rewards.CampaignBlock
  ( blockLength,
    blockStartAt,
    unlockBlockStart,
  )
where

import Data.Time (addUTCTime, diffUTCTime)
import qualified Domain.Types.RewardCohort as DRC
import Kernel.Prelude

blockLength :: DRC.UnlockResetPeriod -> NominalDiffTime
blockLength DRC.Daily = 86400
blockLength DRC.Weekly = 7 * 86400

-- | Start of the block containing @t@ for a campaign starting at @campaignStart@.
-- A @t@ before the campaign start (not expected: campaigns are only evaluated
-- once started) falls in the first block.
blockStartAt :: DRC.UnlockResetPeriod -> UTCTime -> UTCTime -> UTCTime
blockStartAt period campaignStart t =
  addUTCTime (fromIntegral k * len) campaignStart
  where
    len = blockLength period
    k = max 0 (floor (diffUTCTime t campaignStart / len) :: Integer)

-- | Start of the current cap block for a cohort whose cap resets per block,
-- or 'Nothing' when its cap covers the whole campaign.
unlockBlockStart :: UTCTime -> DRC.RewardCohort -> UTCTime -> Maybe UTCTime
unlockBlockStart campaignStart cohort t =
  (\period -> blockStartAt period campaignStart t) <$> cohort.unlockResetPeriod
