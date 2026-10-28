{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The budget cap of a Minimum Guarantee (MG) campaign is enforced when a driver is opted in, never
-- when the top-up is paid. Each enrollment reserves the campaign's full daily guarantee, which is
-- the most it can ever pay, so a driver who has been promised a guarantee and did the work is
-- always paid. When the reservations reach the budget, further opt-ins are refused.
--
-- A reservation is released when the day is opted out or cancelled. A day that was settled or
-- disqualified keeps it, which only makes the cap more conservative.
module SharedLogic.MinimumGuarantee.Budget
  ( reserveBudget,
    releaseBudget,
  )
where

import qualified Domain.Types.MgCampaign as DMgC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import SharedLogic.MinimumGuarantee.Context (MgEventFlow)
import qualified Storage.Queries.MgEnrollmentExtra as QMgEx

budgetKey :: DMgC.MgCampaign -> Text
budgetKey campaign = "MgBudget:" <> campaign.id.getId

moneyUnits :: HighPrecMoney -> Integer
moneyUnits = roundToIntegral . getHighPrecMoney

-- | Reserved total of the campaign, kept in Redis so each opt-in is O(1). When the key is missing
-- (first use, or Redis lost it) it is rebuilt from the enrollments that still hold a reservation.
reservedTotal :: MgEventFlow m r => DMgC.MgCampaign -> m Integer
reservedTotal campaign = do
  stored :: Maybe Integer <- Redis.withCrossAppRedis $ Redis.safeGet (budgetKey campaign)
  case stored of
    Just total -> pure total
    Nothing -> do
      holding <- QMgEx.findAllHoldingReservationByCampaignId campaign.id
      let total = toInteger (length holding) * moneyUnits campaign.guaranteeAmount
      Redis.withCrossAppRedis $ Redis.setExp (budgetKey campaign) total budgetKeyTtlSeconds
      pure total

-- | Reserves one day's guarantee, or refuses with an error when that would exceed the budget. A
-- campaign without a budget reserves nothing.
reserveBudget :: MgEventFlow m r => DMgC.MgCampaign -> m ()
reserveBudget campaign = whenJust campaign.maxBudget $ \budget -> do
  -- seed the key first, then add: the increment is atomic, so two opt-ins at once cannot both slip under the cap
  void $ reservedTotal campaign
  let amount = moneyUnits campaign.guaranteeAmount
  newTotal <- Redis.withCrossAppRedis $ Redis.incrby (budgetKey campaign) amount
  when (newTotal > moneyUnits budget) $ do
    void $ Redis.withCrossAppRedis $ Redis.decrby (budgetKey campaign) amount
    throwError $ InvalidRequest "The campaign budget is fully subscribed"

-- | Gives back one day's reservation. Does nothing when the counter is gone; it is rebuilt from the
-- enrollments the next time it is needed.
releaseBudget :: MgEventFlow m r => DMgC.MgCampaign -> m ()
releaseBudget campaign = whenJust campaign.maxBudget $ \_ -> do
  stored :: Maybe Integer <- Redis.withCrossAppRedis $ Redis.safeGet (budgetKey campaign)
  whenJust stored $ \_ ->
    void $ Redis.withCrossAppRedis $ Redis.decrby (budgetKey campaign) (moneyUnits campaign.guaranteeAmount)

budgetKeyTtlSeconds :: Int
budgetKeyTtlSeconds = 365 * 24 * 3600
