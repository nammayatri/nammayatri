{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | FY-to-date driver / fleet-owner earnings, used by the TDS threshold gate and
-- by the FY-quarter earnings report.
--
-- Definitions (see docs/fy-quarter-earnings-approach.md):
--
-- @
--   TDS base             = Total Ride Fare - GST
--   Net Driver Take Home = Total Ride Fare - GST - TDS (if applicable)
-- @
--
-- The accumulator stores net take home. That is enough for the threshold because
-- below it no TDS is deducted, so net and base are the same number; they diverge
-- only after the crossing, by which point both are above the threshold.
module SharedLogic.DriverFyEarnings
  ( makeFyEarningsLockKey,
    getFyToDateNetEarnings,
    addQuarterNetEarnings,
  )
where

import Data.Time.Calendar (Day)
import qualified Domain.Types.DriverFyEarnings as DFE
import Domain.Types.FinancialYear (fyAndQuarterOf)
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverFyEarnings as QDFE

-- | Serialises the read-decide-write around the accumulator.
--
-- Keyed on the /person/, not the driver: for a fleet ride the accumulator belongs
-- to the fleet owner, so a driver-keyed lock would let two of that fleet's drivers
-- race the same row. Distinct from 'makeWalletRunningBalanceLockKey', which is
-- driver-keyed and wraps the whole ledger/invoice block.
makeFyEarningsLockKey :: Text -> Text
makeFyEarningsLockKey personId = "DriverFyEarningsLockKey:" <> personId

-- | Net take home so far this financial year, across its quarters.
--
-- At most four rows, so the sum is done here rather than in SQL. That is
-- deliberate: KV reads consult Redis first and therefore see writes that have not
-- yet drained to Postgres, whereas a SQL aggregate would silently miss them.
getFyToDateNetEarnings ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Int ->
  m HighPrecMoney
getFyToDateNetEarnings personId financialYear = do
  rows <- QDFE.findAllByPersonIdAndFinancialYear personId financialYear
  pure $ sum (map (.netEarningsTotal) rows)

-- | Add one ride's net take home and TDS to the quarter it belongs to.
--
-- The bucket comes from the ride's own local date, never from @now@, so a late or
-- replayed ride lands in the period it belongs to.
--
-- Call inside 'makeFyEarningsLockKey': this is a read-modify-write.
addQuarterNetEarnings ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  -- Args: person, financial-year start month (from
  -- TransporterConfig.analyticsConfig), the ride's merchant-local date, this
  -- ride's net take home, and the TDS deducted (0 when below the threshold).
  Id DP.Person ->
  Int ->
  Day ->
  HighPrecMoney ->
  HighPrecMoney ->
  m ()
addQuarterNetEarnings personId fyStartMonth rideLocalDate netEarnings tdsAmount = do
  let (financialYear, quarter) = fyAndQuarterOf fyStartMonth rideLocalDate
  mbRow <- QDFE.findByPersonIdAndFinancialYearAndQuarter personId financialYear quarter
  case mbRow of
    Just row ->
      QDFE.updateEarningsTotals (row.netEarningsTotal + netEarnings) (row.tdsAmountTotal + tdsAmount) personId financialYear quarter
    Nothing -> do
      newId <- generateGUID
      now <- getCurrentTime
      QDFE.create
        DFE.DriverFyEarnings
          { id = newId,
            personId = personId,
            financialYear = financialYear,
            quarter = quarter,
            netEarningsTotal = netEarnings,
            tdsAmountTotal = tdsAmount,
            createdAt = now,
            updatedAt = now
          }
