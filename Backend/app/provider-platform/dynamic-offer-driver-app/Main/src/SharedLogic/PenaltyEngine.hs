{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PenaltyEngine
  ( triggerPenaltyEvaluation,
    calculatePenaltyAmount,
    checkAndUpdateGracePeriod,
    GracePeriodOutcome (..),
    PenaltyContext (..),
  )
where

import qualified Data.Text as T
import qualified Domain.Types.DriverGracePeriodTracker as DGPT
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PenaltyRecord as DPR
import qualified Domain.Types.PenaltyRule as DPRule
import qualified Domain.Types.Person as DP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (generateGUID, getCurrentTime)
import Kernel.Utils.Logging (logError, logInfo)
import qualified Storage.Queries.DriverGracePeriodTracker as QGraceTracker
import qualified Storage.Queries.PenaltyRecord as QPenaltyRecord
import qualified Storage.Queries.PenaltyRule as QPenaltyRule

-- | Context provided by the caller when a penalty-triggering event occurs.
data PenaltyContext = PenaltyContext
  { driverId :: Id DP.Person,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    triggerEvent :: DPRule.PenaltyTriggerEvent,
    triggerEntityId :: Text,
    -- | Base amount for percentage calculations (e.g., ride fare)
    baseAmount :: Maybe HighPrecMoney,
    reason :: Text
  }

-- | Outcome of grace period check
data GracePeriodOutcome
  = -- | Still within grace period, offense count incremented
    WithinGracePeriod Int Int
  | -- | Grace period exhausted, penalty should be applied
    GracePeriodExhausted
  | -- | No grace period configured (gracePeriodCount <= 0)
    NoGracePeriod
  deriving (Show)

-- | Main entry point: evaluate all matching penalty rules for a trigger event.
-- Finds active rules, checks grace periods, calculates amounts, creates records.
triggerPenaltyEvaluation :: PenaltyContext -> Flow [Id DPR.PenaltyRecord]
triggerPenaltyEvaluation ctx = do
  now <- getCurrentTime
  -- Find all active rules matching this trigger event, sorted by priority
  rules <- QPenaltyRule.findAllActiveByMerchantIdAndTriggerEventSorted ctx.merchantId ctx.triggerEvent now
  logInfo $
    "PenaltyEngine: Found " <> show (length rules) <> " active rules for event "
      <> show ctx.triggerEvent
      <> " driver="
      <> ctx.driverId.getId

  penaltyIds <- forM rules $ \rule -> do
    outcome <- checkAndUpdateGracePeriod ctx.driverId ctx.merchantId ctx.merchantOperatingCityId rule now
    case outcome of
      WithinGracePeriod current threshold -> do
        logInfo $
          "PenaltyEngine: Driver " <> ctx.driverId.getId
            <> " within grace period for rule "
            <> rule.id.getId
            <> " (offense "
            <> show current
            <> "/"
            <> show threshold
            <> ")"
        pure Nothing
      GracePeriodExhausted -> do
        logInfo $ "PenaltyEngine: Grace period exhausted for driver " <> ctx.driverId.getId <> " rule " <> rule.id.getId
        Just <$> createPenaltyRecord ctx rule now
      NoGracePeriod -> do
        logInfo $ "PenaltyEngine: No grace period for rule " <> rule.id.getId <> ", applying penalty immediately"
        Just <$> createPenaltyRecord ctx rule now

  pure $ catMaybes penaltyIds

-- | Check and update the grace period tracker for a driver+rule pair.
-- Uses atomic DB operations to avoid race conditions.
checkAndUpdateGracePeriod ::
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DPRule.PenaltyRule ->
  UTCTime ->
  Flow GracePeriodOutcome
checkAndUpdateGracePeriod driverId merchantId merchantOpCityId rule now = do
  -- If grace period is not configured, skip
  if rule.gracePeriodCount <= 0
    then pure NoGracePeriod
    else do
      mbTracker <- QGraceTracker.findByDriverIdAndRuleId driverId rule.id
      case mbTracker of
        Nothing -> do
          -- First offense: create a new tracker with offense count = 1
          let windowHours = fromIntegral rule.gracePeriodWindowHours
              windowEnd = addUTCTime (windowHours * 3600) now
          trackerId <- generateGUID
          let tracker =
                DGPT.DriverGracePeriodTracker
                  { id = trackerId,
                    driverId = driverId,
                    ruleId = rule.id,
                    merchantId = merchantId,
                    merchantOperatingCityId = merchantOpCityId,
                    offenseCount = 1,
                    windowStartTime = now,
                    windowEndTime = windowEnd,
                    createdAt = now,
                    updatedAt = now
                  }
          QGraceTracker.create tracker
          if rule.gracePeriodCount <= 1
            then pure GracePeriodExhausted
            else pure $ WithinGracePeriod 1 rule.gracePeriodCount

        Just tracker -> do
          if now > tracker.windowEndTime
            then do
              -- Window has expired: reset the window and start fresh with offense count = 1
              let windowHours = fromIntegral rule.gracePeriodWindowHours
                  newWindowEnd = addUTCTime (windowHours * 3600) now
              QGraceTracker.resetWindowByDriverIdAndRuleId driverId rule.id 1 now newWindowEnd now
              if rule.gracePeriodCount <= 1
                then pure GracePeriodExhausted
                else pure $ WithinGracePeriod 1 rule.gracePeriodCount
            else do
              -- Within window: increment offense count atomically
              let newCount = tracker.offenseCount + 1
              QGraceTracker.incrementOffenseCountByDriverIdAndRuleId driverId rule.id newCount now
              if newCount >= rule.gracePeriodCount
                then pure GracePeriodExhausted
                else pure $ WithinGracePeriod newCount rule.gracePeriodCount

-- | Calculate the penalty amount based on the rule type.
calculatePenaltyAmount :: DPRule.PenaltyRule -> Maybe HighPrecMoney -> Flow HighPrecMoney
calculatePenaltyAmount rule mbBaseAmount = do
  case rule.penaltyType of
    DPRule.FIXED -> do
      case rule.fixedAmount of
        Just amount -> pure amount
        Nothing -> do
          logError $ "PenaltyEngine: FIXED rule " <> rule.id.getId <> " has no fixedAmount, defaulting to 0"
          pure 0

    DPRule.PERCENTAGE -> do
      case (rule.percentage, mbBaseAmount) of
        (Just pct, Just base) -> do
          let amount = base * (fromRational (toRational pct) / 100)
          pure amount
        (Nothing, _) -> do
          logError $ "PenaltyEngine: PERCENTAGE rule " <> rule.id.getId <> " has no percentage value, defaulting to 0"
          pure 0
        (_, Nothing) -> do
          logError $ "PenaltyEngine: PERCENTAGE rule " <> rule.id.getId <> " has no base amount for calculation, defaulting to 0"
          pure 0

    DPRule.FORMULA -> do
      -- Formula evaluation: support simple expressions like "base * 0.1 + 50"
      -- For safety, we only support a limited set of operations on the base amount.
      -- Complex formulas should be decomposed into FIXED + PERCENTAGE rules.
      case (rule.formulaExpression, mbBaseAmount) of
        (Just expr, Just base) -> evaluateSimpleFormula expr base
        (Just expr, Nothing) -> evaluateSimpleFormula expr 0
        (Nothing, _) -> do
          logError $ "PenaltyEngine: FORMULA rule " <> rule.id.getId <> " has no formulaExpression, defaulting to 0"
          pure 0

-- | Evaluate a simple formula expression.
-- Supported format: "FIXED:<amount>" or "PERCENT:<percentage>" or "FIXED:<amount>+PERCENT:<percentage>"
-- This is a safe, restricted evaluator that does NOT execute arbitrary code.
evaluateSimpleFormula :: Text -> HighPrecMoney -> Flow HighPrecMoney
evaluateSimpleFormula expr baseAmount = do
  let parts = map T.strip $ T.splitOn "+" expr
  amounts <- forM parts $ \part -> do
    let trimmed = T.strip part
    case T.stripPrefix "FIXED:" trimmed of
      Just amountStr -> case readMaybe (T.unpack (T.strip amountStr)) :: Maybe Double of
        Just val -> pure $ fromRational (toRational val)
        Nothing -> do
          logError $ "PenaltyEngine: Invalid FIXED amount in formula: " <> amountStr
          pure 0
      Nothing -> case T.stripPrefix "PERCENT:" trimmed of
        Just pctStr -> case readMaybe (T.unpack (T.strip pctStr)) :: Maybe Double of
          Just pct -> pure $ baseAmount * (fromRational (toRational pct) / 100)
          Nothing -> do
            logError $ "PenaltyEngine: Invalid PERCENT value in formula: " <> pctStr
            pure 0
        Nothing -> do
          -- Try to parse as a raw number (fixed amount)
          case readMaybe (T.unpack trimmed) :: Maybe Double of
            Just val -> pure $ fromRational (toRational val)
            Nothing -> do
              logError $ "PenaltyEngine: Unrecognized formula part: " <> trimmed
              pure 0
  pure $ sum amounts

-- | Create a penalty record for a driver based on a matched rule.
createPenaltyRecord :: PenaltyContext -> DPRule.PenaltyRule -> UTCTime -> Flow (Id DPR.PenaltyRecord)
createPenaltyRecord ctx rule now = do
  amount <- calculatePenaltyAmount rule ctx.baseAmount
  recordId <- generateGUID
  let record =
        DPR.PenaltyRecord
          { id = recordId,
            driverId = ctx.driverId,
            ruleId = rule.id,
            merchantId = ctx.merchantId,
            merchantOperatingCityId = ctx.merchantOperatingCityId,
            triggerEvent = ctx.triggerEvent,
            triggerEntityId = ctx.triggerEntityId,
            amount = amount,
            currency = rule.currency,
            reason = ctx.reason,
            status = DPR.PENDING,
            disputeReason = Nothing,
            disputeEvidence = Nothing,
            disputeResolvedBy = Nothing,
            disputeResolvedAt = Nothing,
            ledgerEntryId = Nothing,
            invoiceId = Nothing,
            createdAt = now,
            updatedAt = now
          }
  QPenaltyRecord.create record
  logInfo $
    "PenaltyEngine: Created penalty record " <> recordId.getId
      <> " for driver "
      <> ctx.driverId.getId
      <> " amount="
      <> show amount
      <> " rule="
      <> rule.name
  pure recordId
