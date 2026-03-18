{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.PenaltyManagement
  ( listPenaltyRules,
    createPenaltyRule,
    updatePenaltyRule,
    togglePenaltyRule,
    listPenalties,
    reviewDispute,
    getPenaltyAnalytics,
    PenaltyRuleListRes (..),
    CreatePenaltyRuleReq (..),
    UpdatePenaltyRuleReq (..),
    TogglePenaltyRuleReq (..),
    PenaltyListRes (..),
    PenaltyRecordItem (..),
    ReviewDisputeReq (..),
    DisputeAction (..),
    PenaltyAnalyticsRes (..),
  )
where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PenaltyRecord as DPR
import qualified Domain.Types.PenaltyRule as DPRule
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime)
import Kernel.Utils.Logging (logInfo)
import SharedLogic.Merchant (findMerchantByShortId)
import Tools.Error (GenericError (InvalidRequest))
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.PenaltyRecord as QPenaltyRecord
import qualified Storage.Queries.PenaltyRule as QPenaltyRule

-- -----------------------------------------------------------------
-- Types
-- -----------------------------------------------------------------

data PenaltyRuleListRes = PenaltyRuleListRes
  { rules :: [DPRule.PenaltyRule],
    totalCount :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CreatePenaltyRuleReq = CreatePenaltyRuleReq
  { name :: Text,
    triggerEvent :: DPRule.PenaltyTriggerEvent,
    conditionsJson :: Text,
    penaltyType :: DPRule.PenaltyAmountType,
    fixedAmount :: Maybe HighPrecMoney,
    percentage :: Maybe Double,
    formulaExpression :: Maybe Text,
    currency :: Currency,
    gracePeriodCount :: Int,
    gracePeriodWindowHours :: Int,
    priority :: Int,
    isActive :: Bool,
    startDate :: Maybe UTCTime,
    endDate :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpdatePenaltyRuleReq = UpdatePenaltyRuleReq
  { name :: Maybe Text,
    conditionsJson :: Maybe Text,
    penaltyType :: Maybe DPRule.PenaltyAmountType,
    fixedAmount :: Maybe (Maybe HighPrecMoney),
    percentage :: Maybe (Maybe Double),
    formulaExpression :: Maybe (Maybe Text),
    gracePeriodCount :: Maybe Int,
    gracePeriodWindowHours :: Maybe Int,
    priority :: Maybe Int,
    startDate :: Maybe (Maybe UTCTime),
    endDate :: Maybe (Maybe UTCTime)
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data TogglePenaltyRuleReq = TogglePenaltyRuleReq
  { isActive :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data PenaltyRecordItem = PenaltyRecordItem
  { penaltyRecord :: DPR.PenaltyRecord,
    ruleName :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data PenaltyListRes = PenaltyListRes
  { penalties :: [PenaltyRecordItem],
    totalCount :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DisputeAction = WAIVE | REJECT
  deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, ToSchema)

data ReviewDisputeReq = ReviewDisputeReq
  { action :: DisputeAction,
    resolverNote :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data PenaltyAnalyticsRes = PenaltyAnalyticsRes
  { totalPenalties :: Int,
    totalPenaltyAmount :: HighPrecMoney,
    disputedCount :: Int,
    waivedCount :: Int,
    rejectedCount :: Int,
    pendingCount :: Int,
    appliedCount :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- -----------------------------------------------------------------
-- Handlers
-- -----------------------------------------------------------------

listPenaltyRules ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe DPRule.PenaltyTriggerEvent ->
  Maybe Int ->
  Maybe Int ->
  Flow PenaltyRuleListRes
listPenaltyRules merchantShortId opCity mbTriggerEvent mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  rules <- case mbTriggerEvent of
    Just triggerEvent ->
      QPenaltyRule.findAllByMerchantIdAndTriggerEvent merchant.id triggerEvent True
    Nothing ->
      QPenaltyRule.findAllByMerchantIdWithLimitOffset merchant.id mbLimit mbOffset
  pure $ PenaltyRuleListRes {rules = rules, totalCount = length rules}

-- | Validate that the required fields are present for the given penalty type.
validatePenaltyTypeFields :: DPRule.PenaltyAmountType -> Maybe HighPrecMoney -> Maybe Double -> Maybe Text -> Flow ()
validatePenaltyTypeFields penaltyType fixedAmount pct formulaExpr = do
  case penaltyType of
    DPRule.FIXED ->
      when (isNothing fixedAmount) $
        throwError (InvalidRequest "fixedAmount is required for FIXED penalty type")
    DPRule.PERCENTAGE ->
      case pct of
        Nothing -> throwError (InvalidRequest "percentage is required for PERCENTAGE penalty type")
        Just p -> do
          when (p < 0 || p > 100) $
            throwError (InvalidRequest "percentage must be between 0 and 100")
    DPRule.FORMULA ->
      when (isNothing formulaExpr) $
        throwError (InvalidRequest "formulaExpression is required for FORMULA penalty type")

createPenaltyRule ::
  ShortId DM.Merchant ->
  Context.City ->
  CreatePenaltyRuleReq ->
  Flow APISuccess.APISuccess
createPenaltyRule merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  -- [M3] Validate penalty type vs required fields
  validatePenaltyTypeFields req.penaltyType req.fixedAmount req.percentage req.formulaExpression

  -- [m6] Validate non-negative grace period values
  when (req.gracePeriodCount < 0) $
    throwError (InvalidRequest "gracePeriodCount must be non-negative")
  when (req.gracePeriodWindowHours < 0) $
    throwError (InvalidRequest "gracePeriodWindowHours must be non-negative")

  now <- getCurrentTime
  newId <- generateGUID
  let rule =
        DPRule.PenaltyRule
          { id = newId,
            merchantId = merchant.id,
            merchantOperatingCityId = merchantOpCityId,
            name = req.name,
            triggerEvent = req.triggerEvent,
            conditionsJson = req.conditionsJson,
            penaltyType = req.penaltyType,
            fixedAmount = req.fixedAmount,
            percentage = req.percentage,
            formulaExpression = req.formulaExpression,
            currency = req.currency,
            gracePeriodCount = req.gracePeriodCount,
            gracePeriodWindowHours = req.gracePeriodWindowHours,
            priority = req.priority,
            isActive = req.isActive,
            startDate = req.startDate,
            endDate = req.endDate,
            createdAt = now,
            updatedAt = now
          }
  QPenaltyRule.create rule
  logInfo $ "Created penalty rule: " <> rule.name <> " (id=" <> newId.getId <> ")"
  pure APISuccess.Success

updatePenaltyRule ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DPRule.PenaltyRule ->
  UpdatePenaltyRuleReq ->
  Flow APISuccess.APISuccess
updatePenaltyRule merchantShortId _opCity ruleId req = do
  merchant <- findMerchantByShortId merchantShortId
  existingRule <- QPenaltyRule.findById ruleId >>= fromMaybeM (InvalidRequest "Penalty rule not found")

  -- [M5] Verify the rule belongs to the requesting merchant
  unless (existingRule.merchantId == merchant.id) $
    throwError (InvalidRequest "Penalty rule does not belong to this merchant")

  now <- getCurrentTime
  -- [M4] Use Maybe (Maybe a) pattern so fields can be explicitly cleared
  let updatedRule =
        existingRule
          { DPRule.name = fromMaybe existingRule.name req.name,
            DPRule.conditionsJson = fromMaybe existingRule.conditionsJson req.conditionsJson,
            DPRule.penaltyType = fromMaybe existingRule.penaltyType req.penaltyType,
            DPRule.fixedAmount = maybe existingRule.fixedAmount id req.fixedAmount,
            DPRule.percentage = maybe existingRule.percentage id req.percentage,
            DPRule.formulaExpression = maybe existingRule.formulaExpression id req.formulaExpression,
            DPRule.gracePeriodCount = fromMaybe existingRule.gracePeriodCount req.gracePeriodCount,
            DPRule.gracePeriodWindowHours = fromMaybe existingRule.gracePeriodWindowHours req.gracePeriodWindowHours,
            DPRule.priority = fromMaybe existingRule.priority req.priority,
            DPRule.startDate = maybe existingRule.startDate id req.startDate,
            DPRule.endDate = maybe existingRule.endDate id req.endDate,
            DPRule.updatedAt = now
          }

  -- [M3] Validate the final penalty type vs fields
  validatePenaltyTypeFields updatedRule.penaltyType updatedRule.fixedAmount updatedRule.percentage updatedRule.formulaExpression

  QPenaltyRule.updateByPrimaryKey updatedRule
  logInfo $ "Updated penalty rule: " <> ruleId.getId
  pure APISuccess.Success

togglePenaltyRule ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DPRule.PenaltyRule ->
  TogglePenaltyRuleReq ->
  Flow APISuccess.APISuccess
togglePenaltyRule merchantShortId _opCity ruleId req = do
  merchant <- findMerchantByShortId merchantShortId
  existingRule <- QPenaltyRule.findById ruleId >>= fromMaybeM (InvalidRequest "Penalty rule not found")

  -- [M6] Verify the rule belongs to the requesting merchant
  unless (existingRule.merchantId == merchant.id) $
    throwError (InvalidRequest "Penalty rule does not belong to this merchant")

  now <- getCurrentTime
  QPenaltyRule.updateIsActiveById req.isActive now ruleId
  logInfo $ "Toggled penalty rule: " <> ruleId.getId <> " isActive=" <> show req.isActive
  pure APISuccess.Success

listPenalties ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe DPR.PenaltyStatus ->
  Maybe Int ->
  Maybe Int ->
  Flow PenaltyListRes
listPenalties merchantShortId _opCity mbStatus mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  -- [M9] Use paginated query instead of loading all records
  records <- QPenaltyRecord.findAllByMerchantIdWithLimitOffset merchant.id mbStatus mbLimit mbOffset
  items <- mapM enrichWithRuleName records
  pure $ PenaltyListRes {penalties = items, totalCount = length items}
  where
    enrichWithRuleName :: DPR.PenaltyRecord -> Flow PenaltyRecordItem
    enrichWithRuleName record = do
      mbRule <- QPenaltyRule.findById record.ruleId
      pure $ PenaltyRecordItem {penaltyRecord = record, ruleName = (.name) <$> mbRule}

reviewDispute ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DPR.PenaltyRecord ->
  Text ->
  ReviewDisputeReq ->
  Flow APISuccess.APISuccess
reviewDispute merchantShortId _opCity penaltyId requestorId req = do
  merchant <- findMerchantByShortId merchantShortId
  penalty <- QPenaltyRecord.findById penaltyId >>= fromMaybeM (InvalidRequest "Penalty record not found")

  -- [M7] Verify the penalty record belongs to the requesting merchant
  unless (penalty.merchantId == merchant.id) $
    throwError (InvalidRequest "Penalty record does not belong to this merchant")

  unless (penalty.status == DPR.DISPUTED) $
    throwError (InvalidRequest "Only disputed penalties can be reviewed")
  now <- getCurrentTime
  let newStatus = case req.action of
        WAIVE -> DPR.WAIVED
        REJECT -> DPR.REJECTED
  -- [m9 fix] Store resolverNote along with the dispute resolution
  QPenaltyRecordExtra.updateDisputeResolutionWithNoteById newStatus (Just requestorId) (Just now) req.resolverNote now penaltyId
  logInfo $
    "Dispute reviewed for penalty: " <> penaltyId.getId
      <> " action="
      <> show req.action
      <> " by "
      <> requestorId
      <> maybe "" (\note -> " note=" <> note) req.resolverNote
  pure APISuccess.Success

-- | [M8] Use per-status DB queries instead of loading all records into memory
getPenaltyAnalytics ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow PenaltyAnalyticsRes
getPenaltyAnalytics merchantShortId _opCity = do
  merchant <- findMerchantByShortId merchantShortId
  -- Count each status independently at DB level
  pendingCount <- QPenaltyRecord.countByMerchantIdAndStatus merchant.id DPR.PENDING
  appliedCount <- QPenaltyRecord.countByMerchantIdAndStatus merchant.id DPR.APPLIED
  disputedCount <- QPenaltyRecord.countByMerchantIdAndStatus merchant.id DPR.DISPUTED
  waivedCount <- QPenaltyRecord.countByMerchantIdAndStatus merchant.id DPR.WAIVED
  rejectedCount <- QPenaltyRecord.countByMerchantIdAndStatus merchant.id DPR.REJECTED
  totalPenaltyAmount <- QPenaltyRecord.sumAmountByMerchantId merchant.id
  let totalPenalties = pendingCount + appliedCount + disputedCount + waivedCount + rejectedCount
  pure $
    PenaltyAnalyticsRes
      { totalPenalties = totalPenalties,
        totalPenaltyAmount = totalPenaltyAmount,
        disputedCount = disputedCount,
        waivedCount = waivedCount,
        rejectedCount = rejectedCount,
        pendingCount = pendingCount,
        appliedCount = appliedCount
      }
