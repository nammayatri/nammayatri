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
import Kernel.Utils.Error (throwError)
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
    fixedAmount :: Maybe HighPrecMoney,
    percentage :: Maybe Double,
    formulaExpression :: Maybe Text,
    gracePeriodCount :: Maybe Int,
    gracePeriodWindowHours :: Maybe Int,
    priority :: Maybe Int,
    startDate :: Maybe UTCTime,
    endDate :: Maybe UTCTime
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
  Flow PenaltyRuleListRes
listPenaltyRules merchantShortId opCity mbTriggerEvent = do
  merchant <- findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  rules <- case mbTriggerEvent of
    Just triggerEvent ->
      QPenaltyRule.findAllByMerchantIdAndTriggerEvent merchant.id triggerEvent True
    Nothing ->
      QPenaltyRule.findAllByMerchantId merchant.id
  pure $ PenaltyRuleListRes {rules = rules, totalCount = length rules}

createPenaltyRule ::
  ShortId DM.Merchant ->
  Context.City ->
  CreatePenaltyRuleReq ->
  Flow APISuccess.APISuccess
createPenaltyRule merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
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
  _merchant <- findMerchantByShortId merchantShortId
  existingRule <- QPenaltyRule.findById ruleId >>= fromMaybeM (InvalidRequest "Penalty rule not found")
  now <- getCurrentTime
  let updatedRule =
        existingRule
          { DPRule.name = fromMaybe existingRule.name req.name,
            DPRule.conditionsJson = fromMaybe existingRule.conditionsJson req.conditionsJson,
            DPRule.penaltyType = fromMaybe existingRule.penaltyType req.penaltyType,
            DPRule.fixedAmount = req.fixedAmount <|> existingRule.fixedAmount,
            DPRule.percentage = req.percentage <|> existingRule.percentage,
            DPRule.formulaExpression = req.formulaExpression <|> existingRule.formulaExpression,
            DPRule.gracePeriodCount = fromMaybe existingRule.gracePeriodCount req.gracePeriodCount,
            DPRule.gracePeriodWindowHours = fromMaybe existingRule.gracePeriodWindowHours req.gracePeriodWindowHours,
            DPRule.priority = fromMaybe existingRule.priority req.priority,
            DPRule.startDate = req.startDate <|> existingRule.startDate,
            DPRule.endDate = req.endDate <|> existingRule.endDate,
            DPRule.updatedAt = now
          }
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
  _merchant <- findMerchantByShortId merchantShortId
  _existingRule <- QPenaltyRule.findById ruleId >>= fromMaybeM (InvalidRequest "Penalty rule not found")
  now <- getCurrentTime
  QPenaltyRule.updateIsActiveById req.isActive now ruleId
  logInfo $ "Toggled penalty rule: " <> ruleId.getId <> " isActive=" <> show req.isActive
  pure APISuccess.Success

listPenalties ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe DPR.PenaltyStatus ->
  Flow PenaltyListRes
listPenalties merchantShortId _opCity mbStatus = do
  merchant <- findMerchantByShortId merchantShortId
  records <- case mbStatus of
    Just status ->
      QPenaltyRecord.findAllByMerchantIdAndStatus merchant.id status
    Nothing ->
      QPenaltyRecord.findAllByMerchantId merchant.id
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
  _merchant <- findMerchantByShortId merchantShortId
  penalty <- QPenaltyRecord.findById penaltyId >>= fromMaybeM (InvalidRequest "Penalty record not found")
  unless (penalty.status == DPR.DISPUTED) $
    throwError (InvalidRequest "Only disputed penalties can be reviewed")
  now <- getCurrentTime
  let newStatus = case req.action of
        WAIVE -> DPR.WAIVED
        REJECT -> DPR.REJECTED
  QPenaltyRecord.updateDisputeResolutionById newStatus (Just requestorId) (Just now) now penaltyId
  logInfo $
    "Dispute reviewed for penalty: " <> penaltyId.getId
      <> " action="
      <> show req.action
      <> " by "
      <> requestorId
  pure APISuccess.Success

getPenaltyAnalytics ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow PenaltyAnalyticsRes
getPenaltyAnalytics merchantShortId _opCity = do
  merchant <- findMerchantByShortId merchantShortId
  allRecords <- QPenaltyRecord.findAllByMerchantId merchant.id
  let totalPenalties = length allRecords
      totalPenaltyAmount = sum $ map (.amount) allRecords
      disputedCount = length $ filter (\r -> r.status == DPR.DISPUTED) allRecords
      waivedCount = length $ filter (\r -> r.status == DPR.WAIVED) allRecords
      rejectedCount = length $ filter (\r -> r.status == DPR.REJECTED) allRecords
      pendingCount = length $ filter (\r -> r.status == DPR.PENDING) allRecords
      appliedCount = length $ filter (\r -> r.status == DPR.APPLIED) allRecords
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
