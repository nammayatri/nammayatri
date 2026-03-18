{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverPenalty
  ( listMyPenalties,
    getPenaltyDetails,
    disputePenalty,
    getGracePeriodStatus,
    DriverPenaltyListRes (..),
    DriverPenaltyItem (..),
    DriverPenaltyDetailRes (..),
    DisputePenaltyReq (..),
    GracePeriodStatusRes (..),
    GracePeriodItem (..),
  )
where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.DriverGracePeriodTracker as DGPT
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PenaltyRecord as DPR
import qualified Domain.Types.PenaltyRule as DPRule
import qualified Domain.Types.Person as DP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, getCurrentTime)
import Kernel.Utils.Error (throwError)
import Kernel.Utils.Logging (logInfo)
import Tools.Error (GenericError (InvalidRequest))
import qualified Storage.Queries.DriverGracePeriodTracker as QGraceTracker
import qualified Storage.Queries.PenaltyRecord as QPenaltyRecord
import qualified Storage.Queries.PenaltyRule as QPenaltyRule
import qualified Storage.Queries.Person as QPerson

-- -----------------------------------------------------------------
-- Types
-- -----------------------------------------------------------------

data DriverPenaltyItem = DriverPenaltyItem
  { penaltyId :: Id DPR.PenaltyRecord,
    triggerEvent :: DPRule.PenaltyTriggerEvent,
    amount :: HighPrecMoney,
    currency :: Currency,
    reason :: Text,
    status :: DPR.PenaltyStatus,
    ruleName :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverPenaltyListRes = DriverPenaltyListRes
  { penalties :: [DriverPenaltyItem],
    totalCount :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverPenaltyDetailRes = DriverPenaltyDetailRes
  { penaltyRecord :: DPR.PenaltyRecord,
    ruleName :: Maybe Text,
    ruleDescription :: Maybe Text,
    triggerEventType :: Maybe DPRule.PenaltyTriggerEvent
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DisputePenaltyReq = DisputePenaltyReq
  { reason :: Text,
    evidence :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data GracePeriodItem = GracePeriodItem
  { ruleId :: Id DPRule.PenaltyRule,
    ruleName :: Maybe Text,
    triggerEvent :: Maybe DPRule.PenaltyTriggerEvent,
    offenseCount :: Int,
    gracePeriodCount :: Maybe Int,
    windowStartTime :: UTCTime,
    windowEndTime :: UTCTime,
    isWindowExpired :: Bool,
    remainingGrace :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data GracePeriodStatusRes = GracePeriodStatusRes
  { trackers :: [GracePeriodItem],
    totalCount :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- -----------------------------------------------------------------
-- Handlers
-- -----------------------------------------------------------------

-- | [M9] Properly implements pagination using limit/offset at DB level
listMyPenalties ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Maybe DPR.PenaltyStatus ->
  Maybe Int ->
  Maybe Int ->
  Flow DriverPenaltyListRes
listMyPenalties (mbDriverId, _merchantId, _merchantOpCityId) mbStatus mbLimit mbOffset = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  _driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  records <- QPenaltyRecord.findAllByDriverIdWithLimitOffset driverId mbStatus mbLimit mbOffset
  items <- mapM toDriverPenaltyItem records
  pure $ DriverPenaltyListRes {penalties = items, totalCount = length items}
  where
    toDriverPenaltyItem :: DPR.PenaltyRecord -> Flow DriverPenaltyItem
    toDriverPenaltyItem record = do
      mbRule <- QPenaltyRule.findById record.ruleId
      pure $
        DriverPenaltyItem
          { penaltyId = record.id,
            triggerEvent = record.triggerEvent,
            amount = record.amount,
            currency = record.currency,
            reason = record.reason,
            status = record.status,
            ruleName = (.name) <$> mbRule,
            createdAt = record.createdAt
          }

getPenaltyDetails ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Id DPR.PenaltyRecord ->
  Flow DriverPenaltyDetailRes
getPenaltyDetails (mbDriverId, _merchantId, _merchantOpCityId) penaltyId = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  _driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  penalty <- QPenaltyRecord.findById penaltyId >>= fromMaybeM (InvalidRequest "Penalty record not found")
  unless (penalty.driverId == driverId) $
    throwError (InvalidRequest "Penalty does not belong to this driver")
  mbRule <- QPenaltyRule.findById penalty.ruleId
  pure $
    DriverPenaltyDetailRes
      { penaltyRecord = penalty,
        ruleName = (.name) <$> mbRule,
        ruleDescription = Nothing,
        triggerEventType = (.triggerEvent) <$> mbRule
      }

disputePenalty ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Id DPR.PenaltyRecord ->
  DisputePenaltyReq ->
  Flow APISuccess.APISuccess
disputePenalty (mbDriverId, _merchantId, _merchantOpCityId) penaltyId req = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  _driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  penalty <- QPenaltyRecord.findById penaltyId >>= fromMaybeM (InvalidRequest "Penalty record not found")
  unless (penalty.driverId == driverId) $
    throwError (InvalidRequest "Penalty does not belong to this driver")
  unless (penalty.status == DPR.PENDING || penalty.status == DPR.APPLIED) $
    throwError (InvalidRequest "Penalty cannot be disputed in its current status")
  now <- getCurrentTime
  QPenaltyRecord.updateDisputeById DPR.DISPUTED (Just req.reason) req.evidence now penaltyId
  logInfo $ "Driver " <> driverId.getId <> " disputed penalty " <> penaltyId.getId
  pure APISuccess.Success

-- | [C2] Grace period status endpoint enriched with window expiry info
getGracePeriodStatus ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Flow GracePeriodStatusRes
getGracePeriodStatus (mbDriverId, _merchantId, _merchantOpCityId) = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  _driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  now <- getCurrentTime
  trackers <- QGraceTracker.findAllByDriverId driverId
  items <- mapM (toGracePeriodItem now) trackers
  pure $ GracePeriodStatusRes {trackers = items, totalCount = length items}
  where
    toGracePeriodItem :: UTCTime -> DGPT.DriverGracePeriodTracker -> Flow GracePeriodItem
    toGracePeriodItem now tracker = do
      mbRule <- QPenaltyRule.findById tracker.ruleId
      let isExpired = now > tracker.windowEndTime
          remaining = case mbRule of
            Just rule ->
              if isExpired
                then Just rule.gracePeriodCount
                else Just (max 0 (rule.gracePeriodCount - tracker.offenseCount))
            Nothing -> Nothing
      pure $
        GracePeriodItem
          { ruleId = tracker.ruleId,
            ruleName = (.name) <$> mbRule,
            triggerEvent = (.triggerEvent) <$> mbRule,
            offenseCount = tracker.offenseCount,
            gracePeriodCount = (.gracePeriodCount) <$> mbRule,
            windowStartTime = tracker.windowStartTime,
            windowEndTime = tracker.windowEndTime,
            isWindowExpired = isExpired,
            remainingGrace = remaining
          }
