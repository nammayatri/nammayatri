{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.TollManagement
  ( listTollBooths,
    createTollBooth,
    updateTollBooth,
    deactivateTollBooth,
    listTollRates,
    createTollRate,
    updateTollRate,
    listTollReimbursements,
    reviewTollClaim,
    getTollAnalytics,
    bulkImportTollBooths,
    CreateTollBoothReq (..),
    UpdateTollBoothReq (..),
    CreateTollRateReq (..),
    UpdateTollRateReq (..),
    ReviewTollClaimReq (..),
    TollBoothListRes (..),
    TollRateListRes (..),
    TollReimbursementListRes (..),
    TollAnalyticsRes (..),
    BulkImportTollBoothReq (..),
    BulkImportTollBoothEntry (..),
    BulkImportTollBoothRes (..),
    ReviewAction (..),
  )
where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.TollBooth as DTB
import qualified Domain.Types.TollRate as DTR
import qualified Domain.Types.TollReimbursement as DTRB
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (UTCTime)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common (Currency (..), HighPrecMoney)
import Kernel.Types.Error
import Kernel.Types.Id (Id (..), ShortId)
import Kernel.Utils.Common (generateGUID, getCurrentTime, logInfo)
import Kernel.Utils.Error (fromMaybeM)
import qualified SharedLogic.Merchant as SMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.TollBooth as QTollBooth
import qualified Storage.Queries.TollRate as QTollRate
import qualified Storage.Queries.TollReimbursement as QTollReimbursement

-- Request/Response types

data CreateTollBoothReq = CreateTollBoothReq
  { name :: Text,
    latitude :: Double,
    longitude :: Double,
    geofenceRadiusMeters :: Int,
    tollType :: DTB.TollType,
    highwayName :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data UpdateTollBoothReq = UpdateTollBoothReq
  { name :: Maybe Text,
    latitude :: Maybe Double,
    longitude :: Maybe Double,
    geofenceRadiusMeters :: Maybe Int,
    tollType :: Maybe DTB.TollType,
    highwayName :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data CreateTollRateReq = CreateTollRateReq
  { tollBoothId :: Id DTB.TollBooth,
    vehicleCategory :: Text,
    amount :: HighPrecMoney,
    currency :: Currency,
    effectiveFrom :: UTCTime,
    effectiveTo :: Maybe UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data UpdateTollRateReq = UpdateTollRateReq
  { amount :: Maybe HighPrecMoney,
    currency :: Maybe Currency,
    effectiveFrom :: Maybe UTCTime,
    effectiveTo :: Maybe UTCTime,
    isActive :: Maybe Bool
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data ReviewAction = APPROVE | REJECT
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data ReviewTollClaimReq = ReviewTollClaimReq
  { action :: ReviewAction,
    reviewerName :: Text,
    reason :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data TollBoothListRes = TollBoothListRes
  { tollBooths :: [DTB.TollBooth],
    totalCount :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data TollRateListRes = TollRateListRes
  { tollRates :: [DTR.TollRate],
    totalCount :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data TollReimbursementListRes = TollReimbursementListRes
  { reimbursements :: [DTRB.TollReimbursement],
    totalCount :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data TollAnalyticsRes = TollAnalyticsRes
  { totalReimbursements :: Int,
    totalAmount :: HighPrecMoney,
    detectedCount :: Int,
    creditedCount :: Int,
    disputedCount :: Int,
    rejectedCount :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data BulkImportTollBoothEntry = BulkImportTollBoothEntry
  { name :: Text,
    latitude :: Double,
    longitude :: Double,
    geofenceRadiusMeters :: Int,
    tollType :: DTB.TollType,
    highwayName :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data BulkImportTollBoothReq = BulkImportTollBoothReq
  { entries :: [BulkImportTollBoothEntry]
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data BulkImportTollBoothRes = BulkImportTollBoothRes
  { imported :: Int,
    failed :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- Handlers

listTollBooths ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow TollBoothListRes
listTollBooths merchantShortId opCity = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  tollBooths <- QTollBooth.findAllByMerchantOperatingCityId merchantOpCityId
  pure $ TollBoothListRes {tollBooths, totalCount = length tollBooths}

createTollBooth ::
  ShortId DM.Merchant ->
  Context.City ->
  CreateTollBoothReq ->
  Flow DTB.TollBooth
createTollBooth merchantShortId opCity req = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  now <- getCurrentTime
  tollBoothId <- generateGUID
  let tollBooth =
        DTB.TollBooth
          { id = Id tollBoothId,
            name = req.name,
            latitude = req.latitude,
            longitude = req.longitude,
            geofenceRadiusMeters = req.geofenceRadiusMeters,
            tollType = req.tollType,
            highwayName = req.highwayName,
            merchantOperatingCityId = merchantOpCityId,
            isActive = True,
            createdAt = now,
            updatedAt = now
          }
  QTollBooth.create tollBooth
  logInfo $ "Created toll booth: " <> tollBoothId
  pure tollBooth

updateTollBooth ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DTB.TollBooth ->
  UpdateTollBoothReq ->
  Flow DTB.TollBooth
updateTollBooth merchantShortId opCity tollBoothId req = do
  _merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing _merchant (Just opCity)
  now <- getCurrentTime
  existingBooth <- QTollBooth.findById tollBoothId >>= fromMaybeM (InvalidRequest "Toll booth not found")
  let updatedBooth =
        existingBooth
          { DTB.name = fromMaybe existingBooth.name req.name,
            DTB.latitude = fromMaybe existingBooth.latitude req.latitude,
            DTB.longitude = fromMaybe existingBooth.longitude req.longitude,
            DTB.geofenceRadiusMeters = fromMaybe existingBooth.geofenceRadiusMeters req.geofenceRadiusMeters,
            DTB.tollType = fromMaybe existingBooth.tollType req.tollType,
            DTB.highwayName = req.highwayName <|> existingBooth.highwayName,
            DTB.updatedAt = now
          }
  QTollBooth.updateByPrimaryKey updatedBooth
  logInfo $ "Updated toll booth: " <> show tollBoothId
  pure updatedBooth

deactivateTollBooth ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DTB.TollBooth ->
  Flow ()
deactivateTollBooth merchantShortId opCity tollBoothId = do
  _merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing _merchant (Just opCity)
  now <- getCurrentTime
  _ <- QTollBooth.findById tollBoothId >>= fromMaybeM (InvalidRequest "Toll booth not found")
  QTollBooth.updateIsActiveById False now tollBoothId
  logInfo $ "Deactivated toll booth: " <> show tollBoothId

listTollRates ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DTB.TollBooth ->
  Flow TollRateListRes
listTollRates merchantShortId opCity tollBoothId = do
  _merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing _merchant (Just opCity)
  _ <- QTollBooth.findById tollBoothId >>= fromMaybeM (InvalidRequest "Toll booth not found")
  tollRates <- QTollRate.findAllByTollBoothId tollBoothId
  pure $ TollRateListRes {tollRates, totalCount = length tollRates}

createTollRate ::
  ShortId DM.Merchant ->
  Context.City ->
  CreateTollRateReq ->
  Flow DTR.TollRate
createTollRate merchantShortId opCity req = do
  _merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing _merchant (Just opCity)
  _ <- QTollBooth.findById req.tollBoothId >>= fromMaybeM (InvalidRequest "Toll booth not found")
  now <- getCurrentTime
  tollRateId <- generateGUID
  let tollRate =
        DTR.TollRate
          { id = Id tollRateId,
            tollBoothId = req.tollBoothId,
            vehicleCategory = req.vehicleCategory,
            amount = req.amount,
            currency = req.currency,
            effectiveFrom = req.effectiveFrom,
            effectiveTo = req.effectiveTo,
            isActive = True,
            createdAt = now,
            updatedAt = now
          }
  QTollRate.create tollRate
  logInfo $ "Created toll rate: " <> tollRateId
  pure tollRate

updateTollRate ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DTR.TollRate ->
  UpdateTollRateReq ->
  Flow DTR.TollRate
updateTollRate merchantShortId opCity tollRateId req = do
  _merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing _merchant (Just opCity)
  now <- getCurrentTime
  existingRate <- QTollRate.findById tollRateId >>= fromMaybeM (InvalidRequest "Toll rate not found")
  let updatedRate =
        existingRate
          { DTR.amount = fromMaybe existingRate.amount req.amount,
            DTR.currency = fromMaybe existingRate.currency req.currency,
            DTR.effectiveFrom = fromMaybe existingRate.effectiveFrom req.effectiveFrom,
            DTR.effectiveTo = req.effectiveTo <|> existingRate.effectiveTo,
            DTR.isActive = fromMaybe existingRate.isActive req.isActive,
            DTR.updatedAt = now
          }
  QTollRate.updateByPrimaryKey updatedRate
  logInfo $ "Updated toll rate: " <> show tollRateId
  pure updatedRate

listTollReimbursements ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe DTRB.TollReimbursementStatus ->
  Flow TollReimbursementListRes
listTollReimbursements merchantShortId opCity mbStatus = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  reimbursements <- case mbStatus of
    Just status -> QTollReimbursement.findAllByMerchantIdAndStatus merchant.id status
    Nothing -> QTollReimbursement.findAllByMerchantIdAndStatus merchant.id DTRB.DETECTED
  pure $ TollReimbursementListRes {reimbursements, totalCount = length reimbursements}

reviewTollClaim ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DTRB.TollReimbursement ->
  ReviewTollClaimReq ->
  Flow ()
reviewTollClaim merchantShortId opCity reimbursementId req = do
  _merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing _merchant (Just opCity)
  now <- getCurrentTime
  _ <- QTollReimbursement.findById reimbursementId >>= fromMaybeM (InvalidRequest "Toll reimbursement not found")
  let newStatus = case req.action of
        APPROVE -> DTRB.CREDITED
        REJECT -> DTRB.REJECTED
  QTollReimbursement.updateReviewById newStatus (Just req.reviewerName) (Just now) now reimbursementId
  logInfo $ "Reviewed toll claim " <> show reimbursementId <> " with action: " <> show req.action

getTollAnalytics ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow TollAnalyticsRes
getTollAnalytics merchantShortId opCity = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  detected <- QTollReimbursement.findAllByMerchantIdAndStatus merchant.id DTRB.DETECTED
  credited <- QTollReimbursement.findAllByMerchantIdAndStatus merchant.id DTRB.CREDITED
  disputed <- QTollReimbursement.findAllByMerchantIdAndStatus merchant.id DTRB.DISPUTED
  rejected <- QTollReimbursement.findAllByMerchantIdAndStatus merchant.id DTRB.REJECTED
  let allReimbursements = detected <> credited <> disputed <> rejected
      totalAmount = sum $ map (.amount) allReimbursements
  pure $
    TollAnalyticsRes
      { totalReimbursements = length allReimbursements,
        totalAmount,
        detectedCount = length detected,
        creditedCount = length credited,
        disputedCount = length disputed,
        rejectedCount = length rejected
      }

bulkImportTollBooths ::
  ShortId DM.Merchant ->
  Context.City ->
  BulkImportTollBoothReq ->
  Flow BulkImportTollBoothRes
bulkImportTollBooths merchantShortId opCity req = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  now <- getCurrentTime
  results <- forM req.entries $ \entry -> do
    tollBoothId <- generateGUID
    let tollBooth =
          DTB.TollBooth
            { id = Id tollBoothId,
              name = entry.name,
              latitude = entry.latitude,
              longitude = entry.longitude,
              geofenceRadiusMeters = entry.geofenceRadiusMeters,
              tollType = entry.tollType,
              highwayName = entry.highwayName,
              merchantOperatingCityId = merchantOpCityId,
              isActive = True,
              createdAt = now,
              updatedAt = now
            }
    QTollBooth.create tollBooth
    pure True
  let imported = length $ filter id results
      failed = length results - imported
  logInfo $ "Bulk imported " <> show imported <> " toll booths, " <> show failed <> " failed"
  pure $ BulkImportTollBoothRes {imported, failed}
