{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.FinancialAudit
  ( getFinancialAuditList,
    getFinancialAuditDetails,
    getFinancialAuditSummary,
    getFinancialAuditAdminActions,
    postFinancialAuditExport,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.FinancialAudit as API
import qualified Dashboard.Common
import Data.Aeson (Value, toJSON)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (addGregorianMonthsClip, fromGregorian)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime (..))
import qualified Domain.Types.Merchant as DM
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id (Id (..), ShortId (..))
import Kernel.Utils.Common (fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.AuditEntry as AuditEntry
import qualified Lib.Finance.Storage.Queries.AuditEntry as QAudit
import qualified Lib.Finance.Storage.Queries.AuditEntryExtra as QAuditExtra
import SharedLogic.Merchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

-- | List audit entries with filters and pagination
getFinancialAuditList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text -> -- entityType
  Maybe Text -> -- action
  Maybe Text -> -- actorType
  Maybe Text -> -- actorId
  Maybe Text -> -- entityId
  Maybe UTCTime -> -- dateFrom
  Maybe UTCTime -> -- dateTo
  Maybe Text -> -- search
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  Flow API.AuditEntryListRes
getFinancialAuditList merchantShortId opCity mbEntityType mbAction mbActorType _mbActorId mbEntityId mbDateFrom mbDateTo _mbSearch mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let limit = fromMaybe 20 mbLimit
      offset = fromMaybe 0 mbOffset
      merchantId = merchant.id.getId
  -- Parse action string to AuditAction if provided
  let mbParsedAction = mbAction >>= parseAuditAction

  -- If entityId is provided, search by entity directly
  entries <- case mbEntityId of
    Just entityId -> QAuditExtra.findByEntityIdSearch merchantId entityId
    Nothing ->
      QAuditExtra.findAllWithFilters
        merchantId
        mbEntityType
        mbParsedAction
        mbActorType
        mbDateFrom
        mbDateTo
        limit
        offset

  totalCount <-
    QAuditExtra.countWithFilters
      merchantId
      mbEntityType
      mbParsedAction
      mbActorType
      mbDateFrom
      mbDateTo

  let items = map toAuditEntryItem entries
      summary =
        Dashboard.Common.Summary
          { totalCount = totalCount,
            count = length items
          }
  pure $
    API.AuditEntryListRes
      { totalItems = totalCount,
        summary = summary,
        entries = items
      }

-- | Get audit entry details with full state diff
getFinancialAuditDetails ::
  ShortId DM.Merchant ->
  Context.City ->
  Text -> -- auditId
  Flow API.AuditDetailsRes
getFinancialAuditDetails merchantShortId _opCity auditId = do
  _merchant <- findMerchantByShortId merchantShortId
  mbEntry <- QAudit.findById (Id auditId)
  entry <- fromMaybeM (InvalidRequest "Audit entry not found") (pure mbEntry)
  let item = toAuditEntryItem entry
      -- Build a simple state diff representation
      stateDiff = buildStateDiff entry.previousState entry.newState
  pure $
    API.AuditDetailsRes
      { entry = item,
        stateDiff = stateDiff
      }

-- | Monthly compliance summary with aggregated counts
getFinancialAuditSummary ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int -> -- month
  Maybe Int -> -- year
  Flow API.AuditSummaryRes
getFinancialAuditSummary merchantShortId _opCity mbMonth mbYear = do
  merchant <- findMerchantByShortId merchantShortId
  now <- getCurrentTime
  let (currentYear, currentMonth, _) = toGregorian (utctDay now)
      month = fromMaybe (fromIntegral currentMonth) mbMonth
      year = fromMaybe (fromIntegral currentYear) mbYear
      startDate = UTCTime (fromGregorian (fromIntegral year) month 1) 0
      endDate = UTCTime (addGregorianMonthsClip 1 (fromGregorian (fromIntegral year) month 1)) 0
      merchantId = merchant.id.getId

  entries <- QAuditExtra.findByDateRange merchantId startDate endDate

  -- Aggregate by entity type
  let byEntityType = aggregateBy (.entityType) entries
      byAction = aggregateBy (show . (.action)) entries
      byActorType = aggregateBy (.actorType) entries

  pure $
    API.AuditSummaryRes
      { totalEntries = length entries,
        month = month,
        year = year,
        byEntityType = map (\(k, v) -> API.AuditSummaryByType k v) byEntityType,
        byAction = map (\(k, v) -> API.AuditSummaryByAction k v) byAction,
        byActorType = map (\(k, v) -> API.AuditSummaryByActor k v) byActorType
      }

-- | List admin-specific audit actions
getFinancialAuditAdminActions ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime -> -- dateFrom
  Maybe UTCTime -> -- dateTo
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  Flow API.AuditEntryListRes
getFinancialAuditAdminActions merchantShortId _opCity mbDateFrom mbDateTo mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  let limit = fromMaybe 20 mbLimit
      offset = fromMaybe 0 mbOffset
      merchantId = merchant.id.getId

  entries <- QAuditExtra.findAdminActions merchantId mbDateFrom mbDateTo limit offset

  -- Count admin actions for total
  totalCount <-
    QAuditExtra.countWithFilters
      merchantId
      Nothing
      Nothing
      (Just "ADMIN")
      mbDateFrom
      mbDateTo

  let items = map toAuditEntryItem entries
      summary =
        Dashboard.Common.Summary
          { totalCount = totalCount,
            count = length items
          }
  pure $
    API.AuditEntryListRes
      { totalItems = totalCount,
        summary = summary,
        entries = items
      }

-- | Export audit log as CSV
postFinancialAuditExport ::
  ShortId DM.Merchant ->
  Context.City ->
  API.AuditExportReq ->
  Flow API.AuditExportRes
postFinancialAuditExport merchantShortId _opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  let merchantId = merchant.id.getId
      mbParsedAction = req.action >>= parseAuditAction

  entries <-
    QAuditExtra.findAllWithFilters
      merchantId
      req.entityType
      mbParsedAction
      req.actorType
      req.dateFrom
      req.dateTo
      10000 -- max export size
      0

  let csvContent = generateCsv entries
      fileName = "audit-export-" <> merchantId <> ".csv"

  pure $
    API.AuditExportRes
      { content = csvContent,
        contentType = "text/csv",
        fileName = fileName
      }

-- ==========================================
-- Helper functions
-- ==========================================

-- | Convert domain AuditEntry to API AuditEntryItem
toAuditEntryItem :: AuditEntry.AuditEntry -> API.AuditEntryItem
toAuditEntryItem entry =
  API.AuditEntryItem
    { auditId = entry.id.getId,
      entityType = entry.entityType,
      entityId = entry.entityId,
      action = show entry.action,
      actorType = entry.actorType,
      actorId = entry.actorId,
      previousState = entry.previousState,
      newState = entry.newState,
      metadata = entry.metadata,
      ipAddress = entry.ipAddress,
      hashChain = entry.hashChain,
      merchantId = entry.merchantId,
      createdAt = entry.createdAt
    }

-- | Parse an action string to AuditAction
parseAuditAction :: Text -> Maybe AuditEntry.AuditAction
parseAuditAction "Created" = Just AuditEntry.Created
parseAuditAction "Updated" = Just AuditEntry.Updated
parseAuditAction "Reversed" = Just AuditEntry.Reversed
parseAuditAction "StatusChanged" = Just AuditEntry.StatusChanged
parseAuditAction _ = Nothing

-- | Build a simple state diff object from before/after states
buildStateDiff :: Maybe Value -> Maybe Value -> Maybe Value
buildStateDiff mbBefore mbAfter =
  case (mbBefore, mbAfter) of
    (Nothing, Nothing) -> Nothing
    _ ->
      Just $
        toJSON $
          Map.fromList
            [ ("previousState" :: Text, mbBefore),
              ("newState", mbAfter)
            ]

-- | Aggregate entries by a key extraction function
aggregateBy :: (AuditEntry.AuditEntry -> Text) -> [AuditEntry.AuditEntry] -> [(Text, Int)]
aggregateBy keyFn entries =
  let grouped = foldr (\e acc -> Map.insertWith (+) (keyFn e) (1 :: Int) acc) Map.empty entries
   in Map.toList grouped

-- | Generate CSV content from audit entries
generateCsv :: [AuditEntry.AuditEntry] -> Text
generateCsv entries =
  let header = "Audit ID,Entity Type,Entity ID,Action,Actor Type,Actor ID,IP Address,Created At"
      rows = map entryToCsvRow entries
   in T.intercalate "\n" (header : rows)

-- | Convert a single audit entry to a CSV row
entryToCsvRow :: AuditEntry.AuditEntry -> Text
entryToCsvRow entry =
  T.intercalate
    ","
    [ entry.id.getId,
      entry.entityType,
      entry.entityId,
      show entry.action,
      entry.actorType,
      fromMaybe "" entry.actorId,
      fromMaybe "" entry.ipAddress,
      show entry.createdAt
    ]
