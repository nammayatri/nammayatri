module SharedLogic.FareBreakupInfo
  ( getFareBreakupInfo,
    findFareBreakupItems,
    findFareBreakupsFromInfo,
    getFareBreakupsWithFallback,
    fareBreakupToItem,
    setFareBreakupInfoFromFareBreakups,
    addFareBreakupInfoItems,
    upsertFareBreakupInfo,
    tipFareBreakupTitle,
  )
where

import qualified Data.Map.Strict as Map
import qualified Domain.Types.FareBreakup as DFareBreakup
import Domain.Types.FareBreakupInfo
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FareBreakupInfo as QFareBreakupInfo

-- | Canonical description for the rider's tip line in a RIDE fare breakup. The BPP emits the
-- same amount over Beckn as BUYER_ADDITIONAL_AMOUNT; ride completion normalises it to this.
tipFareBreakupTitle :: Text
tipFareBreakupTitle = "RIDE_TIP"

getFareBreakupInfo ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  DFareBreakup.FareBreakupEntityType ->
  m (Maybe FareBreakupInfo)
getFareBreakupInfo = QFareBreakupInfo.findByEntityIdAndEntityType

findFareBreakupItems ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  DFareBreakup.FareBreakupEntityType ->
  m (Maybe [FareBreakupInfoItem])
findFareBreakupItems entityId entityType = fmap (.fareBreakups) <$> getFareBreakupInfo entityId entityType

findFareBreakupsFromInfo ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  DFareBreakup.FareBreakupEntityType ->
  m (Maybe [DFareBreakup.FareBreakup])
findFareBreakupsFromInfo entityId entityType =
  fmap (map (itemToFareBreakup entityId entityType)) <$> findFareBreakupItems entityId entityType

getFareBreakupsWithFallback ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  DFareBreakup.FareBreakupEntityType ->
  m [DFareBreakup.FareBreakup] ->
  m [DFareBreakup.FareBreakup]
getFareBreakupsWithFallback entityId entityType oldQuery = do
  mbFromInfo <- findFareBreakupsFromInfo entityId entityType
  maybe oldQuery pure mbFromInfo

fareBreakupToItem :: DFareBreakup.FareBreakup -> FareBreakupInfoItem
fareBreakupToItem fb =
  FareBreakupInfoItem
    { description = fb.description,
      amount = fb.amount.amount,
      currency = fb.amount.currency
    }

itemToFareBreakup :: Text -> DFareBreakup.FareBreakupEntityType -> FareBreakupInfoItem -> DFareBreakup.FareBreakup
itemToFareBreakup entityId entityType item =
  DFareBreakup.FareBreakup
    { id = Id entityId,
      entityId = entityId,
      entityType = entityType,
      description = item.description,
      amount = Kernel.Types.Common.mkPrice (Just item.currency) item.amount
    }

setFareBreakupInfoFromFareBreakups ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  [DFareBreakup.FareBreakup] ->
  m ()
setFareBreakupInfoFromFareBreakups mbMerchantId mbMocId fareBreakups = do
  let grouped = Map.toList $ Map.fromListWith (\new old -> old ++ new) [((fb.entityId, fb.entityType), [fareBreakupToItem fb]) | fb <- fareBreakups]
  forM_ grouped $ \((entityId, entityType), items) -> upsertFareBreakupInfo entityId entityType items mbMerchantId mbMocId

upsertFareBreakupInfo ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  DFareBreakup.FareBreakupEntityType ->
  [FareBreakupInfoItem] ->
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  m ()
upsertFareBreakupInfo entityId entityType items mbMerchantId mbMocId =
  Redis.withWaitAndLockMasterCloudCrossAppRedis (upsertLockKey entityId entityType) 10 100 $ do
    mbExisting <- QFareBreakupInfo.findByEntityIdAndEntityType entityId entityType
    case mbExisting of
      Just _ -> QFareBreakupInfo.updateFareBreakupsByEntityIdAndEntityType items entityId entityType
      Nothing -> createFareBreakupInfoRow entityId entityType items mbMerchantId mbMocId

addFareBreakupInfoItems ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  DFareBreakup.FareBreakupEntityType ->
  [FareBreakupInfoItem] ->
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  m ()
addFareBreakupInfoItems entityId entityType newItems mbMerchantId mbMocId =
  Redis.withWaitAndLockMasterCloudCrossAppRedis (upsertLockKey entityId entityType) 10 100 $ do
    mbExisting <- QFareBreakupInfo.findByEntityIdAndEntityType entityId entityType
    case mbExisting of
      Just existing -> QFareBreakupInfo.updateFareBreakupsByEntityIdAndEntityType (mergeItemsByDescription existing.fareBreakups newItems) entityId entityType
      Nothing -> createFareBreakupInfoRow entityId entityType newItems mbMerchantId mbMocId

createFareBreakupInfoRow ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  DFareBreakup.FareBreakupEntityType ->
  [FareBreakupInfoItem] ->
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  m ()
createFareBreakupInfoRow entityId entityType items mbMerchantId mbMocId = do
  now <- getCurrentTime
  newId <- generateGUID
  QFareBreakupInfo.create
    FareBreakupInfo
      { id = Id newId,
        entityId = entityId,
        entityType = entityType,
        fareBreakups = items,
        merchantId = mbMerchantId,
        merchantOperatingCityId = mbMocId,
        createdAt = now,
        updatedAt = now
      }

mergeItemsByDescription :: [FareBreakupInfoItem] -> [FareBreakupInfoItem] -> [FareBreakupInfoItem]
mergeItemsByDescription existing newItems =
  let newDescriptions = map (.description) newItems
   in filter (\item -> item.description `notElem` newDescriptions) existing ++ newItems

upsertLockKey :: Text -> DFareBreakup.FareBreakupEntityType -> Text
upsertLockKey entityId entityType = "FareBreakupInfo:Upsert:" <> entityId <> ":" <> show entityType
