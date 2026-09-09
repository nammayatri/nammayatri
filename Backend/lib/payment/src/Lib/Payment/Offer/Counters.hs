module Lib.Payment.Offer.Counters
  ( OfferRiderContext (..),
    countedEntityTypes,
    upsertOfferFrequencyStats,
    countUses,
    isUsedUp,
    isCurrent,
    bumpCount,
    currentCount,
    applyToRow,
    countFromRow,
  )
where

import qualified Data.Text as T
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.Offer as DOffer
import qualified Lib.Payment.Domain.Types.OfferFrequencyStatsHistory as DHistory
import qualified Lib.Payment.Domain.Types.OfferStats as DOfferStats
import qualified Lib.Payment.Domain.Types.PersonOfferFrequencyStats as DStats
import qualified Lib.Payment.Offer.Frequency as Frequency
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.Offer as QOffer
import qualified Lib.Payment.Storage.Queries.OfferFrequencyStatsHistory as QHistory
import qualified Lib.Payment.Storage.Queries.OfferStats as QOfferStats
import qualified Lib.Payment.Storage.Queries.PersonOfferFrequencyStats as QStats

data OfferRiderContext = OfferRiderContext
  { personId :: Text,
    deviceId :: Maybe Text,
    timeDiffFromUtc :: Seconds
  }
  deriving (Show)

countedEntityTypes :: [DOfferStats.OfferStatsEntityType]
countedEntityTypes = [DOfferStats.Person, DOfferStats.Device]

-- | A stored start at or after this instant's period start is current, so a late write never moves a period backwards.
isCurrent :: Frequency.Window -> Maybe UTCTime -> Bool
isCurrent window = maybe False (>= window.windowStart)

bumpCount :: Frequency.Window -> Int -> Maybe UTCTime -> Int
bumpCount window count mbStart = if isCurrent window mbStart then count + 1 else 1

currentCount :: Frequency.Window -> Int -> Maybe UTCTime -> Int
currentCount window count mbStart = if isCurrent window mbStart then count else 0

applyToRow :: Frequency.Window -> UTCTime -> HighPrecMoney -> HighPrecMoney -> DStats.PersonOfferFrequencyStats -> DStats.PersonOfferFrequencyStats
applyToRow window now discount cashback row =
  let continues = isCurrent window row.periodStart
      carry amount = if continues then amount else 0
   in row{DStats.appliedCount = bumpCount window row.appliedCount row.periodStart,
          DStats.periodStart = if continues then row.periodStart else Just window.windowStart,
          DStats.totalDiscountAmount = carry row.totalDiscountAmount + discount,
          DStats.totalCashbackAmount = carry row.totalCashbackAmount + cashback,
          DStats.updatedAt = now
         }

countFromRow :: Frequency.Window -> Maybe DStats.PersonOfferFrequencyStats -> Int
countFromRow _ Nothing = 0
countFromRow window (Just row) = currentCount window row.appliedCount row.periodStart

-- | One apply for the rider's account and phone: bumps the period row and the history row under a waiting lock.
upsertOfferFrequencyStats ::
  (PaymentBeamFlow.BeamFlow m r) =>
  Id DOffer.Offer ->
  [(DOfferStats.OfferStatsEntityType, Text)] ->
  Seconds ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Currency ->
  Text ->
  Text ->
  UTCTime ->
  m ()
upsertOfferFrequencyStats offerId entities timeDiffFromUtc discountAmount payoutAmount currency merchantId merchantOperatingCityId now = do
  mbOffer <- QOffer.findById offerId
  case mbOffer of
    Nothing -> logDebug $ "Offer usage counter: no offer row (gateway offer), nothing counted: " <> offerId.getId
    Just offer -> whenJust offer.frequencyType $ \frequency -> do
      let window = Frequency.windowOf frequency timeDiffFromUtc now
      forM_ (filter ((`elem` countedEntityTypes) . fst) entities) $ \(entityType, entityId) -> do
        let lockKey = "UpsertOfferStats:Frequency:" <> show entityType <> ":" <> entityId <> ":OfferId:" <> offerId.getId
        Redis.withWaitOnLockRedisWithExpiry lockKey 10 20 $ do
          upsertCurrent window entityType entityId
          upsertHistory frequency window entityType entityId
  where
    discount = fromMaybe 0.0 discountAmount
    cashback = fromMaybe 0.0 payoutAmount

    upsertCurrent window entityType entityId = do
      mbRow <- QStats.findByEntityIdOfferIdEntityType entityId offerId entityType
      case mbRow of
        Just row -> QStats.updateByPrimaryKey (applyToRow window now discount cashback row)
        Nothing -> do
          rowId <- generateGUID
          QStats.create
            DStats.PersonOfferFrequencyStats
              { id = rowId,
                entityId,
                entityType,
                offerId,
                appliedCount = 1,
                periodStart = Just window.windowStart,
                totalDiscountAmount = discount,
                totalCashbackAmount = cashback,
                currency,
                merchantId,
                merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }

    upsertHistory frequency window entityType entityId = do
      mbRow <- QHistory.findByEntityIdEntityTypeOfferIdFrequencyTypeAndPeriodStart entityId entityType offerId frequency window.windowStart
      case mbRow of
        Just row ->
          QHistory.updateByPrimaryKey
            row{DHistory.appliedCount = row.appliedCount + 1,
                DHistory.totalDiscountAmount = row.totalDiscountAmount + discount,
                DHistory.totalCashbackAmount = row.totalCashbackAmount + cashback,
                DHistory.updatedAt = now
               }
        Nothing -> do
          rowId <- generateGUID
          QHistory.create
            DHistory.OfferFrequencyStatsHistory
              { id = rowId,
                entityId,
                entityType,
                offerId,
                frequencyType = frequency,
                periodStart = window.windowStart,
                periodEnd = window.windowEnd,
                appliedCount = 1,
                totalDiscountAmount = discount,
                totalCashbackAmount = cashback,
                currency,
                merchantId,
                merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }

-- | Uses in the offer's current period (ever, without a frequency): the larger of the account and phone rows, never the sum.
countUses ::
  (PaymentBeamFlow.BeamFlow m r) =>
  OfferRiderContext ->
  UTCTime ->
  Id DOffer.Offer ->
  Maybe DOffer.OfferFrequency ->
  m Int
countUses rider now offerId = \case
  Nothing -> largest <$> forM entities lifetimeCount
  Just frequency -> do
    let window = Frequency.windowOf frequency rider.timeDiffFromUtc now
    largest <$> forM entities (periodCount window)
  where
    entities = (DOfferStats.Person, rider.personId) : [(DOfferStats.Device, deviceId) | Just deviceId <- [rider.deviceId], not (T.null deviceId)]

    lifetimeCount (entityType, entityId) =
      maybe 0 (.offerAppliedCount) <$> QOfferStats.findByOfferIdAndEntityIdAndEntityType offerId entityId entityType

    periodCount window (entityType, entityId) =
      countFromRow window <$> QStats.findByEntityIdOfferIdEntityType entityId offerId entityType

    largest :: [Int] -> Int
    largest = maximum . (0 :)

isUsedUp :: Maybe Int -> Int -> Bool
isUsedUp mbMaxApplyCount uses = maybe False (uses >=) mbMaxApplyCount
