{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Driver coins on the finance-kernel ledger.
--
--   A driver's coins are a Liability account (counterparty DRIVER, sub-ledger "COINS") in coin units; the
--   other side of every entry is one per-merchant Expense account (sub-ledger "COINS"), the coin budget.
--
--   * An earn is a LiabilityCreated entry into the driver's account carrying @expiresAt@ -- a lot
--     ("Lib.Finance.Lot"). What is left of it is its amount minus the entries that name it.
--   * Spending coins (redeem) and expiry are LiabilitySettled entries out of the account that name the lot
--     they take from (entityReferenceType = ExpiringLot), earliest expiry first.
--   * A penalty or reversal is a LiabilitySettled entry out of the account that names no lot. It is
--     held against the balance until its own @expiresAt@, if any.
--
--   The spendable balance is the account's balance less what has lapsed but is not booked yet
--   ('getSpendableCoins'), so lapsed coins stop counting the moment they lapse. The entry that books a lapse
--   is posted lazily ('sweepLapsedCoins', run from the balance read), so there is no expiry job.
--
--   Every writer must hold the driver's owner lock (Lib.Finance.withOwnerLock): this module reads the lots
--   and then posts against them.
module SharedLogic.Finance.CoinLedger
  ( coinsSubLedger,
    coinUnitCurrency,
    CoinFlow,
    CoinAttrs (..),
    CoinCredit (..),
    CoinDebit (..),
    CoinEvent (..),
    withCoinLock,
    postCoinEvent,
    findCoinAccount,
    ensureCoinAccount,
    postCoinCredit,
    postCoinDebit,
    spendCoins,
    sweepLapsedCoins,
    getSpendableCoins,
    coinCashSubLedger,
    getCoinCashLeft,
    creditCoinCash,
    debitCoinCash,
    findCoinLotsByEvent,
    getCoinsByEvent,
    coinActivity,
    coinsOfDay,
    coinsOfPreviousDay,
    coinHistoryPage,
    liveLotsExpiringBetween,
    countCoinsExpiringBetween,
    entryToCoinHistory,
  )
where

import qualified "this" API.Types.ProviderPlatform.Management.DriverCoins as DCoins
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (UTCTime, utctDay), addDays)
import qualified Domain.Types.Coins.CoinHistory as DTCC
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.DriverStats as DDS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleCategory as DTV
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT
import Lib.Finance
  ( Account (..),
    AccountInput (..),
    AccountType (Expense, Liability),
    CounterpartyType (DRIVER, SELLER),
    EntityReferenceType (ExpiringLot),
    EntryStatus (SETTLED),
    EntryType (LiabilityCreated, LiabilitySettled),
    LedgerEntry (..),
    LedgerEntryInput (..),
    SettlementStatus (UNSETTLED),
    createEntryWithBalanceUpdate,
    findAccountByCounterpartyAndType,
    getOrCreateAccount,
    markEntriesAsClaimed,
  )
import Lib.Finance.Domain.Types.Extra.LedgerEntry (LedgerEntryMetadata (..))
import qualified Lib.Finance.Domain.Types.LedgerEntry as DLE
import Lib.Finance.Lot (LotView (..), allocateInExpiryOrder, lapsedLots, openLots, partitionByExpiry)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerExtra
import qualified SharedLogic.Merchant as SMerchant
import qualified Storage.Queries.Coins.CoinHistory as CHistory
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.Person as QP
import Tools.Error

-- | Sub-ledger label of both coin accounts (the driver's liability and the merchant's coin budget).
coinsSubLedger :: Text
coinsSubLedger = "COINS"

-- | Ledger amounts are coin counts; this only fills the entry's currency field.
coinUnitCurrency :: Currency
coinUnitCurrency = INR

-- | Reference types of the entries that spend or expire part of a lot.
coinExpiryReferenceType :: Text
coinExpiryReferenceType = "CoinExpiry"

-- | Reference type of the entry that gives back a debit that lapsed.
coinDebitLapseReferenceType :: Text
coinDebitLapseReferenceType = "CoinDebitLapse"

-- | What the coin ledger needs from the monad: finance storage, the actor for the audit trail, and the
--   driver app's own storage and cache.
type CoinFlow m r = (BeamFlow m r, MonadFlow m, HasActorInfo m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r)

-- | Serialises every coin write of one driver (reading the lots and posting against them). Not the finance
--   owner lock: that one is shared with the driver's wallet and is not re-entrant, so a coin write made from
--   inside a wallet flow would wait on itself. Not re-entrant either: take it once, at the outermost coin write.
withCoinLock :: CoinFlow m r => Id DP.Person -> m a -> m a
withCoinLock driverId = Redis.withWaitAndLockRedis ("CoinLedger:" <> driverId.getId) 60 50000

-- | The driver's local time, which is what a coin's expiry is compared to (an expiry is a day boundary in
--   the driver's zone, stored as the same wall-clock time in UTC).
localNow :: MonadFlow m => Seconds -> m UTCTime
localNow timeDiffFromUtc = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) <$> getCurrentTime

-- | The coin-specific details of an entry that the ledger has no column for, kept as JSON in the entry's
--   metadata @reason@. Read only by this module.
data CoinAttrs = CoinAttrs
  { bulkUploadTitle :: Maybe DCoins.Translations,
    vehicleCategory :: Maybe DTV.VehicleCategory,
    serviceTierType :: Maybe DTC.ServiceTierType,
    -- | When the coins were really earned, for lots opened from before the ledger held coins.
    earnedAt :: Maybe UTCTime
  }
  deriving (Generic, ToJSON, FromJSON)

noCoinAttrs :: CoinAttrs
noCoinAttrs = CoinAttrs {bulkUploadTitle = Nothing, vehicleCategory = Nothing, serviceTierType = Nothing, earnedAt = Nothing}

data CoinCredit = CoinCredit
  { driverId :: Id DP.Person,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    eventFunction :: DCT.DriverCoinsFunctionType,
    coins :: Int,
    expiresAt :: Maybe UTCTime,
    entityId :: Maybe Text,
    -- | Reference id of the entry; Nothing = a fresh one, so every credit is its own leg. A fixed id makes
    --   posting the same credit again a no-op (used to open balances).
    referenceId :: Maybe Text,
    attrs :: CoinAttrs
  }

data CoinDebit = CoinDebit
  { driverId :: Id DP.Person,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    eventFunction :: DCT.DriverCoinsFunctionType,
    coins :: Int,
    expiresAt :: Maybe UTCTime,
    entityId :: Maybe Text,
    referenceId :: Maybe Text,
    attrs :: CoinAttrs
  }

-- | A coin movement as an event gives it: positive coins are earned, negative are taken off.
data CoinEvent = CoinEvent
  { driverId :: Id DP.Person,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    eventFunction :: DCT.DriverCoinsFunctionType,
    coins :: Int,
    expiresAt :: Maybe UTCTime,
    entityId :: Maybe Text,
    attrs :: CoinAttrs
  }

-- | Posts the event under the driver's coin lock: a lot for a positive amount, a debit for a negative one,
--   nothing for zero.
postCoinEvent :: CoinFlow m r => CoinEvent -> m ()
postCoinEvent e =
  withCoinLock e.driverId $
    if
        | e.coins > 0 ->
          void $ postCoinCredit CoinCredit {driverId = e.driverId, merchantId = e.merchantId, merchantOperatingCityId = e.merchantOperatingCityId, eventFunction = e.eventFunction, coins = e.coins, expiresAt = e.expiresAt, entityId = e.entityId, referenceId = Nothing, attrs = e.attrs}
        | e.coins < 0 ->
          void $ postCoinDebit CoinDebit {driverId = e.driverId, merchantId = e.merchantId, merchantOperatingCityId = e.merchantOperatingCityId, eventFunction = e.eventFunction, coins = negate e.coins, expiresAt = e.expiresAt, entityId = e.entityId, referenceId = Nothing, attrs = e.attrs}
        | otherwise -> pure ()

findCoinAccount :: BeamFlow m r => Id DP.Person -> m (Maybe Account)
findCoinAccount driverId =
  findAccountByCounterpartyAndType (Just DRIVER) (Just driverId.getId) Liability (Just coinsSubLedger)

getCoinBudgetAccount :: (BeamFlow m r, MonadFlow m) => Text -> Text -> m Account
getCoinBudgetAccount mId cityId =
  getOrCreateAccount
    AccountInput
      { accountType = Expense,
        counterpartyType = Just SELLER,
        counterpartyId = Just mId,
        subLedger = Just coinsSubLedger,
        currency = coinUnitCurrency,
        merchantId = mId,
        merchantOperatingCityId = cityId
      }
    >>= fromEitherM (\e -> InternalError ("Coin ledger: coin budget account: " <> show e))

createDriverCoinAccount :: (BeamFlow m r, MonadFlow m) => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m Account
createDriverCoinAccount driverId mId cityId =
  getOrCreateAccount
    AccountInput
      { accountType = Liability,
        counterpartyType = Just DRIVER,
        counterpartyId = Just driverId.getId,
        subLedger = Just coinsSubLedger,
        currency = coinUnitCurrency,
        merchantId = mId.getId,
        merchantOperatingCityId = cityId.getId
      }
    >>= fromEitherM (\e -> InternalError ("Coin ledger: driver coin account: " <> show e))

-- | The driver's coin account. A driver with no account yet gets one, and the live coins they hold in the
--   old coin history are opened into it as lots, once: the opening entries use the old row's id as their
--   reference, so a repeat after an interrupted first run posts nothing twice.
ensureCoinAccount ::
  (BeamFlow m r, MonadFlow m, HasActorInfo m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  Id DP.Person ->
  m Account
ensureCoinAccount driverId =
  findCoinAccount driverId >>= \case
    Just acc -> pure acc
    Nothing -> do
      driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      Redis.withWaitAndLockRedis ("CoinAccountOpen:" <> driverId.getId) 60 10000 $
        findCoinAccount driverId >>= \case
          Just acc -> pure acc
          Nothing -> do
            void $ createDriverCoinAccount driverId driver.merchantId driver.merchantOperatingCityId
            openFromLegacyHistory driverId driver.merchantId driver.merchantOperatingCityId
            -- read it again: the account as created does not have the opening entries in its balance
            findCoinAccount driverId >>= fromMaybeM (InternalError "Coin ledger: coin account missing after opening")

-- | Opens the driver's live legacy coins as ledger entries: unexpired positive rows as lots holding what is
--   left of them, unexpired negative rows as debits. Oldest first, so lots of equal expiry keep their order.
openFromLegacyHistory ::
  (BeamFlow m r, MonadFlow m, HasActorInfo m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
openFromLegacyHistory driverId mId cityId = do
  -- the old queries take the zone's offset to compare expiry; the ledger compares instants, so use UTC here
  -- and let the first sweep drop whatever lapsed in the zone's offset.
  rows <- CHistory.getTotalCoins driverId 0
  forM_ (sortOn (.createdAt) rows) $ \row -> do
    let attrs =
          CoinAttrs
            { bulkUploadTitle = row.bulkUploadTitle,
              vehicleCategory = row.vehicleCategory,
              serviceTierType = row.serviceTierType,
              earnedAt = Just row.createdAt
            }
        remaining = row.coins - row.coinsUsed
    if
        | row.coins > 0 && remaining > 0 ->
          void $ postCoinCredit CoinCredit {driverId, merchantId = mId, merchantOperatingCityId = cityId, eventFunction = row.eventFunction, coins = remaining, expiresAt = row.expirationAt, entityId = row.entityId, referenceId = Just row.id.getId, attrs}
        | row.coins < 0 ->
          void $ postCoinDebit CoinDebit {driverId, merchantId = mId, merchantOperatingCityId = cityId, eventFunction = row.eventFunction, coins = negate row.coins, expiresAt = row.expirationAt, entityId = row.entityId, referenceId = Just row.id.getId, attrs}
        | otherwise -> pure ()

encodeAttrs :: CoinAttrs -> Text
encodeAttrs = TE.decodeUtf8 . BSL.toStrict . A.encode

decodeAttrs :: LedgerEntry -> CoinAttrs
decodeAttrs entry =
  fromMaybe noCoinAttrs $ do
    metadata <- entry.metadataV2
    reason <- metadata.reason
    A.decodeStrict (TE.encodeUtf8 reason)

mkMetadata :: CoinAttrs -> LedgerEntryMetadata
mkMetadata attrs =
  LedgerEntryMetadata
    { d2cReferralEarnings = Nothing,
      d2dReferralEarnings = Nothing,
      dailyStatsId = Nothing,
      driverPayable = Nothing,
      payoutOrderId = Nothing,
      reason = Just (encodeAttrs attrs),
      subscriptionAllocations = Nothing
    }

post :: (BeamFlow m r, MonadFlow m, HasActorInfo m r) => LedgerEntryInput -> m LedgerEntry
post input = createEntryWithBalanceUpdate input >>= fromEitherM (\e -> InternalError ("Coin ledger: posting: " <> show e))

-- | Gives the driver coins as a lot. Returns the lot's entry.
postCoinCredit ::
  (BeamFlow m r, MonadFlow m, HasActorInfo m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  CoinCredit ->
  m LedgerEntry
postCoinCredit c = do
  driverAcc <- ensureCoinAccount c.driverId
  budgetAcc <- getCoinBudgetAccount c.merchantId.getId c.merchantOperatingCityId.getId
  freshId <- generateGUIDText
  post
    LedgerEntryInput
      { fromAccountId = budgetAcc.id,
        toAccountId = driverAcc.id,
        concernedIndividualId = Just c.driverId.getId,
        amount = fromIntegral c.coins,
        currency = coinUnitCurrency,
        entryType = LiabilityCreated,
        status = SETTLED,
        referenceType = show c.eventFunction,
        referenceId = fromMaybe freshId c.referenceId,
        entityReferenceId = c.entityId,
        entityReferenceType = Nothing,
        metadata = Just (mkMetadata c.attrs),
        merchantId = c.merchantId.getId,
        merchantOperatingCityId = c.merchantOperatingCityId.getId,
        settlementStatus = Just UNSETTLED,
        expiresAt = c.expiresAt
      }

-- | Takes coins off the driver without naming a lot (penalty, reversal). Held against the balance until
--   its @expiresAt@, if it has one.
postCoinDebit ::
  (BeamFlow m r, MonadFlow m, HasActorInfo m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  CoinDebit ->
  m LedgerEntry
postCoinDebit d = do
  driverAcc <- ensureCoinAccount d.driverId
  budgetAcc <- getCoinBudgetAccount d.merchantId.getId d.merchantOperatingCityId.getId
  freshId <- generateGUIDText
  post
    LedgerEntryInput
      { fromAccountId = driverAcc.id,
        toAccountId = budgetAcc.id,
        concernedIndividualId = Just d.driverId.getId,
        amount = fromIntegral d.coins,
        currency = coinUnitCurrency,
        entryType = LiabilitySettled,
        status = SETTLED,
        referenceType = show d.eventFunction,
        referenceId = fromMaybe freshId d.referenceId,
        entityReferenceId = d.entityId,
        entityReferenceType = Nothing,
        metadata = Just (mkMetadata d.attrs),
        merchantId = d.merchantId.getId,
        merchantOperatingCityId = d.merchantOperatingCityId.getId,
        settlementStatus = Just UNSETTLED,
        expiresAt = d.expiresAt
      }

-- | Takes @coins@ from the driver's live lots, earliest expiry first, one entry per lot taken from, all under
--   the given reference. Returns False, posting nothing, if the lots cannot cover it. Lapsed coins are booked
--   first. The caller holds 'withCoinLock'.
spendCoins ::
  (BeamFlow m r, MonadFlow m, HasActorInfo m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  Id DP.Person ->
  Seconds ->
  Int ->
  Text ->
  Text ->
  m Bool
spendCoins driverId timeDiffFromUtc coins referenceType referenceId = do
  now <- localNow timeDiffFromUtc
  sweepLapsedCoins driverId now
  driverAcc <- ensureCoinAccount driverId
  budgetAcc <- getCoinBudgetAccount driverAcc.merchantId driverAcc.merchantOperatingCityId
  views <- openLots driverAcc.id
  let (live, _) = partitionByExpiry now views
      (takes, shortfall) = allocateInExpiryOrder (fromIntegral coins) live
  if shortfall > 0
    then pure False
    else do
      forM_ takes $ \(view, taken) ->
        post
          LedgerEntryInput
            { fromAccountId = driverAcc.id,
              toAccountId = budgetAcc.id,
              concernedIndividualId = Just driverId.getId,
              amount = taken,
              currency = coinUnitCurrency,
              entryType = LiabilitySettled,
              status = SETTLED,
              referenceType = referenceType,
              referenceId = referenceId,
              entityReferenceId = Just view.lot.id.getId,
              entityReferenceType = Just ExpiringLot,
              metadata = Nothing,
              merchantId = driverAcc.merchantId,
              merchantOperatingCityId = driverAcc.merchantOperatingCityId,
              settlementStatus = Just UNSETTLED,
              expiresAt = Nothing
            }
      markEntriesAsClaimed [view.lot.id | (view, taken) <- takes, taken >= view.remaining]
      pure True

-- | Books what lapsed: the unspent part of each lapsed lot is taken out (naming the lot), and a lapsed debit
--   is given back. Both are idempotent on the lot / debit id, and retired afterwards so later reads skip them.
sweepLapsedCoins ::
  (BeamFlow m r, MonadFlow m, HasActorInfo m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  Id DP.Person ->
  UTCTime ->
  m ()
sweepLapsedCoins driverId now = do
  driverAcc <- ensureCoinAccount driverId
  budgetAcc <- getCoinBudgetAccount driverAcc.merchantId driverAcc.merchantOperatingCityId
  lapsed <- lapsedLots driverAcc.id now
  forM_ lapsed $ \view ->
    post
      LedgerEntryInput
        { fromAccountId = driverAcc.id,
          toAccountId = budgetAcc.id,
          concernedIndividualId = Just driverId.getId,
          amount = view.remaining,
          currency = coinUnitCurrency,
          entryType = LiabilitySettled,
          status = SETTLED,
          referenceType = coinExpiryReferenceType,
          referenceId = view.lot.id.getId,
          entityReferenceId = Just view.lot.id.getId,
          entityReferenceType = Just ExpiringLot,
          metadata = Nothing,
          merchantId = driverAcc.merchantId,
          merchantOperatingCityId = driverAcc.merchantOperatingCityId,
          settlementStatus = Just UNSETTLED,
          expiresAt = Nothing
        }
  lapsedDebits <- QLedgerExtra.findLapsedUnlinkedDebitsByFromAccount driverAcc.id now
  forM_ lapsedDebits $ \debit ->
    post
      LedgerEntryInput
        { fromAccountId = budgetAcc.id,
          toAccountId = driverAcc.id,
          concernedIndividualId = Just driverId.getId,
          amount = debit.amount,
          currency = coinUnitCurrency,
          entryType = DLE.Adjustment,
          status = SETTLED,
          referenceType = coinDebitLapseReferenceType,
          referenceId = debit.id.getId,
          entityReferenceId = Nothing,
          entityReferenceType = Nothing,
          metadata = Nothing,
          merchantId = driverAcc.merchantId,
          merchantOperatingCityId = driverAcc.merchantOperatingCityId,
          settlementStatus = Just UNSETTLED,
          expiresAt = Nothing
        }
  markEntriesAsClaimed (map (.lot.id) lapsed <> map (.id) lapsedDebits)

-- | Coins the driver can spend now: the account's balance (every posted entry), less what is left of lots that
--   have lapsed but are not booked yet, plus debits that have lapsed but are not given back yet. Only the lapsed
--   items are read, so this costs one account read however long the driver's history is. Whatever is found
--   lapsed is booked and retired here, so those sets stay small.
getSpendableCoins ::
  (BeamFlow m r, MonadFlow m, HasActorInfo m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  Id DP.Person ->
  Seconds ->
  m Int
getSpendableCoins driverId timeDiffFromUtc = do
  now <- localNow timeDiffFromUtc
  driverAcc <- ensureCoinAccount driverId
  lapsed <- lapsedLots driverAcc.id now
  lapsedDebits <- QLedgerExtra.findLapsedUnlinkedDebitsByFromAccount driverAcc.id now
  unless (null lapsed && null lapsedDebits) $ withCoinLock driverId (sweepLapsedCoins driverId now)
  pure . round . getHighPrecMoney $ driverAcc.balance - sum (map (.remaining) lapsed) + sum (map (.amount) lapsedDebits)

-- | The lots the driver earned under one event function and entity (the old "was this already awarded" check).
findCoinLotsByEvent ::
  (BeamFlow m r, MonadFlow m, HasActorInfo m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  Id DP.Person ->
  DCT.DriverCoinsFunctionType ->
  Maybe Text ->
  m [DTCC.CoinHistory]
findCoinLotsByEvent driverId eventFunction entityId =
  findCoinAccount driverId >>= \case
    Nothing -> pure []
    Just driverAcc -> do
      entries <- QLedgerExtra.findLotsByToAccountReferenceAndEntity driverAcc.id (show eventFunction) entityId
      pure $ mapMaybe (entryToCoinHistory Map.empty) entries

-- | The earn the driver already has for an event function and entity: the old coin history first (it is
--   frozen, and keeps what was originally earned), then the ledger.
getCoinsByEvent ::
  CoinFlow m r =>
  Id DP.Person ->
  DCT.DriverCoinsFunctionType ->
  Maybe Text ->
  m (Maybe DTCC.CoinHistory)
getCoinsByEvent driverId eventFunction entityId =
  CHistory.getCoinsByEventFunction driverId eventFunction entityId >>= \case
    Just row -> pure (Just row)
    Nothing -> listToMaybe <$> findCoinLotsByEvent driverId eventFunction entityId

-- | The driver's earns and penalties in time order, newest first, as the old coin history rows, with what was
--   spent from each earn filled in.
coinActivity ::
  (BeamFlow m r, MonadFlow m, HasActorInfo m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  Id DP.Person ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m [DTCC.CoinHistory]
coinActivity driverId mbFrom mbTo mbLimit mbOffset = do
  driverAcc <- ensureCoinAccount driverId
  entries <- QLedgerExtra.findAccountActivity driverAcc.id mbFrom mbTo mbLimit mbOffset
  consumptions <- QLedgerExtra.findLotConsumptions [e.id.getId | e <- entries, e.toAccountId == driverAcc.id]
  let spentFromLot =
        Map.fromListWith
          (+)
          [(lotId, c.amount) | c <- consumptions, c.referenceType /= coinExpiryReferenceType, Just lotId <- [c.entityReferenceId]]
  pure $ mapMaybe (entryToCoinHistory spentFromLot) (filter (not . isOpenedFromLegacy) entries)

-- | A lot that opened an old coin history row into the ledger; the old row is what the history shows.
isOpenedFromLegacy :: LedgerEntry -> Bool
isOpenedFromLegacy = isJust . (.earnedAt) . decodeAttrs

-- | Earns and penalties made in [from, to), newest first: the old coin history for what came before the ledger
--   held coins, then the ledger's.
coinHistoryBetween :: CoinFlow m r => Id DP.Person -> UTCTime -> UTCTime -> m [DTCC.CoinHistory]
coinHistoryBetween driverId from to = do
  legacy <- CHistory.getCoinHistoryBetween driverId from to
  ledger <- coinActivity driverId (Just from) (Just to) Nothing Nothing
  pure $ sortOn (Down . (.createdAt)) (legacy <> ledger)

-- | What the driver earned or lost on the local day of @istTime@.
coinsOfDay :: CoinFlow m r => Id DP.Person -> UTCTime -> NominalDiffTime -> m [DTCC.CoinHistory]
coinsOfDay driverId istTime timeDiffFromUtc =
  coinHistoryBetween driverId (addUTCTime (- timeDiffFromUtc) todayStart) (addUTCTime (- timeDiffFromUtc) (addUTCTime 86400 todayStart))
  where
    todayStart = UTCTime (utctDay istTime) 0

-- | What the driver earned or lost on the local day before that of @istTime@.
coinsOfPreviousDay :: CoinFlow m r => Id DP.Person -> UTCTime -> NominalDiffTime -> m [DTCC.CoinHistory]
coinsOfPreviousDay driverId istTime timeDiffFromUtc =
  coinHistoryBetween driverId (addUTCTime (- timeDiffFromUtc) yesterdayStart) (addUTCTime (- timeDiffFromUtc) todayStart)
  where
    todayStart = UTCTime (utctDay istTime) 0
    yesterdayStart = UTCTime (addDays (-1) (utctDay istTime)) 0

-- | A page of the driver's earns and penalties, newest first, over the old coin history and the ledger.
coinHistoryPage :: CoinFlow m r => Id DP.Person -> Maybe Integer -> Maybe Integer -> m [DTCC.CoinHistory]
coinHistoryPage driverId mbLimit mbOffset = do
  let limitVal = maybe 10 fromInteger mbLimit
      offsetVal = maybe 0 fromInteger mbOffset
  legacy <- CHistory.totalCoinEarnHistory driverId (Just (toInteger (limitVal + offsetVal))) (Just 0)
  ledger <- coinActivity driverId Nothing Nothing (Just (limitVal + offsetVal)) (Just 0)
  pure . take limitVal . drop offsetVal $ sortOn (Down . (.createdAt)) (legacy <> ledger)

-- | Live lots with coins left that lapse inside [now, now + window]: the coins about to expire.
liveLotsExpiringBetween ::
  (BeamFlow m r, MonadFlow m, HasActorInfo m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  Id DP.Person ->
  UTCTime ->
  UTCTime ->
  m [LotView]
liveLotsExpiringBetween driverId from to = do
  driverAcc <- ensureCoinAccount driverId
  views <- openLots driverAcc.id
  let (live, _) = partitionByExpiry from views
  pure [v | v <- live, Just expiry <- [v.lot.expiresAt], expiry <= to]

-- | How many coins are about to lapse inside [from, to].
countCoinsExpiringBetween :: CoinFlow m r => Id DP.Person -> UTCTime -> UTCTime -> m Int
countCoinsExpiringBetween driverId from to = do
  lots <- liveLotsExpiringBetween driverId from to
  pure . round . getHighPrecMoney . sum $ map (.remaining) lots

-- | A lot or a penalty as the coin history row the old APIs return. @spentFromLot@ maps a lot's id to the coins
--   spent from it (expiry not counted); an entry whose reference type is not a coin event is dropped.
entryToCoinHistory :: Map.Map Text HighPrecMoney -> LedgerEntry -> Maybe DTCC.CoinHistory
entryToCoinHistory spentFromLot entry = do
  eventFunction <- readMaybe (T.unpack entry.referenceType)
  let attrs = decodeAttrs entry
      isLot = entry.entryType == LiabilityCreated
      amountCoins = round (getHighPrecMoney entry.amount) :: Int
      usedCoins = round (getHighPrecMoney (Map.findWithDefault 0 entry.id.getId spentFromLot)) :: Int
  pure
    DTCC.CoinHistory
      { id = Id entry.id.getId,
        driverId = fromMaybe "" entry.concernedIndividualId,
        merchantId = entry.merchantId,
        merchantOptCityId = entry.merchantOperatingCityId,
        eventFunction = eventFunction,
        coins = if isLot then amountCoins else negate amountCoins,
        createdAt = fromMaybe entry.timestamp attrs.earnedAt,
        updatedAt = entry.updatedAt,
        expirationAt = entry.expiresAt,
        coinsUsed = if isLot then usedCoins else 0,
        status = if isLot && usedCoins < amountCoins then DCT.Remaining else DCT.Used,
        bulkUploadTitle = attrs.bulkUploadTitle,
        entityId = entry.entityReferenceId,
        vehicleCategory = attrs.vehicleCategory,
        serviceTierType = attrs.serviceTierType
      }

-- ─── Coin cash ──────────────────────────────────────────────────────────────
--
--   Coins redeemed for subscription become coin cash: money that can only pay the plan fee. A redemption
--   credits it ('creditCoinCash') and a plan-fee payment debits it ('debitCoinCash'); it is opened from the old
--   driver stats the first time it is read. It is a Liability account of the driver with its own sub-ledger, so
--   it is never the wallet (nothing that reads the wallet -- payout, airport entry -- can see or spend it). The
--   other side is one per-merchant Expense account on the same sub-ledger.

coinCashSubLedger :: Text
coinCashSubLedger = "COIN_CASH"

findCoinCashAccount :: BeamFlow m r => Id DP.Person -> m (Maybe Account)
findCoinCashAccount driverId =
  findAccountByCounterpartyAndType (Just DRIVER) (Just driverId.getId) Liability (Just coinCashSubLedger)

-- | The driver's coin cash account. A driver without one gets it, opened with what the old driver stats held
--   as coin cash left (once: the opening entry's reference is the driver, so a repeat posts nothing twice).
ensureCoinCashAccount :: CoinFlow m r => Id DP.Person -> m Account
ensureCoinCashAccount driverId =
  findCoinCashAccount driverId >>= \case
    Just acc -> pure acc
    Nothing -> do
      driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      Redis.withWaitAndLockRedis ("CoinCashOpen:" <> driverId.getId) 60 10000 $
        findCoinCashAccount driverId >>= \case
          Just acc -> pure acc
          Nothing -> do
            currency <- SMerchant.getCurrencyByMerchantOpCity driver.merchantOperatingCityId
            acc <-
              getOrCreateAccount
                AccountInput
                  { accountType = Liability,
                    counterpartyType = Just DRIVER,
                    counterpartyId = Just driverId.getId,
                    subLedger = Just coinCashSubLedger,
                    currency = currency,
                    merchantId = driver.merchantId.getId,
                    merchantOperatingCityId = driver.merchantOperatingCityId.getId
                  }
                >>= fromEitherM (\e -> InternalError ("Coin ledger: coin cash account: " <> show e))
            mbStats <- QDS.findById (cast driverId)
            let legacyLeft = maybe 0 DDS.coinCovertedToCashLeft mbStats
            when (legacyLeft > 0) $ postCoinCash acc driverId LiabilityCreated legacyLeft "CoinCashOpening" driverId.getId Nothing
            findCoinCashAccount driverId >>= fromMaybeM (InternalError "Coin ledger: coin cash account missing after opening")

getCoinCashBudgetAccount :: (BeamFlow m r, MonadFlow m) => Account -> m Account
getCoinCashBudgetAccount acc =
  getOrCreateAccount
    AccountInput
      { accountType = Expense,
        counterpartyType = Just SELLER,
        counterpartyId = Just acc.merchantId,
        subLedger = Just coinCashSubLedger,
        currency = acc.currency,
        merchantId = acc.merchantId,
        merchantOperatingCityId = acc.merchantOperatingCityId
      }
    >>= fromEitherM (\e -> InternalError ("Coin ledger: coin cash budget account: " <> show e))

-- | One coin cash entry: 'LiabilityCreated' puts money into the driver's account, 'LiabilitySettled' takes it out.
postCoinCash :: (BeamFlow m r, MonadFlow m, HasActorInfo m r) => Account -> Id DP.Person -> EntryType -> HighPrecMoney -> Text -> Text -> Maybe Text -> m ()
postCoinCash acc driverId entryType amount refType refId entityRefId = do
  budget <- getCoinCashBudgetAccount acc
  let (fromAcc, toAcc) = if entryType == LiabilityCreated then (budget.id, acc.id) else (acc.id, budget.id)
  void $
    post
      LedgerEntryInput
        { fromAccountId = fromAcc,
          toAccountId = toAcc,
          concernedIndividualId = Just driverId.getId,
          amount = amount,
          currency = acc.currency,
          entryType = entryType,
          status = SETTLED,
          referenceType = refType,
          referenceId = refId,
          entityReferenceId = entityRefId,
          entityReferenceType = Nothing,
          metadata = Nothing,
          merchantId = acc.merchantId,
          merchantOperatingCityId = acc.merchantOperatingCityId,
          settlementStatus = Nothing,
          expiresAt = Nothing
        }

-- | Coin cash the driver has left to pay plan fees with.
getCoinCashLeft :: CoinFlow m r => Id DP.Person -> m HighPrecMoney
getCoinCashLeft driverId = (.balance) <$> ensureCoinCashAccount driverId

-- | Adds coin cash (a redemption for subscription). The reference id makes posting the same redemption twice a no-op.
creditCoinCash :: CoinFlow m r => Id DP.Person -> HighPrecMoney -> Text -> Text -> m ()
creditCoinCash driverId amount refType refId = withCoinLock driverId $ do
  acc <- ensureCoinCashAccount driverId
  postCoinCash acc driverId LiabilityCreated amount refType refId Nothing

-- | Uses coin cash (a plan fee paid with it). Every use is its own entry; @entityRefId@ names what it paid for.
debitCoinCash :: CoinFlow m r => Id DP.Person -> HighPrecMoney -> Text -> Maybe Text -> m ()
debitCoinCash driverId amount refType entityRefId = withCoinLock driverId $ do
  acc <- ensureCoinCashAccount driverId
  freshId <- generateGUIDText
  postCoinCash acc driverId LiabilitySettled amount refType freshId entityRefId
