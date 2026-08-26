{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Dashboard CRUD for the CancellationConsequenceMatrix and the GLOBAL fault-rule
-- registry (dev/docs/cancellation-consequence-matrix-plan.md). Validations:
--   * dimension values must parse (verdict/cancelledBy constructor names, TripCategory,
--     ServiceTierType, Area, collection mode);
--   * a referenced faultRule must be an ACTIVE entry of the global registry;
--   * no two ACTIVE rows may share an identical dimension tuple (ambiguous resolution).
module Domain.Action.Dashboard.Management.CancellationConsequence
  ( getCancellationConsequenceList,
    postCancellationConsequenceCreate,
    postCancellationConsequenceUpdate,
    getCancellationConsequenceRegistryList,
    postCancellationConsequenceRegistryUpsert,
  )
where

import qualified API.Types.ProviderPlatform.Management.CancellationConsequence as Common
import qualified Data.Text as Text
import qualified Domain.Types.CancellationConsequenceMatrix as DCCM
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Extra.CancellationConsequenceMatrix as DExtra
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (InvalidRequest), TransporterError (TransporterConfigNotFound))
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, throwError)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.CancellationConsequence (FaultRuleRegistryEntry (..))
import qualified SharedLogic.CancellationConsequence as CancellationConsequence
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.CancellationConsequenceMatrix as CQCCM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.CancellationConsequenceMatrix as QCCM

getCancellationConsequenceList ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Environment.Flow Common.CancellationConsequenceListRes
getCancellationConsequenceList merchantShortId opCity mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  rows <- CQCCM.findAllByMerchantOpCityId merchantOpCityId
  let offsetRows = maybe rows (`drop` rows) mbOffset
      limited = maybe offsetRows (`take` offsetRows) mbLimit
  pure $ Common.CancellationConsequenceListRes {rows = map toListItem limited}

postCancellationConsequenceCreate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateCancellationConsequenceReq ->
  Environment.Flow APISuccess
postCancellationConsequenceCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  newId <- generateGUID
  row <- buildRow merchant.id merchantOpCityId newId req.row
  validateRow merchantOpCityId Nothing row
  CQCCM.create row
  CQCCM.clearCacheByCity merchantOpCityId
  pure Success

postCancellationConsequenceUpdate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateCancellationConsequenceReq ->
  Environment.Flow APISuccess
postCancellationConsequenceUpdate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let rowId = ID.cast req.rowId
  _existing <- QCCM.findByPrimaryKey rowId >>= fromMaybeM (InvalidRequest $ "CancellationConsequenceMatrix row not found: " <> rowId.getId)
  row <- buildRow merchant.id merchantOpCityId rowId req.row
  validateRow merchantOpCityId (Just rowId) row
  QCCM.updateByPrimaryKey row
  CQCCM.clearCacheByCity merchantOpCityId
  pure Success

getCancellationConsequenceRegistryList ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow Common.FaultRuleRegistryListRes
getCancellationConsequenceRegistryList _merchantShortId _opCity = do
  entries <- CancellationConsequence.getFaultRuleRegistry
  pure $ Common.FaultRuleRegistryListRes {entries = map (\e -> Common.FaultRuleRegistryEntryAPI {name = e.name, description = e.description, active = e.active}) entries}

postCancellationConsequenceRegistryUpsert ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpsertFaultRuleRegistryReq ->
  Environment.Flow APISuccess
postCancellationConsequenceRegistryUpsert _merchantShortId _opCity req = do
  entries <- CancellationConsequence.getFaultRuleRegistry
  let newEntry = FaultRuleRegistryEntry {name = req.name, description = req.description, active = req.active}
      updated =
        if any (\e -> e.name == req.name) entries
          then map (\e -> if e.name == req.name then newEntry else e) entries
          else entries <> [newEntry]
  CancellationConsequence.putFaultRuleRegistry updated
  pure Success

---------------------------------------------------------------------------------------

buildRow :: ID.Id Domain.Types.Merchant.Merchant -> ID.Id DMOC.MerchantOperatingCity -> ID.Id DCCM.CancellationConsequenceMatrix -> Common.CancellationConsequenceRowAPI -> Environment.Flow DCCM.CancellationConsequenceMatrix
buildRow merchantId merchantOpCityId rowId apiRow = do
  whenJust apiRow.faultVerdict $ \v ->
    unless (v `elem` allowedFaultVerdicts) $ throwError (InvalidRequest $ "Invalid faultVerdict: " <> v <> ". Allowed: " <> Text.intercalate ", " allowedFaultVerdicts)
  cancelledBy <- forM apiRow.cancelledBy $ \c ->
    fromMaybeM (InvalidRequest $ "Invalid cancelledBy: " <> c) (readMaybe (Text.unpack c) :: Maybe DCT.CancellationType)
  paymentInstrument <- forM apiRow.paymentInstrument $ \pi' ->
    fromMaybeM (InvalidRequest $ "Invalid paymentInstrument: " <> pi') (readMaybe (Text.unpack pi') :: Maybe DMPM.PaymentInstrument)
  tripCategory <- forM apiRow.tripCategory $ \tc ->
    fromMaybeM (InvalidRequest $ "Invalid tripCategory: " <> tc) (readMaybe (Text.unpack tc) :: Maybe DTC.TripCategory)
  vehicleServiceTier <- forM apiRow.vehicleServiceTier $ \st ->
    fromMaybeM (InvalidRequest $ "Invalid vehicleServiceTier: " <> st) (readMaybe (Text.unpack st) :: Maybe DTC.ServiceTierType)
  area <- forM apiRow.area $ \a ->
    fromMaybeM (InvalidRequest $ "Invalid area: " <> a) (readMaybe (Text.unpack a) :: Maybe SL.Area)
  collectionMode <- forM apiRow.collectionMode $ \cm ->
    fromMaybeM (InvalidRequest $ "Invalid collectionMode: " <> cm) (readMaybe (Text.unpack cm) :: Maybe DCCM.ConsequenceCollectionMode)
  whenJust apiRow.faultRule $ \ruleName -> do
    registered <- CancellationConsequence.isRegisteredFaultRule ruleName
    unless registered $ throwError (InvalidRequest $ "faultRule '" <> ruleName <> "' is not an active entry of the global fault-rule registry — register it first via /registry/upsert")
  let mbCustomerDeduction = toDomainDeduction <$> apiRow.customerDeduction
      mbDriverDeduction = toDomainDeduction <$> apiRow.driverDeduction
  whenJust mbCustomerDeduction $ validateDeduction "customerDeduction"
  whenJust mbDriverDeduction $ validateDeduction "driverDeduction"
  -- Coin consequences fire straight from the matrix (no coin_config prerequisite any
  -- more), so a coin row in a city without the coin feature would write coins the driver
  -- can never see — reject at config time.
  whenJust mbDriverDeduction $ \ded ->
    when (isCoinConsequence ded) $ do
      transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      unless transporterConfig.coinFeature $
        throwError (InvalidRequest "driverDeduction: coin consequences require the coin feature (transporterConfig.coinFeature) to be enabled for this city")
  now <- getCurrentTime
  pure
    DCCM.CancellationConsequenceMatrix
      { id = rowId,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOpCityId,
        faultVerdict = apiRow.faultVerdict,
        faultRule = apiRow.faultRule,
        cancelledBy = cancelledBy,
        tripCategory = tripCategory,
        vehicleServiceTier = vehicleServiceTier,
        area = area,
        paymentInstrument = paymentInstrument,
        customerDeduction = mbCustomerDeduction,
        customerCommissionAndTax = toDomainCommissionAndTax <$> apiRow.customerCommissionAndTax,
        driverDeduction = mbDriverDeduction,
        collectionMode = collectionMode,
        carryForwardDues = apiRow.carryForwardDues,
        consumeRideCreditOnCancellation = apiRow.consumeRideCreditOnCancellation,
        waiveOffAllowed = apiRow.waiveOffAllowed,
        maxWaiveOffsPerPeriod = apiRow.maxWaiveOffsPerPeriod,
        waiveOffPeriodDays = apiRow.waiveOffPeriodDays,
        blacklistDriverForRiderSeconds = fromIntegral <$> apiRow.blacklistDriverForRiderSeconds,
        countsTowardDriverCancellationRate = apiRow.countsTowardDriverCancellationRate,
        countsTowardCustomerCancellationStats = apiRow.countsTowardCustomerCancellationStats,
        exemptDashboardBookings = apiRow.exemptDashboardBookings,
        driverNotificationKey = apiRow.driverNotificationKey,
        customerNotificationKey = apiRow.customerNotificationKey,
        active = apiRow.active,
        createdAt = now,
        updatedAt = now
      }

-- Two ACTIVE rows with the same dimension tuple would tie in resolution — reject.
validateRow :: ID.Id DMOC.MerchantOperatingCity -> Maybe (ID.Id DCCM.CancellationConsequenceMatrix) -> DCCM.CancellationConsequenceMatrix -> Environment.Flow ()
validateRow merchantOpCityId mbSelfId row =
  when row.active $ do
    rows <- CQCCM.findAllByMerchantOpCityId merchantOpCityId
    let clashes = filter (\r -> r.active && Just r.id /= mbSelfId && sameDimensions r row) rows
    unless (null clashes) $
      throwError (InvalidRequest $ "An active row with the same dimension tuple already exists: " <> Text.intercalate ", " (map (.id.getId) clashes))
  where
    sameDimensions a b =
      a.faultVerdict == b.faultVerdict
        && a.faultRule == b.faultRule
        && a.cancelledBy == b.cancelledBy
        && a.tripCategory == b.tripCategory
        && a.vehicleServiceTier == b.vehicleServiceTier
        && a.area == b.area
        && a.paymentInstrument == b.paymentInstrument

allowedFaultVerdicts :: [Text]
allowedFaultVerdicts = ["DriverAtFault", "CustomerAtFault", "SharedFault", "NoFault"]

toDomainDeduction :: Common.DeductionAPIEntity -> DExtra.ConsequenceDeduction
toDomainDeduction = \case
  Common.CoinDeductionAPIEntity c -> DExtra.CoinDeduction {coins = c.coins, expirySeconds = c.expirySeconds}
  Common.MoneyDeductionAPIEntity m -> DExtra.MoneyDeduction (toDomainMoney m)
  Common.CoinAdditionAPIEntity c -> DExtra.CoinAddition {coins = c.coins, expirySeconds = c.expirySeconds}
  Common.MoneyAdditionAPIEntity m -> DExtra.MoneyAddition (toDomainMoney m)

isCoinConsequence :: DExtra.ConsequenceDeduction -> Bool
isCoinConsequence = \case
  DExtra.CoinDeduction {} -> True
  DExtra.CoinAddition {} -> True
  _ -> False

-- | All amounts in a deduction/addition are POSITIVE; direction lives in the constructor.
-- Signed values are rejected so "give back" can never be smuggled in as a negative charge.
validateDeduction :: Text -> DExtra.ConsequenceDeduction -> Environment.Flow ()
validateDeduction fieldName ded = case ded of
  DExtra.CoinDeduction {coins} -> unless (coins > 0) $ bad "coins must be positive"
  DExtra.CoinAddition {coins} -> unless (coins > 0) $ bad "coins must be positive"
  DExtra.MoneyDeduction m -> checkMoney m
  DExtra.MoneyAddition m -> do
    checkMoney m
    case m of
      DExtra.FixedMoney {overdueAmount = Just _} -> bad "overdueAmount is not applicable to an addition"
      _ -> pure ()
  where
    bad msg = throwError (InvalidRequest $ fieldName <> ": " <> msg <> " (direction is expressed by the Deduction/Addition constructor, never by a sign)")
    checkMoney = \case
      DExtra.FixedMoney {amount, overdueAmount} -> do
        unless (amount > 0) $ bad "amount must be positive"
        whenJust overdueAmount $ \o -> unless (o > 0) $ bad "overdueAmount must be positive"
      DExtra.PercentageMoney {percentage, minAmount, maxAmount} -> do
        unless (percentage > 0) $ bad "percentage must be positive"
        whenJust minAmount $ \v -> unless (v > 0) $ bad "minAmount must be positive"
        whenJust maxAmount $ \v -> unless (v > 0) $ bad "maxAmount must be positive"

toDomainMoney :: Common.MoneyDeductionAPI -> DExtra.MoneyDeduction
toDomainMoney = \case
  Common.FixedMoneyAPIEntity f -> DExtra.FixedMoney {amount = f.amount, overdueAmount = f.overdueAmount}
  Common.PercentageMoneyAPIEntity p -> DExtra.PercentageMoney {percentage = p.percentage, minAmount = p.minAmount, maxAmount = p.maxAmount}

toDomainCommissionAndTax :: Common.CommissionAndTaxAPI -> DExtra.CommissionAndTax
toDomainCommissionAndTax c =
  DExtra.CommissionAndTax
    { taxPercentage = c.taxPercentage,
      commission =
        c.commission <&> \case
          Common.FixedRateAPIEntity amt -> DExtra.FixedRate {amount = amt}
          Common.PercentageRateAPIEntity pct -> DExtra.PercentageRate {percentage = pct}
    }

toListItem :: DCCM.CancellationConsequenceMatrix -> Common.CancellationConsequenceListItem
toListItem row =
  Common.CancellationConsequenceListItem
    { rowId = ID.cast row.id,
      row =
        Common.CancellationConsequenceRowAPI
          { faultVerdict = row.faultVerdict,
            faultRule = row.faultRule,
            cancelledBy = show <$> row.cancelledBy,
            tripCategory = show <$> row.tripCategory,
            vehicleServiceTier = show <$> row.vehicleServiceTier,
            area = show <$> row.area,
            paymentInstrument = show <$> row.paymentInstrument,
            customerDeduction = toAPIDeduction <$> row.customerDeduction,
            customerCommissionAndTax = toAPICommissionAndTax <$> row.customerCommissionAndTax,
            driverDeduction = toAPIDeduction <$> row.driverDeduction,
            collectionMode = show <$> row.collectionMode,
            carryForwardDues = row.carryForwardDues,
            consumeRideCreditOnCancellation = row.consumeRideCreditOnCancellation,
            waiveOffAllowed = row.waiveOffAllowed,
            maxWaiveOffsPerPeriod = row.maxWaiveOffsPerPeriod,
            waiveOffPeriodDays = row.waiveOffPeriodDays,
            blacklistDriverForRiderSeconds = (.getSeconds) <$> row.blacklistDriverForRiderSeconds,
            countsTowardDriverCancellationRate = row.countsTowardDriverCancellationRate,
            countsTowardCustomerCancellationStats = row.countsTowardCustomerCancellationStats,
            exemptDashboardBookings = row.exemptDashboardBookings,
            driverNotificationKey = row.driverNotificationKey,
            customerNotificationKey = row.customerNotificationKey,
            active = row.active
          }
    }

toAPIDeduction :: DExtra.ConsequenceDeduction -> Common.DeductionAPIEntity
toAPIDeduction = \case
  DExtra.CoinDeduction {coins, expirySeconds} -> Common.CoinDeductionAPIEntity (Common.CoinDeductionAPI {coins, expirySeconds})
  DExtra.MoneyDeduction m -> Common.MoneyDeductionAPIEntity (toAPIMoney m)
  DExtra.CoinAddition {coins, expirySeconds} -> Common.CoinAdditionAPIEntity (Common.CoinDeductionAPI {coins, expirySeconds})
  DExtra.MoneyAddition m -> Common.MoneyAdditionAPIEntity (toAPIMoney m)

toAPIMoney :: DExtra.MoneyDeduction -> Common.MoneyDeductionAPI
toAPIMoney = \case
  DExtra.FixedMoney {amount, overdueAmount} -> Common.FixedMoneyAPIEntity (Common.FixedMoneyAPI {amount, overdueAmount})
  DExtra.PercentageMoney {percentage, minAmount, maxAmount} -> Common.PercentageMoneyAPIEntity (Common.PercentageMoneyAPI {percentage, minAmount, maxAmount})

toAPICommissionAndTax :: DExtra.CommissionAndTax -> Common.CommissionAndTaxAPI
toAPICommissionAndTax c =
  Common.CommissionAndTaxAPI
    { taxPercentage = c.taxPercentage,
      commission =
        c.commission <&> \case
          DExtra.FixedRate {amount} -> Common.FixedRateAPIEntity amount
          DExtra.PercentageRate {percentage} -> Common.PercentageRateAPIEntity percentage
    }
