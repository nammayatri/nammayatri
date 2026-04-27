{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.Wallet.Service
  ( LoyaltyReferenceType (..),
    loyaltyReferenceTypeText,
    getOrCreateWalletForPerson,
    getOrCreatePlatformAsset,
    mapOrderStatusToWalletStatus,
    LedgerWriteOutcome (..),
    recordLoyaltyEarn,
    recordLoyaltyBurn,
    recordLoyaltyEarnReversal,
    recordLoyaltyBurnReversal,
    bumpWalletAggregatesOnEarn,
    bumpWalletAggregatesOnBurn,
    reconcileWalletFromLoyaltyInfo,
    processLoyaltyInfoFromOrderStatus,
    createTopupWalletPaymentRow,
  )
where

import qualified Data.Text as T
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Wallet.Interface.Types as WalletTypes
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Account as FAccount
import qualified Lib.Finance.Domain.Types.LedgerEntry as FLE
import Lib.Finance.Error.Types (FinanceError (..), LedgerErrorCode (..))
import Lib.Finance.Ledger.Interface (LedgerEntryInput (..))
import qualified Lib.Finance.Ledger.Service as FLedger
import qualified Lib.Finance.Storage.Beam.BeamFlow as FBeamFlow
import qualified Lib.Finance.Storage.Queries.Account as QFAccount
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.Wallet as DWallet
import qualified Lib.Payment.Domain.Types.WalletPayments as DWP
import qualified Lib.Payment.Storage.Beam.BeamFlow as PBeamFlow
import qualified Lib.Payment.Storage.Queries.Wallet as QWallet
import qualified Lib.Payment.Storage.Queries.WalletPayments as QWP

--------------------------------------------------------------------------------
-- Reference type
-- The `referenceType` column on finance_kernel's ledger_entry is kept Text
-- so each domain can define its own controlled vocabulary. Loyalty's
-- vocabulary is this enum; `loyaltyReferenceTypeText` converts at the last
-- mile when writing to the ledger / querying back.
--------------------------------------------------------------------------------

data LoyaltyReferenceType
  = LOYALTY_EARN_TOPUP
  | LOYALTY_EARN_CASHBACK
  | LOYALTY_BURN
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

loyaltyReferenceTypeText :: LoyaltyReferenceType -> Text
loyaltyReferenceTypeText = T.pack . show

mapOrderStatusToWalletStatus :: Payment.TransactionStatus -> DWP.WalletPaymentStatus
mapOrderStatusToWalletStatus = \case
  Payment.NEW -> DWP.NEW
  Payment.CHARGED -> DWP.CHARGED
  Payment.AUTHENTICATION_FAILED -> DWP.FAILED
  Payment.AUTHORIZATION_FAILED -> DWP.FAILED
  Payment.JUSPAY_DECLINED -> DWP.FAILED
  Payment.CLIENT_AUTH_TOKEN_EXPIRED -> DWP.FAILED
  Payment.AUTO_REFUNDED -> DWP.FAILED
  _ -> DWP.PENDING

getOrCreateWalletForPerson ::
  (FBeamFlow.BeamFlow m r, PBeamFlow.BeamFlow m r) =>
  Text -> -- personId
  FAccount.CounterpartyType -> -- programType
  Text -> -- programId
  Currency ->
  Text -> -- merchantId
  Maybe Text -> -- merchantOperatingCityId
  m (DWallet.Wallet, FAccount.Account)
getOrCreateWalletForPerson personId programType programId currency merchantId mbMerchantOperatingCityId = do
  mbWallet <- QWallet.findByPersonAndProgram personId programType
  case mbWallet of
    Just w -> do
      mbAcc <- QFAccount.findById w.accountId
      case mbAcc of
        Just acc -> pure (w, acc)
        Nothing -> createAccountAndWallet -- account somehow missing; recreate
    Nothing -> createAccountAndWallet
  where
    createAccountAndWallet = do
      now <- getCurrentTime
      accountId <- Id <$> generateGUID
      walletId <- Id <$> generateGUID
      let account =
            FAccount.Account
              { id = accountId,
                accountType = FAccount.Liability,
                counterpartyType = Just programType,
                counterpartyId = Just personId,
                currency = currency,
                balance = 0,
                status = FAccount.Active,
                description = Just ("Loyalty wallet for person " <> personId),
                merchantId = merchantId,
                merchantOperatingCityId = fromMaybe "" mbMerchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }

      QFAccount.create account
      let wallet =
            DWallet.Wallet
              { id = walletId,
                personId = personId,
                programType = programType,
                programId = programId,
                accountId = accountId,
                currency = currency,
                availableBalance = 0,
                currentAvailablePoints = 0,
                lifetimeEarned = 0,
                lifetimeBurned = 0,
                topupEarned = 0,
                cashbackEarned = 0,
                merchantId = merchantId,
                merchantOperatingCityId = mbMerchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      QWallet.create wallet
      pure (wallet, account)

getOrCreatePlatformAsset ::
  (FBeamFlow.BeamFlow m r) =>
  Currency ->
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  m FAccount.Account
getOrCreatePlatformAsset currency merchantId cityId = do
  mbAcc <-
    QFAccount.findByCounterpartyAndType
      (Just FAccount.SELLER)
      (Just merchantId)
      FAccount.Asset
      currency
  case mbAcc of
    Just acc -> pure acc
    Nothing -> do
      now <- getCurrentTime
      accountId <- Id <$> generateGUID
      let acc =
            FAccount.Account
              { id = accountId,
                accountType = FAccount.Asset,
                counterpartyType = Just FAccount.SELLER,
                counterpartyId = Just merchantId,
                currency = currency,
                balance = 0,
                status = FAccount.Active,
                description = Just "Platform asset (loyalty offset)",
                merchantId = merchantId,
                merchantOperatingCityId = cityId,
                createdAt = now,
                updatedAt = now
              }
      QFAccount.create acc
      pure acc

data LedgerWriteOutcome
  = WrittenNew (Id FLE.LedgerEntry)
  | AlreadyWritten (Id FLE.LedgerEntry)
  deriving (Eq, Show, Generic)

recordLoyaltyEarn ::
  (FBeamFlow.BeamFlow m r) =>
  DWallet.Wallet ->
  DWP.WalletPayments ->
  m (Either FinanceError LedgerWriteOutcome)
recordLoyaltyEarn wallet wp = do
  let refType = loyaltyReferenceTypeText $ case wp.kind of
        DWP.TOPUP -> LOYALTY_EARN_TOPUP
        DWP.CASHBACK -> LOYALTY_EARN_CASHBACK
  existing <- FLedger.getEntriesByReference refType wp.id.getId
  case nonReversalEntries existing of
    (e : _) -> pure (Right (AlreadyWritten e.id))
    [] -> do
      platformAcc <-
        getOrCreatePlatformAsset
          wp.currency
          wp.merchantId
          (fromMaybe "" wp.merchantOperatingCityId)
      let input =
            LedgerEntryInput
              { fromAccountId = platformAcc.id,
                toAccountId = wallet.accountId,
                amount = wp.points,
                currency = wp.currency,
                entryType = FLE.LiabilityCreated,
                status = FLE.SETTLED,
                referenceType = refType,
                referenceId = wp.id.getId,
                metadata = Nothing,
                merchantId = wp.merchantId,
                merchantOperatingCityId = fromMaybe "" wp.merchantOperatingCityId,
                settlementStatus = Nothing
              }
      result <- FLedger.createEntryWithBalanceUpdate input
      case result of
        Left err -> pure (Left err)
        Right entry -> pure (Right (WrittenNew entry.id))

recordLoyaltyBurn ::
  (FBeamFlow.BeamFlow m r) =>
  DWallet.Wallet ->
  Text -> -- refId
  HighPrecMoney -> -- points to burn
  Currency ->
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  m (Either FinanceError LedgerWriteOutcome)
recordLoyaltyBurn wallet refId points currency merchantId cityId = do
  existing <- FLedger.getEntriesByReference (loyaltyReferenceTypeText LOYALTY_BURN) refId
  case nonReversalEntries existing of
    (e : _) -> pure (Right (AlreadyWritten e.id))
    [] -> do
      platformAcc <- getOrCreatePlatformAsset currency merchantId cityId
      let input =
            LedgerEntryInput
              { fromAccountId = wallet.accountId,
                toAccountId = platformAcc.id,
                amount = points,
                currency = currency,
                entryType = FLE.LiabilitySettled,
                status = FLE.SETTLED,
                referenceType = loyaltyReferenceTypeText LOYALTY_BURN,
                referenceId = refId,
                metadata = Nothing,
                merchantId = merchantId,
                merchantOperatingCityId = cityId,
                settlementStatus = Nothing
              }
      result <- FLedger.createEntryWithBalanceUpdate input
      case result of
        Left err -> pure (Left err)
        Right entry -> pure (Right (WrittenNew entry.id))

recordLoyaltyEarnReversal ::
  (FBeamFlow.BeamFlow m r) =>
  DWallet.Wallet ->
  DWP.WalletPayments ->
  m (Either FinanceError LedgerWriteOutcome)
recordLoyaltyEarnReversal _wallet wp = do
  let refType = loyaltyReferenceTypeText $ case wp.kind of
        DWP.TOPUP -> LOYALTY_EARN_TOPUP
        DWP.CASHBACK -> LOYALTY_EARN_CASHBACK
  entries <- FLedger.getEntriesByReference refType wp.id.getId
  case nonReversalEntries entries of
    [] -> pure (Left $ LedgerError InvalidReversal ("No original earn entry found for wallet_payments " <> wp.id.getId))
    (original : _) ->
      case findReversalOf original.id entries of
        Just rev -> pure (Right (AlreadyWritten rev.id))
        Nothing -> do
          res <- FLedger.createReversal original.id "Juspay loyalty earn reversed"
          pure $ fmap (WrittenNew . (.id)) res

recordLoyaltyBurnReversal ::
  (FBeamFlow.BeamFlow m r) =>
  DWallet.Wallet ->
  Text -> -- refId of the original burn
  m (Either FinanceError LedgerWriteOutcome)
recordLoyaltyBurnReversal _wallet refId = do
  entries <- FLedger.getEntriesByReference (loyaltyReferenceTypeText LOYALTY_BURN) refId
  case nonReversalEntries entries of
    [] -> pure (Left $ LedgerError InvalidReversal ("No original burn entry found for refId " <> refId))
    (original : _) ->
      case findReversalOf original.id entries of
        Just rev -> pure (Right (AlreadyWritten rev.id))
        Nothing -> do
          res <- FLedger.createReversal original.id "Juspay loyalty burn reversed"
          pure $ fmap (WrittenNew . (.id)) res

nonReversalEntries :: [FLE.LedgerEntry] -> [FLE.LedgerEntry]
nonReversalEntries = filter (\e -> e.entryType /= FLE.Reversal)

findReversalOf :: Id FLE.LedgerEntry -> [FLE.LedgerEntry] -> Maybe FLE.LedgerEntry
findReversalOf originalId =
  find (\e -> e.entryType == FLE.Reversal && e.reversalOf == Just originalId)

bumpWalletAggregatesOnEarn ::
  (PBeamFlow.BeamFlow m r) =>
  DWallet.Wallet ->
  DWP.WalletPaymentKind ->
  HighPrecMoney ->
  m ()
bumpWalletAggregatesOnEarn wallet kind points = do
  let newAvailable = wallet.availableBalance + points
      newLifetimeEarned = wallet.lifetimeEarned + points
      newTopup = if kind == DWP.TOPUP then wallet.topupEarned + points else wallet.topupEarned
      newCashback = if kind == DWP.CASHBACK then wallet.cashbackEarned + points else wallet.cashbackEarned
  QWallet.updateAggregatesOnEarn newAvailable newLifetimeEarned newTopup newCashback wallet.id

bumpWalletAggregatesOnBurn ::
  (PBeamFlow.BeamFlow m r) =>
  DWallet.Wallet ->
  HighPrecMoney ->
  m ()
bumpWalletAggregatesOnBurn wallet points = do
  let newAvailable = wallet.availableBalance - points
      newLifetimeBurned = wallet.lifetimeBurned + points
  QWallet.updateAggregatesOnBurn newAvailable newLifetimeBurned wallet.id

reconcileWalletFromLoyaltyInfo ::
  (FBeamFlow.BeamFlow m r, PBeamFlow.BeamFlow m r, Log m, MonadTime m) =>
  DWallet.Wallet ->
  WalletTypes.LoyaltyInfoResponse ->
  m ()
reconcileWalletFromLoyaltyInfo wallet resp = do
  let mbProgram = findProgramById wallet.programId resp
  case mbProgram of
    Nothing ->
      logWarning $
        "reconcileWalletFromLoyaltyInfo: program "
          <> wallet.programId
          <> " not found in Juspay loyaltyInfo response for wallet "
          <> wallet.id.getId
    Just program -> do
      let pw = program.wallet
          pockets = fromMaybe [] pw.pockets
          -- Juspay pocket labels observed in practice: "TOPUP Points",
          -- "REWARD Points" (reward = cashback). Match by substring so minor
          -- label wording changes don't break the mapping.
          pocketByKeyword kw =
            find (\p -> maybe False (T.isInfixOf kw . T.toLower) p.label) pockets
          pocketTopup = pocketByKeyword "topup"
          pocketCashback = pocketByKeyword "reward"
          mbJuspayAvailable = parsePoints =<< pw.availablePoints
          mbJuspayLifeEarned = parsePoints =<< pw.lifetimeEarned
          mbJuspayLifeRedeemed = parsePoints =<< pw.lifetimeRedeemed
          mbJuspayTopup = parsePoints =<< ((.lifetimeEarned) =<< pocketTopup)
          mbJuspayCashback = parsePoints =<< ((.lifetimeEarned) =<< pocketCashback)
          mismatched mbJuspay localVal = maybe False (/= localVal) mbJuspay
          drifted =
            mismatched mbJuspayAvailable wallet.availableBalance
              || mismatched mbJuspayAvailable wallet.currentAvailablePoints
              || mismatched mbJuspayLifeEarned wallet.lifetimeEarned
              || mismatched mbJuspayLifeRedeemed wallet.lifetimeBurned
              || mismatched mbJuspayTopup wallet.topupEarned
              || mismatched mbJuspayCashback wallet.cashbackEarned
          syncAvailable = fromMaybe wallet.availableBalance mbJuspayAvailable
          syncCurrent = fromMaybe wallet.currentAvailablePoints mbJuspayAvailable
          syncLifeEarned = fromMaybe wallet.lifetimeEarned mbJuspayLifeEarned
          syncLifeBurned = fromMaybe wallet.lifetimeBurned mbJuspayLifeRedeemed
          syncTopup = fromMaybe wallet.topupEarned mbJuspayTopup
          syncCashback = fromMaybe wallet.cashbackEarned mbJuspayCashback
      when drifted $ do
        logInfo $
          "wallet drift for " <> wallet.id.getId
            <> " sync: available="
            <> show syncAvailable
            <> " lifeEarned="
            <> show syncLifeEarned
            <> " lifeBurned="
            <> show syncLifeBurned
            <> " topup="
            <> show syncTopup
            <> " cashback="
            <> show syncCashback
        QWallet.syncFromLoyaltyInfo syncCurrent syncAvailable syncLifeEarned syncLifeBurned syncTopup syncCashback wallet.id
      whenJust mbJuspayAvailable $ \v -> QFAccount.updateBalance v wallet.accountId
  where
    findProgramById pid r =
      let programs = fromMaybe [] r.programs
       in find (\p -> p.id_ == pid) programs

    parsePoints :: Text -> Maybe HighPrecMoney
    parsePoints t = realToFrac <$> (readMaybe (T.unpack t) :: Maybe Double)

processLoyaltyInfoFromOrderStatus ::
  (FBeamFlow.BeamFlow m r, PBeamFlow.BeamFlow m r, Log m, MonadTime m) =>
  Text -> -- personId
  DOrder.PaymentOrder ->
  Text -> -- burnRefId
  Payment.LoyaltyInfo ->
  (Text -> m (Maybe FAccount.CounterpartyType)) -> -- resolveProgram
  m WalletTypes.LoyaltyInfoResponse -> -- fetchFullInfo
  m ()
processLoyaltyInfoFromOrderStatus personId order burnRefId loyalty resolveProgram fetchFullInfo = do
  let merchantId = order.merchantId.getId
      mbMerchantOperatingCityId = (.getId) <$> order.merchantOperatingCityId
      merchantOperatingCityIdText = fromMaybe "" mbMerchantOperatingCityId
      currency = order.currency
      isTopup = order.paymentServiceType == Just DOrder.Wallet
      walletStatus = mapOrderStatusToWalletStatus order.status
      isOrderCharged = walletStatus == DWP.CHARGED
  logInfo $
    "[loyaltySvc] enter orderId=" <> order.id.getId
      <> " status="
      <> show order.status
      <> " walletStatus="
      <> show walletStatus
      <> " isOrderCharged="
      <> show isOrderCharged
      <> " isTopup="
      <> show isTopup
      <> " earnCount="
      <> show (length loyalty.earnDetails)
      <> " burnCount="
      <> show (length loyalty.burnDetails)

  forM_ loyalty.earnDetails $ \earn -> do
    logInfo $ "[loyaltySvc] earn programId=" <> earn.programId <> " points=" <> show earn.points
    mbProgramType <- resolveProgram earn.programId
    case mbProgramType of
      Nothing ->
        logError $
          "loyalty earn skipped: program "
            <> earn.programId
            <> " not configured for merchant "
            <> merchantId
      Just programType -> do
        logInfo $ "[loyaltySvc] earn resolved programType=" <> show programType
        (wallet, _acc) <-
          getOrCreateWalletForPerson personId programType earn.programId currency merchantId mbMerchantOperatingCityId
        logInfo $ "[loyaltySvc] earn wallet walletId=" <> wallet.id.getId
        let kind = if isTopup then DWP.TOPUP else DWP.CASHBACK
        wp <- upsertWalletPayment wallet personId order earn kind walletStatus
        logInfo $ "[loyaltySvc] earn upserted wp id=" <> wp.id.getId <> " kind=" <> show wp.kind <> " status=" <> show wp.status
        when isOrderCharged $ do
          res <- recordLoyaltyEarn wallet wp
          case res of
            Left err -> logError $ "recordLoyaltyEarn failed: " <> show err
            Right (WrittenNew _) -> do
              logInfo $ "[loyaltySvc] earn ledger WrittenNew; bumping aggregates walletId=" <> wallet.id.getId
              bumpWalletAggregatesOnEarn wallet kind earn.points
            Right (AlreadyWritten _) -> logInfo "[loyaltySvc] earn ledger AlreadyWritten"
        whenJust earn.reversedPoints $ \rev -> when (rev > 0) $ do
          revRes <- recordLoyaltyEarnReversal wallet wp
          case revRes of
            Left err -> logError $ "recordLoyaltyEarnReversal failed: " <> show err
            Right (WrittenNew _) -> bumpWalletAggregatesOnEarn wallet kind (negate rev)
            Right (AlreadyWritten _) -> pure ()

  forM_ loyalty.burnDetails $ \burn -> do
    logInfo $ "[loyaltySvc] burn programId=" <> burn.programId <> " options=" <> show (length burn.burnOptions)
    mbProgramType <- resolveProgram burn.programId
    case mbProgramType of
      Nothing ->
        logError $
          "loyalty burn skipped: program "
            <> burn.programId
            <> " not configured for merchant "
            <> merchantId
      Just programType -> do
        let totalPoints = sum (map (.points) burn.burnOptions)
        logInfo $ "[loyaltySvc] burn resolved programType=" <> show programType <> " totalPoints=" <> show totalPoints
        (wallet, _acc) <-
          getOrCreateWalletForPerson personId programType burn.programId currency merchantId mbMerchantOperatingCityId
        logInfo $ "[loyaltySvc] burn wallet walletId=" <> wallet.id.getId
        when (isOrderCharged && totalPoints > 0) $ do
          res <- recordLoyaltyBurn wallet burnRefId totalPoints currency merchantId merchantOperatingCityIdText
          case res of
            Left err -> logError $ "recordLoyaltyBurn failed: " <> show err
            Right (WrittenNew _) -> do
              logInfo $ "[loyaltySvc] burn ledger WrittenNew; bumping aggregates walletId=" <> wallet.id.getId
              bumpWalletAggregatesOnBurn wallet totalPoints
            Right (AlreadyWritten _) -> logInfo "[loyaltySvc] burn ledger AlreadyWritten"
        whenJust burn.reversedPoints $ \rev -> when (rev > 0) $ do
          revRes <- recordLoyaltyBurnReversal wallet burnRefId
          case revRes of
            Left err -> logError $ "recordLoyaltyBurnReversal failed: " <> show err
            Right (WrittenNew _) -> bumpWalletAggregatesOnBurn wallet (negate rev)
            Right (AlreadyWritten _) -> pure ()

  logInfo "[loyaltySvc] earn/burn loop complete; calling fetchFullInfo for reconcile"
  reconcileRes <- try @_ @SomeException fetchFullInfo
  case reconcileRes of
    Left e -> logError $ "loyaltyInfo reconcile fetch failed: " <> show e
    Right loyaltyInfoResp -> do
      logInfo $ "[loyaltySvc] reconcile fetched programs=" <> show (length (fromMaybe [] loyaltyInfoResp.programs))
      forM_ (fromMaybe [] loyaltyInfoResp.programs) $ \program -> do
        mbProgramType <- resolveProgram program.id_
        case mbProgramType of
          Nothing -> pure ()
          Just programType -> do
            (w, _) <- getOrCreateWalletForPerson personId programType program.id_ currency merchantId mbMerchantOperatingCityId
            reconcileWalletFromLoyaltyInfo w loyaltyInfoResp
  logInfo "[loyaltySvc] exit"

createTopupWalletPaymentRow ::
  (FBeamFlow.BeamFlow m r, PBeamFlow.BeamFlow m r) =>
  Text -> -- personId
  Id DOrder.PaymentOrder ->
  Text -> -- programId
  FAccount.CounterpartyType -> -- programType
  HighPrecMoney -> -- anticipated points (often equal to order amount at 1:1)
  Currency ->
  Text -> -- merchantId
  Maybe Text -> -- merchantOperatingCityId
  m DWP.WalletPayments
createTopupWalletPaymentRow personId orderId programId programType points currency merchantId mbMerchantOperatingCityId = do
  (wallet, _) <- getOrCreateWalletForPerson personId programType programId currency merchantId mbMerchantOperatingCityId
  now <- getCurrentTime
  wpId <- Id <$> generateGUID
  let wp =
        DWP.WalletPayments
          { id = wpId,
            walletId = wallet.id,
            personId = personId,
            orderId = orderId,
            programId = programId,
            kind = DWP.TOPUP,
            points = points,
            currency = currency,
            status = DWP.NEW,
            campaignId = Nothing,
            merchantId = merchantId,
            merchantOperatingCityId = mbMerchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
  QWP.create wp
  pure wp

upsertWalletPayment ::
  (PBeamFlow.BeamFlow m r) =>
  DWallet.Wallet ->
  Text -> -- personId
  DOrder.PaymentOrder ->
  Payment.LoyaltyEarnDetail ->
  DWP.WalletPaymentKind ->
  DWP.WalletPaymentStatus ->
  m DWP.WalletPayments
upsertWalletPayment wallet personId order earn kind walletStatus = do
  mbExisting <- QWP.findByOrderAndProgramAndKind order.id earn.programId kind
  now <- getCurrentTime
  case mbExisting of
    Just wp -> do
      if wp.status == walletStatus
        then pure wp
        else do
          QWP.updateStatus walletStatus wp.id
          pure wp {DWP.status = walletStatus, DWP.updatedAt = now}
    Nothing -> do
      wpId <- Id <$> generateGUID
      let firstCampaignId = fmap (.campaignId) (listToMaybe earn.campaigns)
      let wp =
            DWP.WalletPayments
              { id = wpId,
                walletId = wallet.id,
                personId = personId,
                orderId = order.id,
                programId = earn.programId,
                kind = kind,
                points = earn.points,
                currency = order.currency,
                status = walletStatus,
                campaignId = firstCampaignId,
                merchantId = order.merchantId.getId,
                merchantOperatingCityId = (.getId) <$> order.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      QWP.create wp
      pure wp
