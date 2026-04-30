module Lib.Payment.Wallet.Service
  ( LoyaltyReferenceType (..),
    loyaltyReferenceTypeText,
    getOrCreateWalletForPerson,
    mapOrderStatusToWalletStatus,
    LedgerWriteOutcome (..),
    recordLoyaltyHistory,
    recordLoyaltyHistoryReversal,
    bumpWalletAggregatesOnEarn,
    bumpWalletAggregatesOnBurn,
    reconcileWalletFromLoyaltyInfo,
    processLoyaltyInfoFromOrderStatus,
    getOrCreateWalletPayment,
  )
where

import qualified Data.Text as T
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Wallet.Interface.Types as WalletTypes
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Account.Service as FAccountSvc
import qualified Lib.Finance.Domain.Types.Account as FAccount
import qualified Lib.Finance.Domain.Types.LedgerEntry as FLE
import Lib.Finance.Error.Types (FinanceError (..), LedgerErrorCode (..))
import qualified Lib.Finance.FinanceM as Finance
import qualified Lib.Finance.Ledger.Service as FLedger
import qualified Lib.Finance.Storage.Beam.BeamFlow as FBeamFlow
import qualified Lib.Finance.Storage.Queries.Account as QFAccount
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.Wallet as DWallet
import qualified Lib.Payment.Domain.Types.WalletHistory as DWH
import qualified Lib.Payment.Domain.Types.WalletPayments as DWP
import qualified Lib.Payment.Storage.Beam.BeamFlow as PBeamFlow
import qualified Lib.Payment.Storage.Queries.Wallet as QWallet
import qualified Lib.Payment.Storage.Queries.WalletHistory as QWH
import qualified Lib.Payment.Storage.Queries.WalletPayments as QWP

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
getOrCreateWalletForPerson personId programType programId currency merchantId mbMerchantOperatingCityId =
  Redis.withLockRedisAndReturnValue lockKey 10 $ do
    mbWallet <- QWallet.findByPersonAndProgram personId programType
    case mbWallet of
      Just w -> do
        acc <-
          QFAccount.findById w.accountId
            >>= fromMaybeM
              ( InternalError $
                  "Loyalty wallet " <> w.id.getId
                    <> " references missing accountId "
                    <> w.accountId.getId
              )
        pure (w, acc)
      Nothing -> createAccountAndWallet
  where
    lockKey = "Loyalty:Wallet:GetOrCreate:Person:" <> personId <> ":Program:" <> show programType
    createAccountAndWallet = do
      now <- getCurrentTime
      walletId <- Id <$> generateGUID
      let accountInput =
            FAccountSvc.AccountInput
              { accountType = FAccount.Liability,
                counterpartyType = Just programType,
                counterpartyId = Just personId,
                currency = currency,
                merchantId = merchantId,
                merchantOperatingCityId = fromMaybe "" mbMerchantOperatingCityId
              }
      account <-
        FAccountSvc.getOrCreateAccount accountInput
          >>= either
            ( \err ->
                throwError . InternalError $
                  "Failed to get/create finance account for loyalty wallet (person="
                    <> personId
                    <> "): "
                    <> show err
            )
            pure
      let wallet =
            DWallet.Wallet
              { id = walletId,
                personId = personId,
                programType = programType,
                programId = programId,
                accountId = account.id,
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

mkLoyaltyFinanceCtx :: DWallet.Wallet -> Text -> Finance.FinanceCtx
mkLoyaltyFinanceCtx wallet refId =
  Finance.FinanceCtx
    { merchantId = wallet.merchantId,
      merchantOpCityId = fromMaybe "" wallet.merchantOperatingCityId,
      currency = wallet.currency,
      isOnline = True,
      counterpartyType = wallet.programType,
      counterpartyId = wallet.personId,
      referenceId = refId,
      merchantName = Nothing,
      merchantShortId = Nothing,
      issuedByAddress = Nothing,
      supplierName = Nothing,
      supplierGSTIN = Nothing,
      supplierVatNumber = Nothing,
      supplierAddress = Nothing,
      merchantGstin = Nothing,
      merchantVatNumber = Nothing,
      supplierId = Nothing,
      panOfParty = Nothing,
      panType = Nothing,
      tdsRateReason = Nothing,
      emitLedgerEntries = True
    }

data LedgerWriteOutcome
  = WrittenNew (Id FLE.LedgerEntry)
  | AlreadyWritten (Id FLE.LedgerEntry)
  | Skipped
  deriving (Eq, Show, Generic)

runLoyaltyTransfer ::
  (FBeamFlow.BeamFlow m r, MonadFlow m) =>
  DWallet.Wallet ->
  Text -> -- referenceId for the FinanceCtx (== ledger entry's reference_id)
  Finance.FinanceM m (Maybe (Id FLE.LedgerEntry)) ->
  m (Either FinanceError LedgerWriteOutcome)
runLoyaltyTransfer wallet refId action = do
  result <- Finance.runFinance (mkLoyaltyFinanceCtx wallet refId) action
  case result of
    Left err -> pure (Left err)
    Right (Just entryId, _) -> pure (Right (WrittenNew entryId))
    Right (Nothing, _) -> do
      logError $ "loyalty transfer skipped refId=" <> refId <> " (amount <= 0 or ledger entries disabled)"
      pure (Right Skipped)

-- | Pick (refType, source, dest) for a wallet history row.
historyLedgerLegs ::
  DWP.WalletPaymentKind ->
  (LoyaltyReferenceType, Finance.AccountRole, Finance.AccountRole)
historyLedgerLegs = \case
  DWP.TOPUP -> (LOYALTY_EARN_TOPUP, Finance.BuyerAsset, Finance.OwnerLiability)
  DWP.CASHBACK -> (LOYALTY_EARN_CASHBACK, Finance.BuyerExpense, Finance.OwnerLiability)
  DWP.BURN -> (LOYALTY_BURN, Finance.OwnerLiability, Finance.BuyerAsset)

findEntriesForWallet ::
  (FBeamFlow.BeamFlow m r) =>
  DWP.WalletPaymentKind ->
  Text -> -- refType
  Text -> -- refId
  Id FAccount.Account ->
  m [FLE.LedgerEntry]
findEntriesForWallet DWP.BURN = FLedger.getEntriesByReferenceAndFromAccount
findEntriesForWallet _ = FLedger.getEntriesByReferenceAndToAccount

recordLoyaltyHistory ::
  (FBeamFlow.BeamFlow m r, MonadFlow m) =>
  DWallet.Wallet ->
  DWP.WalletPaymentKind ->
  HighPrecMoney -> -- aggregated points across this (program, kind) for the transaction
  Text -> -- domainEntityId (used as refId)
  m (Either FinanceError LedgerWriteOutcome)
recordLoyaltyHistory wallet kind points domainEntityId = do
  let (refTypeEnum, source, dest) = historyLedgerLegs kind
      refType = loyaltyReferenceTypeText refTypeEnum
  entriesForWallet <- findEntriesForWallet kind refType domainEntityId wallet.accountId
  case nonReversalEntries entriesForWallet of
    (e : _) -> pure (Right (AlreadyWritten e.id))
    [] ->
      runLoyaltyTransfer
        wallet
        domainEntityId
        (Finance.transfer source dest points refType)

recordLoyaltyHistoryReversal ::
  (FBeamFlow.BeamFlow m r) =>
  DWallet.Wallet ->
  DWP.WalletPaymentKind ->
  Text -> -- domainEntityId
  m (Either FinanceError LedgerWriteOutcome)
recordLoyaltyHistoryReversal wallet kind domainEntityId = do
  let (refTypeEnum, _, _) = historyLedgerLegs kind
      refType = loyaltyReferenceTypeText refTypeEnum
  entriesForWallet <- findEntriesForWallet kind refType domainEntityId wallet.accountId
  case nonReversalEntries entriesForWallet of
    [] -> pure (Left $ LedgerError InvalidReversal ("No original entry to reverse for refId=" <> domainEntityId <> " kind=" <> show kind))
    (original : _) ->
      case findReversalOf original.id entriesForWallet of
        Just rev -> pure (Right (AlreadyWritten rev.id))
        Nothing -> do
          res <- FLedger.createReversal original.id "Juspay loyalty entry reversed"
          pure $ fmap (WrittenNew . (.id)) res

nonReversalEntries :: [FLE.LedgerEntry] -> [FLE.LedgerEntry]
nonReversalEntries = filter (\e -> e.entryType /= FLE.Reversal)

findReversalOf :: Id FLE.LedgerEntry -> [FLE.LedgerEntry] -> Maybe FLE.LedgerEntry
findReversalOf originalId =
  find (\e -> e.entryType == FLE.Reversal && e.reversalOf == Just originalId)

walletAggregateLockKey :: Id DWallet.Wallet -> Text
walletAggregateLockKey wid = "Loyalty:Wallet:UpdateAggregates:" <> wid.getId

bumpWalletAggregatesOnEarn ::
  (PBeamFlow.BeamFlow m r) =>
  Id DWallet.Wallet ->
  DWP.WalletPaymentKind ->
  HighPrecMoney ->
  m ()
bumpWalletAggregatesOnEarn walletId kind points =
  Redis.withLockRedisAndReturnValue (walletAggregateLockKey walletId) 10 $ do
    wallet <-
      QWallet.findByPrimaryKey walletId
        >>= fromMaybeM (InternalError $ "Wallet not found while bumping earn aggregates: " <> walletId.getId)
    let newAvailable = wallet.availableBalance + points
        newLifetimeEarned = wallet.lifetimeEarned + points
        newTopup = if kind == DWP.TOPUP then wallet.topupEarned + points else wallet.topupEarned
        newCashback = if kind == DWP.CASHBACK then wallet.cashbackEarned + points else wallet.cashbackEarned
    QWallet.updateAggregatesOnEarn newAvailable newLifetimeEarned newTopup newCashback walletId

bumpWalletAggregatesOnBurn ::
  (PBeamFlow.BeamFlow m r) =>
  Id DWallet.Wallet ->
  HighPrecMoney ->
  m ()
bumpWalletAggregatesOnBurn walletId points =
  Redis.withLockRedisAndReturnValue (walletAggregateLockKey walletId) 10 $ do
    wallet <-
      QWallet.findByPrimaryKey walletId
        >>= fromMaybeM (InternalError $ "Wallet not found while bumping burn aggregates: " <> walletId.getId)
    let newAvailable = wallet.availableBalance - points
        newLifetimeBurned = wallet.lifetimeBurned + points
    QWallet.updateAggregatesOnBurn newAvailable newLifetimeBurned walletId

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
        logError $
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
  Text -> -- domainEntityId (e.g. frfs_ticket_booking_payment.id; caller falls back to orderId when absent) — stamped on each WalletHistory row
  Payment.LoyaltyInfo ->
  (Text -> m (Maybe FAccount.CounterpartyType)) -> -- resolveProgram
  m WalletTypes.LoyaltyInfoResponse -> -- fetchFullInfo
  m ()
processLoyaltyInfoFromOrderStatus personId order domainEntityId loyalty resolveProgram fetchFullInfo = do
  let merchantId = order.merchantId.getId
      mbMerchantOperatingCityId = (.getId) <$> order.merchantOperatingCityId
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

  let payloadTotalEarned = sum (map (.points) loyalty.earnDetails)
      payloadTotalBurned = sum [sum (map (.points) burn.burnOptions) | burn <- loyalty.burnDetails]
  wp <-
    getOrCreateWalletPayment
      personId
      order.id
      currency
      merchantId
      mbMerchantOperatingCityId
      walletStatus
      payloadTotalEarned
      payloadTotalBurned
      (Just domainEntityId)

  forM_ loyalty.earnDetails $ \earn -> do
    mbProgramType <- resolveProgram earn.programId
    case mbProgramType of
      Nothing ->
        logError $
          "loyalty earn skipped: program "
            <> earn.programId
            <> " not configured for merchant "
            <> merchantId
      Just programType -> do
        (wallet, _acc) <-
          getOrCreateWalletForPerson personId programType earn.programId currency merchantId mbMerchantOperatingCityId
        let kind = if isTopup then DWP.TOPUP else DWP.CASHBACK
            buckets =
              if null earn.campaigns
                then [(Nothing, earn.points, earn.reversedPoints)]
                else map (\c -> (c.campaignId, c.points, c.reversedPoints)) earn.campaigns
            mbBenefit = case kind of
              DWP.TOPUP -> Just (max 0 (earn.points - order.amount))
              DWP.CASHBACK -> Nothing
              DWP.BURN -> Nothing
        forM_ buckets $ \(mbCampaignId, pts, mbBucketReversed) ->
          void $ upsertWalletHistory wp wallet programType kind mbCampaignId pts mbBucketReversed mbBenefit domainEntityId
        processProgramLedger wallet kind earn.points domainEntityId isOrderCharged earn.reversedPoints

  forM_ loyalty.burnDetails $ \burn -> do
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
        when (totalPoints > 0) $ do
          (wallet, _acc) <-
            getOrCreateWalletForPerson personId programType burn.programId currency merchantId mbMerchantOperatingCityId
          void $ upsertWalletHistory wp wallet programType DWP.BURN Nothing totalPoints burn.reversedPoints Nothing domainEntityId
          processProgramLedger wallet DWP.BURN totalPoints domainEntityId isOrderCharged burn.reversedPoints

  reconcileRes <- try @_ @SomeException fetchFullInfo
  case reconcileRes of
    Left e -> logError $ "loyaltyInfo reconcile fetch failed: " <> show e
    Right loyaltyInfoResp ->
      forM_ (fromMaybe [] loyaltyInfoResp.programs) $ \program -> do
        mbProgramType <- resolveProgram program.id_
        case mbProgramType of
          Nothing -> pure ()
          Just programType -> do
            (w, _) <- getOrCreateWalletForPerson personId programType program.id_ currency merchantId mbMerchantOperatingCityId
            reconcileWalletFromLoyaltyInfo w loyaltyInfoResp

getOrCreateWalletPayment ::
  (PBeamFlow.BeamFlow m r, Log m) =>
  Text -> -- personId
  Id DOrder.PaymentOrder ->
  Currency ->
  Text -> -- merchantId
  Maybe Text -> -- merchantOperatingCityId
  DWP.WalletPaymentStatus ->
  HighPrecMoney -> -- payloadTotalEarned (seed value used only when creating a new WP)
  HighPrecMoney -> -- payloadTotalBurned
  Maybe Text -> -- domainEntityId
  m DWP.WalletPayments
getOrCreateWalletPayment personId orderId currency merchantId mbMerchantOperatingCityId walletStatus payloadTotalEarned payloadTotalBurned mbDomainEntityId =
  Redis.withLockRedisAndReturnValue lockKey 10 $ do
    mbExisting <- QWP.findByOrderId orderId
    case mbExisting of
      Just wp -> do
        when (wp.status /= walletStatus) $ QWP.updateStatus walletStatus wp.id
        pure wp {DWP.status = walletStatus}
      Nothing -> do
        now <- getCurrentTime
        wpId <- Id <$> generateGUID
        let wp =
              DWP.WalletPayments
                { id = wpId,
                  personId = personId,
                  orderId = orderId,
                  currency = currency,
                  status = walletStatus,
                  totalEarned = payloadTotalEarned,
                  totalBurned = payloadTotalBurned,
                  domainEntityId = mbDomainEntityId,
                  merchantId = merchantId,
                  merchantOperatingCityId = mbMerchantOperatingCityId,
                  createdAt = now,
                  updatedAt = now
                }
        QWP.create wp
        pure wp
  where
    lockKey = "Loyalty:WalletPayments:GetOrCreate:Order:" <> orderId.getId

upsertWalletHistory ::
  (PBeamFlow.BeamFlow m r, Log m) =>
  DWP.WalletPayments ->
  DWallet.Wallet ->
  FAccount.CounterpartyType -> -- programType
  DWP.WalletPaymentKind ->
  Maybe Text -> -- campaignId
  HighPrecMoney ->
  Maybe HighPrecMoney -> -- reversedPoints
  Maybe HighPrecMoney -> -- benefitValue
  Text -> -- domainEntityId
  m DWH.WalletHistory
upsertWalletHistory wp wallet programType kind mbCampaignId points mbReversed mbBenefit domainEntityId = do
  mbExisting <- QWH.findByWalletPaymentsAndProgramAndKindAndCampaign wp.id programType kind mbCampaignId
  now <- getCurrentTime
  case mbExisting of
    Just existing -> do
      when (existing.points /= points || existing.reversedPoints /= mbReversed || existing.benefitValue /= mbBenefit) $
        QWH.updatePointsAndBenefit points mbReversed mbBenefit existing.id
      pure existing {DWH.points = points, DWH.reversedPoints = mbReversed, DWH.benefitValue = mbBenefit, DWH.updatedAt = now}
    Nothing -> do
      whId <- Id <$> generateGUID
      let wh =
            DWH.WalletHistory
              { id = whId,
                walletPaymentsId = wp.id,
                walletId = wallet.id,
                programType = programType,
                kind = kind,
                campaignId = mbCampaignId,
                points = points,
                reversedPoints = mbReversed,
                benefitValue = mbBenefit,
                domainEntityId = domainEntityId,
                merchantId = wp.merchantId,
                merchantOperatingCityId = wp.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      QWH.create wh
      pure wh

processProgramLedger ::
  (FBeamFlow.BeamFlow m r, PBeamFlow.BeamFlow m r, Log m, MonadFlow m) =>
  DWallet.Wallet ->
  DWP.WalletPaymentKind ->
  HighPrecMoney -> -- aggregated points
  Text -> -- domainEntityId
  Bool -> -- isOrderCharged
  Maybe HighPrecMoney -> -- reversedPoints
  m ()
processProgramLedger wallet kind points domainEntityId isOrderCharged mbReversed = do
  when isOrderCharged $ do
    res <- recordLoyaltyHistory wallet kind points domainEntityId
    case res of
      Left err -> logError $ "recordLoyaltyHistory failed refId=" <> domainEntityId <> " kind=" <> show kind <> ": " <> show err
      Right (WrittenNew _) -> do
        logInfo $ "[loyaltySvc] ledger entry refId=" <> domainEntityId <> " kind=" <> show kind <> " points=" <> show points
        applyAggregateBump kind points
      Right (AlreadyWritten _) -> pure ()
      Right Skipped -> pure ()
  whenJust mbReversed $ \rev -> when (rev > 0) $ do
    revRes <- recordLoyaltyHistoryReversal wallet kind domainEntityId
    case revRes of
      Left err -> logError $ "recordLoyaltyHistoryReversal failed refId=" <> domainEntityId <> " kind=" <> show kind <> ": " <> show err
      Right (WrittenNew _) -> do
        logInfo $ "[loyaltySvc] ledger reversal refId=" <> domainEntityId <> " kind=" <> show kind <> " rev=" <> show rev
        applyAggregateBump kind (negate rev)
      Right (AlreadyWritten _) -> pure ()
      Right Skipped -> pure ()
  where
    applyAggregateBump DWP.BURN amt = bumpWalletAggregatesOnBurn wallet.id amt
    applyAggregateBump k amt = bumpWalletAggregatesOnEarn wallet.id k amt
