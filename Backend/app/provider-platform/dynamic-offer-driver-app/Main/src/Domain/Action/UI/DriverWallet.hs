{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverWallet (getWalletTransactions, postWalletPayout, postWalletTopup) where

import qualified API.Types.UI.DriverWallet as DriverWallet
import qualified Data.Aeson as A
import Data.List (lookup)
import qualified Data.Text as T
import qualified Data.Time
import Domain.Action.UI.Plan hiding (mkDriverFee)
import Domain.Action.UI.Ride.EndRide.Internal (makeWalletRunningBalanceLockKey)
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverWallet as DriverWallet
import Domain.Types.Extra.Plan
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person as DP
import Domain.Types.VehicleCategory
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payment.Types as Payment
import qualified Kernel.External.Payout.Interface as Juspay
import qualified Kernel.External.Payout.Types as TPayout
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import SharedLogic.Finance.Prepaid (counterpartyDriver)
import SharedLogic.Finance.Wallet
import qualified SharedLogic.Payment as SPayment
import Storage.Cac.TransporterConfig (findByMerchantOpCityId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverFeeExtra as QDFE
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as Payout
import qualified Tools.Utils as Utils

getWalletTransactions ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe DriverWallet.WalletTransactionType ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.Flow DriverWallet.TransactionResponse
  )
getWalletTransactions (mbPersonId, _, _) mbFromDate mbToDate mbTransactionType mbLimit mbOffset = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  now <- getCurrentTime
  let fromDate = fromMaybe (Data.Time.UTCTime (Data.Time.utctDay now) 0) mbFromDate
      toDate = fromMaybe now mbToDate
      limit = min maxLimit . fromMaybe 10 $ mbLimit
      offset = fromMaybe 0 mbOffset
  mbAccount <- getWalletAccountByOwner counterpartyDriver driverId.getId
  case mbAccount of
    Nothing -> pure $ DriverWallet.TransactionResponse {transactions = [], currentBalance = Nothing}
    Just account -> do
      let referenceTypes =
            maybe
              ( rideReferenceTypes
                  <> [ walletReferenceTopup,
                       walletReferencePayout
                     ]
              )
              transactionTypeToRefs
              mbTransactionType
      entries <- findByAccountWithFilters account.id (Just fromDate) (Just toDate) Nothing Nothing Nothing (Just referenceTypes)
      let balanceByEntryId = buildRunningBalances account.id (sortOn (.timestamp) entries)
      let pagedEntries = take limit . drop offset $ sortOn (Down . (.timestamp)) entries
      let rideIds = mapMaybe (\e -> if e.referenceType `elem` rideReferenceTypes then Just (Kernel.Types.Id.Id e.referenceId) else Nothing) pagedEntries
      (rideMap, bookingMap) <- Utils.fetchRideLocationData rideIds
      transactionsAPIEntity <-
        forM pagedEntries $ \entry -> do
          let transactionType = referenceTypeToTransactionType entry.referenceType
          let rideId = if entry.referenceType `elem` rideReferenceTypes then Just (Kernel.Types.Id.Id entry.referenceId) else Nothing
          (fromLocation, toLocation) <- case transactionType of
            DriverWallet.RIDE_TRANSACTION -> Utils.extractLocationFromMaps rideId rideMap bookingMap
            _ -> return (Nothing, Nothing)
          let (collectionAmount, gstDeduction, merchantPayable, driverPayable) =
                case entry.referenceType of
                  ref | ref == walletReferenceBaseRide -> (Nothing, Nothing, Just entry.amount, Nothing)
                  ref | ref == walletReferenceTollCharges -> (Nothing, Nothing, Just entry.amount, Nothing)
                  ref | ref == walletReferenceParkingCharges -> (Nothing, Nothing, Just entry.amount, Nothing)
                  ref | ref == walletReferenceGSTOnline -> (Nothing, Just entry.amount, Nothing, Just entry.amount)
                  ref | ref == walletReferenceGSTCash -> (Nothing, Just entry.amount, Nothing, Just entry.amount)
                  _ -> (Nothing, Nothing, Nothing, Nothing)
          let runningBalance = fromMaybe 0 (lookup entry.id balanceByEntryId)
          return $
            DriverWallet.TransactionDetails
              { id = entry.id,
                rideId = rideId,
                transactionType = transactionType,
                collectionAmount = collectionAmount,
                gstDeduction = gstDeduction,
                merchantPayable = merchantPayable,
                driverPayable = driverPayable,
                runningBalance = runningBalance,
                fromLocation = fromLocation,
                toLocation = toLocation,
                createdAt = entry.timestamp
              }
      mbBalance <- getWalletBalanceByOwner counterpartyDriver driverId.getId
      pure $ DriverWallet.TransactionResponse {transactions = transactionsAPIEntity, currentBalance = mbBalance}
  where
    maxLimit = 10
    transactionTypeToRefs = \case
      DriverWallet.RIDE_TRANSACTION -> rideReferenceTypes
      DriverWallet.TOPUP -> [walletReferenceTopup]
      DriverWallet.PAYOUT -> [walletReferencePayout]
    referenceTypeToTransactionType = \case
      ref | ref `elem` rideReferenceTypes -> DriverWallet.RIDE_TRANSACTION
      ref | ref == walletReferenceTopup -> DriverWallet.TOPUP
      _ -> DriverWallet.PAYOUT
    rideReferenceTypes =
      [ walletReferenceBaseRide,
        walletReferenceGSTOnline,
        walletReferenceGSTCash,
        walletReferenceTollCharges,
        walletReferenceParkingCharges,
        walletReferenceTDSDeductionOnline,
        walletReferenceTDSDeductionCash
      ]
    buildRunningBalances accountId entries =
      let step (accBal, accMap) entry =
            let delta =
                  if entry.fromAccountId == accountId
                    then (-1) * entry.amount
                    else
                      if entry.toAccountId == accountId
                        then entry.amount
                        else 0
                newBal = accBal + delta
             in (newBal, accMap <> [(entry.id, newBal)])
       in snd $ foldl step (0, []) entries

postWalletPayout ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow APISuccess.APISuccess
  )
postWalletPayout (mbPersonId, merchantId, mocId) = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  transporterConfig <- findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  driverInfo <- QDI.findById driverId >>= fromMaybeM DriverInfoNotFound
  subscriptionConfig <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName person.merchantOperatingCityId Nothing PREPAID_SUBSCRIPTION >>= fromMaybeM (NoSubscriptionConfigForService person.merchantOperatingCityId.getId "PREPAID_SUBSCRIPTION") -- Driver wallet is not required for postpaid
  unless transporterConfig.driverWalletConfig.enableWalletPayout $ throwError $ InvalidRequest "Payouts are disabled"
  Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey driverId.getId) 10 10 $ do
    mbAccount <- getWalletAccountByOwner counterpartyDriver driverId.getId
    walletBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterpartyDriver driverId.getId
    let minPayoutAmount = transporterConfig.driverWalletConfig.minimumWalletPayoutAmount
    utcTimeNow <- getCurrentTime
    let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
        localTime = addUTCTime timeDiff utcTimeNow
        localDay = Data.Time.utctDay localTime
        startOfLocalDay = Data.Time.UTCTime localDay 0
        endOfLocalDay = Data.Time.UTCTime localDay 86399
        utcStartOfDay = addUTCTime (negate timeDiff) startOfLocalDay
        utcEndOfDay = addUTCTime (negate timeDiff) endOfLocalDay
    whenJust transporterConfig.driverWalletConfig.maxWalletPayoutsPerDay $ \maxPayoutsPerDay -> do
      payoutsToday <- case mbAccount of
        Nothing -> pure []
        Just account -> findByAccountWithFilters account.id (Just utcStartOfDay) (Just utcEndOfDay) Nothing Nothing Nothing (Just [walletReferencePayout])
      when (length payoutsToday >= maxPayoutsPerDay) $ throwError $ InvalidRequest "Maximum payouts per day reached"
    let mbVpa = driverInfo.payoutVpa
    vpa <- fromMaybeM (InternalError $ "Payout vpa not present for " <> driverId.getId) mbVpa
    payoutId <- generateGUID
    phoneNo <- mapM decrypt person.mobileNumber
    payoutServiceName <- Payout.decidePayoutService (fromMaybe (DEMSC.PayoutService TPayout.Juspay) subscriptionConfig.payoutServiceName) person.clientSdkVersion person.merchantOperatingCityId
    let cutOffDate = Data.Time.addDays (negate (fromIntegral transporterConfig.driverWalletConfig.payoutCutOffDays)) localDay
        utcCutOffTime = addUTCTime (negate timeDiff) (Data.Time.UTCTime cutOffDate 0)
    entriesAfterCutoff <- case mbAccount of
      Nothing -> pure []
      Just account -> findByAccountWithFilters account.id (Just utcCutOffTime) (Just utcTimeNow) Nothing Nothing Nothing (Just [walletReferenceBaseRide])
    let unsettledReceivables = sum $ map (.amount) entriesAfterCutoff
    let payoutableBalance = walletBalance - unsettledReceivables
    when (payoutableBalance < minPayoutAmount) $ throwError $ InvalidRequest ("Minimum payout amount is " <> show minPayoutAmount)
    let createPayoutOrderReq = mkPayoutReq person vpa payoutId phoneNo payoutableBalance
        entityName = DPayment.DRIVER_WALLET_TRANSACTION
        createPayoutOrderCall = Payout.createPayoutOrder person.merchantId person.merchantOperatingCityId payoutServiceName (Just person.id.getId)
    merchantOperatingCity <- CQMOC.findById (Kernel.Types.Id.cast person.merchantOperatingCityId) >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
    logDebug $ "calling create payoutOrder with driverId: " <> driverId.getId <> " | amount: " <> show createPayoutOrderReq.amount <> " | orderId: " <> show payoutId
    when (createPayoutOrderReq.amount > 0.0) $ do
      (_, mbPayoutOrder) <- DPayment.createPayoutService (Kernel.Types.Id.cast person.merchantId) (Just $ Kernel.Types.Id.cast person.merchantOperatingCityId) (Kernel.Types.Id.cast driverId) Nothing (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall
      whenJust mbPayoutOrder $ \payoutOrder -> do
        let metadata =
              A.object
                [ "driverPayable" A..= (-1 * payoutableBalance),
                  "payoutOrderId" A..= payoutOrder.id.getId
                ]
        _ <-
          createWalletEntryDelta
            counterpartyDriver
            driverId.getId
            (negate payoutableBalance)
            transporterConfig.currency
            merchantId.getId
            mocId.getId
            walletReferencePayout
            payoutOrder.id.getId
            (Just metadata)
            >>= fromEitherM (\err -> InternalError ("Failed to create wallet payout entry: " <> show err))
        let notificationTitle = "Payout Initiated"
            notificationMessage = "Your payout of " <> show payoutableBalance <> " has been initiated."
        Notify.sendNotificationToDriver person.merchantOperatingCityId FCM.SHOW Nothing FCM.PAYOUT_INITIATED notificationTitle notificationMessage person person.deviceToken
  pure APISuccess.Success

mkPayoutReq :: DP.Person -> Text -> T.Text -> Maybe Text -> HighPrecMoney -> Juspay.CreatePayoutOrderReq
mkPayoutReq person vpa uid phoneNo amount =
  Juspay.CreatePayoutOrderReq
    { orderId = uid,
      amount = SPayment.roundToTwoDecimalPlaces amount,
      customerPhone = fromMaybe "6666666666" phoneNo, -- dummy no.
      customerEmail = fromMaybe "dummymail@gmail.com" person.email, -- dummy mail
      customerId = person.id.getId,
      orderType = "FULFILL_ONLY",
      remark = "Settlement for wallet",
      customerName = person.firstName,
      customerVpa = vpa,
      isDynamicWebhookRequired = False
    }

postWalletTopup ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    DriverWallet.TopUpRequest ->
    Environment.Flow PlanSubscribeRes
  )
postWalletTopup (mbPersonId, merchantId, mocId) req = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  transporterConfig <- findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  unless transporterConfig.driverWalletConfig.enableWalletTopup $ throwError $ InvalidRequest "Wallet topups are disabled"
  when (req.amount <= 0) $ throwError $ InvalidRequest "Top-up amount must be greater than zero"
  eitherResult <- Redis.whenWithLockRedisAndReturnValue (makeWalletTopupLockKey driverId.getId) 10 $ do
    existingTopUpFee <- QDFE.findLatestByFeeTypeAndStatusWithTotalEarnings DF.WALLET_TOPUP [DF.PAYMENT_PENDING] driverId req.amount
    case existingTopUpFee of
      Just fee -> pure fee
      Nothing -> do
        driverFee <- mkDriverFee driverId req.amount transporterConfig.currency
        QDF.create driverFee
        pure driverFee
  case eitherResult of
    Right fee -> createTopupOrder driverId [fee]
    Left _ -> throwError $ InternalError "Could not acquire lock for wallet topup."
  where
    makeWalletTopupLockKey :: Text -> Text
    makeWalletTopupLockKey driverId = "wallet-topup-lock:" <> driverId

    createTopupOrder driverId driverFees = do
      (createOrderResp, orderId) <-
        SPayment.createOrder
          (driverId, merchantId, mocId)
          (DEMSC.PaymentService Payment.Juspay)
          (driverFees, [])
          Nothing
          INV.WALLET_TOPUP_INVOICE
          Nothing
          []
          Nothing
          False
          (Just DPayment.DRIVER_WALLET_TOPUP)
      return $
        PlanSubscribeRes
          { orderId = orderId,
            orderResp = createOrderResp
          }

    mkDriverFee driverId amount currency = do
      now <- getCurrentTime
      feeId <- generateGUID
      pure $
        DF.DriverFee
          { id = feeId,
            driverId = driverId,
            merchantId = merchantId,
            merchantOperatingCityId = mocId,
            feeType = DF.WALLET_TOPUP,
            status = DF.PAYMENT_PENDING,
            currency,
            platformFee = DF.PlatformFee {fee = 0, cgst = 0, sgst = 0, currency},
            govtCharges = 0,
            specialZoneAmount = 0,
            totalEarnings = amount,
            numRides = 0,
            specialZoneRideCount = 0,
            startTime = now,
            endTime = now,
            payBy = now,
            createdAt = now,
            updatedAt = now,
            serviceName = PREPAID_SUBSCRIPTION,
            vehicleCategory = CAR,
            notificationRetryCount = 0,
            schedulerTryCount = 0,
            overlaySent = False,
            amountPaidByCoin = Nothing,
            autopayPaymentStage = Nothing,
            badDebtDeclarationDate = Nothing,
            badDebtRecoveryDate = Nothing,
            billNumber = Nothing,
            collectedAt = Nothing,
            collectedBy = Nothing,
            feeWithoutDiscount = Nothing,
            hasSibling = Nothing,
            offerId = Nothing,
            planId = Nothing,
            planMode = Nothing,
            planOfferTitle = Nothing,
            refundEntityId = Nothing,
            refundedAmount = Nothing,
            refundedAt = Nothing,
            refundedBy = Nothing,
            siblingFeeId = Nothing,
            splitOfDriverFeeId = Nothing,
            stageUpdatedAt = Nothing,
            validDays = Nothing,
            vehicleNumber = Nothing,
            cancellationPenaltyAmount = Nothing,
            addedToFeeId = Nothing,
            collectedAtVendorId = Nothing
          }
