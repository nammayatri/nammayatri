{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverWallet (getWalletTransactions, postWalletPayout) where

import qualified API.Types.UI.DriverWallet
import qualified Data.Text as T
import qualified Data.Time
import Domain.Action.UI.Ride.EndRide.Internal (makeWalletRunningBalanceLockKey)
import qualified Domain.Types.DriverWallet as DW
import Domain.Types.Extra.Plan
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payout.Interface as Juspay
import qualified Kernel.External.Payout.Types as TPayout
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import SharedLogic.Payment
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverWallet as QDW
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Payout as Payout
import qualified Tools.Utils as Utils

getWalletTransactions ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe DW.TransactionType ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.Flow API.Types.UI.DriverWallet.TransactionResponse
  )
getWalletTransactions (mbPersonId, _, _) mbFromDate mbToDate mbTransactionType mbLimit mbOffset = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  driverInfo <- QDI.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  now <- getCurrentTime
  let fromDate = fromMaybe (Data.Time.UTCTime (Data.Time.utctDay now) 0) mbFromDate
      toDate = fromMaybe now mbToDate
      limit = min maxLimit . fromMaybe 10 $ mbLimit
      offset = fromMaybe 0 mbOffset
  transactions <- QDW.findAllByDriverIdRangeAndTransactionType driverId fromDate toDate mbTransactionType limit offset
  let rideIds = mapMaybe (\dw -> if dw.transactionType == DW.RIDE_TRANSACTION then dw.rideId else Nothing) transactions
  (rideMap, bookingMap) <- Utils.fetchRideLocationData rideIds
  transactionsAPIEntity <-
    forM transactions $ \dw -> do
      (fromLocation, toLocation) <- case dw.transactionType of
        DW.RIDE_TRANSACTION -> Utils.extractLocationFromMaps dw.rideId rideMap bookingMap
        _ -> return (Nothing, Nothing)
      return $
        API.Types.UI.DriverWallet.TransactionDetails
          { id = dw.id,
            rideId = dw.rideId,
            transactionType = dw.transactionType,
            collectionAmount = dw.collectionAmount,
            gstDeduction = dw.gstDeduction,
            merchantPayable = dw.merchantPayable,
            driverPayable = dw.driverPayable,
            runningBalance = dw.runningBalance,
            fromLocation = fromLocation,
            toLocation = toLocation,
            createdAt = dw.createdAt
          }
  pure $ API.Types.UI.DriverWallet.TransactionResponse {transactions = transactionsAPIEntity, currentBalance = driverInfo.walletBalance}
  where
    maxLimit = 10

postWalletPayout ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow APISuccess.APISuccess
  )
postWalletPayout (mbPersonId, _, mocId) = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey driverId.getId) 10 10 $ do
    driverInfo <- QDI.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    let walletBalance = fromMaybe 0 driverInfo.walletBalance
    when (walletBalance <= 0) $ throwError $ InvalidRequest "No payout required as wallet balance is less than or equal to zero"
    person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    subscriptionConfig <-
      CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName person.merchantOperatingCityId Nothing PREPAID_SUBSCRIPTION -- Driver wallet is not required for postpaid
        >>= fromMaybeM (NoSubscriptionConfigForService person.merchantOperatingCityId.getId "PREPAID_SUBSCRIPTION")
    let mbVpa = driverInfo.payoutVpa
    vpa <- fromMaybeM (InternalError $ "payout vpa not present for " <> driverId.getId) mbVpa
    payoutId <- generateGUID
    phoneNo <- mapM decrypt person.mobileNumber
    payoutServiceName <- Payout.decidePayoutService (fromMaybe (DEMSC.PayoutService TPayout.Juspay) subscriptionConfig.payoutServiceName) person.clientSdkVersion person.merchantOperatingCityId
    let createPayoutOrderReq = mkPayoutReq person vpa payoutId phoneNo walletBalance
        entityName = DPayment.DRIVER_WALLET_TRANSACTION
        createPayoutOrderCall = Payout.createPayoutOrder person.merchantId person.merchantOperatingCityId payoutServiceName (Just person.id.getId)
    merchantOperatingCity <- CQMOC.findById (Kernel.Types.Id.cast person.merchantOperatingCityId) >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
    logDebug $ "calling create payoutOrder with driverId: " <> driverId.getId <> " | amount: " <> show createPayoutOrderReq.amount <> " | orderId: " <> show payoutId
    when (createPayoutOrderReq.amount > 0.0) $ do
      (_, mbPayoutOrder) <- DPayment.createPayoutService (Kernel.Types.Id.cast person.merchantId) (Just $ Kernel.Types.Id.cast person.merchantOperatingCityId) (Kernel.Types.Id.cast driverId) Nothing (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall
      whenJust mbPayoutOrder $ \payoutOrder -> do
        now <- getCurrentTime
        newId <- generateGUID
        let transaction =
              DW.DriverWallet
                { id = newId,
                  merchantId = driverInfo.merchantId,
                  merchantOperatingCityId = mocId,
                  driverId = driverId,
                  rideId = Nothing,
                  transactionType = DW.PAYOUT,
                  collectionAmount = Nothing,
                  gstDeduction = Nothing,
                  merchantPayable = Nothing,
                  driverPayable = Just (-1 * walletBalance),
                  runningBalance = 0,
                  payoutOrderId = Just payoutOrder.id,
                  payoutStatus = Just DW.INITIATED,
                  createdAt = now,
                  updatedAt = now
                }
        QDW.create transaction
        QDI.updateWalletBalance (Just 0) driverId
  pure APISuccess.Success

mkPayoutReq :: Domain.Types.Person.Person -> Text -> T.Text -> Maybe Text -> HighPrecMoney -> Juspay.CreatePayoutOrderReq
mkPayoutReq person vpa uid phoneNo amount =
  Juspay.CreatePayoutOrderReq
    { orderId = uid,
      amount = roundToTwoDecimalPlaces amount,
      customerPhone = fromMaybe "6666666666" phoneNo, -- dummy no.
      customerEmail = fromMaybe "dummymail@gmail.com" person.email, -- dummy mail
      customerId = person.id.getId,
      orderType = "FULFILL_ONLY",
      remark = "Settlement for wallet",
      customerName = person.firstName,
      customerVpa = vpa,
      isDynamicWebhookRequired = False
    }
