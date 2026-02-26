{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PPF
  ( buildMobilityPPFReconEntry,
    buildPPFNetworkObservabilityLog,
    isPPFEnabled,
  )
where

import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import Domain.Types.PPFRecon
import qualified Domain.Types.Ride as DRide
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Common
import qualified TransactionLogs.ONDC.PPFLog as PPFLog

-- | Check if PPF is enabled for the given BecknConfig
isPPFEnabled :: DBC.BecknConfig -> Bool
isPPFEnabled bapConfig = fromMaybe False bapConfig.ppfEnabled

-- | Build a PPFRecon entry for a completed ride (Mobility domain).
-- Called from rideCompletedReqHandler when PPF is enabled.
buildMobilityPPFReconEntry ::
  (MonadFlow m) =>
  DBC.BecknConfig ->
  DRB.Booking ->
  DRide.Ride ->
  Price -> -- totalFare
  m PPFRecon
buildMobilityPPFReconEntry bapConfig booking ride totalFare = do
  reconId <- generateGUID
  now <- getCurrentTime
  let buyerFinderFee = mkPrice (Just totalFare.currency) $ fromMaybe 0 $ (readMaybe . T.unpack) =<< bapConfig.buyerFinderFee
      sellerShareAmount = totalFare `subtractPrice'` buyerFinderFee
      bppOrderId = ride.bppRideId.getId
  pure
    PPFRecon
      { id = reconId,
        domain = MOBILITY,
        networkOrderId = bppOrderId,
        transactionId = booking.transactionId,
        collectorSubscriberId = bapConfig.subscriberId,
        receiverSubscriberId = booking.providerId,
        paymentTransactionId = Nothing, -- populated after payment collection
        paymentReference = Nothing,
        orderAmount = totalFare,
        paymentAmount = totalFare,
        sellerShare = sellerShareAmount,
        buyerAppCommission = buyerFinderFee,
        networkFee = Nothing,
        gstAmount = Nothing,
        withholdingAmount = Nothing,
        tds = Nothing,
        tcs = Nothing,
        orderStatus = "COMPLETED",
        paymentStatus = INITIATED,
        settlementStatus = PENDING,
        settlementRefNo = Nothing,
        settlementAmount = Nothing,
        settlementDate = Nothing,
        fulfilledTimestamp = ride.rideEndTime,
        settledTimestamp = Nothing,
        entityId = booking.id.getId,
        entityType = RIDE_BOOKING,
        collectorIFSC = bapConfig.bapIFSC,
        collectorBankAccount = Nothing,
        beneficiaryIFSC = Nothing, -- populated from BPP data
        beneficiaryBankAccount = Nothing,
        reconInitiatedAt = Nothing,
        reconCompletedAt = Nothing,
        differenceAmount = Nothing,
        message = Nothing,
        merchantId = Just booking.merchantId,
        merchantOperatingCityId = Just booking.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }
  where
    subtractPrice' :: Price -> Price -> Price
    subtractPrice' p1 p2 = mkPrice (Just p1.currency) (max 0 (p1.amount - p2.amount))

-- | Build a PPF Network Observability log from a PPFRecon entry
buildPPFNetworkObservabilityLog :: PPFRecon -> PPFLog.PPFNetworkObservabilityLog
buildPPFNetworkObservabilityLog recon =
  PPFLog.PPFNetworkObservabilityLog
    { networkOrderId = recon.networkOrderId,
      transactionId = recon.transactionId,
      paymentTransactionId = recon.paymentTransactionId,
      paymentReference = recon.paymentReference,
      orderAmount = recon.orderAmount.amount,
      paymentAmount = recon.paymentAmount.amount,
      sellerShare = recon.sellerShare.amount,
      buyerAppCommission = recon.buyerAppCommission.amount,
      logisticsFee = Nothing,
      networkFee = (.amount) <$> recon.networkFee,
      gstAmount = (.amount) <$> recon.gstAmount,
      orderStatus = recon.orderStatus,
      paymentStatus = show recon.paymentStatus,
      settlementStatus = show recon.settlementStatus,
      domain = show recon.domain,
      collectorSubscriberId = recon.collectorSubscriberId,
      receiverSubscriberId = recon.receiverSubscriberId,
      withholdingAmount = (.amount) <$> recon.withholdingAmount,
      settlementRefNo = recon.settlementRefNo,
      currency = recon.orderAmount.currency,
      createdTimestamp = recon.createdAt,
      fulfilledTimestamp = recon.fulfilledTimestamp,
      settledTimestamp = recon.settledTimestamp
    }
