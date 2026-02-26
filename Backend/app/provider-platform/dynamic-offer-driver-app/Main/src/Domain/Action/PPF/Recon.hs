{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.PPF.Recon
  ( processRecon,
  )
where

import qualified Beckn.ACL.Recon as ACLRecon
import qualified Data.Text as T
import Domain.Types.PPFRecon
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Storage.Queries.PPFRecon as QPPFRecon

-- | Process incoming recon request from BAP (Collector).
-- Validates against local ride/order records and creates BPP-side recon entries.
-- Returns list of (recon entry, status) for building on_recon response.
processRecon ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Text -> -- BPP's own subscriber ID (from BecknConfig)
  ACLRecon.DReconReq ->
  m [(PPFRecon, Text)]
processRecon bppSubscriberId reconReq = do
  results <- forM reconReq.orderEntries $ \entry -> do
    logInfo $ "Processing recon for orderId: " <> entry.orderId
    -- Idempotency: check if we already have a recon entry for this order
    mbExisting <- QPPFRecon.findByNetworkOrderId entry.orderId
    case mbExisting of
      Just existing -> do
        logInfo $ "Recon entry already exists for orderId: " <> entry.orderId
        pure (existing, "ACCEPTED")
      Nothing -> do
        reconEntry <- buildBPPReconEntry bppSubscriberId reconReq entry
        QPPFRecon.create reconEntry
        logInfo $ "Created BPP-side recon entry for orderId: " <> entry.orderId
        -- TODO: Validate against local ride records for amount matching
        pure (reconEntry, "ACCEPTED")
  pure results

-- | Build a BPP-side PPFRecon entry from incoming BAP recon data.
-- The BPP subscriber ID is passed in from the caller's BecknConfig.
buildBPPReconEntry ::
  (MonadFlow m) =>
  Text -> -- BPP subscriber ID
  ACLRecon.DReconReq ->
  ACLRecon.DReconOrderEntry ->
  m PPFRecon
buildBPPReconEntry bppSubscriberId reconReq entry = do
  reconId <- generateGUID
  now <- getCurrentTime
  currency <- parseCurrency entry.currency
  let mkPriceFromAmount amt = mkPrice (Just currency) amt
  domainParsed <- parsePPFDomain reconReq.domain
  let entityTypeParsed = case domainParsed of
        FRFS -> FRFS_TICKET_BOOKING
        _ -> RIDE_BOOKING
  pure
    PPFRecon
      { id = reconId,
        domain = domainParsed,
        networkOrderId = entry.orderId,
        transactionId = entry.transactionId,
        collectorSubscriberId = reconReq.collectorSubscriberId,
        receiverSubscriberId = bppSubscriberId,
        paymentTransactionId = entry.paymentTransactionId,
        paymentReference = entry.paymentReference,
        orderAmount = mkPriceFromAmount entry.orderAmount,
        paymentAmount = mkPriceFromAmount entry.paymentAmount,
        sellerShare = mkPriceFromAmount entry.sellerShare,
        buyerAppCommission = mkPriceFromAmount entry.buyerAppCommission,
        networkFee = Nothing,
        gstAmount = Nothing,
        withholdingAmount = Nothing,
        tds = Nothing,
        tcs = Nothing,
        orderStatus = entry.orderStatus,
        paymentStatus = INITIATED,
        settlementStatus = PENDING,
        settlementRefNo = Nothing,
        settlementAmount = Nothing,
        settlementDate = Nothing,
        fulfilledTimestamp = Nothing,
        settledTimestamp = Nothing,
        entityId = entry.orderId,
        entityType = entityTypeParsed,
        collectorIFSC = Nothing,
        collectorBankAccount = Nothing,
        beneficiaryIFSC = Nothing,
        beneficiaryBankAccount = Nothing,
        reconInitiatedAt = Just now,
        reconCompletedAt = Nothing,
        differenceAmount = Nothing,
        message = Nothing,
        merchantId = Nothing, -- TODO: Resolve from ride lookup via QRide.findByBPPRideId
        merchantOperatingCityId = Nothing,
        createdAt = now,
        updatedAt = now
      }

-- | Parse currency text with warning on failure (defaults to INR)
parseCurrency :: (MonadFlow m) => Text -> m Currency
parseCurrency currencyText =
  case readMaybe (T.unpack currencyText) of
    Just c -> pure c
    Nothing -> do
      logWarning $ "Invalid currency in recon: " <> currencyText <> ", defaulting to INR"
      pure INR

-- | Parse PPF domain text with warning on unrecognized domain
parsePPFDomain :: (MonadFlow m) => Text -> m PPFDomain
parsePPFDomain "MOBILITY" = pure MOBILITY
parsePPFDomain "FRFS" = pure FRFS
parsePPFDomain other = do
  logWarning $ "Unknown PPF domain: " <> other <> ", defaulting to MOBILITY"
  pure MOBILITY
