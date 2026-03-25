
{-
  Finance.Recon.Service

  Domain-agnostic PPF reconciliation processing.
  Compares the buyer's reported settled amount against the actual fare
  (looked up via a caller-provided callback) and determines recon accord.

  The accord decision:
    - If actual fare matches the settled amount (within tolerance) -> accord = True
    - If actual fare differs -> accord = False, expected amount set to actual fare
    - If no local fare record exists -> accord = True (cannot validate)
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Recon.Service
  ( processRecon,
    processReconOrder,
    defaultTolerance,
    module Lib.Finance.Recon.Interface,
  )
where

import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.PPFRecon
import Lib.Finance.Error.Types (FinanceError (..), ReconErrorCode (..))
import Lib.Finance.Recon.Interface
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.PPFRecon as QPPFRecon

defaultTolerance :: HighPrecMoney
defaultTolerance = 0.01

-- | Process an incoming recon request.
-- Returns list of Either FinanceError (PPFRecon, Bool).
processRecon ::
  (BeamFlow.BeamFlow m r) =>
  FareLookup m ->
  HighPrecMoney ->
  ReconInput ->
  m [Either FinanceError (PPFRecon, Bool)]
processRecon lookupFare tolerance reconInput = do
  forM reconInput.orderEntries $ \entry -> do
    logInfo $ "Processing recon for orderId: " <> entry.orderId
    processReconOrder lookupFare tolerance reconInput entry

-- | Process a single recon order entry.
processReconOrder ::
  (BeamFlow.BeamFlow m r) =>
  FareLookup m ->
  HighPrecMoney ->
  ReconInput ->
  ReconOrderInput ->
  m (Either FinanceError (PPFRecon, Bool))
processReconOrder lookupFare tolerance reconInput entry = do
  mbExisting <- QPPFRecon.findByNetworkOrderId entry.orderId
  case mbExisting of
    Just existing -> do
      logInfo $ "Recon entry already exists for orderId: " <> entry.orderId
      let accord =
            maybe True (== entry.orderAmountSettled) (existing.orderAmount.expected)
              || isNothing existing.orderAmount.expected
      pure $ Right (existing, accord)
    Nothing -> do
      mbActualFare <- lookupFare entry.orderId
      let reconResult = compareAmounts tolerance entry.orderAmountSettled mbActualFare
      buildResult <- buildReconEntry reconInput entry reconResult
      case buildResult of
        Left err -> pure $ Left err
        Right reconEntry -> do
          QPPFRecon.create reconEntry
          logInfo $
            "Created recon entry for orderId: " <> entry.orderId
              <> ", accord: "
              <> show reconResult.accord
          pure $ Right (reconEntry, reconResult.accord)

-- | Compare the reported settled amount with the actual fare.
compareAmounts ::
  HighPrecMoney ->
  HighPrecMoney ->
  Maybe HighPrecMoney ->
  ReconResult
compareAmounts _ _ Nothing =
  ReconResult
    { accord = True,
      expectedAmount = Nothing,
      message = Just "No local fare record found; accepted by default"
    }
compareAmounts tolerance settledAmount (Just actualFare)
  | abs (settledAmount - actualFare) <= tolerance =
    ReconResult
      { accord = True,
        expectedAmount = Nothing,
        message = Nothing
      }
  | otherwise =
    ReconResult
      { accord = False,
        expectedAmount = Just actualFare,
        message =
          Just $
            "Fare mismatch: settled="
              <> show settledAmount
              <> " actual="
              <> show actualFare
      }

-- | Build a PPFRecon entry from the order input and comparison result.
buildReconEntry ::
  (MonadFlow m) =>
  ReconInput ->
  ReconOrderInput ->
  ReconResult ->
  m (Either FinanceError PPFRecon)
buildReconEntry reconInput entry reconResult = do
  case parsePPFDomain reconInput.domain of
    Left err -> do
      logError $ show err
      pure $ Left err
    Right domainParsed -> do
      reconId <- generateGUID
      now <- getCurrentTime
      let firstSettlement = listToMaybe entry.settlements
          commissionSettled = fromMaybe 0 $ firstSettlement >>= (.commission)
      pure $
        Right
          PPFRecon
            { id = Id reconId,
              domain = domainParsed,
              networkOrderId = entry.orderId,
              currency = reconInput.currency,
              transactionId = entry.orderId,
              collectorSubscriberId = reconInput.collectorSubscriberId,
              receiverSubscriberId = reconInput.receiverSubscriberId,
              orderAmount =
                ReconSettlementAmount
                  { settled = entry.orderAmountSettled,
                    expected = reconResult.expectedAmount
                  },
              buyerAppCommission =
                ReconSettlementAmount
                  { settled = commissionSettled,
                    expected = Nothing
                  },
              networkFee = Nothing,
              gstAmount = Nothing,
              settlementStatus = PENDING,
              utr = Nothing,
              settlementDate = Nothing,
              settlementId = fromMaybe "" $ firstSettlement >>= (.settlementId),
              merchantId = reconInput.merchantId,
              merchantOperatingCityId = reconInput.merchantOperatingCityId,
              createdAt = now,
              updatedAt = now
            }

parsePPFDomain :: Text -> Either FinanceError PPFDomain
parsePPFDomain "ONDC:TRV10" = Right MOBILITY
parsePPFDomain "ONDC:TRV11" = Right FRFS
parsePPFDomain "MOBILITY" = Right MOBILITY
parsePPFDomain "FRFS" = Right FRFS
parsePPFDomain other = Left $ ReconError InvalidDomain ("Unknown PPF domain: " <> other)
