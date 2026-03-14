{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.PGFee
  ( PGFeeType (..),
    PGFeeConfig (..),
    PGFeeResult (..),
    computePGFee,
    recordPGFeeLedgerEntries,
  )
where

import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import qualified Lib.Finance.Domain.Types.Account as Account
import Lib.Finance.Error.Types (FinanceError)
import Lib.Finance.FinanceM
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow

-- | Whether this fee is for a payment or payout operation
data PGFeeType = PGPayment | PGPayout
  deriving (Eq, Show, Generic)

-- | Fee configuration extracted from JuspayConfig by the domain.
--   Passed alongside the payment/payout callback.
data PGFeeConfig = PGFeeConfig
  { pgBaseFee :: HighPrecMoney,
    pgGstRate :: Double,
    pgFeeCurrency :: Currency
  }
  deriving (Eq, Show, Generic)

-- | Result of PG fee ledger entry creation
data PGFeeResult = PGFeeResult
  { pgBaseFee :: HighPrecMoney,
    pgGst :: HighPrecMoney
  }
  deriving (Eq, Show, Generic)

-- | Pure computation: derive fee amounts from config.
--   Used by the payment lib to store pgBaseFee/pgGst on order records.
computePGFee :: PGFeeConfig -> PGFeeResult
computePGFee config =
  PGFeeResult
    { pgBaseFee = config.pgBaseFee,
      pgGst = config.pgBaseFee * realToFrac config.pgGstRate
    }

-- | Record PG fee ledger entries using FinanceM.
--   Creates 2 entries using role-based transfers:
--     1. Liability → Expense : baseFee   (PG fee expense)
--     2. Liability → GstAsset: gstAmount (GST on PG fee)
--
--   The Liability account accumulates the total owed to the PG (baseFee + GST).
recordPGFeeLedgerEntries ::
  (BeamFlow.BeamFlow m r) =>
  PGFeeType ->
  PGFeeConfig ->
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  Text -> -- referenceId (orderId)
  m (Either FinanceError PGFeeResult)
recordPGFeeLedgerEntries feeType config merchantId merchantOpCityId referenceId = do
  let gstAmount = config.pgBaseFee * realToFrac config.pgGstRate
      (expenseRole, liabilityRole) = case feeType of
        PGPayment -> (PGPaymentExpense, PGPaymentLiability)
        PGPayout -> (PGPayoutExpense, PGPayoutLiability)
      ctx =
        FinanceCtx
          { merchantId = merchantId,
            merchantOpCityId = merchantOpCityId,
            currency = config.pgFeeCurrency,
            counterpartyType = Account.SELLER,
            counterpartyId = merchantId,
            referenceId = referenceId,
            merchantName = Nothing,
            merchantShortId = Nothing,
            issuedByAddress = Nothing,
            supplierName = Nothing,
            supplierGSTIN = Nothing,
            supplierId = Nothing,
            panOfParty = Nothing,
            panType = Nothing,
            tdsRateReason = Nothing
          }
  result <- runFinance ctx $ do
    -- 1. Base fee: Liability → Expense (DR Expense, CR Liability)
    transfer_ liabilityRole expenseRole config.pgBaseFee "PGFeeExpense"
    -- 2. GST: Liability → GstAsset (DR GstAsset, CR Liability)
    transfer_ liabilityRole PGGstAsset gstAmount "PGFeeGST"
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right PGFeeResult {pgBaseFee = config.pgBaseFee, pgGst = gstAmount}
