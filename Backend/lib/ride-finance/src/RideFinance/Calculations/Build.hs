module RideFinance.Calculations.Build
  ( RideFinanceInputs (..),
    fromProjection,
  )
where

import qualified Data.Map.Strict as Map
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import RideFinance.Core.Components
import RideFinance.Core.Projection
import RideFinance.Core.Tax
import RideFinance.Core.Types

data RideFinanceInputs = RideFinanceInputs
  { currency :: Currency,
    regime :: TaxRegime,
    breakup :: ProjectFareParamsBreakup,
    requestedDiscount :: HighPrecMoney,
    tip :: HighPrecMoney,
    commission :: TaxedAmount,
    cancellationCommission :: TaxedAmount,
    chargeBearer :: Maybe ChargeBearer,
    paymentCharge :: TaxedAmount,
    leviedPaymentCharge :: Maybe TaxedAmount,
    memos :: [(MemoItem, HighPrecMoney)],
    settlement :: Settlement,
    tds :: Maybe HighPrecMoney
  }
  deriving (Show, Eq, Generic)

fromProjection :: RideFinanceInputs -> RideFinance
fromProjection inputs =
  RideFinance
    { currency = inputs.currency,
      regime = inputs.regime,
      charges = chargesFrom b,
      chargesAfterDiscount = chargesFrom discounted,
      deductions = Map.fromList [(c, deductedFrom deductionAmounts) | c <- allComponents, Just deductedFrom <- [(componentSpec c).deductedFromDriver]],
      appliedDiscount = discountResult.clampedDiscount,
      tip = inputs.tip,
      chargeBearer = inputs.chargeBearer,
      paymentCharge = leviedCharge,
      memos = Map.filter (/= 0) (Map.fromListWith (+) inputs.memos),
      settlement = inputs.settlement,
      tds = inputs.tds
    }
  where
    b = inputs.breakup
    (discounted, discountResult) = applyDiscountAndRepriceCharge (paymentChargeRateFromBreakup b) b inputs.requestedDiscount
    repriceRatio = if fareOnlyTotal b > 0 then fareOnlyTotal discounted / fareOnlyTotal b else 1
    leviedCharge = fromMaybe TaxedAmount {taxExclusive = inputs.paymentCharge.taxExclusive * repriceRatio, tax = inputs.paymentCharge.tax * repriceRatio} inputs.leviedPaymentCharge
    chargesFrom breakupSlots = Map.fromList [(c, chargedFrom breakupSlots) | c <- allComponents, Just chargedFrom <- [(componentSpec c).chargeFromCustomer]]
    deductionAmounts =
      DeductionAmounts
        { commission = inputs.commission,
          cancellationCommission = inputs.cancellationCommission,
          driverBorneCharge = if inputs.chargeBearer == Just PAYMENT_DRIVER then leviedCharge else zeroTaxed
        }
