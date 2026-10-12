module RideFinance.Calculations.Totals
  ( CashOnlineSplit (..),
    componentTaxExclusive,
    componentTax,
    deduction,
    customerFareTotal,
    customerPayable,
    taxRemitted,
    driverNetBeforeTds,
    cashOnlineSplit,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import RideFinance.Core.Tax (inclusive, zeroTaxed)
import RideFinance.Core.Types

data CashOnlineSplit = CashOnlineSplit
  { collectInCash :: Maybe HighPrecMoney,
    settledOnline :: Maybe HighPrecMoney
  }
  deriving (Show, Eq, Generic)

earned :: Map.Map FareComponent [TaxedAmount] -> [TaxedAmount]
earned amounts = concat [ps | (c, ps) <- Map.toList amounts, earnedByDriver c]

componentTaxExclusive :: Map.Map FareComponent [TaxedAmount] -> FareComponent -> HighPrecMoney
componentTaxExclusive amounts c = sum (map (.taxExclusive) (Map.findWithDefault [] c amounts))

componentTax :: Map.Map FareComponent [TaxedAmount] -> FareComponent -> HighPrecMoney
componentTax amounts c = sum (map (.tax) (Map.findWithDefault [] c amounts))

deduction :: RideFinance -> FareComponent -> TaxedAmount
deduction rf c = Map.findWithDefault zeroTaxed c rf.deductions

customerFareTotal :: RideFinance -> HighPrecMoney
customerFareTotal rf = sum (map inclusive (concat (Map.elems rf.charges)))

customerPayable :: RideFinance -> HighPrecMoney
customerPayable rf = customerFareTotal rf - rf.appliedDiscount + rf.tip

taxRemitted :: RideFinance -> HighPrecMoney
taxRemitted rf = case rf.regime of
  GST -> sum (map (.tax) (earned rf.chargesAfterDiscount))
  VAT -> 0

-- | What the driver keeps from the ride before TDS.
--   GST is an indirect tax collected from the customer on the government's behalf; it is never the
--   driver's money, so under GST it is removed here via 'taxRemitted' (under VAT the driver keeps it).
--   TDS is different: it is a direct tax on the driver's income, withheld separately ('tds') and filed
--   by the driver against their own income, so it is not deducted here.
driverNetBeforeTds :: RideFinance -> HighPrecMoney
driverNetBeforeTds rf =
  sum (map inclusive (earned rf.charges)) + rf.tip
    - taxRemitted rf
    - sum (map inclusive (Map.elems rf.deductions))

cashOnlineSplit :: RideFinance -> CashOnlineSplit
cashOnlineSplit rf
  | customerPaysDriverInCash =
    CashOnlineSplit
      { collectInCash = Just (max 0 (customerPayable rf - boothCollected)),
        settledOnline = if rf.appliedDiscount > 0 then Just rf.appliedDiscount else Nothing
      }
  | otherwise =
    CashOnlineSplit
      { collectInCash = Nothing,
        settledOnline = Just (max 0 (driverNetBeforeTds rf))
      }
  where
    isCollectedAtBooth c = Set.member c rf.settlement.collectedAtBooth
    boothTookEverything = all isCollectedAtBooth (Map.keys rf.charges)
    customerPaysDriverInCash = rf.settlement.settlementType == Cash && not boothTookEverything
    boothCollected =
      sum [inclusive p | (c, ps) <- Map.toList rf.chargesAfterDiscount, isCollectedAtBooth c, p <- ps]
        + sum [inclusive p | (c, p) <- Map.toList rf.deductions, isCollectedAtBooth c]
