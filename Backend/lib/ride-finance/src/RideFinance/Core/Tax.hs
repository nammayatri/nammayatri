module RideFinance.Core.Tax
  ( zeroTaxed,
    fromGrossAtRate,
    inclusive,
  )
where

import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney (..))
import RideFinance.Core.Types

zeroTaxed :: TaxedAmount
zeroTaxed = TaxedAmount {taxExclusive = 0, tax = 0}

fromGrossAtRate :: Maybe Double -> HighPrecMoney -> TaxedAmount
fromGrossAtRate mbPct gross = case mbPct of
  Just pct
    | pct > 0 ->
      let vat = HighPrecMoney (gross.getHighPrecMoney * (toRational pct / toRational (100 + pct)))
       in TaxedAmount {taxExclusive = gross - vat, tax = vat}
  _ -> TaxedAmount {taxExclusive = gross, tax = 0}

inclusive :: TaxedAmount -> HighPrecMoney
inclusive t = t.taxExclusive + t.tax
