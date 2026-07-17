module Lib.Finance.Utils.GstRates
  ( mkComponentRateFromTxn,
  )
where

import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as FITxn

-- | Component GST rate (%) from an IndirectTaxTransaction amount selector.
mkComponentRateFromTxn ::
  (FITxn.IndirectTaxTransaction -> Maybe HighPrecMoney) ->
  FITxn.IndirectTaxTransaction ->
  Maybe Double
mkComponentRateFromTxn getAmount txn = do
  componentAmount <- getAmount txn
  guard (txn.taxableValue > 0)
  pure $ realToFrac (componentAmount / txn.taxableValue) * 100.0
