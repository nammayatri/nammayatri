module Domain.Action.UI.FareBreakup where

import Domain.Types.FareBreakup
import Kernel.Prelude
import Kernel.Types.Common

mkFareBreakupAPIEntity :: FareBreakup -> FareBreakupAPIEntity
mkFareBreakupAPIEntity FareBreakup {..} =
  FareBreakupAPIEntity
    { amount = roundToIntegral amount.amount,
      amountWithCurrency = mkPriceAPIEntity amount,
      ..
    }
