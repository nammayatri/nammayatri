module Storage.Queries.Transformers.DailyStats where

import Kernel.Prelude
import qualified Kernel.Types.Common

getHighPrecMoney :: (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney)
getHighPrecMoney = fromMaybe 0.0
