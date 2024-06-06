module Storage.Queries.Transformers.RentalDetails where

import Domain.Types.RentalDetails
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Utils.Common

mkNightShiftInfo :: Kernel.Prelude.Maybe Kernel.Utils.Common.Money -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay -> Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay -> Maybe Currency -> Kernel.Prelude.Maybe NightShiftInfo
mkNightShiftInfo nightShiftCharge nightShiftChargeAmount nightShiftEnd nightShiftStart currency =
  ((,,) <$> nightShiftCharge <*> nightShiftStart <*> nightShiftEnd) <&> \(nightShiftCharge', nightShiftStart', nightShiftEnd') -> do
    let nightShiftCharge_ = mkPriceWithDefault nightShiftChargeAmount currency nightShiftCharge'
    NightShiftInfo nightShiftCharge_ Nothing nightShiftStart' nightShiftEnd'
