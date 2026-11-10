module Storage.Queries.Transformers.Estimate where

import Domain.Types.FarePolicy (FarePolicy)
import qualified Domain.Types.FarePolicy as Fields
import Domain.Types.FarePolicy.DriverExtraFeeBounds
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id as ID

getFarePolicyId :: Maybe FarePolicy -> Maybe Text
getFarePolicyId = fmap (ID.getId . (.id))

mkDriverExtraFeeBounds :: Maybe Meters -> Maybe DistanceUnit -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe DriverExtraFeeBounds
mkDriverExtraFeeBounds driverExtraFeeStartDistance driverExtraFeeDistanceUnit driverExtraFeeStepFee driverExtraFeeDefaultStepFee driverExtraFeeMinFee driverExtraFeeMaxFee =
  DriverExtraFeeBounds
    <$> driverExtraFeeStartDistance
    <*> driverExtraFeeDistanceUnit
    <*> driverExtraFeeStepFee
    <*> driverExtraFeeDefaultStepFee
    <*> driverExtraFeeMinFee
    <*> driverExtraFeeMaxFee
