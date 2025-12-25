module Storage.Queries.Transformers.Estimate where

import Domain.Types.FarePolicy.DriverExtraFeeBounds
import Kernel.Prelude
import Kernel.Types.Common

mkDriverExtraFeeBounds :: Maybe Meters -> Maybe DistanceUnit -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe DriverExtraFeeBounds
mkDriverExtraFeeBounds driverExtraFeeStartDistance driverExtraFeeDistanceUnit driverExtraFeeStepFee driverExtraFeeDefaultStepFee driverExtraFeeMinFee driverExtraFeeMaxFee =
  DriverExtraFeeBounds
    <$> driverExtraFeeStartDistance
    <*> driverExtraFeeDistanceUnit
    <*> driverExtraFeeStepFee
    <*> driverExtraFeeDefaultStepFee
    <*> driverExtraFeeMinFee
    <*> driverExtraFeeMaxFee
