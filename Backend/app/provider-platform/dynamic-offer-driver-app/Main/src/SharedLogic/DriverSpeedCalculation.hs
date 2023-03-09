module SharedLogic.DriverSpeedCalculation where

import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Dhall (FromDhall)

data SpeedCalculationConfig = SpeedCalculationConfig
  { minLocationUpdates :: Int,
    locationUpdateSampleTime :: Minutes
  }
  deriving (Generic, FromDhall)

type HasSpeedCalculationConfig r =
  ( HasField "speedCalculationConfig" r SpeedCalculationConfig
  )
