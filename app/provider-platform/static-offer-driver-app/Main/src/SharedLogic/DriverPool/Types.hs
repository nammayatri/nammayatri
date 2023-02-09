module SharedLogic.DriverPool.Types
  ( DriverPoolResult (..),
    PoolRadiusStep,
    PoolBatchNum,
  )
where

import Domain.Types.Person (Driver)
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Maps as Google

type PoolBatchNum = Int

type PoolRadiusStep = Int

data DriverPoolResult = DriverPoolResult
  { driverId :: Id Driver,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    variant :: Vehicle.Variant,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show, HasCoordinates, FromJSON, ToJSON)
