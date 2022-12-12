module SharedLogic.DriverPool.Types
  ( DriverPoolResult (..),
    PoolRadiusStep,
    PoolBatchNum,
  )
where

import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Person (Driver)
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (id)
import Tools.Maps as Google

type PoolBatchNum = Integer

type PoolRadiusStep = Integer

data DriverPoolResult = DriverPoolResult
  { driverId :: Id Driver,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    variant :: Vehicle.Variant,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show, HasCoordinates, FromJSON, ToJSON)
