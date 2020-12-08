module Types.API.DriversInformation
  ( ActiveDriversResponse (..),
    DriverInformation (..),
  )
where

import Beckn.Types.App (PersonId)
import Data.Time (NominalDiffTime)
import EulerHS.Prelude

data ActiveDriversResponse = ActiveDriversResponse
  { active_drivers :: [DriverInformation],
    time_period :: NominalDiffTime
  }
  deriving (Generic, ToJSON)

data DriverInformation = DriverInformation
  { driver_id :: PersonId,
    completed_rides_over_time :: Int,
    earnings_over_time :: Float,
    total_ride_time :: NominalDiffTime
  }
  deriving (Generic, ToJSON)
