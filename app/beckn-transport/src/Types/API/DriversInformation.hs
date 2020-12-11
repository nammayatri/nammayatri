module Types.API.DriversInformation
  ( ActiveDriversResponse (..),
    DriverInformation (..),
  )
where

import Beckn.Types.App (PersonId)
import Data.Time (NominalDiffTime)
import EulerHS.Prelude

data ActiveDriversResponse = ActiveDriversResponse
  { time :: NominalDiffTime,
    active_drivers :: [DriverInformation]
  }
  deriving (Eq, Show, Generic, ToJSON)

data DriverInformation = DriverInformation
  { driver_id :: PersonId,
    completed_rides_over_time :: Int,
    earnings_over_time :: Float
  }
  deriving (Eq, Show, Generic, ToJSON)
