module Types.API.DriverInformation
  ( ActiveDriversResponse (..),
    DriverInformation (..),
  )
where

import Beckn.Types.App (PersonId)
import EulerHS.Prelude

newtype ActiveDriversResponse = ActiveDriversResponse
  { active_drivers :: [DriverInformation]
  }
  deriving (Eq, Show, Generic, ToJSON)

data DriverInformation = DriverInformation
  { driver_id :: PersonId,
    completed_rides_over_time :: Int,
    earnings_over_time :: Float
  }
  deriving (Eq, Show, Generic, ToJSON)
