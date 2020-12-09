module Types.API.DriversInformation
  ( ActiveDriversResponse,
    DriverInformation (..),
  )
where

import Beckn.Types.App (PersonId)
import EulerHS.Prelude

type ActiveDriversResponse = [DriverInformation]

data DriverInformation = DriverInformation
  { driver_id :: PersonId,
    completed_rides_over_time :: Int,
    earnings_over_time :: Float
  }
  deriving (Generic, ToJSON)
