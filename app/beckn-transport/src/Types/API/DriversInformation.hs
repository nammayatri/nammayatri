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
    completed_rides_over_24h :: Int,
    earnings_over_24h :: Float
  }
  deriving (Generic, ToJSON)
