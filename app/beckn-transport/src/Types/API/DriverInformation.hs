module Types.API.DriverInformation
  ( ActiveDriversResponse (..),
    DriverRidesInformation (..),
    DriverInformationResponse (..),
  )
where

import Beckn.Types.App (PersonId)
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import EulerHS.Prelude
import Types.Storage.DriverInformation

newtype ActiveDriversResponse = ActiveDriversResponse
  { active_drivers :: [DriverRidesInformation]
  }
  deriving (Eq, Show, Generic, ToJSON)

data DriverRidesInformation = DriverRidesInformation
  { driver_id :: PersonId,
    completed_rides_over_time :: Int,
    earnings_over_time :: Float
  }
  deriving (Eq, Show, Generic, ToJSON)

data DriverInformationResponse = DriverInformationResponse
  { transporter :: Organization.Organization,
    person :: Person.Person,
    driver_information :: DriverInformation
  }
  deriving (Generic, ToJSON)
