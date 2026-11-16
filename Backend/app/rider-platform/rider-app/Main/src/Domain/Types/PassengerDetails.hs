module Domain.Types.PassengerDetails where

import Kernel.Prelude

-- | The person a ride is booked for when the rider books on someone else's behalf
-- ("book for others"). Sent by the app on search (and optionally overridden on select).
data PassengerDetails = PassengerDetails
  { name :: Text,
    phoneNumber :: Text,
    countryCode :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
