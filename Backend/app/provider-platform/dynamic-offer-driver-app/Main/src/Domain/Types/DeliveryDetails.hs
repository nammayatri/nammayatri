module Domain.Types.DeliveryDetails where

import Domain.Types.Location (LocationAddress)
import Domain.Types.Trip
import Kernel.Prelude

data DeliveryDetails = DeliveryDetails
  { senderDetails :: PersonDetails,
    receiverDetails :: PersonDetails,
    initiatedAs :: TripParty
  }
  deriving (Show, Eq)

data PersonDetails = PersonDetails
  { name :: Text,
    phoneNumber :: Text,
    address :: LocationAddress
  }
  deriving (Show, Eq)
