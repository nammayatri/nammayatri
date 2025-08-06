module Domain.Types.DeliveryDetails where

import Domain.Types.LocationAddress
import Domain.Types.ParcelType
import Domain.Types.Trip
import Kernel.Prelude

data DeliveryDetails = DeliveryDetails
  { senderDetails :: PersonDetails,
    receiverDetails :: PersonDetails,
    initiatedAs :: DeliveryParties,
    parcelType :: Maybe ParcelType,
    parcelQuantity :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PersonDetails = PersonDetails
  { name :: Text,
    phoneNumber :: Text,
    countryCode :: Maybe Text,
    address :: LocationAddress
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
