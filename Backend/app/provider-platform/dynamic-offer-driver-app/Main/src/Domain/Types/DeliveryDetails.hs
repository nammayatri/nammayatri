module Domain.Types.DeliveryDetails where

import Domain.Types.Location (LocationAddress)
import Domain.Types.Trip
import Kernel.Prelude
import Tools.Beam.UtilsTH

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

data ParcelType = Household | Electronics | Perishables | Fragile | ConstructionMaterials | Others Kernel.Prelude.Text deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''ParcelType)
