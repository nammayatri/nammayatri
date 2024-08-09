{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.PersonDefaultEmergencyNumber where

import Domain.Types.Merchant as DM
import Domain.Types.Person (Person, RideShareOptions)
import Domain.Types.PersonDefaultEmergencyNumber
import Kernel.Prelude
import Kernel.Types.Id

data PersonDefaultEmergencyNumberAPIEntity = PersonDefaultEmergencyNumberAPIEntity
  { personId :: Id Person,
    name :: Text,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    priority :: Int,
    contactPersonId :: Maybe (Id Person),
    merchantId :: Maybe (Id DM.Merchant),
    enableForFollowing :: Bool,
    enableForShareRide :: Bool,
    onRide :: Bool,
    shareTripWithEmergencyContactOption :: Maybe RideShareOptions
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

makePersonDefaultEmergencyNumberAPIEntity :: Bool -> DecryptedPersonDefaultEmergencyNumber -> PersonDefaultEmergencyNumberAPIEntity
makePersonDefaultEmergencyNumberAPIEntity onRide PersonDefaultEmergencyNumber {..} =
  PersonDefaultEmergencyNumberAPIEntity
    { ..
    }
