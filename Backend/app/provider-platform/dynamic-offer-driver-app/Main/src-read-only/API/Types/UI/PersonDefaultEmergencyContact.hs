{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PersonDefaultEmergencyContact where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Safety.Domain.Types.Common
import Servant
import Tools.Auth

data PersonDefaultEmergencyContact = PersonDefaultEmergencyContact
  { contactPersonId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    enableForFollowing :: Kernel.Prelude.Bool,
    enableForShareRide :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    priority :: Kernel.Prelude.Int,
    shareTripWithEmergencyContactOption :: Kernel.Prelude.Maybe Safety.Domain.Types.Common.RideShareOptions
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdatePersonDefaultEmergencyContactsReq = UpdatePersonDefaultEmergencyContactsReq {emergencyContacts :: [PersonDefaultEmergencyContact]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
