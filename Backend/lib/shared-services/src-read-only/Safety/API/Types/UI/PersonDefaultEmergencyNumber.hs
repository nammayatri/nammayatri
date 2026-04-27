{-# OPTIONS_GHC -Wno-unused-imports #-}

module Safety.API.Types.UI.PersonDefaultEmergencyNumber where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Safety.Domain.Types.Common
import Servant

data EmergencyContact = EmergencyContact
  { contactPersonId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Safety.Domain.Types.Common.Person),
    enableForFollowing :: Kernel.Prelude.Bool,
    enableForShareRide :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Safety.Domain.Types.Common.Merchant),
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    onRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    priority :: Kernel.Prelude.Int,
    shareTripWithEmergencyContactOption :: Kernel.Prelude.Maybe Safety.Domain.Types.Common.RideShareOptions
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EmergencyContactReq = EmergencyContactReq
  { enableForFollowing :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    enableForShareRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    priority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    shareTripWithEmergencyContactOption :: Kernel.Prelude.Maybe Safety.Domain.Types.Common.RideShareOptions
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GetEmergencyContactsRes = GetEmergencyContactsRes {defaultEmergencyNumbers :: [EmergencyContact]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateEmergencyContactsReq = UpdateEmergencyContactsReq {defaultEmergencyNumbers :: [EmergencyContactReq]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
