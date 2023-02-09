module Domain.Types.DriverOnboarding.DriverRCAssociation where

import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Types.Id

data DriverRCAssociation = DriverRCAssociation
  { id :: Id DriverRCAssociation,
    driverId :: Id Person,
    rcId :: Id VehicleRegistrationCertificate,
    associatedOn :: UTCTime,
    associatedTill :: Maybe UTCTime,
    consent :: Bool,
    consentTimestamp :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)
