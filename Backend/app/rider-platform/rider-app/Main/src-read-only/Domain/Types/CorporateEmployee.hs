{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CorporateEmployee (module Domain.Types.CorporateEmployee, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.CorporateEmployee as ReExport
import qualified Domain.Types.CorporateEntity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CorporateEmployee = CorporateEmployee
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateEmployee.CorporateEmployee,
    corporateEntityId :: Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity,
    personId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    employeeCode :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    email :: Kernel.Prelude.Text,
    phone :: Kernel.Prelude.Text,
    department :: Kernel.Prelude.Text,
    costCenter :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gender :: Domain.Types.Person.Gender,
    officeLocationId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    defaultPickupLat :: Kernel.Prelude.Double,
    defaultPickupLon :: Kernel.Prelude.Double,
    defaultPickupAddress :: Kernel.Prelude.Text,
    reportingManagerEmail :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.CorporateEmployee.CorporateEmployeeStatus,
    linkedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CorporateEmployeeStatus = ACTIVE | INACTIVE | ON_LEAVE | TERMINATED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CorporateEmployeeStatus)
