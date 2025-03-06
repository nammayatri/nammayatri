{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverOperatorAssociation where

import Data.Aeson
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverOperatorAssociation = DriverOperatorAssociation
  { associatedOn :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    associatedTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation,
    isActive :: Kernel.Prelude.Bool,
    operatorId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
