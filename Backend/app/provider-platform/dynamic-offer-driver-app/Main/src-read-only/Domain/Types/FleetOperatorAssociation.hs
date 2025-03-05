{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetOperatorAssociation where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FleetOperatorAssociation = FleetOperatorAssociation
  { associatedOn :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    associatedTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    fleetOwnerId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation,
    isActive :: Kernel.Prelude.Bool,
    operatorId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
