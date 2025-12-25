{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetMemberAssociation where

import Data.Aeson
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH

data FleetMemberAssociation = FleetMemberAssociation
  { createdAt :: Kernel.Prelude.UTCTime,
    enabled :: Kernel.Prelude.Bool,
    fleetMemberId :: Kernel.Prelude.Text,
    fleetOwnerId :: Kernel.Prelude.Text,
    groupCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isFleetOwner :: Kernel.Prelude.Bool,
    level :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    order :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    parentGroupCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
