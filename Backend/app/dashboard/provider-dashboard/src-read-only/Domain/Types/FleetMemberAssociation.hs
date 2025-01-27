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
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
