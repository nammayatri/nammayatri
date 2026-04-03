{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FleetBadgeAssociation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.FleetBadgeType
import qualified Database.Beam as B



data FleetBadgeAssociationT f
    = FleetBadgeAssociationT {associatedOn :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                              associatedTill :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                              badgeId :: (B.C f Kernel.Prelude.Text),
                              badgeType :: (B.C f Domain.Types.FleetBadgeType.FleetBadgeType),
                              createdAt :: (B.C f Kernel.Prelude.UTCTime),
                              driverId :: (B.C f Kernel.Prelude.Text),
                              fleetOwnerId :: (B.C f Kernel.Prelude.Text),
                              id :: (B.C f Kernel.Prelude.Text),
                              isActive :: (B.C f Kernel.Prelude.Bool),
                              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FleetBadgeAssociationT
    where data PrimaryKey FleetBadgeAssociationT f = FleetBadgeAssociationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FleetBadgeAssociationId . id
type FleetBadgeAssociation = FleetBadgeAssociationT Identity

$(enableKVPG (''FleetBadgeAssociationT) [('id)] [[('badgeId)], [('badgeType)], [('driverId)], [('fleetOwnerId)]])

$(mkTableInstances (''FleetBadgeAssociationT) "fleet_badge_association")

