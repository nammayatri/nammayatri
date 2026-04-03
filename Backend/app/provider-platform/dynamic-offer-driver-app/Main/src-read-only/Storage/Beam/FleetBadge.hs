{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FleetBadge where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.FleetBadgeType
import qualified Database.Beam as B



data FleetBadgeT f
    = FleetBadgeT {badgeName :: (B.C f Kernel.Prelude.Text),
                   badgeRank :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   badgeType :: (B.C f Domain.Types.FleetBadgeType.FleetBadgeType),
                   createdAt :: (B.C f Kernel.Prelude.UTCTime),
                   fleetOwnerId :: (B.C f Kernel.Prelude.Text),
                   id :: (B.C f Kernel.Prelude.Text),
                   merchantId :: (B.C f Kernel.Prelude.Text),
                   merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                   personId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                   updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FleetBadgeT
    where data PrimaryKey FleetBadgeT f = FleetBadgeId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FleetBadgeId . id
type FleetBadge = FleetBadgeT Identity

$(enableKVPG (''FleetBadgeT) [('id)] [[('badgeName)], [('badgeType)], [('fleetOwnerId)]])

$(mkTableInstances (''FleetBadgeT) "fleet_badge")

