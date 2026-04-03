{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FleetMemberAssociation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data FleetMemberAssociationT f
    = FleetMemberAssociationT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                               enabled :: (B.C f Kernel.Prelude.Bool),
                               fleetMemberId :: (B.C f Kernel.Prelude.Text),
                               fleetOwnerId :: (B.C f Kernel.Prelude.Text),
                               groupCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                               isFleetOwner :: (B.C f Kernel.Prelude.Bool),
                               level :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                               order :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                               parentGroupCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                               updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FleetMemberAssociationT
    where data PrimaryKey FleetMemberAssociationT f = FleetMemberAssociationId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FleetMemberAssociationId <$> fleetMemberId <*> fleetOwnerId
type FleetMemberAssociation = FleetMemberAssociationT Identity

$(enableKVPG (''FleetMemberAssociationT) [('fleetMemberId), ('fleetOwnerId)] [])

$(mkTableInstances (''FleetMemberAssociationT) "fleet_member_association")

