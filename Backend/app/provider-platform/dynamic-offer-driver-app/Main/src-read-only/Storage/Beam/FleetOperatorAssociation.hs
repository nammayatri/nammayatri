{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FleetOperatorAssociation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data FleetOperatorAssociationT f
    = FleetOperatorAssociationT {associatedOn :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                                 associatedTill :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                                 fleetOwnerId :: (B.C f Kernel.Prelude.Text),
                                 id :: (B.C f Kernel.Prelude.Text),
                                 isActive :: (B.C f Kernel.Prelude.Bool),
                                 operatorId :: (B.C f Kernel.Prelude.Text),
                                 merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                 merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                 createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                 updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FleetOperatorAssociationT
    where data PrimaryKey FleetOperatorAssociationT f = FleetOperatorAssociationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FleetOperatorAssociationId . id
type FleetOperatorAssociation = FleetOperatorAssociationT Identity

$(enableKVPG (''FleetOperatorAssociationT) [('id)] [[('fleetOwnerId)], [('operatorId)]])

$(mkTableInstances (''FleetOperatorAssociationT) "fleet_operator_association")

