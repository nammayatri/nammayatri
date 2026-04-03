{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DriverOperatorAssociation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.VehicleCategory
import qualified Database.Beam as B



data DriverOperatorAssociationT f
    = DriverOperatorAssociationT {associatedOn :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                                  associatedTill :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                                  driverId :: (B.C f Kernel.Prelude.Text),
                                  id :: (B.C f Kernel.Prelude.Text),
                                  isActive :: (B.C f Kernel.Prelude.Bool),
                                  onboardingVehicleCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory)),
                                  operatorId :: (B.C f Kernel.Prelude.Text),
                                  merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                  merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                  createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                  updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table DriverOperatorAssociationT
    where data PrimaryKey DriverOperatorAssociationT f = DriverOperatorAssociationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DriverOperatorAssociationId . id
type DriverOperatorAssociation = DriverOperatorAssociationT Identity

$(enableKVPG (''DriverOperatorAssociationT) [('id)] [[('driverId)], [('operatorId)]])

$(mkTableInstances (''DriverOperatorAssociationT) "driver_operator_association")

