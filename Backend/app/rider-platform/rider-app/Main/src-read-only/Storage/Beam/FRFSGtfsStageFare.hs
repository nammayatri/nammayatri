{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FRFSGtfsStageFare where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B



data FRFSGtfsStageFareT f
    = FRFSGtfsStageFareT {amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                          cessCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                          currency :: (B.C f Kernel.Types.Common.Currency),
                          discountIds :: (B.C f [Kernel.Prelude.Text]),
                          id :: (B.C f Kernel.Prelude.Text),
                          merchantId :: (B.C f Kernel.Prelude.Text),
                          merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                          stage :: (B.C f Kernel.Prelude.Int),
                          vehicleServiceTierId :: (B.C f Kernel.Prelude.Text),
                          vehicleType :: (B.C f BecknV2.FRFS.Enums.VehicleCategory),
                          createdAt :: (B.C f Kernel.Prelude.UTCTime),
                          updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FRFSGtfsStageFareT
    where data PrimaryKey FRFSGtfsStageFareT f = FRFSGtfsStageFareId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FRFSGtfsStageFareId . id
type FRFSGtfsStageFare = FRFSGtfsStageFareT Identity

$(enableKVPG (''FRFSGtfsStageFareT) [('id)] [])

$(mkTableInstances (''FRFSGtfsStageFareT) "frfs_gtfs_stage_fare")

