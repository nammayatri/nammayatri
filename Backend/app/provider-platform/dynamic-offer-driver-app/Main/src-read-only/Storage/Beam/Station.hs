{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Station where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.StationType
import qualified Kernel.Types.TimeBound
import qualified Domain.Types.VehicleCategory
import qualified Database.Beam as B



data StationT f
    = StationT {address :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                code :: (B.C f Kernel.Prelude.Text),
                id :: (B.C f Kernel.Prelude.Text),
                lat :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                lon :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                merchantId :: (B.C f Kernel.Prelude.Text),
                merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                name :: (B.C f Kernel.Prelude.Text),
                possibleTypes :: (B.C f (Kernel.Prelude.Maybe [Domain.Types.StationType.StationType])),
                timeBounds :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound)),
                vehicleType :: (B.C f Domain.Types.VehicleCategory.VehicleCategory),
                createdAt :: (B.C f Kernel.Prelude.UTCTime),
                updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table StationT
    where data PrimaryKey StationT f = StationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = StationId . id
type Station = StationT Identity

$(enableKVPG (''StationT) [('id)] [[('code)]])

$(mkTableInstances (''StationT) "station")

