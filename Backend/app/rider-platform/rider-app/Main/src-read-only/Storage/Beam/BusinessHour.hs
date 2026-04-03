{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.BusinessHour where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.BusinessHour
import qualified Data.Time
import qualified Database.Beam as B



data BusinessHourT f
    = BusinessHourT {bookingClosingTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay)),
                     btype :: (B.C f Domain.Types.BusinessHour.BusinessHourType),
                     categoryId :: (B.C f [Kernel.Prelude.Text]),
                     expiryDate :: (B.C f (Kernel.Prelude.Maybe Data.Time.Day)),
                     hash :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                     id :: (B.C f Kernel.Prelude.Text),
                     name :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                     placeId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                     merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                     merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                     createdAt :: (B.C f Kernel.Prelude.UTCTime),
                     updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table BusinessHourT
    where data PrimaryKey BusinessHourT f = BusinessHourId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = BusinessHourId . id
type BusinessHour = BusinessHourT Identity

$(enableKVPG (''BusinessHourT) [('id)] [])

$(mkTableInstances (''BusinessHourT) "business_hour")

