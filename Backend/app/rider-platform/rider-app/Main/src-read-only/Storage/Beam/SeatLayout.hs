{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.SeatLayout where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data SeatLayoutT f
    = SeatLayoutT {columns :: (B.C f Kernel.Prelude.Int),
                   id :: (B.C f Kernel.Prelude.Text),
                   merchantId :: (B.C f Kernel.Prelude.Text),
                   merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                   name :: (B.C f Kernel.Prelude.Text),
                   rows :: (B.C f Kernel.Prelude.Int),
                   createdAt :: (B.C f Kernel.Prelude.UTCTime),
                   updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table SeatLayoutT
    where data PrimaryKey SeatLayoutT f = SeatLayoutId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = SeatLayoutId . id
type SeatLayout = SeatLayoutT Identity

$(enableKVPG (''SeatLayoutT) [('id)] [])

$(mkTableInstances (''SeatLayoutT) "seat_layout")

