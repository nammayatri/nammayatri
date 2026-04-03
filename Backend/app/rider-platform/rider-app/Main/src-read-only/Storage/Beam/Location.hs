{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Location where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data LocationT f
    = LocationT {area :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 areaCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 building :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 city :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 country :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 door :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 extras :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 instructions :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 placeId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 state :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 street :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 title :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 ward :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 createdAt :: (B.C f Kernel.Prelude.UTCTime),
                 id :: (B.C f Kernel.Prelude.Text),
                 lat :: (B.C f Kernel.Prelude.Double),
                 lon :: (B.C f Kernel.Prelude.Double),
                 updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                 merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                 merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table LocationT
    where data PrimaryKey LocationT f = LocationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = LocationId . id
type Location = LocationT Identity

$(enableKVPG (''LocationT) [('id)] [])

$(mkTableInstances (''LocationT) "location")

