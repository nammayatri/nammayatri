{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.PopularLocation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Data.Text
import qualified Data.Time.Clock
import qualified Kernel.Prelude
import qualified Database.Beam as B



data PopularLocationT f
    = PopularLocationT {address :: (B.C f Data.Text.Text),
                        createdAt :: (B.C f Data.Time.Clock.UTCTime),
                        id :: (B.C f Data.Text.Text),
                        lat :: (B.C f Kernel.Prelude.Double),
                        lon :: (B.C f Kernel.Prelude.Double),
                        merchantOperatingCityId :: (B.C f Data.Text.Text),
                        name :: (B.C f Data.Text.Text),
                        rating :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                        type_ :: (B.C f Data.Text.Text),
                        updatedAt :: (B.C f Data.Time.Clock.UTCTime),
                        merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table PopularLocationT
    where data PrimaryKey PopularLocationT f = PopularLocationId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
          primaryKey = PopularLocationId . id
type PopularLocation = PopularLocationT Identity

$(enableKVPG (''PopularLocationT) [('id)] [])

$(mkTableInstances (''PopularLocationT) "popular_location")

