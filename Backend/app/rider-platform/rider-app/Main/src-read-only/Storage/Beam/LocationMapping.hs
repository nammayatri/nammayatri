{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.LocationMapping where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.LocationMapping
import qualified Database.Beam as B



data LocationMappingT f
    = LocationMappingT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                        entityId :: (B.C f Kernel.Prelude.Text),
                        id :: (B.C f Kernel.Prelude.Text),
                        locationId :: (B.C f Kernel.Prelude.Text),
                        merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                        merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                        order :: (B.C f Kernel.Prelude.Int),
                        tag :: (B.C f Domain.Types.LocationMapping.LocationMappingTags),
                        updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                        version :: (B.C f Kernel.Prelude.Text)}
    deriving (Generic, B.Beamable)
instance B.Table LocationMappingT
    where data PrimaryKey LocationMappingT f = LocationMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = LocationMappingId . id
type LocationMapping = LocationMappingT Identity

$(enableKVPG (''LocationMappingT) [('id)] [[('entityId)]])

$(mkTableInstances (''LocationMappingT) "location_mapping")

