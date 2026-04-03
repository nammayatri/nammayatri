{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.ParcelDetails where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Domain.Types.ParcelType
import qualified Kernel.Prelude
import qualified Database.Beam as B



data ParcelDetailsT f
    = ParcelDetailsT {merchantId :: (B.C f Kernel.Prelude.Text),
                      merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                      parcelType :: (B.C f Domain.Types.ParcelType.ParcelType),
                      quantity :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                      searchRequestId :: (B.C f Kernel.Prelude.Text),
                      createdAt :: (B.C f Kernel.Prelude.UTCTime),
                      updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table ParcelDetailsT
    where data PrimaryKey ParcelDetailsT f = ParcelDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = ParcelDetailsId . searchRequestId
type ParcelDetails = ParcelDetailsT Identity

$(enableKVPG (''ParcelDetailsT) [('searchRequestId)] [])

$(mkTableInstances (''ParcelDetailsT) "parcel_details")

