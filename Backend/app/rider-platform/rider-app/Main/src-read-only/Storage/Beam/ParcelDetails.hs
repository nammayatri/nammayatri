{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ParcelDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.ParcelType
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ParcelDetailsT f = ParcelDetailsT
  { merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    parcelType :: B.C f Domain.Types.ParcelType.ParcelType,
    quantity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    searchRequestId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ParcelDetailsT where
  data PrimaryKey ParcelDetailsT f = ParcelDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ParcelDetailsId . searchRequestId

type ParcelDetails = ParcelDetailsT Identity

$(enableKVPG ''ParcelDetailsT ['searchRequestId] [])

$(mkTableInstances ''ParcelDetailsT "parcel_details")
