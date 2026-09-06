{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSPassengerDetail where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Person
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSPassengerDetailT f = FRFSPassengerDetailT
  { age :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    bookingId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    dropOffPointPlaceId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    gender :: B.C f Domain.Types.Person.Gender,
    id :: B.C f Kernel.Prelude.Text,
    idProofLookupId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    idProofNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    idProofNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    isChild :: B.C f Kernel.Prelude.Bool,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    pickupPointPlaceId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    quoteId :: B.C f Kernel.Prelude.Text,
    seatId :: B.C f Kernel.Prelude.Text,
    seatLabel :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSPassengerDetailT where
  data PrimaryKey FRFSPassengerDetailT f = FRFSPassengerDetailId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSPassengerDetailId . id

type FRFSPassengerDetail = FRFSPassengerDetailT Identity

$(enableKVPG ''FRFSPassengerDetailT ['id] [['bookingId], ['quoteId]])

$(mkTableInstances ''FRFSPassengerDetailT "frfs_passenger_detail")
