{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PassengerDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PassengerDetailsT f = PassengerDetailsT
  { age :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    bookingId :: B.C f Kernel.Prelude.Text,
    firstName :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    lastName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PassengerDetailsT where
  data PrimaryKey PassengerDetailsT f = PassengerDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PassengerDetailsId . id

type PassengerDetails = PassengerDetailsT Identity

$(enableKVPG ''PassengerDetailsT ['id] [])

$(mkTableInstances ''PassengerDetailsT "passenger_details")
