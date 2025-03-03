{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MeterRideQuote where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MeterRideQuoteT f = MeterRideQuoteT
  { createdAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    id :: (B.C f Kernel.Prelude.Text),
    quoteId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime))
  }
  deriving (Generic, B.Beamable)

instance B.Table MeterRideQuoteT where
  data PrimaryKey MeterRideQuoteT f = MeterRideQuoteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MeterRideQuoteId . id

type MeterRideQuote = MeterRideQuoteT Identity

$(enableKVPG (''MeterRideQuoteT) [('id)] [])

$(mkTableInstances (''MeterRideQuoteT) "meter_ride_quote")
