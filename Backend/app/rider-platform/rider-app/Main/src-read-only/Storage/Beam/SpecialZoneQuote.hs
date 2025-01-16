{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SpecialZoneQuote where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SpecialZoneQuoteT f = SpecialZoneQuoteT
  { createdAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    id :: B.C f Kernel.Prelude.Text,
    quoteId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table SpecialZoneQuoteT where
  data PrimaryKey SpecialZoneQuoteT f = SpecialZoneQuoteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SpecialZoneQuoteId . id

type SpecialZoneQuote = SpecialZoneQuoteT Identity

$(enableKVPG ''SpecialZoneQuoteT ['id] [])

$(mkTableInstances ''SpecialZoneQuoteT "special_zone_quote")
