{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.OneWayScheduledQuote where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data OneWayScheduledQuoteT f = OneWayScheduledQuoteT
  { createdAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    id :: (B.C f Kernel.Prelude.Text),
    quoteId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime))
  }
  deriving (Generic, B.Beamable)

instance B.Table OneWayScheduledQuoteT where
  data PrimaryKey OneWayScheduledQuoteT f = OneWayScheduledQuoteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OneWayScheduledQuoteId . id

type OneWayScheduledQuote = OneWayScheduledQuoteT Identity

$(enableKVPG (''OneWayScheduledQuoteT) [('id)] [])

$(mkTableInstances (''OneWayScheduledQuoteT) "one_way_scheduled_quote")
