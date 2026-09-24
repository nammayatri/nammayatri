{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Beam.NammaTag where

import qualified Data.Aeson
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.NammaTag
import Tools.Beam.UtilsTH

data NammaTagT f = NammaTagT
  { actionEngine :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    category :: B.C f Kernel.Prelude.Text,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    chakra :: B.C f (Kernel.Prelude.Maybe Lib.Yudhishthira.Types.Chakra),
    tagType :: B.C f Lib.Yudhishthira.Types.NammaTag.TagType,
    name :: B.C f Kernel.Prelude.Text,
    rangeEnd :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    rangeStart :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    tags :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    llmContext :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    ruleEngine :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    validity :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Hours),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table NammaTagT where
  data PrimaryKey NammaTagT f = NammaTagId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = NammaTagId . name

type NammaTag = NammaTagT Identity

$(enableKVPG ''NammaTagT ['name] [])

$(mkTableInstancesGenericSchema ''NammaTagT "namma_tag")
