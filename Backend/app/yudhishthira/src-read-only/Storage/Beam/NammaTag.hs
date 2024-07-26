{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.NammaTag where

import qualified Database.Beam as B
import qualified Domain.Types.NammaTag
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Yudhishthira.Types
import Tools.Beam.UtilsTH

data NammaTagT f = NammaTagT
  { category :: (B.C f Kernel.Prelude.Text),
    description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    chakra :: (B.C f (Kernel.Prelude.Maybe Lib.Yudhishthira.Types.Chakra)),
    event :: (B.C f (Kernel.Prelude.Maybe Lib.Yudhishthira.Types.ApplicationEvent)),
    tagType :: (B.C f Domain.Types.NammaTag.TagType),
    validity :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Hours)),
    name :: (B.C f Kernel.Prelude.Text),
    possibleValues :: (B.C f [Kernel.Prelude.Text]),
    rule :: (B.C f Lib.Yudhishthira.Types.TagRule),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table NammaTagT where
  data PrimaryKey NammaTagT f = NammaTagId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = NammaTagId . name

type NammaTag = NammaTagT Identity

$(enableKVPG (''NammaTagT) [('name)] [])

$(mkTableInstances (''NammaTagT) "namma_tag")
