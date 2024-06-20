{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.KnowlarityTest where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data KnowlarityTestT f = KnowlarityTestT {callFrom :: B.C f Kernel.Prelude.Text, callTo :: B.C f Kernel.Prelude.Text, description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)}
  deriving (Generic, B.Beamable)

instance B.Table KnowlarityTestT where
  data PrimaryKey KnowlarityTestT f = KnowlarityTestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = KnowlarityTestId . callFrom

type KnowlarityTest = KnowlarityTestT Identity

$(enableKVPG ''KnowlarityTestT ['callFrom] [])

$(mkTableInstances ''KnowlarityTestT "knowlarity_test")
