{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ChakraQueries where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import Tools.Beam.UtilsTH

data ChakraQueriesT f = ChakraQueriesT
  { chakra :: (B.C f Lib.Yudhishthira.Types.Chakra),
    id :: (B.C f Kernel.Prelude.Text),
    queryResults :: (B.C f [Kernel.Prelude.Text]),
    queryText :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table ChakraQueriesT where
  data PrimaryKey ChakraQueriesT f = ChakraQueriesId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ChakraQueriesId . id

type ChakraQueries = ChakraQueriesT Identity

$(enableKVPG (''ChakraQueriesT) [('id)] [])

$(mkTableInstances (''ChakraQueriesT) "chakra_queries")
