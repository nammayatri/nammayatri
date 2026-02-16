{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Beam.ChakraQueries where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import Tools.Beam.UtilsTH

data ChakraQueriesT f = ChakraQueriesT
  { chakra :: B.C f Lib.Yudhishthira.Types.Chakra,
    queryName :: B.C f Kernel.Prelude.Text,
    queryResults :: B.C f [Lib.Yudhishthira.Types.QueryResult],
    queryText :: B.C f Kernel.Prelude.Text,
    queryType :: B.C f (Kernel.Prelude.Maybe Lib.Yudhishthira.Types.QueryType),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ChakraQueriesT where
  data PrimaryKey ChakraQueriesT f = ChakraQueriesId (B.C f Lib.Yudhishthira.Types.Chakra) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ChakraQueriesId <$> chakra <*> queryName

type ChakraQueries = ChakraQueriesT Identity

$(enableKVPG ''ChakraQueriesT ['chakra, 'queryName] [])

$(mkTableInstancesGenericSchema ''ChakraQueriesT "chakra_queries")
