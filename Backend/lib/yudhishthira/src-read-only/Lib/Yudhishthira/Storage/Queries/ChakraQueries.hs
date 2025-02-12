{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.ChakraQueries where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.ChakraQueries as Beam
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.ChakraQueries
import qualified Sequelize as Se

create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries -> m ())
create = createWithKV

createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries] -> m ())
createMany = traverse_ create

deleteByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.Chakra -> Kernel.Prelude.Text -> m ())
deleteByPrimaryKey chakra queryName = do deleteWithKV [Se.And [Se.Is Beam.chakra $ Se.Eq chakra, Se.Is Beam.queryName $ Se.Eq queryName]]

findAllByChakra :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.Chakra -> m [Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries])
findAllByChakra chakra = do findAllWithKV [Se.Is Beam.chakra $ Se.Eq chakra]

findByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.Chakra -> Kernel.Prelude.Text -> m (Maybe Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries))
findByPrimaryKey chakra queryName = do findOneWithKV [Se.And [Se.Is Beam.chakra $ Se.Eq chakra, Se.Is Beam.queryName $ Se.Eq queryName]]

updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.queryResults queryResults, Se.Set Beam.queryText queryText, Se.Set Beam.createdAt createdAt, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.chakra $ Se.Eq chakra,
          Se.Is Beam.queryName $ Se.Eq queryName
        ]
    ]

instance FromTType' Beam.ChakraQueries Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries where
  fromTType' (Beam.ChakraQueriesT {..}) = do
    pure $
      Just
        Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries
          { chakra = chakra,
            queryName = queryName,
            queryResults = queryResults,
            queryText = queryText,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ChakraQueries Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries where
  toTType' (Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries {..}) = do
    Beam.ChakraQueriesT
      { Beam.chakra = chakra,
        Beam.queryName = queryName,
        Beam.queryResults = queryResults,
        Beam.queryText = queryText,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
