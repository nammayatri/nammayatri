{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ChakraQueries where

import qualified Domain.Types.ChakraQueries
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Types
import qualified Sequelize as Se
import qualified Storage.Beam.ChakraQueries as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ChakraQueries.ChakraQueries -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ChakraQueries.ChakraQueries] -> m ())
createMany = traverse_ create

findAllByChakra :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Lib.Yudhishthira.Types.Chakra -> m ([Domain.Types.ChakraQueries.ChakraQueries]))
findAllByChakra chakra = do findAllWithKV [Se.Is Beam.chakra $ Se.Eq chakra]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.ChakraQueries.ChakraQueries -> m (Maybe Domain.Types.ChakraQueries.ChakraQueries))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ChakraQueries.ChakraQueries -> m ())
updateByPrimaryKey (Domain.Types.ChakraQueries.ChakraQueries {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.chakra chakra,
      Se.Set Beam.queryResults queryResults,
      Se.Set Beam.queryText queryText,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ChakraQueries Domain.Types.ChakraQueries.ChakraQueries where
  fromTType' (Beam.ChakraQueriesT {..}) = do
    pure $
      Just
        Domain.Types.ChakraQueries.ChakraQueries
          { chakra = chakra,
            id = Kernel.Types.Id.Id id,
            queryResults = queryResults,
            queryText = queryText,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ChakraQueries Domain.Types.ChakraQueries.ChakraQueries where
  toTType' (Domain.Types.ChakraQueries.ChakraQueries {..}) = do
    Beam.ChakraQueriesT
      { Beam.chakra = chakra,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.queryResults = queryResults,
        Beam.queryText = queryText,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
