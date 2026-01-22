{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PersonExtra where

import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Person as Beam
import Storage.Queries.OrphanInstances.Person

-- Extra code goes here --

findAllPersonsWithLimitOffset ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Int ->
  Int ->
  m [Domain.Types.Person.Person]
findAllPersonsWithLimitOffset limit offset = do
  findAllWithOptionsKV
    [Se.Is Beam.id $ Se.Not $ Se.Eq ""]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)
