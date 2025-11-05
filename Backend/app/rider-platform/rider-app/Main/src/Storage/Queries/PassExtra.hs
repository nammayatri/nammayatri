{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PassExtra where

import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.PassType as DPassType
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Pass as Beam
import Storage.Queries.OrphanInstances.Pass ()

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPass.Pass ->
  m (Maybe DPass.Pass)
findById passId = findOneWithKV [Se.Is Beam.id $ Se.Eq (getId passId)]

findAllByPassTypeIdAndEnabled ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPassType.PassType ->
  Bool ->
  m [DPass.Pass]
findAllByPassTypeIdAndEnabled passTypeId enabled = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.passTypeId $ Se.Eq (getId passTypeId),
          Se.Is Beam.enable $ Se.Eq enabled
        ]
    ]
    (Se.Asc Beam.order)
    Nothing
    Nothing
