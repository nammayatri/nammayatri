{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.NammaTag (module Storage.Queries.NammaTag, module ReExport) where

import qualified Domain.Types.NammaTag
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.NammaTag as Beam
import Storage.Queries.NammaTagExtra as ReExport
import Storage.Queries.Transformers.NammaTag

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.NammaTag.NammaTag -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.NammaTag.NammaTag] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.NammaTag.NammaTag))
findByPrimaryKey name = do findOneWithKV [Se.And [Se.Is Beam.name $ Se.Eq name]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.NammaTag.NammaTag -> m ())
updateByPrimaryKey (Domain.Types.NammaTag.NammaTag {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.category category,
      Se.Set Beam.description description,
      Se.Set Beam.chakra (getChakra info),
      Se.Set Beam.event (getEvent info),
      Se.Set Beam.tagType (getTag info),
      Se.Set Beam.validity (getValidity info),
      Se.Set Beam.possibleValues possibleValues,
      Se.Set Beam.rule rule,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.name $ Se.Eq name]]
