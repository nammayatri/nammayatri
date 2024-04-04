{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Translations (module Storage.Queries.Translations, module ReExport) where

import qualified Domain.Types.Translations
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Translations as Beam
import Storage.Queries.TranslationsExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.Translations.Translations -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.Translations.Translations] -> m ())
createMany = traverse_ create

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Translations.Translations -> m (Maybe Domain.Types.Translations.Translations))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.Translations.Translations -> m ())
updateByPrimaryKey (Domain.Types.Translations.Translations {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.language language,
      Se.Set Beam.message message,
      Se.Set Beam.messageKey messageKey,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
