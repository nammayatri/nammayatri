{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DisabilityTranslationExtra where

import qualified Domain.Types.DisabilityTranslation
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.DisabilityTranslation as Beam
import Storage.Queries.DisabilityTranslation ()

findByDisabilityIdAndLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.Types.DisabilityTranslation.DisabilityTranslation -> Text -> m (Maybe Domain.Types.DisabilityTranslation.DisabilityTranslation)
findByDisabilityIdAndLanguage disabilityId language =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.disabilityId $ Se.Eq (getId disabilityId),
          Se.Is Beam.language $ Se.Eq language
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.Types.DisabilityTranslation.DisabilityTranslation -> m ()
updateByPrimaryKey Domain.Types.DisabilityTranslation.DisabilityTranslation {..} =
  updateWithKV
    [ Se.Set Beam.disabilityTag disabilityTag,
      Se.Set Beam.translation translation
    ]
    [ Se.And
        [ Se.Is Beam.disabilityId $ Se.Eq (getId disabilityId),
          Se.Is Beam.language $ Se.Eq language
        ]
    ]
