{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Disability where

import Domain.Types.Person.PersonDisability
import Kernel.Beam.Functions
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Disability as BeamD
import qualified Storage.Beam.DisabilityTranslation as BeamDT

findAll :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [DisabilityItem]
findAll = findAllWithKV [Se.Is BeamDT.disabilityId $ Se.Not $ Se.Eq ""]

findByDisabilityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe DisabilityItem)
findByDisabilityId disabilityId = findOneWithKV [Se.Is BeamD.id $ Se.Eq disabilityId]

findAllByLanguage :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Language -> m [DisabilityItem]
findAllByLanguage language = do
  let langString = show language
  findAllWithDb [Se.Is BeamDT.language $ Se.Eq langString]

instance FromTType' BeamDT.DisabilityTranslation DisabilityItem where
  fromTType' BeamDT.DisabilityTranslationT {..} = do
    pure $
      Just
        DisabilityItem
          { id = Id disabilityId,
            tag = disabilityTag,
            description = translation
          }

instance FromTType' BeamD.Disability DisabilityItem where
  fromTType' BeamD.DisabilityT {..} = do
    pure $
      Just
        DisabilityItem
          { id = Id id,
            tag = tag,
            description = description
          }
