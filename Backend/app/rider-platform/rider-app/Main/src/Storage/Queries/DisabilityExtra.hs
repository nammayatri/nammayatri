module Storage.Queries.DisabilityExtra where

import Domain.Types.Disability
import Domain.Types.DisabilityTranslation
import Kernel.Beam.Functions
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Disability as BeamD
import qualified Storage.Beam.DisabilityTranslation as BeamDT
import Storage.Queries.DisabilityTranslation ()
import Storage.Queries.OrphanInstances.Disability ()

disabilityTranslationToDisability :: DisabilityTranslation -> Disability
disabilityTranslationToDisability DisabilityTranslation {..} = Disability {id = Id (getId disabilityId), tag = disabilityTag, description = translation}

findAll :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [Disability]
findAll = map disabilityTranslationToDisability <$> findAllWithKV [Se.Is BeamDT.disabilityId $ Se.Not $ Se.Eq ""]

findByDisabilityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Disability)
findByDisabilityId disabilityId = findOneWithKV [Se.Is BeamD.id $ Se.Eq disabilityId]

findAllByLanguage :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Language -> m [Disability]
findAllByLanguage language = do
  let langString = show language
  map disabilityTranslationToDisability <$> findAllWithDb [Se.Is BeamDT.language $ Se.Eq langString]
