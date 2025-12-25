module Storage.Queries.TranslationsExtra where

import qualified Domain.Types.Translations
import Kernel.Beam.Functions
import qualified Kernel.External.Types
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.Translations as BeamEMT
import Storage.Queries.OrphanInstances.Translations ()

-- Extra code goes here --

findByErrorAndLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Kernel.External.Types.Language -> m (Maybe Domain.Types.Translations.Translations)
findByErrorAndLanguage messageKey language = do
  maybeTranslation <- findOneWithKV [Se.And [Se.Is BeamEMT.messageKey $ Se.Eq messageKey, Se.Is BeamEMT.language $ Se.Eq language]]
  case maybeTranslation of
    Just translation -> return (Just translation)
    Nothing -> findOneWithKV [Se.And [Se.Is BeamEMT.messageKey $ Se.Eq messageKey, Se.Is BeamEMT.language $ Se.Eq Kernel.External.Types.ENGLISH]]

isTranslationExist :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Kernel.External.Types.Language -> m Bool
isTranslationExist messageKey language = do
  isJust <$> findOneWithKV [Se.And [Se.Is BeamEMT.messageKey $ Se.Eq messageKey, Se.Is BeamEMT.language $ Se.Eq language]]
