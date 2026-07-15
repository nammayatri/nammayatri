module Storage.Queries.TranslationsExtra where

import Data.List (nub)
import qualified Domain.Types.Translations
import Kernel.Beam.Functions
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.Translations as BeamEMT
import Storage.Queries.OrphanInstances.Translations ()

-- Extra code goes here --

findByErrorAndLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Kernel.External.Types.Language -> m (Maybe Domain.Types.Translations.Translations)
findByErrorAndLanguage messageKey language = IM.withInMemCache ["FOT", messageKey, show language] 3600 $ do
  maybeTranslation <- findOneWithKV [Se.And [Se.Is BeamEMT.messageKey $ Se.Eq messageKey, Se.Is BeamEMT.language $ Se.Eq language]]
  case maybeTranslation of
    Just translation -> return (Just translation)
    Nothing -> findOneWithKV [Se.And [Se.Is BeamEMT.messageKey $ Se.Eq messageKey, Se.Is BeamEMT.language $ Se.Eq Kernel.External.Types.ENGLISH]]

isTranslationExist :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Kernel.External.Types.Language -> m Bool
isTranslationExist messageKey language = do
  isJust <$> findOneWithKV [Se.And [Se.Is BeamEMT.messageKey $ Se.Eq messageKey, Se.Is BeamEMT.language $ Se.Eq language]]

findAllByMessageKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [Domain.Types.Translations.Translations]
findAllByMessageKey messageKey = findAllWithKV [Se.Is BeamEMT.messageKey $ Se.Eq messageKey]

findAllByMessageKeysAndLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Text] -> Kernel.External.Types.Language -> m [Domain.Types.Translations.Translations]
findAllByMessageKeysAndLanguage [] _ = pure []
findAllByMessageKeysAndLanguage keys language =
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamEMT.messageKey $ Se.In keys,
          Se.Is BeamEMT.language $ Se.In (nub [language, Kernel.External.Types.ENGLISH])
        ]
    ]
    Nothing
