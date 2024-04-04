{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TranslationsExtra where

import qualified Data.Text
import qualified Domain.Types.Translations
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Translations as Beam
import qualified Storage.Beam.Translations as BeamEMT
import Storage.Queries.OrphanInstances.Translations

-- Extra code goes here --

findByErrorAndLanguage :: KvDbFlow m r => Text -> Kernel.External.Types.Language -> m (Maybe Domain.Types.Translations.Translations)
findByErrorAndLanguage messageKey language = do
  maybeTranslation <- findOneWithKV [Se.And [Se.Is BeamEMT.messageKey $ Se.Eq messageKey, Se.Is BeamEMT.language $ Se.Eq language]]
  case maybeTranslation of
    Just translation -> return (Just translation)
    Nothing -> findOneWithKV [Se.And [Se.Is BeamEMT.messageKey $ Se.Eq messageKey, Se.Is BeamEMT.language $ Se.Eq Kernel.External.Types.ENGLISH]]
