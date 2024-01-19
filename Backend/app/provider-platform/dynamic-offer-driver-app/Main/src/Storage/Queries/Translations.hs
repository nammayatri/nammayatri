{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Translations where

import Domain.Types.Translations
import Kernel.Beam.Functions
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import qualified Storage.Beam.Translations as BeamEMT

findByErrorAndLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Language -> m (Maybe Translations)
findByErrorAndLanguage messageKey language = do
  maybeTranslation <- findOneWithKV [Se.And [Se.Is BeamEMT.messageKey $ Se.Eq messageKey, Se.Is BeamEMT.language $ Se.Eq language]]
  case maybeTranslation of
    Just translation -> return (Just translation)
    Nothing -> findOneWithKV [Se.And [Se.Is BeamEMT.messageKey $ Se.Eq messageKey, Se.Is BeamEMT.language $ Se.Eq ENGLISH]]

instance FromTType' BeamEMT.Translations Translations where
  fromTType' BeamEMT.TranslationsT {..} = do
    pure $
      Just
        Translations
          { id = Id id,
            messageKey = messageKey,
            language = language,
            message = message,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamEMT.Translations Translations where
  toTType' Translations {..} = do
    BeamEMT.TranslationsT
      { BeamEMT.id = id.getId,
        BeamEMT.messageKey = messageKey,
        BeamEMT.language = language,
        BeamEMT.message = message,
        BeamEMT.createdAt = createdAt,
        BeamEMT.updatedAt = updatedAt
      }
