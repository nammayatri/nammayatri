{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverOnboarding.ErrorMessagesTranslations where

import Domain.Types.DriverOnboarding.ErrorMessagesTranslations
import Kernel.Beam.Functions
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.ErrorMessagesTranslations as BeamEMT

findByErrorAndLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Language -> m (Maybe ErrorMessagesTranslations)
findByErrorAndLanguage errorType language = do
  maybeTranslation <- findOneWithKV [Se.And [Se.Is BeamEMT.errorType $ Se.Eq errorType, Se.Is BeamEMT.language $ Se.Eq language]]
  case maybeTranslation of
    Just translation -> return (Just translation)
    Nothing -> findOneWithKV [Se.And [Se.Is BeamEMT.errorType $ Se.Eq errorType, Se.Is BeamEMT.language $ Se.Eq ENGLISH]]

instance FromTType' BeamEMT.ErrorMessagesTranslations ErrorMessagesTranslations where
  fromTType' BeamEMT.ErrorMessagesTranslationsT {..} = do
    pure $
      Just
        ErrorMessagesTranslations
          { id = Id id,
            errorType = errorType,
            language = language,
            errorMessage = errorMessage,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamEMT.ErrorMessagesTranslations ErrorMessagesTranslations where
  toTType' ErrorMessagesTranslations {..} = do
    BeamEMT.ErrorMessagesTranslationsT
      { BeamEMT.id = id.getId,
        BeamEMT.errorType = errorType,
        BeamEMT.language = language,
        BeamEMT.errorMessage = errorMessage,
        BeamEMT.createdAt = createdAt,
        BeamEMT.updatedAt = updatedAt
      }
