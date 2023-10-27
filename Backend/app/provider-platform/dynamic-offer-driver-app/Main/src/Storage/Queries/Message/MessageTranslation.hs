{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Message.MessageTranslation where

import qualified Data.Time as T
import qualified Domain.Types.Message.Message as Msg
import Domain.Types.Message.MessageTranslation
import Kernel.Beam.Functions
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Message.MessageTranslation as BeamMT

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => MessageTranslation -> m ()
create = createWithKV

createMany :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [MessageTranslation] -> m ()
createMany = traverse_ createWithKV

findByMessageIdAndLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Msg.Message -> Language -> m (Maybe MessageTranslation)
findByMessageIdAndLanguage (Id messageId) language = findOneWithKV [Se.And [Se.Is BeamMT.messageId $ Se.Eq messageId, Se.Is BeamMT.language $ Se.Eq language]]

findByMessageId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Msg.Message -> m [MessageTranslation]
findByMessageId (Id messageId) = findAllWithKV [Se.Is BeamMT.messageId $ Se.Eq messageId]

instance FromTType' BeamMT.MessageTranslation MessageTranslation where
  fromTType' BeamMT.MessageTranslationT {..} = do
    pure $
      Just
        MessageTranslation
          { messageId = Id messageId,
            language = language,
            title = title,
            label = label,
            description = description,
            shortDescription = shortDescription,
            createdAt = T.localTimeToUTC T.utc createdAt
          }

instance ToTType' BeamMT.MessageTranslation MessageTranslation where
  toTType' MessageTranslation {..} = do
    BeamMT.MessageTranslationT
      { BeamMT.messageId = getId messageId,
        BeamMT.language = language,
        BeamMT.title = title,
        BeamMT.label = label,
        BeamMT.description = description,
        BeamMT.shortDescription = shortDescription,
        BeamMT.createdAt = T.utcToLocalTime T.utc createdAt
      }
