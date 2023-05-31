{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Message.MessageTranslation where

import qualified Domain.Types.Message.Message as Msg
import Domain.Types.Message.MessageTranslation
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Message.MessageTranslation as BeamMT

-- create :: MessageTranslation -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => MessageTranslation -> m (MeshResult ())
create messageTranslation = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainMessageTranslationToBeam messageTranslation)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findByMessageIdAndLanguage :: Transactionable m => Id Msg.Message -> Language -> m (Maybe MessageTranslation)
-- findByMessageIdAndLanguage messageId language =
--   Esq.findOne $ do
--     messageTranslation <- from $ table @MessageTranslationT
--     where_ $
--       messageTranslation ^. MessageTranslationTId ==. val (toKey (messageId, language))
--     return messageTranslation

findByMessageIdAndLanguage :: L.MonadFlow m => Id Msg.Message -> Language -> m (Maybe MessageTranslation)
findByMessageIdAndLanguage (Id messageId) language = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.And [Se.Is BeamMT.messageId $ Se.Eq messageId, Se.Is BeamMT.language $ Se.Eq language]]
      case result of
        Right mt -> pure $ transformBeamMessageTranslationToDomain <$> mt
        Left _ -> pure Nothing
    Nothing -> pure Nothing

-- findByMessageId :: Transactionable m => Id Msg.Message -> m [MessageTranslation]
-- findByMessageId messageId =
--   Esq.findAll $ do
--     messageTranslations <- from $ table @MessageTranslationT
--     where_ $
--       messageTranslations ^. MessageTranslationMessageId ==. val (toKey messageId)
--     return messageTranslations

findByMessageId :: L.MonadFlow m => Id Msg.Message -> m [MessageTranslation]
findByMessageId (Id messageId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamMessageTranslationToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamMT.messageId $ Se.Eq messageId]
    Nothing -> pure []

transformBeamMessageTranslationToDomain :: BeamMT.MessageTranslation -> MessageTranslation
transformBeamMessageTranslationToDomain BeamMT.MessageTranslationT {..} = do
  MessageTranslation
    { messageId = Id messageId,
      language = language,
      title = title,
      label = label,
      description = description,
      shortDescription = shortDescription,
      createdAt = createdAt
    }

transformDomainMessageTranslationToBeam :: MessageTranslation -> BeamMT.MessageTranslation
transformDomainMessageTranslationToBeam MessageTranslation {..} =
  BeamMT.MessageTranslationT
    { BeamMT.messageId = getId messageId,
      BeamMT.language = language,
      BeamMT.title = title,
      BeamMT.label = label,
      BeamMT.description = description,
      BeamMT.shortDescription = shortDescription,
      BeamMT.createdAt = createdAt
    }
