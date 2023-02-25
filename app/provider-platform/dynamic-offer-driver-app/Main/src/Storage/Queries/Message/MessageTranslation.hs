{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Message.MessageTranslation where

import qualified Domain.Types.Message.Message as Msg
import Domain.Types.Message.MessageTranslation
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Message.MessageTranslation

create :: MessageTranslation -> SqlDB ()
create = Esq.create

findByMessageIdAndLanguage :: Transactionable m => Id Msg.Message -> Language -> m (Maybe MessageTranslation)
findByMessageIdAndLanguage messageId language =
  Esq.findOne $ do
    messageTranslation <- from $ table @MessageTranslationT
    where_ $
      messageTranslation ^. MessageTranslationTId ==. val (toKey (messageId, language))
    return messageTranslation

findByMessageId :: Transactionable m => Id Msg.Message -> m [MessageTranslation]
findByMessageId messageId =
  Esq.findAll $ do
    messageTranslations <- from $ table @MessageTranslationT
    where_ $
      messageTranslations ^. MessageTranslationMessageId ==. val (toKey messageId)
    return messageTranslations