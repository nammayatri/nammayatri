{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
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

create :: MessageTranslation -> SqlDB m ()
create = Esq.create

findByMessageIdAndLanguage :: forall m ma. Transactionable ma m => Id Msg.Message -> Language -> Proxy ma -> m (Maybe MessageTranslation)
findByMessageIdAndLanguage messageId language _ =
  Esq.findOne @m @ma $ do
    messageTranslation <- from $ table @MessageTranslationT
    where_ $
      messageTranslation ^. MessageTranslationTId ==. val (toKey (messageId, language))
    return messageTranslation

findByMessageId :: forall m ma. Transactionable ma m => Id Msg.Message -> Proxy ma -> m [MessageTranslation]
findByMessageId messageId _ =
  Esq.findAll @m @ma $ do
    messageTranslations <- from $ table @MessageTranslationT
    where_ $
      messageTranslations ^. MessageTranslationMessageId ==. val (toKey messageId)
    return messageTranslations
