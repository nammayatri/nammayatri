{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Message.MessageTranslation where

import qualified Domain.Types.Message.Message as Msg
import qualified Domain.Types.Message.MessageTranslation as Domain
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Storage.Tabular.Message.Message as Msg
import Storage.Tabular.Person ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MessageTranslationT sql=message_translation
      messageId Msg.MessageTId
      language Language
      title Text
      description Text
      label Text Maybe
      createdAt UTCTime
      Primary messageId language
      deriving Generic
    |]

instance TEntityKey MessageTranslationT where
  type DomainKey MessageTranslationT = (Id Msg.Message, Language)
  fromKey (MessageTranslationTKey _messageId language) = (fromKey _messageId, language)
  toKey (messageId, language) = MessageTranslationTKey (toKey messageId) language

instance FromTType MessageTranslationT Domain.MessageTranslation where
  fromTType MessageTranslationT {..} = do
    return $
      Domain.MessageTranslation
        { messageId = fromKey messageId,
          ..
        }

instance ToTType MessageTranslationT Domain.MessageTranslation where
  toTType Domain.MessageTranslation {..} =
    MessageTranslationT
      { messageId = toKey messageId,
        ..
      }
