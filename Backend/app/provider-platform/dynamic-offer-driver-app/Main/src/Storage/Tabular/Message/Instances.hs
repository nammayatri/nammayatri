{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Message.Instances (FullMessageT) where

import Domain.Types.Message.Message as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Message.Message
import Storage.Tabular.Message.MessageTranslation

instance FromTType MessageT Domain.RawMessage where
  fromTType MessageT {..} = do
    return $
      Domain.RawMessage
        { id = Id id,
          mediaFiles = map fromKey (unPostgresList mediaFiles),
          merchantId = fromKey merchantId,
          _type = messageType,
          ..
        }

instance ToTType MessageT Domain.RawMessage where
  toTType Domain.RawMessage {..} = do
    MessageT
      { id = getId id,
        messageType = _type,
        mediaFiles = PostgresList (map toKey mediaFiles),
        merchantId = toKey merchantId,
        ..
      }

type FullMessageT = (MessageT, [MessageTranslationT])

instance FromTType FullMessageT Domain.Message where
  fromTType (MessageT {..}, messageTranslationsT) = do
    let messageTranslations = mkMessageTranslation <$> messageTranslationsT
    return $
      Domain.Message
        { id = Id id,
          mediaFiles = map fromKey (unPostgresList mediaFiles),
          merchantId = fromKey merchantId,
          _type = messageType,
          ..
        }

instance ToTType FullMessageT Domain.Message where
  toTType Domain.Message {..} = do
    let messageT =
          MessageT
            { id = getId id,
              messageType = _type,
              mediaFiles = PostgresList (map toKey mediaFiles),
              merchantId = toKey merchantId,
              ..
            }
    let messageTranslationsT = mkMessageTranslationsT id <$> messageTranslations
    (messageT, messageTranslationsT)

mkMessageTranslation :: MessageTranslationT -> Domain.MessageTranslation
mkMessageTranslation MessageTranslationT {..} =
  Domain.MessageTranslation
    { ..
    }

mkMessageTranslationsT :: Id Message -> Domain.MessageTranslation -> MessageTranslationT
mkMessageTranslationsT messageId Domain.MessageTranslation {..} =
  MessageTranslationT
    { messageId = toKey messageId,
      ..
    }
