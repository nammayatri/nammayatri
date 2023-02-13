{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Message.Instances (FullMessageT) where

import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Message.Message
import Storage.Tabular.Message.MessageTranslation
import Domain.Types.Message.Message as Domain

instance TType MessageT Domain.RawMessage where
  fromTType MessageT {..} = do
    return $
      Domain.RawMessage
        { id = Id id,
          mediaFiles = map fromKey (unPostgresList mediaFiles),
          merchantId = fromKey merchantId, 
          _type = messageType,
          .. 
        }
  toTType Domain.RawMessage {..} = do
    MessageT
      { id = getId id,
        messageType = _type,
        mediaFiles = PostgresList (map toKey mediaFiles),
        merchantId = toKey merchantId, 
        ..
      }

type FullMessageT = (MessageT, [MessageTranslationT])

instance TType FullMessageT Domain.Message where
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
