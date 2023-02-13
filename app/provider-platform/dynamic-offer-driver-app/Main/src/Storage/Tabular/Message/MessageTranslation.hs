{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Message.MessageTranslation where

import qualified Domain.Types.Message.MessageTranslation as Domain
import Kernel.Storage.Esqueleto
import Kernel.Prelude
import Kernel.Types.Id
import qualified Storage.Tabular.Message.Message as Msg
import Kernel.External.Types (Language)
import qualified Domain.Types.Message.Message as Msg
import Storage.Tabular.Person ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MessageTranslationT sql=message_translation
      messageId Msg.MessageTId
      language Language 
      title Text
      description Text
      createdAt UTCTime
      Primary messageId language
      deriving Generic
    |]

instance TEntityKey MessageTranslationT where
  type DomainKey MessageTranslationT = (Id Msg.Message, Language)
  fromKey (MessageTranslationTKey _messageId language) = (fromKey _messageId ,language)
  toKey (messageId ,language) = MessageTranslationTKey (toKey messageId) language

instance TType MessageTranslationT Domain.MessageTranslation where
  fromTType MessageTranslationT {..} = do
    return $
      Domain.MessageTranslation
        { messageId = fromKey messageId,
          ..
        }
  toTType Domain.MessageTranslation {..} =
    MessageTranslationT
      { messageId = toKey messageId,
        ..
      }
