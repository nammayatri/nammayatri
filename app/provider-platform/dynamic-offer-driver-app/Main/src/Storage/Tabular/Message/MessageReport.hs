{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Message.MessageReport where

import qualified Domain.Types.Message.Message as Msg
import qualified Domain.Types.Message.MessageReport as Domain
import Domain.Types.Person (Driver)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Storage.Tabular.Message.Message as Msg
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.DeliveryStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MessageReportT sql=message_report
      messageId Msg.MessageTId
      driverId PersonTId
      deliveryStatus Domain.DeliveryStatus
      readStatus Bool
      reply Text Maybe
      messageDynamicFields Domain.MessageDynamicFieldsType
      updatedAt UTCTime
      createdAt UTCTime
      Primary messageId driverId
      deriving Generic
    |]

instance TEntityKey MessageReportT where
  type DomainKey MessageReportT = (Id Msg.Message, Id Driver)
  fromKey (MessageReportTKey _messageId _driverId) = (fromKey _messageId, cast (fromKey _driverId))
  toKey (messageId, driverId) = MessageReportTKey (toKey messageId) (toKey $ cast driverId)

instance TType MessageReportT Domain.MessageReport where
  fromTType MessageReportT {..} = do
    return $
      Domain.MessageReport
        { messageId = fromKey messageId,
          driverId = cast $ fromKey driverId,
          ..
        }
  toTType Domain.MessageReport {..} =
    MessageReportT
      { messageId = toKey messageId,
        driverId = toKey $ cast driverId,
        ..
      }
