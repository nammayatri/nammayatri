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

module Lib.Storage.Tabular.Message.MessageReport where

import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Lib.Domain.Types.Message.Message as Msg
import qualified Lib.Domain.Types.Message.MessageReport as Domain
import qualified Lib.Storage.Tabular.Message.Message as Msg

derivePersistField "Domain.DeliveryStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MessageReportT sql=message_report
      messageId Msg.MessageTId
      personId Text
      deliveryStatus Domain.DeliveryStatus
      readStatus Bool
      likeStatus Bool
      reply Text Maybe
      messageDynamicFields Domain.MessageDynamicFieldsType
      updatedAt UTCTime
      createdAt UTCTime
      Primary messageId personId
      deriving Generic
    |]

instance TEntityKey MessageReportT where
  type DomainKey MessageReportT = (Id Msg.Message, Text)
  fromKey (MessageReportTKey _messageId _personId) = (fromKey _messageId, _personId)
  toKey (messageId, personId) = MessageReportTKey (toKey messageId) personId

instance FromTType MessageReportT Domain.MessageReport where
  fromTType MessageReportT {..} = do
    return $
      Domain.MessageReport
        { messageId = fromKey messageId,
          personId = Id personId,
          ..
        }

instance ToTType MessageReportT Domain.MessageReport where
  toTType Domain.MessageReport {..} =
    MessageReportT
      { messageId = toKey messageId,
        personId = personId.getId,
        ..
      }
