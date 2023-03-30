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

module Storage.Tabular.Message.Message where

import qualified Domain.Types.Message.Message as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.MediaFile (MediaFileTId)
import Storage.Tabular.Merchant (MerchantTId)

derivePersistField "Domain.MessageType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MessageT sql=message
      id Text
      messageType Domain.MessageType sql=type
      title Text
      description Text
      label Text Maybe
      likeCount Int
      mediaFiles (PostgresList MediaFileTId)
      merchantId MerchantTId
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey MessageT where
  type DomainKey MessageT = Id Domain.Message
  fromKey (MessageTKey _id) = Id _id
  toKey (Id id) = MessageTKey id
