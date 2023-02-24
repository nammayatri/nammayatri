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
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Message.MediaFile (MediaFileTId)

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
