{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Message.MediaFile where

import qualified Domain.Types.Message.MediaFile as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

derivePersistField "Domain.MediaType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MediaFileT sql=media_file
      id Text
      fileType Domain.MediaType sql=type
      url Text
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey MediaFileT where
  type DomainKey MediaFileT = Id Domain.MediaFile
  fromKey (MediaFileTKey _id) = Id _id
  toKey (Id id) = MediaFileTKey id

instance TType MediaFileT Domain.MediaFile where
  fromTType MediaFileT {..} = do
    return $
      Domain.MediaFile
        { id = Id id,
          _type = fileType,
          ..
        }
  toTType Domain.MediaFile {..} =
    MediaFileT
      { id = getId id,
        fileType = _type,
        ..
      }
