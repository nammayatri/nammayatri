{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Message.MediaFile where

import qualified Domain.Types.Message.MediaFile as Domain
import Kernel.Storage.Esqueleto
import Kernel.Prelude
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
    baseUrl <- parseBaseUrl url
    return $
      Domain.MediaFile
        { id = Id id,
          url = baseUrl, 
          _type = fileType,
          ..
        }
  toTType Domain.MediaFile {..} =
    MediaFileT
      { id = getId id,
        url = showBaseUrl url,
        fileType = _type,
        ..
      }


