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

instance FromTType MediaFileT Domain.MediaFile where
  fromTType MediaFileT {..} = do
    return $
      Domain.MediaFile
        { id = Id id,
          _type = fileType,
          ..
        }

instance ToTType MediaFileT Domain.MediaFile where
  toTType Domain.MediaFile {..} =
    MediaFileT
      { id = getId id,
        fileType = _type,
        ..
      }
