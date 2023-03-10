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

module Storage.Tabular.Search where

import qualified Domain.Types.Search as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchT sql=search
      id Text
      lat Double
      lon Double
      requestorId Text
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchT where
  type DomainKey SearchT = Id Domain.Search
  fromKey (SearchTKey _id) = Id _id
  toKey id = SearchTKey id.getId

instance FromTType SearchT Domain.Search where
  fromTType SearchT {..} = do
    return $
      Domain.Search
        { id = Id id,
          requestorId = Id requestorId,
          ..
        }

instance ToTType SearchT Domain.Search where
  toTType Domain.Search {..} =
    SearchT
      { id = id.getId,
        requestorId = requestorId.getId,
        ..
      }
