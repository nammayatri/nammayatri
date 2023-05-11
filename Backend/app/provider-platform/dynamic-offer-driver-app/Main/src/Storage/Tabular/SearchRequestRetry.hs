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

module Storage.Tabular.SearchRequestRetry where

import qualified Beckn.Types.Core.Taxi.Select as Select
import qualified Domain.Types.SearchRequestRetry as SearchRequestRetry
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.SearchRequest (SearchRequestTId)

derivePersistField "Select.RetryType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRetryT sql=search_retry
      id Text
      parentSearchId SearchRequestTId
      retryCreatedAt UTCTime
      retryType Select.RetryType
      Primary id
      deriving Generic
  |]

instance TEntityKey SearchRetryT where
  type DomainKey SearchRetryT = (Id SearchRequestRetry.SearchRequestRetry)
  fromKey (SearchRetryTKey _id) = Id _id
  toKey (Id id) = SearchRetryTKey id

instance FromTType SearchRetryT SearchRequestRetry.SearchRequestRetry where
  fromTType SearchRetryT {..} =
    return $
      SearchRequestRetry.SearchRequestRetry
        { id = Id id,
          parentSearchId = fromKey parentSearchId,
          ..
        }

instance ToTType SearchRetryT SearchRequestRetry.SearchRequestRetry where
  toTType SearchRequestRetry.SearchRequestRetry {..} =
    SearchRetryT
      { id = getId id,
        parentSearchId = toKey parentSearchId,
        ..
      }
