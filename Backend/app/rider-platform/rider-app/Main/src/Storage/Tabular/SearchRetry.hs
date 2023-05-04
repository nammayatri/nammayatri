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

module Storage.Tabular.SearchRetry where

import qualified Beckn.Types.Core.Taxi.Select as Select
import Domain.Types.SearchRequest (SearchRequest)
import qualified Domain.Types.SearchRetry as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.SearchRequest (SearchRequestTId)

derivePersistField "Select.RetryType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRetryT sql=search_retry
      id SearchRequestTId
      parentSearchId SearchRequestTId
      retryCreatedAt UTCTime
      retryType Select.RetryType
      Primary id
      deriving Generic
  |]

instance TEntityKey SearchRetryT where
  type DomainKey SearchRetryT = Id SearchRequest
  fromKey (SearchRetryTKey _id) = fromKey _id
  toKey id = SearchRetryTKey $ toKey id

instance FromTType SearchRetryT Domain.SearchRetry where
  fromTType SearchRetryT {..} = do
    return $
      Domain.SearchRetry
        { id = fromKey id,
          parentSearchId = fromKey parentSearchId,
          ..
        }

instance ToTType SearchRetryT Domain.SearchRetry where
  toTType Domain.SearchRetry {..} =
    SearchRetryT
      { id = toKey id,
        parentSearchId = toKey parentSearchId,
        ..
      }
