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

module Storage.Tabular.SearchTry where

import qualified Domain.Types.SearchTry as Domain
import qualified Domain.Types.Vehicle.Variant as Variant (Variant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Storage.Tabular.Estimate (EstimateTId)
import Storage.Tabular.SearchRequest (SearchRequestTId)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.SearchTryStatus"
derivePersistField "Domain.SearchRepeatType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchTryT sql=search_try
      id Text
      messageId Text
      requestId SearchRequestTId
      estimateId EstimateTId
      startTime UTCTime
      validTill UTCTime
      customerExtraFee Money Maybe
      status Domain.SearchTryStatus
      vehicleVariant Variant.Variant
      searchRepeatCounter Int
      searchRepeatType Domain.SearchRepeatType
      createdAt UTCTime
      updatedAt UTCTime

      Primary id
      deriving Generic
    |]

instance TEntityKey SearchTryT where
  type DomainKey SearchTryT = Id Domain.SearchTry
  fromKey (SearchTryTKey _id) = Id _id
  toKey (Id id) = SearchTryTKey id

instance FromTType SearchTryT Domain.SearchTry where
  fromTType SearchTryT {..} = do
    return $
      Domain.SearchTry
        { id = Id id,
          requestId = fromKey requestId,
          estimateId = fromKey estimateId,
          ..
        }

instance ToTType SearchTryT Domain.SearchTry where
  toTType Domain.SearchTry {..} =
    SearchTryT
      { id = getId id,
        requestId = toKey requestId,
        estimateId = toKey estimateId,
        ..
      }
