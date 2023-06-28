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

module Storage.Tabular.SearchRequest where

import qualified Domain.Types.SearchRequest as Domain
import qualified Domain.Types.Vehicle.Variant as Variant (Variant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Storage.Tabular.Estimate (EstimateTId)
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.SearchRequestStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRequestT sql=search_request
      id Text
      transactionId Text
      messageId Text
      estimateId EstimateTId
      startTime UTCTime
      validTill UTCTime
      providerId MerchantTId
      bapId Text
      bapUri Text
      estimatedDistance Meters
      estimatedDuration Seconds
      customerExtraFee Money Maybe
      device Text Maybe
      status Domain.SearchRequestStatus
      vehicleVariant Variant.Variant
      searchRepeatCounter Int
      autoAssignEnabled Bool
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchRequestT where
  type DomainKey SearchRequestT = Id Domain.SearchRequest
  fromKey (SearchRequestTKey _id) = Id _id
  toKey (Id id) = SearchRequestTKey id

instance FromTType SearchRequestT Domain.SearchRequestTable where
  fromTType SearchRequestT {..} = do
    pUrl <- parseBaseUrl bapUri

    return $
      Domain.SearchRequestTable
        { id = Id id,
          estimateId = fromKey estimateId,
          providerId = fromKey providerId,
          bapUri = pUrl,
          ..
        }

instance ToTType SearchRequestT Domain.SearchRequestTable where
  toTType Domain.SearchRequestTable {..} =
    SearchRequestT
      { id = getId id,
        estimateId = toKey estimateId,
        providerId = toKey providerId,
        bapUri = showBaseUrl bapUri,
        ..
      }
