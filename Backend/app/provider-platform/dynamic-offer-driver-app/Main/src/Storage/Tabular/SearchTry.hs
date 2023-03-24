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
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Storage.Tabular.Estimate (EstimateTId)
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.SearchRequest.SearchReqLocation (SearchReqLocationT, SearchReqLocationTId, mkDomainSearchReqLocation, mkTabularSearchReqLocation)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.SearchTryStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchTryT sql=search_try
      id Text
      transactionId Text
      messageId Text
      estimateId EstimateTId
      startTime UTCTime
      validTill UTCTime
      providerId MerchantTId
      fromLocationId SearchReqLocationTId
      toLocationId SearchReqLocationTId
      bapId Text
      bapUri Text
      estimatedDistance Meters
      estimatedDuration Seconds
      customerExtraFee Money Maybe
      device Text Maybe
      status Domain.SearchTryStatus
      vehicleVariant Variant.Variant
      searchRepeatCounter Int
      autoAssignEnabled Bool
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchTryT where
  type DomainKey SearchTryT = Id Domain.SearchTry
  fromKey (SearchTryTKey _id) = Id _id
  toKey (Id id) = SearchTryTKey id

type FullSearchTryT = (SearchTryT, SearchReqLocationT, SearchReqLocationT)

instance FromTType FullSearchTryT Domain.SearchTry where
  fromTType (SearchTryT {..}, fromLoc, toLoc) = do
    pUrl <- parseBaseUrl bapUri
    let fromLoc_ = mkDomainSearchReqLocation fromLoc
        toLoc_ = mkDomainSearchReqLocation toLoc

    return $
      Domain.SearchTry
        { id = Id id,
          estimateId = fromKey estimateId,
          providerId = fromKey providerId,
          fromLocation = fromLoc_,
          toLocation = toLoc_,
          bapUri = pUrl,
          ..
        }

instance ToTType FullSearchTryT Domain.SearchTry where
  toTType Domain.SearchTry {..} =
    ( SearchTryT
        { id = getId id,
          estimateId = toKey estimateId,
          providerId = toKey providerId,
          fromLocationId = toKey fromLocation.id,
          toLocationId = toKey toLocation.id,
          bapUri = showBaseUrl bapUri,
          ..
        },
      mkTabularSearchReqLocation fromLocation,
      mkTabularSearchReqLocation toLocation
    )
