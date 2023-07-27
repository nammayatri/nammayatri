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

import qualified Domain.Types.FareProduct as FareProductD
import qualified Domain.Types.SearchRequest as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Storage.Tabular.Location hiding (id)
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()
import qualified Tools.Maps as Maps

derivePersistField "FareProductD.Area"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRequestT sql=search_request
      id Text
      transactionId Text
      providerId MerchantTId
      area FareProductD.Area Maybe
      bapId Text
      bapUri Text
      bapCity Context.City Maybe
      bapCountry Context.Country Maybe
      estimatedDistance Meters
      estimatedDuration Seconds
      device Text Maybe
      autoAssignEnabled Bool Maybe
      customerLanguage Maps.Language Maybe
      specialLocationTag Text Maybe
      createdAt UTCTime

      Primary id
      deriving Generic
    |]

instance TEntityKey SearchRequestT where
  type DomainKey SearchRequestT = Id Domain.SearchRequest
  fromKey (SearchRequestTKey _id) = Id _id
  toKey (Id id) = SearchRequestTKey id

type FullSearchRequestT = (SearchRequestT, LocationT, [LocationT])

instance FromTType FullSearchRequestT Domain.SearchRequest where
  fromTType (SearchRequestT {..}, fromLoc, mbToLoc) = do
    fromLocation <- fromTType fromLoc
    toLocation <- mapM fromTType mbToLoc
    pUrl <- parseBaseUrl bapUri
    return $
      Domain.SearchRequest
        { id = Id id,
          providerId = fromKey providerId,
          bapUri = pUrl,
          ..
        }

instance ToTType FullSearchRequestT Domain.SearchRequest where
  toTType Domain.SearchRequest {..} =
    ( SearchRequestT
        { id = getId id,
          providerId = toKey providerId,
          bapUri = showBaseUrl bapUri,
          ..
        },
      toTType fromLocation,
      map toTType toLocation
    )
