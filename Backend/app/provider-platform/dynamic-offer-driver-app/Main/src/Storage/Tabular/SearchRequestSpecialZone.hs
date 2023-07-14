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

module Storage.Tabular.SearchRequestSpecialZone where

import qualified Domain.Types.FareProduct as FareProductD
import qualified Domain.Types.SearchRequestSpecialZone as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Storage.Tabular.Location as Loc
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

derivePersistField "FareProductD.Area"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRequestSpecialZoneT sql=search_request_special_zone
      id Text
      transactionId Text
      messageId Text
      startTime UTCTime
      validTill UTCTime
      providerId MerchantTId
      area FareProductD.Area Maybe
      bapId Text
      bapUri Text
      estimatedDistance Meters
      estimatedDuration Seconds
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchRequestSpecialZoneT where
  type DomainKey SearchRequestSpecialZoneT = Id Domain.SearchRequestSpecialZone
  fromKey (SearchRequestSpecialZoneTKey _id) = Id _id
  toKey (Id id) = SearchRequestSpecialZoneTKey id

type FullSearchRequestSpecialZoneT = (SearchRequestSpecialZoneT, Loc.LocationT, [Loc.LocationT])

instance FromTType FullSearchRequestSpecialZoneT Domain.SearchRequestSpecialZone where
  fromTType (SearchRequestSpecialZoneT {..}, fromLoc, mbToLoc) = do
    pUrl <- parseBaseUrl bapUri
    fromLocation <- fromTType fromLoc
    toLocation <- mapM fromTType mbToLoc
    return $
      Domain.SearchRequestSpecialZone
        { id = Id id,
          providerId = fromKey providerId,
          bapUri = pUrl,
          ..
        }

instance ToTType FullSearchRequestSpecialZoneT Domain.SearchRequestSpecialZone where
  toTType Domain.SearchRequestSpecialZone {..} =
    ( SearchRequestSpecialZoneT
        { id = getId id,
          providerId = toKey providerId,
          bapUri = showBaseUrl bapUri,
          ..
        },
      toTType fromLocation,
      map toTType toLocation
    )
