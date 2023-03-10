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

module Storage.Tabular.Merchant where

import qualified Domain.Types.Merchant as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Geofencing
import Kernel.Types.Id

derivePersistField "Domain.Status"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantT sql=merchant
      id Text
      name Text
      description Text Maybe
      subscriberId Text
      uniqueKeyId Text
      shortId Text
      exoPhones (PostgresNonEmptyList Text)
      mobileNumber Text Maybe
      mobileCountryCode Text Maybe
      gstin Text Maybe
      fromTime UTCTime Maybe
      toTime UTCTime Maybe
      headCount Int Maybe
      status Domain.Status
      verified Bool
      enabled Bool
      internalApiKey Text
      createdAt UTCTime
      updatedAt UTCTime
      originRestriction GeoRestriction
      destinationRestriction GeoRestriction
      info Text Maybe
      Primary id
      Unique MerchantSubscriberId
      Unique MerchantShortId
      deriving Generic
    |]

instance TEntityKey MerchantT where
  type DomainKey MerchantT = Id Domain.Merchant
  fromKey (MerchantTKey _id) = Id _id
  toKey (Id id) = MerchantTKey id

instance TType MerchantT Domain.Merchant where
  fromTType MerchantT {..} = do
    return $
      Domain.Merchant
        { id = Id id,
          subscriberId = ShortId subscriberId,
          shortId = ShortId shortId,
          geofencingConfig =
            GeofencingConfig
              { origin = originRestriction,
                destination = destinationRestriction
              },
          exoPhones = unPostgresNonEmptyList exoPhones,
          ..
        }
  toTType Domain.Merchant {..} =
    MerchantT
      { id = getId id,
        subscriberId = getShortId subscriberId,
        shortId = getShortId shortId,
        originRestriction = geofencingConfig.origin,
        destinationRestriction = geofencingConfig.destination,
        exoPhones = PostgresNonEmptyList exoPhones,
        ..
      }
