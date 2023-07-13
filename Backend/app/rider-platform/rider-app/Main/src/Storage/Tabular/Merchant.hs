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

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Domain.Types.Merchant (Slot)
import qualified Domain.Types.Merchant as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Base64
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Geofencing (GeoRestriction)
import qualified Kernel.Types.Geofencing as Geo
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)

derivePersistField "ByteString"
mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantT sql=merchant
      id Text
      shortId Text
      subscriberId Text
      name Text
      city Context.City
      country Context.Country
      bapId Text
      bapUniqueKeyId Text
      originRestriction GeoRestriction
      destinationRestriction GeoRestriction
      gatewayUrl Text
      registryUrl Text
      driverOfferBaseUrl Text
      driverOfferApiKey Text
      driverOfferMerchantId Text
      geoHashPrecisionValue Int
      signingPublicKey Base64
      cipherText Base64 Maybe
      signatureExpiry Int
      updatedAt UTCTime
      createdAt UTCTime
      Primary id
      UniqueMerchantShortId shortId
      deriving Generic
      dirCacheSlot ByteString
    |]

instance TEntityKey MerchantT where
  type DomainKey MerchantT = Id Domain.Merchant
  fromKey (MerchantTKey _id) = Id _id
  toKey (Id id) = MerchantTKey id

instance FromTType MerchantT Domain.Merchant where
  fromTType MerchantT {..} = do
    let geofencingConfig =
          Geo.GeofencingConfig
            { origin = originRestriction,
              destination = destinationRestriction
            }
    gwUrl <- parseBaseUrl gatewayUrl
    regUrl <- parseBaseUrl registryUrl
    doBaseUrl <- parseBaseUrl driverOfferBaseUrl
    decodedSlot <- fromMaybeM (InternalError "Failed to parse Slots stored") (decode dirCacheSlot :: Maybe [Slot])
    return $
      Domain.Merchant
        { id = Id id,
          shortId = ShortId shortId,
          subscriberId = ShortId subscriberId,
          registryUrl = regUrl,
          gatewayUrl = gwUrl,
          driverOfferBaseUrl = doBaseUrl,
          dirCacheSlot = decodedSlot,
          ..
        }

instance ToTType MerchantT Domain.Merchant where
  toTType Domain.Merchant {..} = do
    let Geo.GeofencingConfig {..} = geofencingConfig
    MerchantT
      { id = getId id,
        shortId = getShortId shortId,
        subscriberId = getShortId subscriberId,
        originRestriction = origin,
        destinationRestriction = destination,
        gatewayUrl = showBaseUrl gatewayUrl,
        registryUrl = showBaseUrl registryUrl,
        driverOfferBaseUrl = showBaseUrl driverOfferBaseUrl,
        dirCacheSlot = encode dirCacheSlot,
        ..
      }
