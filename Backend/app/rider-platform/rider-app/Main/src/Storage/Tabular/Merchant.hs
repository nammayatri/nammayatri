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
import qualified Kernel.External.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Geofencing (GeoRestriction)
import qualified Kernel.Types.Geofencing as Geo
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantT sql=merchant
      id Text
      shortId Text
      name Text
      exoPhone Text Maybe
      exoPhones (PostgresList Text)
      exoPhoneCountryCode Text Maybe
      fcmUrl Text
      fcmServiceAccount Text
      fcmRedisTokenKeyPrefix Text
      originRestriction GeoRestriction
      destinationRestriction GeoRestriction
      gatewayUrl Text
      registryUrl Text
      driverOfferBaseUrl Text
      driverOfferApiKey Text
      driverOfferMerchantId Text
      updatedAt UTCTime
      createdAt UTCTime
      Primary id
      Unique MerchantShortId
      deriving Generic
    |]

instance TEntityKey MerchantT where
  type DomainKey MerchantT = Id Domain.Merchant
  fromKey (MerchantTKey _id) = Id _id
  toKey (Id id) = MerchantTKey id

instance FromTType MerchantT Domain.Merchant where
  fromTType MerchantT {..} = do
    fcmUrl_ <- parseBaseUrl fcmUrl
    let fcmConfig =
          FCM.FCMConfig
            { fcmUrl = fcmUrl_,
              fcmTokenKeyPrefix = fcmRedisTokenKeyPrefix,
              ..
            }
        geofencingConfig =
          Geo.GeofencingConfig
            { origin = originRestriction,
              destination = destinationRestriction
            }
    gwUrl <- parseBaseUrl gatewayUrl
    regUrl <- parseBaseUrl registryUrl
    doBaseUrl <- parseBaseUrl driverOfferBaseUrl
    return $
      Domain.Merchant
        { id = Id id,
          shortId = ShortId shortId,
          registryUrl = regUrl,
          gatewayUrl = gwUrl,
          exoPhones = unPostgresList exoPhones,
          driverOfferBaseUrl = doBaseUrl,
          ..
        }

instance ToTType MerchantT Domain.Merchant where
  toTType Domain.Merchant {..} = do
    let FCM.FCMConfig {..} = fcmConfig
        Geo.GeofencingConfig {..} = geofencingConfig
    MerchantT
      { id = getId id,
        shortId = getShortId shortId,
        fcmUrl = showBaseUrl fcmUrl,
        fcmRedisTokenKeyPrefix = fcmTokenKeyPrefix,
        originRestriction = origin,
        destinationRestriction = destination,
        gatewayUrl = showBaseUrl gatewayUrl,
        registryUrl = showBaseUrl registryUrl,
        driverOfferBaseUrl = showBaseUrl driverOfferBaseUrl,
        exoPhones = PostgresList exoPhones,
        ..
      }
