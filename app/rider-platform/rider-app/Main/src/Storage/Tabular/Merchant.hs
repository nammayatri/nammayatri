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

instance TType MerchantT Domain.Merchant where
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
    return $
      Domain.Merchant
        { id = Id id,
          shortId = ShortId shortId,
          registryUrl = regUrl,
          gatewayUrl = gwUrl,
          exoPhones = unPostgresList exoPhones,
          ..
        }
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
        exoPhones = PostgresList exoPhones,
        ..
      }
