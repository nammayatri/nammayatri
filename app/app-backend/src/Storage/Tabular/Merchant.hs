{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Merchant where

import qualified Beckn.External.FCM.Types as FCM
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Geofencing (GeoRestriction)
import qualified Beckn.Types.Geofencing as Geo
import Beckn.Types.Id
import qualified Domain.Types.Merchant as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantT sql=merchant
      id Text
      shortId Text
      exoPhone Text Maybe
      exoPhoneCountryCode Text Maybe
      fcmUrl Text
      fcmJsonPath Text Maybe
      fcmRedisTokenKeyPrefix Text
      originRestriction GeoRestriction
      destinationRestriction GeoRestriction
      gatewayUrl Text
      registryUrl Text
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
        ..
      }
