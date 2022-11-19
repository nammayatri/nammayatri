{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Merchant where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Geofencing (GeoRestriction)
import qualified Beckn.Types.Geofencing as Geo
import Beckn.Types.Id
import qualified Domain.Types.Merchant as Domain

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
      mobileNumber Text Maybe
      mobileCountryCode Text Maybe
      gstin Text Maybe
      fromTime UTCTime Maybe
      toTime UTCTime Maybe
      headCount Int Maybe
      status Domain.Status
      verified Bool
      enabled Bool
      createdAt UTCTime
      updatedAt UTCTime
      info Text Maybe
      originRestriction GeoRestriction
      destinationRestriction GeoRestriction
      Primary id
      Unique MerchantShortId
      Unique MerchantSubscriberId
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
            Geo.GeofencingConfig
              { origin = originRestriction,
                destination = destinationRestriction
              },
          ..
        }
  toTType Domain.Merchant {..} =
    MerchantT
      { id = getId id,
        subscriberId = getShortId subscriberId,
        shortId = getShortId shortId,
        originRestriction = geofencingConfig.origin,
        destinationRestriction = geofencingConfig.destination,
        ..
      }
