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
