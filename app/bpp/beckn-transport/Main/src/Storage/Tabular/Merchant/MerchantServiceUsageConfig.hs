{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Merchant.MerchantServiceUsageConfig where

import Beckn.External.Maps.Types
import Beckn.External.SMS.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Merchant as Domain
import qualified Domain.Types.Merchant.MerchantServiceUsageConfig as Domain
import Storage.Tabular.Merchant (MerchantTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantServiceUsageConfigT sql=merchant_service_usage_config
      merchantId MerchantTId
      getDistances MapsService
      getRoutes MapsService
      snapToRoad MapsService
      getPlaceName MapsService
      getPlaceDetails MapsService
      autoComplete MapsService
      updatedAt UTCTime
      createdAt UTCTime
      Primary merchantId
      deriving Generic
      smsProvidersPriorityList [SmsService]
    |]

instance TEntityKey MerchantServiceUsageConfigT where
  type DomainKey MerchantServiceUsageConfigT = Id Domain.Merchant
  fromKey (MerchantServiceUsageConfigTKey _id) = fromKey _id
  toKey id = MerchantServiceUsageConfigTKey $ toKey id

instance TType MerchantServiceUsageConfigT Domain.MerchantServiceUsageConfig where
  fromTType MerchantServiceUsageConfigT {..} = do
    return $
      Domain.MerchantServiceUsageConfig
        { merchantId = fromKey merchantId,
          ..
        }
  toTType Domain.MerchantServiceUsageConfig {..} = do
    MerchantServiceUsageConfigT
      { merchantId = toKey merchantId,
        ..
      }
