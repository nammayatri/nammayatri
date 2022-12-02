{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Merchant.MerchantServiceConfig where

import qualified Beckn.External.Maps as Maps
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Utils.Common (decodeFromText, encodeToText)
import Beckn.Utils.Error
import qualified Domain.Types.Merchant as Domain
import qualified Domain.Types.Merchant.MerchantServiceConfig as Domain
import Storage.Tabular.Merchant (MerchantTId)
import Tools.Error

derivePersistField "Domain.ServiceName"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantServiceConfigT sql=merchant_service_config
      merchantId MerchantTId
      serviceName Domain.ServiceName
      configJSON Text sql=config_json
      updatedAt UTCTime
      createdAt UTCTime
      Primary merchantId
      deriving Generic
    |]

instance TEntityKey MerchantServiceConfigT where
  type DomainKey MerchantServiceConfigT = Id Domain.Merchant
  fromKey (MerchantServiceConfigTKey _id) = fromKey _id
  toKey id = MerchantServiceConfigTKey $ toKey id

instance TType MerchantServiceConfigT Domain.MerchantServiceConfig where
  fromTType MerchantServiceConfigT {..} = do
    serviceConfig <- maybe (throwError $ InternalError "Unable to decode MerchantServiceConfigT.configJSON") return $ case serviceName of
      Domain.MapsService Maps.Google -> Domain.MapsServiceConfig . Maps.GoogleConfig <$> decodeFromText configJSON
      Domain.MapsService Maps.OSRM -> Domain.MapsServiceConfig . Maps.OSRMConfig <$> decodeFromText configJSON
      Domain.MapsService Maps.MMI -> Domain.MapsServiceConfig . Maps.MMIConfig <$> decodeFromText configJSON
    return $
      Domain.MerchantServiceConfig
        { merchantId = fromKey merchantId,
          ..
        }
  toTType Domain.MerchantServiceConfig {..} = do
    MerchantServiceConfigT
      { merchantId = toKey merchantId,
        ..
      }
    where
      (serviceName, configJSON) = case serviceConfig of
        Domain.MapsServiceConfig mapsCfg -> case mapsCfg of
          Maps.GoogleConfig cfg -> (Domain.MapsService Maps.Google, encodeToText cfg)
          Maps.OSRMConfig cfg -> (Domain.MapsService Maps.OSRM, encodeToText cfg)
          Maps.MMIConfig cfg -> (Domain.MapsService Maps.MMI, encodeToText cfg)
