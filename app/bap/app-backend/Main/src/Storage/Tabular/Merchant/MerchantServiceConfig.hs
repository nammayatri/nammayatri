{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Merchant.MerchantServiceConfig where

import qualified Beckn.External.Maps.Interface as Maps
import qualified Beckn.External.SMS.Interface as Sms
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id (Id)
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
      UniqueMerchantServiceConfigTId merchantId serviceName
      Primary merchantId serviceName
      deriving Generic
    |]

instance TEntityKey MerchantServiceConfigT where
  type DomainKey MerchantServiceConfigT = (Id Domain.Merchant, Domain.ServiceName)
  fromKey (MerchantServiceConfigTKey _id serviceName) = (fromKey _id, serviceName)
  toKey (id, serviceName) = MerchantServiceConfigTKey (toKey id) serviceName

instance TType MerchantServiceConfigT Domain.MerchantServiceConfig where
  fromTType MerchantServiceConfigT {..} = do
    serviceConfig <- maybe (throwError $ InternalError "Unable to decode MerchantServiceConfigT.configJSON") return $ case serviceName of
      Domain.MapsService Maps.Google -> Domain.MapsServiceConfig . Maps.GoogleConfig <$> decodeFromText configJSON
      Domain.MapsService Maps.OSRM -> Domain.MapsServiceConfig . Maps.OSRMConfig <$> decodeFromText configJSON
      Domain.MapsService Maps.MMI -> Domain.MapsServiceConfig . Maps.MMIConfig <$> decodeFromText configJSON
      Domain.SmsService Sms.ExotelSms -> Domain.SmsServiceConfig . Sms.ExotelSmsConfig <$> decodeFromText configJSON
      Domain.SmsService Sms.MyValueFirst -> Domain.SmsServiceConfig . Sms.MyValueFirstConfig <$> decodeFromText configJSON
    return $
      Domain.MerchantServiceConfig
        { merchantId = fromKey merchantId,
          ..
        }
  toTType :: Domain.MerchantServiceConfig -> MerchantServiceConfigT
  toTType Domain.MerchantServiceConfig {..} = do
    let (serviceName, configJSON) = getServiceNameConfigJSON serviceConfig
    MerchantServiceConfigT
      { merchantId = toKey merchantId,
        ..
      }

getServiceNameConfigJSON :: Domain.ServiceConfig -> (Domain.ServiceName, Text)
getServiceNameConfigJSON = \case
  Domain.MapsServiceConfig mapsCfg -> case mapsCfg of
    Maps.GoogleConfig cfg -> (Domain.MapsService Maps.Google, encodeToText cfg)
    Maps.OSRMConfig cfg -> (Domain.MapsService Maps.OSRM, encodeToText cfg)
    Maps.MMIConfig cfg -> (Domain.MapsService Maps.MMI, encodeToText cfg)
  Domain.SmsServiceConfig smsCfg -> case smsCfg of
    Sms.ExotelSmsConfig cfg -> (Domain.SmsService Sms.ExotelSms, encodeToText cfg)
    Sms.MyValueFirstConfig cfg -> (Domain.SmsService Sms.MyValueFirst, encodeToText cfg)
