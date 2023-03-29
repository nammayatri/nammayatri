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

module Storage.Tabular.Merchant.MerchantServiceConfig where

import qualified Domain.Types.Merchant as Domain
import qualified Domain.Types.Merchant.MerchantServiceConfig as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Types.CommonImport as Maps
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (decodeFromText, encodeToText)
import Kernel.Utils.Error
import qualified Lib.Maps.Interface.Types as Maps
import qualified Lib.SMS.Interface as Sms
import qualified Lib.Verification.Interface as Verification
import qualified Lib.Whatsapp.Interface as Whatsapp
import Storage.Tabular.Merchant (MerchantTId)

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
      Domain.WhatsappService Whatsapp.GupShup -> Domain.WhatsappServiceConfig . Whatsapp.GupShupConfig <$> decodeFromText configJSON
      Domain.VerificationService Verification.Idfy -> Domain.VerificationServiceConfig . Verification.IdfyConfig <$> decodeFromText configJSON
    return $
      Domain.MerchantServiceConfig
        { merchantId = fromKey merchantId,
          ..
        }
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
  Domain.WhatsappServiceConfig whatsappCfg -> case whatsappCfg of
    Whatsapp.GupShupConfig cfg -> (Domain.WhatsappService Whatsapp.GupShup, encodeToText cfg)
  Domain.VerificationServiceConfig verificationCfg -> case verificationCfg of
    Verification.IdfyConfig cfg -> (Domain.VerificationService Verification.Idfy, encodeToText cfg)
