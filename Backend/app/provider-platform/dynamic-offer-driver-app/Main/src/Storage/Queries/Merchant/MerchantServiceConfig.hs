{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Merchant.MerchantServiceConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant as DOrg
-- import Domain.Types.Merchant.MerchantServiceConfig (MerchantServiceConfig, ServiceName)
import Domain.Types.Merchant.MerchantServiceConfig
import qualified Domain.Types.Merchant.MerchantServiceConfig as Domain
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.External.Call as Call
import qualified Kernel.External.Maps.Interface.Types as Maps
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.External.SMS.Interface as Sms
import qualified Kernel.External.Verification.Interface as Verification
import qualified Kernel.External.Whatsapp.Interface as Whatsapp
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (decodeFromText)
import Kernel.Utils.Error
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.MerchantServiceConfig as BeamMSC
import Storage.Tabular.Merchant.MerchantServiceConfig
import Tools.Error

-- findByMerchantIdAndService :: Transactionable m => Id Merchant -> ServiceName -> m (Maybe MerchantServiceConfig)
-- findByMerchantIdAndService merchantId serviceName =
--   Esq.findOne $ do
--     merchantServiceConfig <- from $ table @MerchantServiceConfigT
--     where_ $
--       merchantServiceConfig ^. MerchantServiceConfigTId ==. val (toKey (merchantId, serviceName))
--     return merchantServiceConfig

findByMerchantIdAndService :: (L.MonadFlow m, Log m) => Id Merchant -> ServiceName -> m (Maybe MerchantServiceConfig)
findByMerchantIdAndService (Id merchantId) serviceName = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.And [Se.Is BeamMSC.merchantId $ Se.Eq merchantId, Se.Is BeamMSC.serviceName $ Se.Eq serviceName]]
      case result of
        Left _ -> pure Nothing
        Right msc -> mapM transformBeamMerchantServiceConfigToDomain msc
    Nothing -> pure Nothing

-- FIXME this query created for backward compatibility
-- findOne :: Transactionable m => ServiceName -> m (Maybe MerchantServiceConfig)
-- findOne serviceName =
--   Esq.findOne $ do
--     merchantServiceConfig <- from $ table @MerchantServiceConfigT
--     where_ $
--       merchantServiceConfig ^. MerchantServiceConfigServiceName ==. val serviceName
--     limit 1
--     return merchantServiceConfig

findOne :: (L.MonadFlow m, Log m) => ServiceName -> m (Maybe MerchantServiceConfig)
findOne serviceName = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamMSC.serviceName $ Se.Eq serviceName]
      case result of
        Right msc -> mapM transformBeamMerchantServiceConfigToDomain msc
        Left _ -> pure Nothing
    Nothing -> pure Nothing

-- upsertMerchantServiceConfig :: MerchantServiceConfig -> SqlDB ()
-- upsertMerchantServiceConfig merchantServiceConfig = do
--   now <- getCurrentTime
--   let (_serviceName, configJSON) = getServiceNameConfigJSON merchantServiceConfig.serviceConfig
--   Esq.upsert
--     merchantServiceConfig
--     [ MerchantServiceConfigConfigJSON =. val configJSON,
--       MerchantServiceConfigUpdatedAt =. val now
--     ]

upsertMerchantServiceConfig :: (L.MonadFlow m, Log m, MonadTime m) => MerchantServiceConfig -> m ()
upsertMerchantServiceConfig merchantServiceConfig = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  let (_serviceName, configJSON) = getServiceNameConfigJSON merchantServiceConfig.serviceConfig
  case dbConf of
    Just dbCOnf' -> do
      res <- KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamMSC.merchantId $ Se.Eq (getId merchantServiceConfig.merchantId)]
      case res of
        Right res' -> do
          if isJust res'
            then
              void $
                KV.updateWoReturningWithKVConnector
                  dbCOnf'
                  Mesh.meshConfig
                  [ Se.Set BeamMSC.configJSON configJSON,
                    Se.Set BeamMSC.updatedAt now
                  ]
                  [Se.Is BeamMSC.merchantId (Se.Eq $ getId merchantServiceConfig.merchantId)]
            else void $ KV.createWoReturingKVConnector dbCOnf' Mesh.meshConfig (transformDomainMerchantServiceConfigToBeam merchantServiceConfig)
        Left _ -> pure ()
    Nothing -> pure ()

transformBeamMerchantServiceConfigToDomain :: (L.MonadFlow m, Log m) => BeamMSC.MerchantServiceConfig -> m MerchantServiceConfig
transformBeamMerchantServiceConfigToDomain BeamMSC.MerchantServiceConfigT {..} = do
  serviceConfigData <- maybe (throwError $ InternalError "Unable to decode MerchantServiceConfigT.configJSON") return $ case serviceName of
    Domain.MapsService Maps.Google -> Domain.MapsServiceConfig . Maps.GoogleConfig <$> decodeFromText configJSON
    Domain.MapsService Maps.OSRM -> Domain.MapsServiceConfig . Maps.OSRMConfig <$> decodeFromText configJSON
    Domain.MapsService Maps.MMI -> Domain.MapsServiceConfig . Maps.MMIConfig <$> decodeFromText configJSON
    Domain.SmsService Sms.ExotelSms -> Domain.SmsServiceConfig . Sms.ExotelSmsConfig <$> decodeFromText configJSON
    Domain.SmsService Sms.MyValueFirst -> Domain.SmsServiceConfig . Sms.MyValueFirstConfig <$> decodeFromText configJSON
    Domain.WhatsappService Whatsapp.GupShup -> Domain.WhatsappServiceConfig . Whatsapp.GupShupConfig <$> decodeFromText configJSON
    Domain.VerificationService Verification.Idfy -> Domain.VerificationServiceConfig . Verification.IdfyConfig <$> decodeFromText configJSON
    Domain.CallService Call.Exotel -> Domain.CallServiceConfig . Call.ExotelConfig <$> decodeFromText configJSON
  pure
    MerchantServiceConfig
      { merchantId = Id merchantId,
        serviceConfig = serviceConfigData,
        updatedAt = updatedAt,
        createdAt = createdAt
      }

transformDomainMerchantServiceConfigToBeam :: MerchantServiceConfig -> BeamMSC.MerchantServiceConfig
transformDomainMerchantServiceConfigToBeam MerchantServiceConfig {..} =
  BeamMSC.MerchantServiceConfigT
    { BeamMSC.merchantId = getId merchantId,
      BeamMSC.serviceName = fst $ getServiceNameConfigJSON serviceConfig,
      BeamMSC.configJSON = snd $ getServiceNameConfigJSON serviceConfig,
      BeamMSC.updatedAt = updatedAt,
      BeamMSC.createdAt = createdAt
    }
