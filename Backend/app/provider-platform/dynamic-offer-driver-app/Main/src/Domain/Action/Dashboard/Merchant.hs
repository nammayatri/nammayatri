{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Merchant
  ( mapsServiceConfigUpdate,
    mapsServiceUsageConfigUpdate,
    merchantUpdate,
    smsServiceConfigUpdate,
    smsServiceUsageConfigUpdate,
  )
where

import Control.Applicative
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Environment
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.SMS as SMS
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.TransporterConfig as CQTC
import Tools.Error

---------------------------------------------------------------------
merchantUpdate :: ShortId DM.Merchant -> Common.MerchantUpdateReq -> Flow Common.MerchantUpdateRes
merchantUpdate merchantShortId req = do
  runRequestValidation Common.validateMerchantUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  let updMerchant =
        merchant{DM.name = fromMaybe (merchant.name) (req.name),
                 DM.description = (req.description) <|> (merchant.description),
                 DM.enabled = fromMaybe (merchant.enabled) (req.enabled)
                }
  -- First solution
  Esq.runTransaction $ do
    CQM.update1 (Proxy @Flow) updMerchant
    whenJust req.fcmConfig $
      \fcmConfig -> CQTC.updateFCMConfig merchant.id fcmConfig.fcmUrl fcmConfig.fcmServiceAccount

  -- Here we can broke finlizer when use different monads within one transaction, like
    -- Esq.runTransaction $ do
    --   Esq.finalize (Proxy @IO) $ putStrLn "broken finalizer"
    --   CQM.update1 (Proxy @Flow) updMerchant
    -- So, second solution it better

  -- Second solution
  Esq.runTransactionF $ \finalize -> do
    CQM.update2 finalize updMerchant
    whenJust req.fcmConfig $
      \fcmConfig -> CQTC.updateFCMConfig merchant.id fcmConfig.fcmUrl fcmConfig.fcmServiceAccount

  -- CQM.clearCache updMerchant
  whenJust req.fcmConfig $ \_ -> CQTC.clearCache merchant.id
  logTagInfo "dashboard -> merchantUpdate : " (show merchant.id)
  return $ mkMerchantUpdateRes updMerchant

mkMerchantUpdateRes :: DM.Merchant -> Common.MerchantUpdateRes
mkMerchantUpdateRes DM.Merchant {..} =
  Common.MerchantUpdateRes
    { name,
      description = description,
      contactNumber = mobileCountryCode <> mobileNumber,
      status = castMerchantStatus status,
      enabled = enabled
    }

castMerchantStatus :: DM.Status -> Common.Status
castMerchantStatus = \case
  DM.PENDING_VERIFICATION -> Common.PENDING_VERIFICATION
  DM.APPROVED -> Common.APPROVED
  DM.REJECTED -> Common.REJECTED

---------------------------------------------------------------------
mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MapsServiceConfigUpdateReq ->
  Flow APISuccess
mapsServiceConfigUpdate merchantShortId req = do
  merchant <- findMerchantByShortId merchantShortId
  let serviceName = DMSC.MapsService $ Common.getMapsServiceFromReq req
  serviceConfig <- DMSC.MapsServiceConfig <$> Common.buildMapsServiceConfig req
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig
  Esq.runTransaction $ do
    CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id serviceName
  logTagInfo "dashboard -> mapsServiceConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.SmsServiceConfigUpdateReq ->
  Flow APISuccess
smsServiceConfigUpdate merchantShortId req = do
  merchant <- findMerchantByShortId merchantShortId
  let serviceName = DMSC.SmsService $ Common.getSmsServiceFromReq req
  serviceConfig <- DMSC.SmsServiceConfig <$> Common.buildSmsServiceConfig req
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig
  Esq.runTransaction $ do
    CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id serviceName
  logTagInfo "dashboard -> smsServiceConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MapsServiceUsageConfigUpdateReq ->
  Flow APISuccess
mapsServiceUsageConfigUpdate merchantShortId req = do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId

  forM_ Maps.availableMapsServices $ \service -> do
    when (Common.mapsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantIdAndService merchant.id (DMSC.MapsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for maps service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantId merchant.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchant.id.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{getDistances = fromMaybe merchantServiceUsageConfig.getDistances req.getDistances,
                                   getEstimatedPickupDistances = fromMaybe merchantServiceUsageConfig.getEstimatedPickupDistances req.getEstimatedPickupDistances,
                                   getRoutes = fromMaybe merchantServiceUsageConfig.getRoutes req.getRoutes,
                                   snapToRoad = fromMaybe merchantServiceUsageConfig.snapToRoad req.snapToRoad,
                                   getPlaceName = fromMaybe merchantServiceUsageConfig.getPlaceName req.getPlaceName,
                                   getPlaceDetails = fromMaybe merchantServiceUsageConfig.getPlaceDetails req.getPlaceDetails,
                                   autoComplete = fromMaybe merchantServiceUsageConfig.autoComplete req.autoComplete
                                  }
  Esq.runTransaction $ do
    CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchant.id
  logTagInfo "dashboard -> mapsServiceUsageConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
smsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Common.SmsServiceUsageConfigUpdateReq ->
  Flow APISuccess
smsServiceUsageConfigUpdate merchantShortId req = do
  runRequestValidation Common.validateSmsServiceUsageConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId

  forM_ SMS.availableSmsServices $ \service -> do
    when (Common.smsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantIdAndService merchant.id (DMSC.SmsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for sms service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantId merchant.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchant.id.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{smsProvidersPriorityList = req.smsProvidersPriorityList
                                  }
  Esq.runTransaction $ do
    CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchant.id
  logTagInfo "dashboard -> smsServiceUsageConfigUpdate : " (show merchant.id)
  pure Success
