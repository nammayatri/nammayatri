module Domain.Action.Dashboard.Merchant
  ( merchantServiceConfigUpdate,
    merchantServiceConfigUsageUpdate,
    merchantUpdate,
  )
where

import qualified Beckn.External.Maps as Maps
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Validation
import Control.Applicative ((<|>))
import qualified "dashboard-bpp-helper-api" Dashboard.BAP.Merchant as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Environment
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import Tools.Error

---------------------------------------------------------------------
merchantUpdate :: ShortId DM.Merchant -> Common.MerchantUpdateReq -> Flow APISuccess
merchantUpdate merchantShortId req = do
  runRequestValidation Common.validateMerchantUpdateReq req
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

  let updMerchant =
        merchant{DM.name = fromMaybe merchant.name req.name,
                 DM.exoPhone = if req.exoPhone == Just "" then Nothing else req.exoPhone <|> merchant.exoPhone,
                 DM.exoPhoneCountryCode = if req.exoPhoneCountryCode == Just "" then Nothing else req.exoPhoneCountryCode <|> merchant.exoPhoneCountryCode,
                 DM.fcmConfig = maybe merchant.fcmConfig (Common.mkFCMConfig merchant.fcmConfig.fcmTokenKeyPrefix) req.fcmConfig,
                 DM.gatewayUrl = fromMaybe merchant.gatewayUrl req.gatewayUrl,
                 DM.registryUrl = fromMaybe merchant.registryUrl req.registryUrl
                }
  Esq.runTransaction $ CQM.update updMerchant
  CQM.clearCache updMerchant
  logTagInfo "dashboard -> merchantUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
merchantServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MerchantServiceConfigUpdateReq ->
  Flow APISuccess
merchantServiceConfigUpdate merchantShortId req = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let serviceName = DMSC.MapsService $ Common.getMapsServiceFromReq req
  serviceConfig <- DMSC.MapsServiceConfig <$> Common.buildMapsServiceConfig req
  mbMerchantServiceConfig <- CQMSC.findByMerchantIdAndService merchant.id serviceName
  case mbMerchantServiceConfig of
    Nothing -> do
      merchantServiceConfig <- buildMerchantServiceConfig merchant.id serviceConfig
      Esq.runTransaction $ do
        CQMSC.create merchantServiceConfig
    Just _merchantServiceConfig -> do
      Esq.runTransaction $ do
        CQMSC.updateMerchantServiceConfig merchant.id serviceConfig
      CQMSC.clearCache merchant.id serviceName
  logTagInfo "dashboard -> merchantServiceConfigUpdate : " (show merchant.id)
  pure Success

buildMerchantServiceConfig ::
  MonadTime m =>
  Id DM.Merchant ->
  DMSC.ServiceConfig ->
  m DMSC.MerchantServiceConfig
buildMerchantServiceConfig merchantId serviceConfig = do
  now <- getCurrentTime
  pure
    DMSC.MerchantServiceConfig
      { merchantId,
        serviceConfig,
        updatedAt = now,
        createdAt = now
      }

---------------------------------------------------------------------
merchantServiceConfigUsageUpdate ::
  ShortId DM.Merchant ->
  Common.MerchantServiceUsageConfigUpdateReq ->
  Flow APISuccess
merchantServiceConfigUsageUpdate merchantShortId req = do
  runRequestValidation Common.validateMerchantServiceUsageConfigUpdateReq req
  whenJust req.getEstimatedPickupDistances $ \_ ->
    throwError (InvalidRequest "getEstimatedPickupDistances is not allowed for bap")
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

  forM_ [Maps.Google, Maps.OSRM, Maps.MMI] $ \service -> do
    when (Common.serviceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantIdAndService merchant.id (DMSC.MapsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantId merchant.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchant.id.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{getDistances = fromMaybe merchantServiceUsageConfig.getDistances req.getDistances,
                                   getRoutes = fromMaybe merchantServiceUsageConfig.getRoutes req.getRoutes,
                                   snapToRoad = fromMaybe merchantServiceUsageConfig.snapToRoad req.snapToRoad,
                                   getPlaceName = fromMaybe merchantServiceUsageConfig.getPlaceName req.getPlaceName,
                                   getPlaceDetails = fromMaybe merchantServiceUsageConfig.getPlaceDetails req.getPlaceDetails,
                                   autoComplete = fromMaybe merchantServiceUsageConfig.autoComplete req.autoComplete
                                  }
  Esq.runTransaction $ do
    CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchant.id
  logTagInfo "dashboard -> merchantServiceConfigUsageUpdate : " (show merchant.id)
  pure Success
