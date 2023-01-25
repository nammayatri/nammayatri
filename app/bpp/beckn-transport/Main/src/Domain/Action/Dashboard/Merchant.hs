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
import Control.Applicative
import qualified "dashboard-bpp-helper-api" Dashboard.BPP.Merchant as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Environment
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.TransporterConfig as CQTC
import Tools.Error

---------------------------------------------------------------------
merchantUpdate :: ShortId DM.Merchant -> Common.MerchantUpdateReq -> Flow Common.MerchantUpdateRes
merchantUpdate merchantShortId req = do
  runRequestValidation Common.validateMerchantUpdateReq req
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let updMerchant =
        merchant{DM.name = fromMaybe (merchant.name) (req.name),
                 DM.description = (req.description) <|> (merchant.description),
                 DM.enabled = fromMaybe (merchant.enabled) (req.enabled)
                }
  Esq.runTransaction $ do
    CQM.update updMerchant
    whenJust req.fcmConfig $
      \fcmConfig -> CQTC.updateFCMConfig merchant.id fcmConfig.fcmUrl fcmConfig.fcmServiceAccount
  CQM.clearCache updMerchant
  whenJust req.fcmConfig $ \_ -> CQTC.clearCache merchant.id
  logTagInfo "dashboard -> merchantUpdate : " (show merchant.id)
  return $ mkMerchantUpdateRes updMerchant

mkMerchantUpdateRes :: DM.Merchant -> Common.MerchantUpdateRes
mkMerchantUpdateRes DM.Merchant {..} =
  Common.MerchantUpdateRes
    { name,
      description,
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
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig
  Esq.runTransaction $ do
    CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id serviceName
  logTagInfo "dashboard -> merchantServiceConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
merchantServiceConfigUsageUpdate ::
  ShortId DM.Merchant ->
  Common.MerchantServiceUsageConfigUpdateReq ->
  Flow APISuccess
merchantServiceConfigUsageUpdate merchantShortId req = do
  runRequestValidation Common.validateMerchantServiceUsageConfigUpdateReq req
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
  logTagInfo "dashboard -> merchantServiceConfigUsageUpdate : " (show merchant.id)
  pure Success
