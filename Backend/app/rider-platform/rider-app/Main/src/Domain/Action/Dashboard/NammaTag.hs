{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.Dashboard.NammaTag
  ( postNammaTagTagCreate,
    postNammaTagTagUpdate,
    deleteNammaTagTagDelete,
    postNammaTagQueryCreate,
    postNammaTagQueryUpdate,
    deleteNammaTagQueryDelete,
    postNammaTagAppDynamicLogicVerify,
    getNammaTagAppDynamicLogic,
    postNammaTagRunJob,
    postNammaTagTimeBoundsCreate,
    deleteNammaTagTimeBoundsDelete,
    getNammaTagAppDynamicLogicGetLogicRollout,
    postNammaTagAppDynamicLogicUpsertLogicRollout,
    getNammaTagTimeBounds,
    getNammaTagAppDynamicLogicVersions,
    getNammaTagAppDynamicLogicDomains,
    getNammaTagQueryAll,
    postNammaTagUpdateCustomerTag,
    postNammaTagConfigPilotGetVersion,
    postNammaTagConfigPilotGetConfig,
    postNammaTagConfigPilotCreateUiConfig,
    getNammaTagConfigPilotAllConfigs,
    getNammaTagConfigPilotConfigDetails,
    getNammaTagConfigPilotGetTableData,
    addCustomerTag,
    removeCustomerTag,
    postNammaTagConfigPilotActionChange,
  )
where

import qualified ConfigPilotFrontend.Flow as CPF
import qualified ConfigPilotFrontend.Types as CPT
import qualified Dashboard.Common as Common
import Data.Singletons
-- import qualified Data.Text as Text
import qualified Domain.Types.FRFSConfig as DFRFS
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantConfig as DTM
import qualified Domain.Types.MerchantPushNotification as DTPN
import qualified Domain.Types.PayoutConfig as DTP
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RideRelatedNotificationConfig as DTRN
import qualified Domain.Types.RiderConfig as DTR
import Domain.Types.UiRiderConfig (UiRiderConfig (..))
import qualified Domain.Types.UiRiderConfig as DTRC
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude as Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Scheduler.JobStorageType.DB.Queries as QDBJ
import Lib.Scheduler.Types (AnyJob (..))
import qualified Lib.Yudhishthira.Flow.Dashboard as YudhishthiraFlow
-- import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as CADLR
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types as LYTU
import qualified Lib.Yudhishthira.TypesTH as YTH
import SharedLogic.JobScheduler (RiderJobType (..))
import SharedLogic.Merchant
import qualified SharedLogic.Scheduler.Jobs.Chakras as Chakras
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.UiRiderConfig as UIRC
import qualified Storage.Queries.Person as QPerson
-- import qualified Storage.Queries.UiRiderConfig as SQU
import qualified Tools.ConfigPilot as TC
-- import qualified Tools.DynamicLogic as TDL
import Tools.Error

$(YTH.generateGenericDefault ''DTR.RiderConfig)
$(YTH.generateGenericDefault ''DTP.PayoutConfig)
$(YTH.generateGenericDefault ''DTRN.RideRelatedNotificationConfig)
$(YTH.generateGenericDefault ''DTM.MerchantConfig)
$(YTH.generateGenericDefault ''DTPN.MerchantPushNotification)
$(YTH.generateGenericDefault ''DFRFS.FRFSConfig)

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.CreateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagCreate _merchantShortId _opCity req = YudhishthiraFlow.postTagCreate req

postNammaTagTagUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.UpdateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagUpdate _merchantShortId _opCity req = YudhishthiraFlow.postTagUpdate req

deleteNammaTagTagDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTagDelete _merchantShortId _opCity tagName = YudhishthiraFlow.deleteTag tagName

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.ChakraQueriesAPIEntity -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate _merchantShortId _opCity req = YudhishthiraFlow.postQueryCreate req

postNammaTagQueryUpdate :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.ChakraQueryUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagQueryUpdate _merchantShortId _opCity = YudhishthiraFlow.postQueryUpdate

deleteNammaTagQueryDelete :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.ChakraQueryDeleteReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteNammaTagQueryDelete _merchantShortId _opCity = YudhishthiraFlow.queryDelete

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.AppDynamicLogicReq -> Environment.Flow LYTU.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  let mbMerchantid = Just $ cast merchant.id
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantOpCityId = merchantOperatingCity.id
  _riderConfig <- QRC.findByMerchantOperatingCityId merchantOpCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
  case req.domain of
    -- LYTU.RIDER_CONFIG (LYTU.UiConfig dt pt) -> do
    --   let uiConfigReq = LYTU.UiConfigRequest {os = dt, platform = pt, merchantId = getId merchant.id, city = opCity, language = Nothing, bundle = Nothing, toss = Nothing}
    --   defaultConfig <- SQU.getUiConfig uiConfigReq merchantOpCityId >>= fromMaybeM (InvalidRequest "No default found for UiRiderConfig")
    --   let configWrap = LYTU.Config defaultConfig.config Nothing 1
    --   logicData :: (LYTU.Config Value) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
    --   url <- TC.getTSServiceUrl
    --   YudhishthiraFlow.verifyAndUpdateUIDynamicLogic mbMerchantid (Proxy :: Proxy (LYTU.Config Value)) _riderConfig.dynamicLogicUpdatePassword req logicData url
    LYTU.RIDER_CONFIG LYTU.RiderConfig -> do
      def <- fromMaybeM (InvalidRequest "RiderConfig not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTR.RiderConfig))
      let configWrap = LYTU.Config def Nothing 1
      logicData :: (LYTU.Config DTR.RiderConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantid (Proxy :: Proxy (LYTU.Config DTR.RiderConfig)) _riderConfig.dynamicLogicUpdatePassword req logicData
    LYTU.RIDER_CONFIG LYTU.PayoutConfig -> do
      def <- fromMaybeM (InvalidRequest "PayoutConfig not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTP.PayoutConfig))
      let configWrap = LYTU.Config def Nothing 1
      logicData :: (LYTU.Config DTP.PayoutConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantid (Proxy :: Proxy (LYTU.Config DTP.PayoutConfig)) _riderConfig.dynamicLogicUpdatePassword req logicData
    LYTU.RIDER_CONFIG LYTU.RideRelatedNotificationConfig -> do
      def <- fromMaybeM (InvalidRequest "RideRelatedNotificationConfig not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTRN.RideRelatedNotificationConfig))
      let configWrap = LYTU.Config def Nothing 1
      logicData :: (LYTU.Config DTRN.RideRelatedNotificationConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantid (Proxy :: Proxy (LYTU.Config DTRN.RideRelatedNotificationConfig)) _riderConfig.dynamicLogicUpdatePassword req logicData
    LYTU.RIDER_CONFIG LYTU.MerchantConfig -> do
      def <- fromMaybeM (InvalidRequest "MerchantConfig not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTM.MerchantConfig))
      let configWrap = LYTU.Config def Nothing 1
      logicData :: (LYTU.Config DTM.MerchantConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantid (Proxy :: Proxy (LYTU.Config DTM.MerchantConfig)) _riderConfig.dynamicLogicUpdatePassword req logicData
    LYTU.RIDER_CONFIG LYTU.MerchantPushNotification -> do
      def <- fromMaybeM (InvalidRequest "MerchantPushNotification not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTPN.MerchantPushNotification))
      let configWrap = LYTU.Config def Nothing 1
      logicData :: (LYTU.Config DTPN.MerchantPushNotification) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantid (Proxy :: Proxy (LYTU.Config DTPN.MerchantPushNotification)) _riderConfig.dynamicLogicUpdatePassword req logicData
    LYTU.RIDER_CONFIG LYTU.FRFSConfig -> do
      def <- fromMaybeM (InvalidRequest "FRFSConfig not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DFRFS.FRFSConfig))
      let configWrap = LYTU.Config def Nothing 1
      logicData :: (LYTU.Config DFRFS.FRFSConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantid (Proxy :: Proxy (LYTU.Config DFRFS.FRFSConfig)) _riderConfig.dynamicLogicUpdatePassword req logicData
    _ -> throwError $ InvalidRequest "Logic Domain not supported"

getNammaTagAppDynamicLogic :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Maybe Int -> LYTU.LogicDomain -> Environment.Flow [LYTU.GetLogicsResp]
getNammaTagAppDynamicLogic _ _ = YudhishthiraFlow.getAppDynamicLogicForDomain

postNammaTagRunJob ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  LYTU.RunKaalChakraJobReq ->
  Environment.Flow LYTU.RunKaalChakraJobRes
postNammaTagRunJob merchantShortId opCity req = do
  mbMerchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
  let mbMerchantOpCityId = mbMerchantOperatingCity <&> (.id)
  let mbMerchantId = mbMerchantOperatingCity <&> (.merchantId)
  -- Logic for complete old jobs works only for DbBased jobs
  whenJust req.completeOldJob $ \oldJobId -> do
    mbOldJob :: Maybe (AnyJob RiderJobType) <- QDBJ.findById oldJobId
    case mbOldJob of
      Nothing -> throwError (InvalidRequest "Job not found")
      Just (AnyJob oldJob) -> do
        let jobType = fromSing $ oldJob.jobInfo.jobType
        unless (castChakra jobType == Just req.chakra) do
          throwError (InvalidRequest "Invalid job type")
        QDBJ.markAsComplete oldJobId
  let kaalChakraHandle = Chakras.mkKaalChakraHandle mbMerchantId mbMerchantOpCityId
  case req.action of
    LYTU.RUN -> YudhishthiraFlow.postRunKaalChakraJob kaalChakraHandle req
    LYTU.SCHEDULE scheduledTime -> do
      now <- getCurrentTime
      when (scheduledTime <= now) $
        throwError (InvalidRequest "Schedule job available only for future")
      case req.usersSet of
        LYTU.ALL_USERS -> pure ()
        _ -> throwError (InvalidRequest "Schedule job available only for all users")
      let jobData = LYTU.mkKaalChakraJobData req (Just scheduledTime)
      kaalChakraHandle.createFetchUserDataJob req.chakra jobData scheduledTime

      logInfo $ "Scheduled new " <> show req.chakra <> " job"
      pure $ LYTU.RunKaalChakraJobRes {eventId = Nothing, tags = Nothing, users = Nothing, chakraBatchState = LYTU.Completed}
  where
    castChakra :: RiderJobType -> Maybe LYTU.Chakra
    castChakra Daily = Just LYTU.Daily
    castChakra Weekly = Just LYTU.Weekly
    castChakra Monthly = Just LYTU.Monthly
    castChakra Quarterly = Just LYTU.Quarterly
    castChakra _ = Nothing

getNammaTagTimeBounds :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.LogicDomain -> Environment.Flow LYTU.TimeBoundResp
getNammaTagTimeBounds merchantShortId opCity domain = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantOpCityId = merchantOperatingCity.id
  YudhishthiraFlow.getTimeBounds (cast merchantOpCityId) domain

postNammaTagTimeBoundsCreate :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.CreateTimeBoundRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagTimeBoundsCreate merchantShortId opCity req = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantOpCityId = merchantOperatingCity.id
  YudhishthiraFlow.createTimeBounds (cast merchantOpCityId) req

deleteNammaTagTimeBoundsDelete :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.LogicDomain -> Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteNammaTagTimeBoundsDelete merchantShortId opCity domain name = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantOpCityId = merchantOperatingCity.id
  YudhishthiraFlow.deleteTimeBounds (cast merchantOpCityId) domain name

getNammaTagAppDynamicLogicGetLogicRollout :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Maybe Text -> LYTU.LogicDomain -> Environment.Flow [LYTU.LogicRolloutObject]
getNammaTagAppDynamicLogicGetLogicRollout merchantShortId opCity timeBound domain = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantOpCityId = merchantOperatingCity.id
  YudhishthiraFlow.getLogicRollout (cast merchantOpCityId) timeBound domain

postNammaTagAppDynamicLogicUpsertLogicRollout :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> [LYTU.LogicRolloutObject] -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagAppDynamicLogicUpsertLogicRollout merchantShortId opCity rolloutReq = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  YudhishthiraFlow.upsertLogicRollout (Just $ cast merchantOperatingCity.merchantId) (cast merchantOperatingCity.id) rolloutReq TC.returnConfigs opCity

getNammaTagAppDynamicLogicVersions :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Maybe Prelude.Int -> Prelude.Maybe Prelude.Int -> LYTU.LogicDomain -> Environment.Flow LYTU.AppDynamicLogicVersionResp
getNammaTagAppDynamicLogicVersions _merchantShortId _opCity = YudhishthiraFlow.getAppDynamicLogicVersions

getNammaTagAppDynamicLogicDomains :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow LYTU.AppDynamicLogicDomainResp
getNammaTagAppDynamicLogicDomains _merchantShortId _opCity = return LYTU.allValues

getNammaTagQueryAll :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.Chakra -> Environment.Flow LYTU.ChakraQueryResp
getNammaTagQueryAll _merchantShortId _opCity = YudhishthiraFlow.getNammaTagQueryAll

postNammaTagUpdateCustomerTag :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Common.User -> LYTU.UpdateTagReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagUpdateCustomerTag merchantShortId opCity userId req = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantOpCityId = merchantOperatingCity.id
  let personId = cast @Common.User @DP.Person userId
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  when (req.isAddingTag && maybe False (LYTU.elemTagNameValue req.tag) person.customerNammaTags) $
    logInfo "Tag already exists, update expiry"
  -- merchant access checking
  unless (merchantOpCityId == person.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  mbNammTag <- YudhishthiraFlow.verifyTag req.tag
  now <- getCurrentTime
  let tag =
        if req.isAddingTag
          then do
            let reqCustomerTagWithExpiry = LYTU.addTagExpiry req.tag (mbNammTag >>= (.validity)) now
            LYTU.replaceTagNameValue person.customerNammaTags reqCustomerTagWithExpiry
          else LYTU.removeTagNameValue person.customerNammaTags req.tag
  unless (Just (LYTU.showRawTags tag) == (LYTU.showRawTags <$> person.customerNammaTags)) $
    QPerson.updateCustomerTags (Just tag) personId
  pure Success

postNammaTagConfigPilotGetVersion :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.UiConfigRequest -> Environment.Flow LYTU.UiConfigGetVersionResponse
postNammaTagConfigPilotGetVersion _ _ _ = do
  throwError $ InvalidRequest "UI config not supported yet"

-- merchantOperatingCity <- CQMOC.findByMerchantIdAndCity (Id uicr.merchantId) uicr.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId: " <> uicr.merchantId <> " ,city: " <> show uicr.city)
-- let merchantOpCityId = merchantOperatingCity.id
-- (_, version) <- UIRC.findUiConfig uicr merchantOpCityId False
-- baseRollout <- CADLR.findBaseRolloutByMerchantOpCityAndDomain (cast merchantOpCityId) (LYTU.RIDER_CONFIG $ LYTU.UiConfig uicr.os uicr.platform) >>= fromMaybeM (InvalidRequest "Base Rollout not found")
-- let baseVersion = Just baseRollout.version
-- case version of
--   Just _ -> pure $ LYTU.UiConfigGetVersionResponse (Text.pack . show <$> version) (Text.pack . show <$> baseVersion)
--   Nothing -> throwError $ InternalError $ "No config found for merchant:" <> show uicr.merchantId <> " and city:" <> show uicr.city <> " and request:" <> show uicr

postNammaTagConfigPilotGetConfig :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.UiConfigRequest -> Environment.Flow LYTU.UiConfigResponse
postNammaTagConfigPilotGetConfig _ _ _ = do
  throwError $ InvalidRequest "UI config not supported yet"

-- merchantOperatingCity <- CQMOC.findByMerchantIdAndCity (Id uicr.merchantId) uicr.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId: " <> uicr.merchantId <> " ,city: " <> show uicr.city)
-- let merchantOpCityId = merchantOperatingCity.id
-- (config, version) <- UIRC.findUiConfig uicr merchantOpCityId False
-- isExp <- TDL.isExperimentRunning (cast merchantOpCityId) (LYTU.RIDER_CONFIG $ LYTU.UiConfig uicr.os uicr.platform)
-- baseRollout <- CADLR.findBaseRolloutByMerchantOpCityAndDomain (cast merchantOpCityId) (LYTU.RIDER_CONFIG $ LYTU.UiConfig uicr.os uicr.platform) >>= fromMaybeM (InvalidRequest "Base Rollout not found")
-- let baseVersion = Just baseRollout.version
-- case config of
--   Just cfg -> pure (LYTU.UiConfigResponse cfg.config (Text.pack . show <$> version) (Text.pack . show <$> baseVersion) isExp)
--   Nothing -> throwError $ InternalError $ "No config found for merchant:" <> show uicr.merchantId <> " and city:" <> show uicr.city <> " and request:" <> show uicr

postNammaTagConfigPilotCreateUiConfig :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.CreateConfigRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagConfigPilotCreateUiConfig _ _ ccr = do
  url <- TC.getTSServiceUrl
  when (ccr.platform == LYTU.TypeScript) $ do
    configValidateResp <- CPF.configValidate url (ccr.config)
    case configValidateResp.status of
      CPT.VALID_CONFIG -> pure ()
      CPT.INVALID_CONFIG -> throwError $ InvalidRequest "Invalid config"
      CPT.INVALID_REQUEST -> throwError $ InvalidRequest "Invalid request"
  merchantOperatingCity <- CQMOC.findByMerchantIdAndCity (Id ccr.merchantId) ccr.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId: " <> ccr.merchantId <> " ,city: " <> show ccr.city)
  let merchantOpCityId = merchantOperatingCity.id
  now <- getCurrentTime
  id' <- generateGUID
  let uicr = LYTU.UiConfigRequest {merchantId = ccr.merchantId, city = ccr.city, os = ccr.os, platform = ccr.platform, bundle = ccr.bundle, language = Nothing, toss = Nothing}
  (config, _) <- UIRC.findUiConfig uicr merchantOpCityId False
  when (isJust config) $ do
    throwError $ InvalidRequest "Config already exists"
  UIRC.create $ cfg now merchantOpCityId id'
  return Kernel.Types.APISuccess.Success
  where
    cfg now merchantOpCityId id' =
      DTRC.UiRiderConfig
        { config = ccr.config,
          createdAt = now,
          id = id',
          os = ccr.os,
          updatedAt = now,
          merchantOperatingCityId = merchantOpCityId,
          platform = ccr.platform
        }

getNammaTagConfigPilotAllConfigs :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Maybe Prelude.Bool -> Environment.Flow [LYTU.ConfigType]
getNammaTagConfigPilotAllConfigs _merchantShortId _opCity mbUnderExp = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity _merchantShortId _opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show _opCity)
  let merchantOpCityId = merchantOperatingCity.id
  YudhishthiraFlow.getNammaTagConfigPilotAllConfigs (cast merchantOpCityId) mbUnderExp LYTU.RiderCfg

getNammaTagConfigPilotConfigDetails :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.ConfigType -> Environment.Flow [LYTU.ConfigDetailsResp]
getNammaTagConfigPilotConfigDetails _merchantShortId _opCity configType = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity _merchantShortId _opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show _opCity)
  let merchantOpCityId = merchantOperatingCity.id
  YudhishthiraFlow.getNammaTagConfigPilotConfigDetails (cast merchantOpCityId) (LYTU.RIDER_CONFIG configType)

getNammaTagConfigPilotGetTableData :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.ConfigType -> Environment.Flow LYTU.TableDataResp
getNammaTagConfigPilotGetTableData _merchantShortId _opCity configType = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity _merchantShortId _opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> _merchantShortId.getShortId <> " ,city: " <> show _opCity)
  merchant <- findMerchantByShortId _merchantShortId
  let merchantOpCityId = merchantOperatingCity.id
  TC.returnConfigs configType (cast merchantOpCityId) (cast merchant.id) _opCity

postNammaTagConfigPilotActionChange :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYTU.ActionChangeRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagConfigPilotActionChange _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just _opCity)
  YudhishthiraFlow.postNammaTagConfigPilotActionChange (Just $ cast merchant.id) (cast merchantOpCityId) req TC.handleConfigDBUpdate TC.returnConfigs _opCity

addCustomerTag :: Maybe [Text] -> Text -> [Text]
addCustomerTag Nothing tag = [tag]
addCustomerTag (Just tags) tag = tags ++ [tag]

removeCustomerTag :: Maybe [Text] -> Text -> [Text]
removeCustomerTag Nothing _ = []
removeCustomerTag (Just tags) tag = filter (/= tag) tags
