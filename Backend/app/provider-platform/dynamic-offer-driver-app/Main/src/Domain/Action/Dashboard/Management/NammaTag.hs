{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.Dashboard.Management.NammaTag
  ( postNammaTagTagCreate,
    postNammaTagTagVerify,
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
    postNammaTagConfigPilotGetVersion,
    postNammaTagConfigPilotGetConfig,
    postNammaTagConfigPilotCreateUiConfig,
    getNammaTagConfigPilotAllConfigs,
    getNammaTagConfigPilotConfigDetails,
    getNammaTagConfigPilotGetTableData,
    postNammaTagConfigPilotActionChange,
    getNammaTagConfigPilotAllUiConfigs,
    getNammaTagConfigPilotUiConfigDetails,
    getNammaTagConfigPilotGetUiTableData,
    postNammaTagConfigPilotGetPatchedElement,
  )
where

import qualified ConfigPilotFrontend.Flow as CPF
import qualified ConfigPilotFrontend.Types as CPT
import qualified Data.Aeson as A
import Data.Default.Class (Default (..))
import qualified Data.List.NonEmpty as NE
import Data.Singletons
import qualified Data.Text as Text
import qualified Domain.Types.DriverPoolConfig as DTD
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantMessage as DTM
import qualified Domain.Types.MerchantPushNotification as DTPN
import qualified Domain.Types.PayoutConfig as DTP
import qualified Domain.Types.RideRelatedNotificationConfig as DTRN
import qualified Domain.Types.TransporterConfig as DTT
import Domain.Types.UiDriverConfig (UiDriverConfig (..))
import qualified Domain.Types.UiDriverConfig as DTDC
import qualified Domain.Types.Yudhishthira
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude as Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Scheduler.JobStorageType.DB.Queries as QDBJ
import Lib.Scheduler.Types (AnyJob (..))
import qualified Lib.Yudhishthira.Flow.Dashboard as YudhishthiraFlow
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as CADLR
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.Common as C
import qualified Lib.Yudhishthira.TypesTH as YTH
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified SharedLogic.BehaviourManagement.GpsTollBehavior as GpsTollBehavior
import SharedLogic.CancellationCoins
import SharedLogic.DriverPool.Config (Config (..))
import SharedLogic.DriverPool.Types
import SharedLogic.DynamicPricing
import qualified SharedLogic.KaalChakra.Chakras as Chakras
import SharedLogic.Merchant
import SharedLogic.UserCancellationDues
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.UiDriverConfig as QUiConfig
import qualified Storage.Queries.UiDriverConfig as SQU
import qualified Tools.ConfigPilot as TC
import qualified Tools.DynamicLogic as TDL

$(YTH.generateGenericDefault ''DTP.PayoutConfig)
$(YTH.generateGenericDefault ''DTRN.RideRelatedNotificationConfig)
$(YTH.generateGenericDefault ''DTT.TransporterConfig) -- TODO ERROR
$(YTH.generateGenericDefault ''DTM.MerchantMessage) -- TODO ERROR
$(YTH.generateGenericDefault ''DTPN.MerchantPushNotification)

$(YTH.generateGenericDefault ''DTD.DriverPoolConfig)

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.CreateNammaTagRequest -> Environment.Flow LYT.CreateNammaTagResponse)
postNammaTagTagCreate _merchantShortId _opCity req = do
  defaultRunResult <- runAgainstDefault
  void $ YudhishthiraFlow.postTagCreate req
  pure defaultRunResult
  where
    runAgainstDefault = do
      case req of
        LYT.ApplicationTag LYT.NammaTagApplication {..} -> do
          case tagRule of
            C.RuleEngine rule -> do
              unless (length tagStages == length (NE.nub tagStages)) $
                throwError (InvalidRequest "Tag stages should be unique")
              createTagResponses <- forM tagStages \event -> do
                let defVal = C.getLogicInputDef event
                case defVal of
                  Nothing -> throwError $ InvalidRequest "No data supplied and failed to get default for the specified event, check if `getLogicInputDef` is defined for your event in `instance YTC.LogicInputLink YA.ApplicationEvent`"
                  Just defaultVal -> do
                    result <- YudhishthiraFlow.verifyDynamicLogic tagPossibleValues [rule] defaultVal
                    pure $ LYT.ApplicationTagRes (LYT.CreateNammaApplicationTagResponse result defaultVal)
              pure $ LYT.CreateNammaTagResponse {results = createTagResponses}
            _ -> throwError $ InvalidRequest "LLMContext not supported yet"
        _ -> pure $ LYT.CreateNammaTagResponse {results = NE.singleton LYT.Success}

postNammaTagTagUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.UpdateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagUpdate _merchantShortId _opCity req = YudhishthiraFlow.postTagUpdate req

postNammaTagTagVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.VerifyNammaTagRequest -> Environment.Flow LYT.VerifyNammaTagResponse)
postNammaTagTagVerify _merchantShortId _opCity LYT.VerifyNammaTagRequest {..} = do
  case source of
    LYT.Application tagStage -> do
      let val =
            if useDefaultData
              then C.getLogicInputDef tagStage
              else logicData <|> C.getLogicInputDef tagStage
      case val of
        Just value -> do
          -- validating data provided gets parsed to Stage InputData type.
          validateInputType tagStage value
          result <- YudhishthiraFlow.verifyEventLogic tagStage [logic] value
          pure $ LYT.VerifyNammaTagResponse {executionResult = result, dataUsed = value}
        Nothing -> throwError $ InvalidRequest "No data supplied and failed to get default for the specified event, check if `getLogicInputDef` is defined for your event in `instance YTC.LogicInputLink YA.ApplicationEvent`"
    _ -> do
      throwError $ InvalidRequest $ "Available only for Application events currenlty"
  where
    validateInputType tagStage value =
      case tagStage of
        LYT.Search -> do
          _ :: Domain.Types.Yudhishthira.TagData <- parseOrThrowError value
          pure ()
        LYT.RideEnd -> do
          _ :: Domain.Types.Yudhishthira.EndRideTagData <- parseOrThrowError value
          pure ()
        LYT.RideCancel -> do
          _ :: Domain.Types.Yudhishthira.CancelRideTagData <- parseOrThrowError value
          pure ()
        _ -> throwError $ InvalidRequest $ "Only supported for Search, Cancel and EndRide event for now"

    parseOrThrowError value =
      case A.fromJSON value of
        A.Success res -> pure res
        A.Error err -> throwError $ InvalidRequest $ show err

deleteNammaTagTagDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTagDelete _merchantShortId _opCity tagName = YudhishthiraFlow.deleteTag tagName

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ChakraQueriesAPIEntity -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate _merchantShortId _opCity req = YudhishthiraFlow.postQueryCreate req

postNammaTagQueryUpdate :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ChakraQueryUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagQueryUpdate _merchantShortId _opCity = YudhishthiraFlow.postQueryUpdate

deleteNammaTagQueryDelete :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ChakraQueryDeleteReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteNammaTagQueryDelete _merchantShortId _opCity = YudhishthiraFlow.queryDelete

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.AppDynamicLogicReq -> Environment.Flow LYT.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let mbMerchantId = Just $ cast merchant.id
  case req.domain of
    LYT.POOLING -> do
      driversData :: [DriverPoolWithActualDistResult] <- mapM (YudhishthiraFlow.createLogicData def . Just) req.inputData
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy TaggedDriverPoolInput) transporterConfig.referralLinkPassword req (TaggedDriverPoolInput driversData False 0)
    LYT.CANCELLATION_COIN_POLICY -> do
      logicData :: CancellationCoinData <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy CancellationCoinResult) transporterConfig.referralLinkPassword req logicData
    LYT.DYNAMIC_PRICING_UNIFIED -> do
      logicData :: DynamicPricingData <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy DynamicPricingResult) transporterConfig.referralLinkPassword req logicData
    LYT.USER_CANCELLATION_DUES -> do
      logicData :: UserCancellationDuesData <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy UserCancellationDuesResult) transporterConfig.referralLinkPassword req logicData
    LYT.GPS_TOLL_BEHAVIOR -> do
      logicData :: GpsTollBehavior.GpsTollBehaviorData <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy GpsTollBehavior.GpsTollBehaviorOutput) transporterConfig.referralLinkPassword req logicData
    LYT.USER_CANCELLATION_DUES_WAIVE_OFF -> do
      logicData :: UserCancellationDuesWaiveOffData <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy UserCancellationDuesWaiveOffResult) transporterConfig.referralLinkPassword req logicData
    LYT.CONFIG LYT.DriverPoolConfig -> do
      logicData :: Config <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy Config) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.DriverPoolConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "DriverPoolConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTD.DriverPoolConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DTD.DriverPoolConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy (LYT.Config DTD.DriverPoolConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.UI_DRIVER dt pt -> do
      let uiConfigReq = LYT.UiConfigRequest {os = dt, platform = pt, merchantId = getId merchant.id, city = opCity, language = Nothing, bundle = Nothing, toss = Nothing}
      defaultConfig <- SQU.findUIConfig uiConfigReq merchantOpCityId >>= fromMaybeM (InvalidRequest "No default found for UiDriverConfig")
      let configWrap = LYT.Config defaultConfig.config Nothing 1
      logicData :: (LYT.Config Value) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      url <- TC.getTSServiceUrl
      YudhishthiraFlow.verifyAndUpdateUIDynamicLogic mbMerchantId (Proxy :: Proxy (LYT.Config Value)) transporterConfig.referralLinkPassword req logicData url
    LYT.DRIVER_CONFIG LYT.PayoutConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "PayoutConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTP.PayoutConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DTP.PayoutConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy (LYT.Config DTP.PayoutConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.RideRelatedNotificationConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "RideRelatedNotificationConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTRN.RideRelatedNotificationConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DTRN.RideRelatedNotificationConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy (LYT.Config DTRN.RideRelatedNotificationConfig)) transporterConfig.referralLinkPassword req logicData
    -- LYT.DRIVER_CONFIG LYT.MerchantMessage -> do
    --   defaultConfig <- (pure $ Prelude.listToMaybe $ YTH.genDef (Proxy @DTM.MerchantMessage)) >>= fromMaybeM (InvalidRequest "MerchantMessage config not found")
    --   let configWrap = LYT.Config defaultConfig Nothing 1
    --   logicData :: (LYT.Config DTM.MerchantMessage) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
    --   YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy DTM.MerchantMessage) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.MerchantPushNotification -> do
      defaultConfig <- fromMaybeM (InvalidRequest "MerchantPushNotification config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTPN.MerchantPushNotification))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DTPN.MerchantPushNotification) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy (LYT.Config DTPN.MerchantPushNotification)) transporterConfig.referralLinkPassword req logicData
    -- LYT.DRIVER_CONFIG LYT.TransporterConfig -> do
    --   def <- (pure $ Prelude.listToMaybe $ YTH.genDef (Proxy @DTT.TransporterConfig)) >>= fromMaybeM (InvalidRequest "Transporter config not found")
    --   let configWrap = LYT.Config def Nothing 1
    --   logicData :: (LYT.Config DTT.TransporterConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
    --   YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy DTT.TransporterConfig) transporterConfig.referralLinkPassword req logicData
    _ -> throwError $ InvalidRequest "Logic Domain not supported"

getNammaTagAppDynamicLogic :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Maybe Int -> LYT.LogicDomain -> Environment.Flow [LYT.GetLogicsResp]
getNammaTagAppDynamicLogic _ _ = YudhishthiraFlow.getAppDynamicLogicForDomain

postNammaTagRunJob ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  LYT.RunKaalChakraJobReq ->
  Environment.Flow LYT.RunKaalChakraJobRes
postNammaTagRunJob merchantShortId opCity req = do
  mbMerchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
  let mbMerchantOpCityId = mbMerchantOperatingCity <&> (.id)
  let mbMerchantId = mbMerchantOperatingCity <&> (.merchantId)
  -- Logic for complete old jobs works only for DbBased jobs
  whenJust req.completeOldJob $ \oldJobId -> do
    mbOldJob :: Maybe (AnyJob AllocatorJobType) <- QDBJ.findById oldJobId
    case mbOldJob of
      Nothing -> throwError (InvalidRequest "Job not found")
      Just (AnyJob oldJob) -> do
        let jobType = fromSing $ oldJob.jobInfo.jobType
        unless (castChakra jobType == Just req.chakra) do
          throwError (InvalidRequest "Invalid job type")
        QDBJ.markAsComplete oldJobId
  let kaalChakraHandle = Chakras.mkKaalChakraHandle mbMerchantId mbMerchantOpCityId
  case req.action of
    LYT.RUN -> YudhishthiraFlow.postRunKaalChakraJob kaalChakraHandle req
    LYT.SCHEDULE scheduledTime -> do
      now <- getCurrentTime
      when (scheduledTime <= now) $
        throwError (InvalidRequest "Schedule job available only for future")
      case req.usersSet of
        LYT.ALL_USERS -> pure ()
        _ -> throwError (InvalidRequest "Schedule job available only for all users")
      let jobData = LYT.mkKaalChakraJobData req (Just scheduledTime)
      kaalChakraHandle.createFetchUserDataJob req.chakra jobData scheduledTime

      logInfo $ "Scheduled new " <> show req.chakra <> " job"
      pure $ LYT.RunKaalChakraJobRes {eventId = Nothing, tags = Nothing, users = Nothing, chakraBatchState = LYT.Completed}
  where
    castChakra :: AllocatorJobType -> Maybe LYT.Chakra
    castChakra Daily = Just LYT.Daily
    castChakra Weekly = Just LYT.Weekly
    castChakra Monthly = Just LYT.Monthly
    castChakra Quarterly = Just LYT.Quarterly
    castChakra _ = Nothing

getNammaTagTimeBounds :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.LogicDomain -> Environment.Flow LYT.TimeBoundResp
getNammaTagTimeBounds merchantShortId opCity domain = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.getTimeBounds (cast merchantOpCityId) domain

postNammaTagTimeBoundsCreate :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.CreateTimeBoundRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagTimeBoundsCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.createTimeBounds (cast merchantOpCityId) req

deleteNammaTagTimeBoundsDelete :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.LogicDomain -> Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteNammaTagTimeBoundsDelete merchantShortId opCity domain name = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.deleteTimeBounds (cast merchantOpCityId) domain name

getNammaTagAppDynamicLogicGetLogicRollout :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Maybe Text -> LYT.LogicDomain -> Environment.Flow [LYT.LogicRolloutObject]
getNammaTagAppDynamicLogicGetLogicRollout merchantShortId opCity timeBound domain = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.getLogicRollout (cast merchantOpCityId) timeBound domain

postNammaTagAppDynamicLogicUpsertLogicRollout :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> [LYT.LogicRolloutObject] -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagAppDynamicLogicUpsertLogicRollout merchantShortId opCity rolloutReq = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.upsertLogicRollout (Just $ cast merchant.id) (cast merchantOpCityId) rolloutReq TC.returnConfigs opCity

getNammaTagAppDynamicLogicVersions :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Maybe Prelude.Int -> Prelude.Maybe Prelude.Int -> LYT.LogicDomain -> Environment.Flow LYT.AppDynamicLogicVersionResp
getNammaTagAppDynamicLogicVersions _merchantShortId _opCity = YudhishthiraFlow.getAppDynamicLogicVersions

getNammaTagAppDynamicLogicDomains :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow LYT.AppDynamicLogicDomainResp
getNammaTagAppDynamicLogicDomains _merchantShortId _opCity = return LYT.allValues

getNammaTagQueryAll :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.Chakra -> Environment.Flow LYT.ChakraQueryResp
getNammaTagQueryAll _merchantShortId _opCity = YudhishthiraFlow.getNammaTagQueryAll

postNammaTagConfigPilotGetVersion :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.UiConfigRequest -> Environment.Flow LYT.UiConfigGetVersionResponse
postNammaTagConfigPilotGetVersion _ _ uicr = do
  merchant <- findById (Id uicr.merchantId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just uicr.city)
  config <- QUiConfig.findUIConfig uicr merchantOpCityId False
  baseRollout <- CADLR.findBaseRolloutByMerchantOpCityAndDomain (cast merchantOpCityId) (LYT.UI_DRIVER uicr.os uicr.platform) >>= fromMaybeM (InvalidRequest "Base Rollout not found")
  let baseVersion = baseRollout.version
  case config of
    Just (_, version) -> pure $ LYT.UiConfigGetVersionResponse {version = Text.pack $ show version, baseVersion = Text.pack $ show baseVersion}
    Nothing -> throwError $ InternalError $ "No config found for merchant:" <> show uicr.merchantId <> " and city:" <> show uicr.city <> " and request:" <> show uicr

postNammaTagConfigPilotGetConfig :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.UiConfigRequest -> Environment.Flow LYT.UiConfigResponse
postNammaTagConfigPilotGetConfig _ _ uicr = do
  merchant <- findById (Id uicr.merchantId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just uicr.city)
  configInfo <- QUiConfig.findUIConfig uicr merchantOpCityId False
  isExp <- TDL.isExperimentRunning (cast merchantOpCityId) (LYT.UI_DRIVER uicr.os uicr.platform)
  baseRollout <- CADLR.findBaseRolloutByMerchantOpCityAndDomain (cast merchantOpCityId) (LYT.UI_DRIVER uicr.os uicr.platform) >>= fromMaybeM (InvalidRequest "Base Rollout not found")
  let baseVersion = baseRollout.version
  case configInfo of
    Just (cfg, version) -> do
      pure (LYT.UiConfigResponse cfg.config (Text.pack $ show version) (Text.pack $ show baseVersion) isExp)
    Nothing -> throwError $ InternalError $ "No config found for merchant:" <> show uicr.merchantId <> " and city:" <> show uicr.city <> " and request:" <> show uicr

postNammaTagConfigPilotCreateUiConfig :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.CreateConfigRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagConfigPilotCreateUiConfig _ _ ccr = do
  url <- TC.getTSServiceUrl
  when (ccr.platform == LYT.TypeScript) $ do
    configValidateResp <- CPF.configValidate url (ccr.config)
    case configValidateResp.status of
      CPT.VALID_CONFIG -> pure ()
      CPT.INVALID_CONFIG -> throwError $ InvalidRequest "Invalid config"
      CPT.INVALID_REQUEST -> throwError $ InvalidRequest "Invalid request"
  merchant <- findById (Id ccr.merchantId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just ccr.city)
  now <- getCurrentTime
  id' <- generateGUID
  let uicr = LYT.UiConfigRequest {merchantId = ccr.merchantId, city = ccr.city, os = ccr.os, platform = ccr.platform, bundle = ccr.bundle, language = Nothing, toss = Nothing}
  configInfo <- QUiConfig.findUIConfig uicr merchantOpCityId False
  when (isJust configInfo) $ do
    throwError $ InvalidRequest "Config already exists"
  QUiConfig.create $ cfg merchantOpCityId now id'
  return Kernel.Types.APISuccess.Success
  where
    cfg merchantOpCityId now id' =
      DTDC.UiDriverConfig
        { platform = ccr.platform,
          config = ccr.config,
          createdAt = now,
          id = id',
          os = ccr.os,
          updatedAt = now,
          merchantOperatingCityId = merchantOpCityId
        }

getNammaTagConfigPilotAllConfigs :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Maybe Prelude.Bool -> Environment.Flow [LYT.ConfigType]
getNammaTagConfigPilotAllConfigs _merchantShortId _opCity mbUnderExp = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  YudhishthiraFlow.getNammaTagConfigPilotAllConfigs (cast merchantOpCityId) mbUnderExp LYT.DriverCfg

getNammaTagConfigPilotConfigDetails :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ConfigType -> Environment.Flow [LYT.ConfigDetailsResp]
getNammaTagConfigPilotConfigDetails _merchantShortId _opCity configType = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  YudhishthiraFlow.getNammaTagConfigPilotConfigDetails (cast merchantOpCityId) (LYT.DRIVER_CONFIG configType)

getNammaTagConfigPilotGetTableData :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ConfigType -> Environment.Flow LYT.TableDataResp
getNammaTagConfigPilotGetTableData _merchantShortId _opCity configType = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let domain = LYT.DRIVER_CONFIG configType
  TC.returnConfigs domain (cast merchantOpCityId) (cast merchant.id) _opCity

postNammaTagConfigPilotActionChange :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ActionChangeRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagConfigPilotActionChange _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  YudhishthiraFlow.postNammaTagConfigPilotActionChange (Just $ cast merchant.id) (cast merchantOpCityId) req TC.handleConfigDBUpdate TC.returnConfigs _opCity

getNammaTagConfigPilotAllUiConfigs :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Maybe Prelude.Bool -> Environment.Flow [LYT.LogicDomain]
getNammaTagConfigPilotAllUiConfigs _merchantShortId _opCity mbUnderExp = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  YudhishthiraFlow.getNammaTagConfigPilotAllUiConfigs (cast merchantOpCityId) mbUnderExp LYT.DriverCfg

getNammaTagConfigPilotUiConfigDetails :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.UiDevicePlatformReq -> Environment.Flow [LYT.ConfigDetailsResp]
getNammaTagConfigPilotUiConfigDetails _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let domain = LYT.UI_DRIVER req.deviceType req.platformType
  YudhishthiraFlow.getNammaTagConfigPilotUiConfigDetails (cast merchantOpCityId) domain

getNammaTagConfigPilotGetUiTableData :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.UiDevicePlatformReq -> Environment.Flow LYT.TableDataResp
getNammaTagConfigPilotGetUiTableData _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let domain = LYT.UI_DRIVER req.deviceType req.platformType
  TC.returnConfigs domain (cast merchantOpCityId) (cast merchant.id) _opCity

postNammaTagConfigPilotGetPatchedElement :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.GetPatchedElementReq -> Environment.Flow LYT.GetPatchedElementResp
postNammaTagConfigPilotGetPatchedElement _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  YudhishthiraFlow.postNammaTagConfigPilotGetPatchedElement (cast merchantOpCityId) req

-- { os :: DeviceType,
--     language :: Language,
--     bundle :: Maybe Text,
--     platform :: PlatformType,
--     merchantId :: Text,
--     city :: Kernel.Types.Beckn.Context.City,
--     toss :: Maybe Int

-- LYT.UI_DRIVER dt pt -> do
--   uiDriverCfg <- QUiC.findUIConfig (LYT.UiConfigRequest dt Nothing Nothing pt (cast merchant.id.getId) (cast merchantOpCityId)) merchantOpCityId
--   let configWrapper :: [LYT.Config DTT.TransporterConfig] =
--         zipWith
--           (\id cfg -> cfg {LYT.identifier = id})
--           [0 ..]
--           [(\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) transporterCfg]
--   patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
--   mbCfgs :: [Maybe (LYT.Config DTT.TransporterConfig)] <-
--     mapM
--       ( \resp ->
--           case (A.fromJSON resp.result :: A.Result (LYT.Config DTT.TransporterConfig)) of
--             A.Success tc -> pure $ Just tc
--             A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to transporter config. " <> show e
--       )
--       patchedConfigs
--   let configsToUpdate :: [DTT.TransporterConfig] =
--         catMaybes $
--           zipWith
--             ( \cfg mbCfg ->
--                 case mbCfg of
--                   Just cfg' | cfg /= cfg' -> Just cfg'.config
--                   _ -> Nothing
--             )
--             configWrapper
--             mbCfgs
--   mapM_ SCMT.update configsToUpdate
