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
  )
where

import qualified Data.Aeson as A
import Data.Default.Class (Default (..))
import Data.Singletons
import qualified Data.Text as Text
import qualified Domain.Types.DriverPoolConfig as DTD
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantMessage as DTM
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.MerchantPushNotification as DTPN
import qualified Domain.Types.PayoutConfig as DTP
import qualified Domain.Types.RideRelatedNotificationConfig as DTRN
import qualified Domain.Types.TransporterConfig as DTT
-- import qualified Storage.Queries.UiDriverConfig as QUiC

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
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicElement as CADLE
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicRollout as LYSQADLR
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.AppDynamicLogicRollout as LYTADLR
import qualified Lib.Yudhishthira.Types.Common as C
import qualified Lib.Yudhishthira.TypesTH as YTH
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.DriverPool.Config (Config (..))
import SharedLogic.DriverPool.Types
import SharedLogic.DynamicPricing
import qualified SharedLogic.KaalChakra.Chakras as Chakras
import SharedLogic.Merchant
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as SCMDPC
import qualified Storage.CachedQueries.Merchant.MerchantMessage as SCMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as SCMMPN
import qualified Storage.CachedQueries.Merchant.PayoutConfig as SCMP
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCMTC
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as SCR
import qualified Storage.CachedQueries.UiDriverConfig as QUiConfig
import qualified Storage.Queries.DriverPoolConfig as SCMD
import qualified Storage.Queries.MerchantMessage as SQM
import qualified Storage.Queries.MerchantPushNotification as SQMPN
import qualified Storage.Queries.PayoutConfig as SCP
import qualified Storage.Queries.RideRelatedNotificationConfig as SQR
import qualified Storage.Queries.TransporterConfig as SCMT
import qualified Tools.DynamicLogic as TDL

$(YTH.generateGenericDefault ''DTP.PayoutConfig)
$(YTH.generateGenericDefault ''DTRN.RideRelatedNotificationConfig)
$(YTH.generateGenericDefault ''DTT.TransporterConfig) -- TODO ERROR
$(YTH.generateGenericDefault ''DTM.MerchantMessage) -- TODO ERROR
$(YTH.generateGenericDefault ''DTPN.MerchantPushNotification)

$(YTH.generateGenericDefault ''DTD.DriverPoolConfig) -- TODO ERROR

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
              let defVal = C.getLogicInputDef tagStage
              case defVal of
                Nothing -> throwError $ InvalidRequest $ "No data supplied and failed to get default for the specified event, check if `getLogicInputDef` is defined for your event in `instance YTC.LogicInputLink YA.ApplicationEvent`"
                Just defaultVal -> do
                  result <- YudhishthiraFlow.verifyDynamicLogic tagPossibleValues [rule] defaultVal
                  pure $ LYT.CreateNammaTagResponse {result = LYT.ApplicationTagRes (LYT.CreateNammaApplicationTagResponse result defaultVal)}
            _ -> throwError $ InvalidRequest "LLMContext not supported yet"
        _ -> pure $ LYT.CreateNammaTagResponse {result = LYT.Success}

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
        Nothing -> throwError $ InvalidRequest $ "No data supplied and failed to get default for the specified event, check if `getLogicInputDef` is defined for your event in `instance YTC.LogicInputLink YA.ApplicationEvent`"
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
    LYT.DYNAMIC_PRICING_UNIFIED -> do
      logicData :: DynamicPricingData <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy DynamicPricingResult) transporterConfig.referralLinkPassword req logicData
    LYT.CONFIG LYT.DriverPoolConfig -> do
      logicData :: Config <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy Config) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.DriverPoolConfig -> do
      defaultConfig <- (pure $ Prelude.listToMaybe $ YTH.genDef (Proxy @DTD.DriverPoolConfig)) >>= fromMaybeM (InvalidRequest "DriverPoolConfig config not found")
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DTD.DriverPoolConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy (LYT.Config DTD.DriverPoolConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.UI_DRIVER _ _ -> do
      let def'' = Prelude.listToMaybe $ LYT.genDef (Proxy @DTDC.UiDriverConfig)
      def' <- maybe (throwError $ InvalidRequest "No default found for UiDriverConfig") pure def''
      logicData :: DTDC.UiDriverConfig <- YudhishthiraFlow.createLogicData def' (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy DTDC.UiDriverConfig) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.PayoutConfig -> do
      defaultConfig <- (pure $ Prelude.listToMaybe $ YTH.genDef (Proxy @DTP.PayoutConfig)) >>= fromMaybeM (InvalidRequest "PayoutConfig config not found")
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DTP.PayoutConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy (LYT.Config DTP.PayoutConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.RideRelatedNotificationConfig -> do
      defaultConfig <- (pure $ Prelude.listToMaybe $ YTH.genDef (Proxy @DTRN.RideRelatedNotificationConfig)) >>= fromMaybeM (InvalidRequest "RideRelatedNotificationConfig config not found")
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DTRN.RideRelatedNotificationConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy (LYT.Config DTRN.RideRelatedNotificationConfig)) transporterConfig.referralLinkPassword req logicData
    -- LYT.DRIVER_CONFIG LYT.MerchantMessage -> do
    --   defaultConfig <- (pure $ Prelude.listToMaybe $ YTH.genDef (Proxy @DTM.MerchantMessage)) >>= fromMaybeM (InvalidRequest "MerchantMessage config not found")
    --   let configWrap = LYT.Config defaultConfig Nothing 1
    --   logicData :: (LYT.Config DTM.MerchantMessage) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
    --   YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy DTM.MerchantMessage) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.MerchantPushNotification -> do
      defaultConfig <- (pure $ Prelude.listToMaybe $ YTH.genDef (Proxy @DTPN.MerchantPushNotification)) >>= fromMaybeM (InvalidRequest "MerchantPushNotification config not found")
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
  YudhishthiraFlow.upsertLogicRollout (Just $ cast merchant.id) (cast merchantOpCityId) rolloutReq

getNammaTagAppDynamicLogicVersions :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Maybe Prelude.Int -> Prelude.Maybe Prelude.Int -> LYT.LogicDomain -> Environment.Flow LYT.AppDynamicLogicVersionResp
getNammaTagAppDynamicLogicVersions _merchantShortId _opCity = YudhishthiraFlow.getAppDynamicLogicVersions

getNammaTagAppDynamicLogicDomains :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow LYT.AppDynamicLogicDomainResp
getNammaTagAppDynamicLogicDomains _merchantShortId _opCity = return $ LYT.allValues

getNammaTagQueryAll :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.Chakra -> Environment.Flow LYT.ChakraQueryResp
getNammaTagQueryAll _merchantShortId _opCity = YudhishthiraFlow.getNammaTagQueryAll

postNammaTagConfigPilotGetVersion :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.UiConfigRequest -> Environment.Flow Text
postNammaTagConfigPilotGetVersion _ _ uicr = do
  merchant <- findById (Id uicr.merchantId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just uicr.city)
  (_, version) <- QUiConfig.findUIConfig uicr merchantOpCityId
  case version of
    Just ver -> pure $ Text.pack (show ver)
    Nothing -> throwError $ InternalError $ "No config found for merchant:" <> show uicr.merchantId <> " and city:" <> show uicr.city <> " and request:" <> show uicr

postNammaTagConfigPilotGetConfig :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.UiConfigRequest -> Environment.Flow LYT.UiConfigResponse
postNammaTagConfigPilotGetConfig _ _ uicr = do
  merchant <- findById (Id uicr.merchantId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just uicr.city)
  (config, version) <- QUiConfig.findUIConfig uicr merchantOpCityId
  isExp <- TDL.isExperimentRunning (cast merchantOpCityId) (LYT.UI_DRIVER uicr.os uicr.platform)
  case config of
    Just cfg -> pure (LYT.UiConfigResponse cfg.config (Text.pack .show <$> version) isExp)
    Nothing -> throwError $ InternalError $ "No config found for merchant:" <> show uicr.merchantId <> " and city:" <> show uicr.city <> " and request:" <> show uicr

postNammaTagConfigPilotCreateUiConfig :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.CreateConfigRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagConfigPilotCreateUiConfig _ _ ccr = do
  merchant <- findById (Id ccr.merchantId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just ccr.city)
  now <- getCurrentTime
  id' <- generateGUID
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
  YudhishthiraFlow.getNammaTagConfigPilotAllConfigsProvider (cast merchantOpCityId) mbUnderExp

getNammaTagConfigPilotConfigDetails :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ConfigType -> Environment.Flow [LYT.ConfigDetailsResp]
getNammaTagConfigPilotConfigDetails _merchantShortId _opCity configType = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  YudhishthiraFlow.getNammaTagConfigPilotConfigDetailsProvider (cast merchantOpCityId) configType

getNammaTagConfigPilotGetTableData :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ConfigType -> Environment.Flow LYT.TableDataResp
getNammaTagConfigPilotGetTableData _merchantShortId _opCity configType = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  mbBaseRollout <- LYSQADLR.findByCityAndDomainAndIsBase (cast merchantOpCityId) (LYT.DRIVER_CONFIG configType)
  case mbBaseRollout of
    Just baseRollout -> do
      baseElements <- CADLE.findByDomainAndVersion (LYT.DRIVER_CONFIG configType) baseRollout.version
      let logics = map (.logic) baseElements
      returnConfigs configType logics merchantOpCityId
    Nothing -> returnConfigs configType [] merchantOpCityId
  where
    returnConfigs :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LYT.ConfigType -> [Value] -> Id MerchantOperatingCity -> m LYT.TableDataResp
    returnConfigs cfgType logics merchantOpCityId = do
      case cfgType of
        LYT.DriverPoolConfig -> do
          driverPoolCfg <- SCMDPC.findAllByMerchantOpCityId merchantOpCityId (Just []) Nothing
          let configWrapper :: [LYT.Config DTD.DriverPoolConfig] =
                zipWith
                  (\id cfg -> cfg {LYT.identifier = id})
                  [0 ..]
                  (map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) driverPoolCfg)
          patchedConfigs <- mapM (LYTU.runLogics logics) configWrapper
          return LYT.TableDataResp {configs = map (.result) patchedConfigs}
        LYT.TransporterConfig -> do
          transporterCfg <- SCMTC.getTransporterConfigFromDB merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
          let configWrapper :: [LYT.Config DTT.TransporterConfig] =
                zipWith
                  (\id cfg -> cfg {LYT.identifier = id})
                  [0 ..]
                  [(\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) transporterCfg]
          patchedConfigs <- mapM (LYTU.runLogics logics) configWrapper
          return LYT.TableDataResp {configs = map (.result) patchedConfigs}
        LYT.PayoutConfig -> do
          payoutCfg <- SCMP.findAllByMerchantOpCityId merchantOpCityId (Just [])
          let configWrapper :: [LYT.Config DTP.PayoutConfig] =
                zipWith
                  (\id cfg -> cfg {LYT.identifier = id})
                  [0 ..]
                  (map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) payoutCfg)
          patchedConfigs <- mapM (LYTU.runLogics logics) configWrapper
          return LYT.TableDataResp {configs = map (.result) patchedConfigs}
        LYT.RideRelatedNotificationConfig -> do
          rideRelatedNotificationCfg <- SCR.findAllByMerchantOperatingCityId merchantOpCityId (Just [])
          let configWrapper :: [LYT.Config DTRN.RideRelatedNotificationConfig] =
                zipWith
                  (\id cfg -> cfg {LYT.identifier = id})
                  [0 ..]
                  (map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) rideRelatedNotificationCfg)
          patchedConfigs <- mapM (LYTU.runLogics logics) configWrapper
          return LYT.TableDataResp {configs = map (.result) patchedConfigs}
        LYT.MerchantMessage -> do
          merchantMessage <- SCMM.findAllByMerchantOpCityId merchantOpCityId (Just [])
          let configWrapper :: [LYT.Config DTM.MerchantMessage] =
                zipWith
                  (\id cfg -> cfg {LYT.identifier = id})
                  [0 ..]
                  (map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) merchantMessage)
          patchedConfigs <- mapM (LYTU.runLogics logics) configWrapper
          return LYT.TableDataResp {configs = map (.result) patchedConfigs}
        LYT.MerchantPushNotification -> do
          merchantPushNotification <- SCMMPN.findAllByMerchantOpCityId merchantOpCityId (Just [])
          let configWrapper :: [LYT.Config DTPN.MerchantPushNotification] =
                zipWith
                  (\id cfg -> cfg {LYT.identifier = id})
                  [0 ..]
                  (map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) merchantPushNotification)
          patchedConfigs <- mapM (LYTU.runLogics logics) configWrapper
          return LYT.TableDataResp {configs = map (.result) patchedConfigs}
        _ -> throwError $ InvalidRequest "Unsupported config type."

postNammaTagConfigPilotActionChange :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ActionChangeRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagConfigPilotActionChange _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  case req of
    LYT.Conclude concludeReq -> do
      expRollout <- LYSQADLR.findByPrimaryKey concludeReq.domain (cast merchantOpCityId) "Unbounded" concludeReq.version >>= fromMaybeM (InvalidRequest $ "Rollout not found for Domain: " <> show concludeReq.domain <> " City: " <> show merchantOpCityId <> " TimeBounds: " <> "Unbounded" <> " Version: " <> show concludeReq.version)
      logDebug $ show expRollout
      mbBaseRollout <- LYSQADLR.findByCityAndDomainAndIsBase (cast merchantOpCityId) concludeReq.domain
      case mbBaseRollout of
        Just baseRollout -> do
          when (concludeReq.version == baseRollout.version) $ throwError $ InvalidRequest "Cannot conclude the base rollout"
          baseElements <- CADLE.findByDomainAndVersion concludeReq.domain baseRollout.version
          let baseLogics = fmap (.logic) baseElements
          case concludeReq.domain of
            LYT.DRIVER_CONFIG LYT.DriverPoolConfig -> do
              driverPoolCfg <- SCMDPC.findAllByMerchantOpCityId merchantOpCityId (Just []) Nothing
              let configWrapper :: [LYT.Config DTD.DriverPoolConfig] =
                    zipWith
                      (\id cfg -> cfg {LYT.identifier = id})
                      [0 ..]
                      (map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) driverPoolCfg)
              patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
              cfgs :: [LYT.Config DTD.DriverPoolConfig] <-
                mapM
                  ( \resp ->
                      case (A.fromJSON resp.result :: A.Result (LYT.Config DTD.DriverPoolConfig)) of
                        A.Success dpc -> pure dpc
                        A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to driver pool config. " <> show e
                  )
                  patchedConfigs
              let sortedCfgs :: [LYT.Config DTD.DriverPoolConfig] = sortOn LYT.identifier cfgs
                  configsToUpdate :: [DTD.DriverPoolConfig] =
                    catMaybes $
                      zipWith
                        ( \cfg1 cfg2 ->
                            if cfg1.identifier == cfg2.identifier && cfg1.config /= cfg2.config
                              then Just cfg2.config
                              else Nothing
                        )
                        configWrapper
                        sortedCfgs
              mapM_ SCMD.updateByPrimaryKey configsToUpdate
              SCMDPC.clearCache merchantOpCityId
            LYT.DRIVER_CONFIG LYT.TransporterConfig -> do
              transporterCfg <- SCMTC.getTransporterConfigFromDB merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
              let configWrapper :: [LYT.Config DTT.TransporterConfig] =
                    zipWith
                      (\id cfg -> cfg {LYT.identifier = id})
                      [0 ..]
                      [(\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) transporterCfg]
              patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
              cfgs :: [LYT.Config DTT.TransporterConfig] <-
                mapM
                  ( \resp ->
                      case (A.fromJSON resp.result :: A.Result (LYT.Config DTT.TransporterConfig)) of
                        A.Success tc -> pure tc
                        A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to transporter config. " <> show e
                  )
                  patchedConfigs
              let sortedCfgs :: [LYT.Config DTT.TransporterConfig] = sortOn LYT.identifier cfgs
                  configsToUpdate :: [DTT.TransporterConfig] =
                    catMaybes $
                      zipWith
                        ( \cfg1 cfg2 ->
                            if cfg1.identifier == cfg2.identifier && cfg1.config /= cfg2.config
                              then Just cfg2.config
                              else Nothing
                        )
                        configWrapper
                        sortedCfgs
              mapM_ SCMT.update configsToUpdate
              SCMTC.clearCache merchantOpCityId
            LYT.DRIVER_CONFIG LYT.PayoutConfig -> do
              payoutCfg <- SCMP.findAllByMerchantOpCityId merchantOpCityId (Just [])
              let configWrapper :: [LYT.Config DTP.PayoutConfig] =
                    zipWith
                      (\id cfg -> cfg {LYT.identifier = id})
                      [0 ..]
                      (map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) payoutCfg)
              patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
              cfgs :: [LYT.Config DTP.PayoutConfig] <-
                mapM
                  ( \resp ->
                      case (A.fromJSON resp.result :: A.Result (LYT.Config DTP.PayoutConfig)) of
                        A.Success tc -> pure tc
                        A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to transporter config. " <> show e
                  )
                  patchedConfigs
              let sortedCfgs :: [LYT.Config DTP.PayoutConfig] = sortOn LYT.identifier cfgs
                  configsToUpdate :: [DTP.PayoutConfig] =
                    catMaybes $
                      zipWith
                        ( \cfg1 cfg2 ->
                            if cfg1.identifier == cfg2.identifier && cfg1.config /= cfg2.config
                              then Just cfg2.config
                              else Nothing
                        )
                        configWrapper
                        sortedCfgs
              mapM_ SCP.updateByPrimaryKey configsToUpdate
              SCMP.clearCacheById merchantOpCityId -- TODO use clearConfigCache
            LYT.DRIVER_CONFIG LYT.RideRelatedNotificationConfig -> do
              rideRelatedNotificationCfg <- SCR.findAllByMerchantOperatingCityId merchantOpCityId (Just [])
              let configWrapper :: [LYT.Config DTRN.RideRelatedNotificationConfig] =
                    zipWith
                      (\id cfg -> cfg {LYT.identifier = id})
                      [0 ..]
                      (map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) rideRelatedNotificationCfg)
              patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
              cfgs :: [LYT.Config DTRN.RideRelatedNotificationConfig] <-
                mapM
                  ( \resp ->
                      case (A.fromJSON resp.result :: A.Result (LYT.Config DTRN.RideRelatedNotificationConfig)) of
                        A.Success tc -> pure tc
                        A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to transporter config. " <> show e
                  )
                  patchedConfigs
              let sortedCfgs :: [LYT.Config DTRN.RideRelatedNotificationConfig] = sortOn LYT.identifier cfgs
                  configsToUpdate :: [DTRN.RideRelatedNotificationConfig] =
                    catMaybes $
                      zipWith
                        ( \cfg1 cfg2 ->
                            if cfg1.identifier == cfg2.identifier && cfg1.config /= cfg2.config
                              then Just cfg2.config
                              else Nothing
                        )
                        configWrapper
                        sortedCfgs
              mapM_ SQR.updateByPrimaryKey configsToUpdate
            LYT.DRIVER_CONFIG LYT.MerchantMessage -> do
              merchantMessage <- SCMM.findAllByMerchantOpCityId merchantOpCityId (Just [])
              let configWrapper :: [LYT.Config DTM.MerchantMessage] =
                    zipWith
                      (\id cfg -> cfg {LYT.identifier = id})
                      [0 ..]
                      (map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) merchantMessage)
              patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
              cfgs :: [LYT.Config DTM.MerchantMessage] <-
                mapM
                  ( \resp ->
                      case (A.fromJSON resp.result :: A.Result (LYT.Config DTM.MerchantMessage)) of
                        A.Success tc -> pure tc
                        A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to transporter config. " <> show e
                  )
                  patchedConfigs
              let sortedCfgs :: [LYT.Config DTM.MerchantMessage] = sortOn LYT.identifier cfgs
                  configsToUpdate :: [DTM.MerchantMessage] =
                    catMaybes $
                      zipWith
                        ( \cfg1 cfg2 ->
                            if cfg1.identifier == cfg2.identifier && cfg1.config /= cfg2.config
                              then Just cfg2.config
                              else Nothing
                        )
                        configWrapper
                        sortedCfgs
              mapM_ SQM.updateByPrimaryKey configsToUpdate
              SCMM.clearCacheById merchantOpCityId
            LYT.DRIVER_CONFIG LYT.MerchantPushNotification -> do
              merchantPushNotification <- SCMMPN.findAllByMerchantOpCityId merchantOpCityId (Just [])
              let configWrapper :: [LYT.Config DTPN.MerchantPushNotification] =
                    zipWith
                      (\id cfg -> cfg {LYT.identifier = id})
                      [0 ..]
                      (map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) merchantPushNotification)
              patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
              cfgs :: [LYT.Config DTPN.MerchantPushNotification] <-
                mapM
                  ( \resp ->
                      case (A.fromJSON resp.result :: A.Result (LYT.Config DTPN.MerchantPushNotification)) of
                        A.Success tc -> pure tc
                        A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to transporter config. " <> show e
                  )
                  patchedConfigs
              let sortedCfgs :: [LYT.Config DTPN.MerchantPushNotification] = sortOn LYT.identifier cfgs
                  configsToUpdate :: [DTPN.MerchantPushNotification] =
                    catMaybes $
                      zipWith
                        ( \cfg1 cfg2 ->
                            if cfg1.identifier == cfg2.identifier && cfg1.config /= cfg2.config
                              then Just cfg2.config
                              else Nothing
                        )
                        configWrapper
                        sortedCfgs
              mapM_ SQMPN.updateByPrimaryKey configsToUpdate
              SCMMPN.clearCacheById merchantOpCityId

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

            _ -> throwError $ InvalidRequest $ "Logic Domain not supported" <> show concludeReq.domain
          let originalBasePercentage = baseRollout.percentageRollout
              updatedBaseRollout =
                baseRollout
                  { LYTADLR.isBaseVersion = Nothing,
                    LYTADLR.experimentStatus = Just LYT.CONCLUDED,
                    LYTADLR.percentageRollout = 0
                  }
          LYSQADLR.updateByPrimaryKey updatedBaseRollout
          YudhishthiraFlow.postNammaTagConfigPilotActionChange (Just $ cast merchant.id) (cast merchantOpCityId) req (Just originalBasePercentage)
        _ -> YudhishthiraFlow.postNammaTagConfigPilotActionChange (Just $ cast merchant.id) (cast merchantOpCityId) req Nothing
    _ ->
      YudhishthiraFlow.postNammaTagConfigPilotActionChange (Just $ cast merchant.id) (cast merchantOpCityId) req Nothing
