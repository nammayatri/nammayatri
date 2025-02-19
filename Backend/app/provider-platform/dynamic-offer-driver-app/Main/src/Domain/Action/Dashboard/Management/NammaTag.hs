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
    getNammaTagConfigPilotAllConfigs,
    getNammaTagConfigPilotConfigDetails,
    getNammaTagConfigPilotGetTableData,
    postNammaTagConfigPilotActionChange,
  )
where

import qualified Data.Aeson as A
import Data.Default.Class (Default (..))
import Data.Singletons
import qualified Domain.Types.DriverPoolConfig as DTD
import qualified Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
-- import qualified Domain.Types.MerchantMessage as DTM
import qualified Domain.Types.MerchantPushNotification as DTPN
import qualified Domain.Types.PayoutConfig as DTP
import qualified Domain.Types.RideRelatedNotificationConfig as DTRN
import qualified Domain.Types.TransporterConfig as DTT
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
import qualified Lib.Yudhishthira.Types
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
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverPoolConfig as SCMD
import qualified Storage.Queries.TransporterConfig as SCMT

-- import qualified Domain.Types.DriverPoolConfig as DTDP

$(YTH.generateGenericDefault ''DTP.PayoutConfig)
$(YTH.generateGenericDefault ''DTRN.RideRelatedNotificationConfig)

-- $(YTH.generateGenericDefault ''DTM.MerchantMessage)

$(YTH.generateGenericDefault ''DTPN.MerchantPushNotification)

-- $(YTH.generateGenericDefault ''DTDP.DriverPoolConfig)

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.CreateNammaTagRequest -> Environment.Flow Lib.Yudhishthira.Types.CreateNammaTagResponse)
postNammaTagTagCreate _merchantShortId _opCity req = do
  defaultRunResult <- runAgainstDefault
  void $ YudhishthiraFlow.postTagCreate req
  pure defaultRunResult
  where
    runAgainstDefault = do
      case req of
        Lib.Yudhishthira.Types.ApplicationTag Lib.Yudhishthira.Types.NammaTagApplication {..} -> do
          case tagRule of
            C.RuleEngine rule -> do
              let defVal = C.getLogicInputDef tagStage
              case defVal of
                Nothing -> throwError $ InvalidRequest $ "No data supplied and failed to get default for the specified event, check if `getLogicInputDef` is defined for your event in `instance YTC.LogicInputLink YA.ApplicationEvent`"
                Just defaultVal -> do
                  result <- YudhishthiraFlow.verifyDynamicLogic tagPossibleValues [rule] defaultVal
                  pure $ Lib.Yudhishthira.Types.CreateNammaTagResponse {result = Lib.Yudhishthira.Types.ApplicationTagRes (Lib.Yudhishthira.Types.CreateNammaApplicationTagResponse result defaultVal)}
            _ -> throwError $ InvalidRequest "LLMContext not supported yet"
        _ -> pure $ Lib.Yudhishthira.Types.CreateNammaTagResponse {result = Lib.Yudhishthira.Types.Success}

postNammaTagTagUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.UpdateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagUpdate _merchantShortId _opCity req = YudhishthiraFlow.postTagUpdate req

postNammaTagTagVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.VerifyNammaTagRequest -> Environment.Flow Lib.Yudhishthira.Types.VerifyNammaTagResponse)
postNammaTagTagVerify _merchantShortId _opCity Lib.Yudhishthira.Types.VerifyNammaTagRequest {..} = do
  case source of
    Lib.Yudhishthira.Types.Application tagStage -> do
      let val =
            if useDefaultData
              then C.getLogicInputDef tagStage
              else logicData <|> C.getLogicInputDef tagStage
      case val of
        Just value -> do
          -- validating data provided gets parsed to Stage InputData type.
          validateInputType tagStage value
          result <- YudhishthiraFlow.verifyEventLogic tagStage [logic] value
          pure $ Lib.Yudhishthira.Types.VerifyNammaTagResponse {executionResult = result, dataUsed = value}
        Nothing -> throwError $ InvalidRequest $ "No data supplied and failed to get default for the specified event, check if `getLogicInputDef` is defined for your event in `instance YTC.LogicInputLink YA.ApplicationEvent`"
    _ -> do
      throwError $ InvalidRequest $ "Available only for Application events currenlty"
  where
    validateInputType tagStage value =
      case tagStage of
        Lib.Yudhishthira.Types.Search -> do
          _ :: Domain.Types.Yudhishthira.TagData <- parseOrThrowError value
          pure ()
        Lib.Yudhishthira.Types.RideEnd -> do
          _ :: Domain.Types.Yudhishthira.EndRideTagData <- parseOrThrowError value
          pure ()
        Lib.Yudhishthira.Types.RideCancel -> do
          _ :: Domain.Types.Yudhishthira.CancelRideTagData <- parseOrThrowError value
          pure ()
        _ -> throwError $ InvalidRequest $ "Only supported for Search, Cancel and EndRide event for now"

    parseOrThrowError value =
      case A.fromJSON value of
        A.Success res -> pure res
        A.Error err -> throwError $ InvalidRequest $ show err

deleteNammaTagTagDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTagDelete _merchantShortId _opCity tagName = YudhishthiraFlow.deleteTag tagName

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate _merchantShortId _opCity req = YudhishthiraFlow.postQueryCreate req

postNammaTagQueryUpdate :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.ChakraQueryUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagQueryUpdate _merchantShortId _opCity = YudhishthiraFlow.postQueryUpdate

deleteNammaTagQueryDelete :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.ChakraQueryDeleteReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteNammaTagQueryDelete _merchantShortId _opCity = YudhishthiraFlow.queryDelete

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.AppDynamicLogicReq -> Environment.Flow Lib.Yudhishthira.Types.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let mbMerchantId = Just $ cast merchant.id
  case req.domain of
    Lib.Yudhishthira.Types.POOLING -> do
      driversData :: [DriverPoolWithActualDistResult] <- mapM (YudhishthiraFlow.createLogicData def . Just) req.inputData
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy TaggedDriverPoolInput) transporterConfig.referralLinkPassword req (TaggedDriverPoolInput driversData False 0)
    Lib.Yudhishthira.Types.DYNAMIC_PRICING_UNIFIED -> do
      logicData :: DynamicPricingData <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy DynamicPricingResult) transporterConfig.referralLinkPassword req logicData
    Lib.Yudhishthira.Types.CONFIG Lib.Yudhishthira.Types.DriverPoolConfig -> do
      logicData :: Config <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy Config) transporterConfig.referralLinkPassword req logicData
    Lib.Yudhishthira.Types.DRIVER_CONFIG Lib.Yudhishthira.Types.DriverPoolConfig -> do
      logicData :: Config <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy Config) transporterConfig.referralLinkPassword req logicData
    Lib.Yudhishthira.Types.DRIVER_CONFIG Lib.Yudhishthira.Types.PayoutConfig -> do
      let defaultType = Prelude.listToMaybe $ YTH.genDef (Proxy @DTP.PayoutConfig)
      logicData :: Maybe DTP.PayoutConfig <- YudhishthiraFlow.createLogicData defaultType (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy DTP.PayoutConfig) transporterConfig.referralLinkPassword req logicData
    Lib.Yudhishthira.Types.DRIVER_CONFIG Lib.Yudhishthira.Types.RideRelatedNotificationConfig -> do
      let defaultType = Prelude.listToMaybe $ YTH.genDef (Proxy @DTRN.RideRelatedNotificationConfig)
      logicData :: Maybe DTRN.RideRelatedNotificationConfig <- YudhishthiraFlow.createLogicData defaultType (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy DTRN.RideRelatedNotificationConfig) transporterConfig.referralLinkPassword req logicData
    -- Lib.Yudhishthira.Types.DRIVER_CONFIG Lib.Yudhishthira.Types.MerchantMessage -> do
    --   let defaultType = Prelude.listToMaybe $ YTH.genDef (Proxy @DTM.MerchantMessage)
    --   logicData :: Maybe DTM.MerchantMessage <- YudhishthiraFlow.createLogicData defaultType (Prelude.listToMaybe req.inputData)
    --   YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy DTM.MerchantMessage) transporterConfig.referralLinkPassword req logicData
    Lib.Yudhishthira.Types.DRIVER_CONFIG Lib.Yudhishthira.Types.MerchantPushNotification -> do
      let defaultType = Prelude.listToMaybe $ YTH.genDef (Proxy @DTPN.MerchantPushNotification)
      logicData :: Maybe DTPN.MerchantPushNotification <- YudhishthiraFlow.createLogicData defaultType (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy DTPN.MerchantPushNotification) transporterConfig.referralLinkPassword req logicData
    _ -> throwError $ InvalidRequest "Logic Domain not supported"

getNammaTagAppDynamicLogic :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Maybe Int -> Lib.Yudhishthira.Types.LogicDomain -> Environment.Flow [Lib.Yudhishthira.Types.GetLogicsResp]
getNammaTagAppDynamicLogic _ _ = YudhishthiraFlow.getAppDynamicLogicForDomain

postNammaTagRunJob ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Lib.Yudhishthira.Types.RunKaalChakraJobReq ->
  Environment.Flow Lib.Yudhishthira.Types.RunKaalChakraJobRes
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
    Lib.Yudhishthira.Types.RUN -> YudhishthiraFlow.postRunKaalChakraJob kaalChakraHandle req
    Lib.Yudhishthira.Types.SCHEDULE scheduledTime -> do
      now <- getCurrentTime
      when (scheduledTime <= now) $
        throwError (InvalidRequest "Schedule job available only for future")
      case req.usersSet of
        Lib.Yudhishthira.Types.ALL_USERS -> pure ()
        _ -> throwError (InvalidRequest "Schedule job available only for all users")
      let jobData = Lib.Yudhishthira.Types.mkKaalChakraJobData req (Just scheduledTime)
      kaalChakraHandle.createFetchUserDataJob req.chakra jobData scheduledTime

      logInfo $ "Scheduled new " <> show req.chakra <> " job"
      pure $ Lib.Yudhishthira.Types.RunKaalChakraJobRes {eventId = Nothing, tags = Nothing, users = Nothing, chakraBatchState = Lib.Yudhishthira.Types.Completed}
  where
    castChakra :: AllocatorJobType -> Maybe Lib.Yudhishthira.Types.Chakra
    castChakra Daily = Just Lib.Yudhishthira.Types.Daily
    castChakra Weekly = Just Lib.Yudhishthira.Types.Weekly
    castChakra Monthly = Just Lib.Yudhishthira.Types.Monthly
    castChakra Quarterly = Just Lib.Yudhishthira.Types.Quarterly
    castChakra _ = Nothing

getNammaTagTimeBounds :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.LogicDomain -> Environment.Flow Lib.Yudhishthira.Types.TimeBoundResp
getNammaTagTimeBounds merchantShortId opCity domain = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.getTimeBounds (cast merchantOpCityId) domain

postNammaTagTimeBoundsCreate :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.CreateTimeBoundRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagTimeBoundsCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.createTimeBounds (cast merchantOpCityId) req

deleteNammaTagTimeBoundsDelete :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.LogicDomain -> Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteNammaTagTimeBoundsDelete merchantShortId opCity domain name = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.deleteTimeBounds (cast merchantOpCityId) domain name

getNammaTagAppDynamicLogicGetLogicRollout :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Maybe Text -> Lib.Yudhishthira.Types.LogicDomain -> Environment.Flow [Lib.Yudhishthira.Types.LogicRolloutObject]
getNammaTagAppDynamicLogicGetLogicRollout merchantShortId opCity timeBound domain = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.getLogicRollout (cast merchantOpCityId) timeBound domain

postNammaTagAppDynamicLogicUpsertLogicRollout :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> [Lib.Yudhishthira.Types.LogicRolloutObject] -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagAppDynamicLogicUpsertLogicRollout merchantShortId opCity rolloutReq = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.upsertLogicRollout (Just $ cast merchant.id) (cast merchantOpCityId) rolloutReq

getNammaTagAppDynamicLogicVersions :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Maybe Prelude.Int -> Prelude.Maybe Prelude.Int -> Lib.Yudhishthira.Types.LogicDomain -> Environment.Flow Lib.Yudhishthira.Types.AppDynamicLogicVersionResp
getNammaTagAppDynamicLogicVersions _merchantShortId _opCity = YudhishthiraFlow.getAppDynamicLogicVersions

getNammaTagAppDynamicLogicDomains :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow Lib.Yudhishthira.Types.AppDynamicLogicDomainResp
getNammaTagAppDynamicLogicDomains _merchantShortId _opCity = return $ Lib.Yudhishthira.Types.allValues

getNammaTagQueryAll :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.Chakra -> Environment.Flow Lib.Yudhishthira.Types.ChakraQueryResp
getNammaTagQueryAll _merchantShortId _opCity = YudhishthiraFlow.getNammaTagQueryAll

getNammaTagConfigPilotAllConfigs :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Maybe Prelude.Bool -> Environment.Flow [Lib.Yudhishthira.Types.ConfigType]
getNammaTagConfigPilotAllConfigs _merchantShortId _opCity mbUnderExp = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  YudhishthiraFlow.getNammaTagConfigPilotAllConfigsProvider (cast merchantOpCityId) mbUnderExp

getNammaTagConfigPilotConfigDetails :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.ConfigType -> Environment.Flow [Lib.Yudhishthira.Types.ConfigDetailsResp]
getNammaTagConfigPilotConfigDetails _merchantShortId _opCity configType = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  YudhishthiraFlow.getNammaTagConfigPilotConfigDetailsProvider (cast merchantOpCityId) configType

getNammaTagConfigPilotGetTableData :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.ConfigType -> Environment.Flow Lib.Yudhishthira.Types.TableDataResp
getNammaTagConfigPilotGetTableData _merchantShortId _opCity configType = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  logicElements <- CADLE.findByDomainAndVersion (Lib.Yudhishthira.Types.DRIVER_CONFIG configType) 1
  let logics = map (.logic) logicElements
  returnConfigs configType logics merchantOpCityId
  where
    returnConfigs :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Lib.Yudhishthira.Types.ConfigType -> [Value] -> Id MerchantOperatingCity -> m Lib.Yudhishthira.Types.TableDataResp
    returnConfigs cfgType logics merchantOpCityId = do
      -- TODO use defaults

      -- allConfigs <- defaultDbCall cfgType merchantOpCityId
      -- let wrapppedConfigs = map (\cfg -> Lib.Yudhishthira.Types.Config cfg Nothing) allConfigs
      -- finalConfigs <- mapM (LYTU.runLogics logics) wrapppedConfigs
      -- return Lib.Yudhishthira.Types.TableDataResp {configs = map (.result) finalConfigs}
      case cfgType of
        Lib.Yudhishthira.Types.DriverPoolConfig -> do
          driverPoolCfg <- SCMD.findAllByMerchantOpCityId Nothing Nothing merchantOpCityId
          -- driverPoolCfg<-defaultDbCall Lib.Yudhishthira.Types.DriverPoolConfig merchantOpCityId
          let configWrapper :: [Lib.Yudhishthira.Types.Config DTD.DriverPoolConfig] =
                zipWith
                  (\id cfg -> cfg {Lib.Yudhishthira.Types.identifier = id})
                  [0 ..]
                  (map (\cfg -> Lib.Yudhishthira.Types.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) driverPoolCfg)
          patchedConfigs <- mapM (LYTU.runLogics logics) configWrapper
          return Lib.Yudhishthira.Types.TableDataResp {configs = map (.result) patchedConfigs}
        Lib.Yudhishthira.Types.TransporterConfig -> do
          transporterCfg <- SCMT.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
          let configWrapper :: [Lib.Yudhishthira.Types.Config DTT.TransporterConfig] =
                zipWith
                  (\id cfg -> cfg {Lib.Yudhishthira.Types.identifier = id})
                  [0 ..]
                  [(\cfg -> Lib.Yudhishthira.Types.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) transporterCfg]
          patchedConfigs <- mapM (LYTU.runLogics logics) configWrapper
          return Lib.Yudhishthira.Types.TableDataResp {configs = map (.result) patchedConfigs}
        _ -> throwError $ InvalidRequest "Unsupported config type."

postNammaTagConfigPilotActionChange :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.ActionChangeRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagConfigPilotActionChange _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  case req of
    Lib.Yudhishthira.Types.Conclude concludeReq -> do
      expRollout <- LYSQADLR.findByPrimaryKey concludeReq.domain (cast merchantOpCityId) "Unbounded" concludeReq.version >>= fromMaybeM (InvalidRequest $ "Rollout not found for Domain: " <> show concludeReq.domain <> " City: " <> show merchantOpCityId <> " TimeBounds: " <> "Unbounded" <> " Version: " <> show concludeReq.version)
      logDebug $ show expRollout
      mbBaseRollout <- LYSQADLR.findByCityAndDomainAndIsBase (cast merchantOpCityId) concludeReq.domain
      case mbBaseRollout of
        Just baseRollout -> do
          when (concludeReq.version == baseRollout.version) $ throwError $ InvalidRequest "Cannot conclude the base rollout"
          baseElements <- CADLE.findByDomainAndVersion concludeReq.domain baseRollout.version
          let baseLogics = fmap (.logic) baseElements
          case concludeReq.domain of
            Lib.Yudhishthira.Types.DRIVER_CONFIG Lib.Yudhishthira.Types.DriverPoolConfig -> do
              driverPoolCfg <- SCMD.findAllByMerchantOpCityId Nothing Nothing merchantOpCityId
              let configWrapper :: [Lib.Yudhishthira.Types.Config DTD.DriverPoolConfig] =
                    zipWith
                      (\id cfg -> cfg {Lib.Yudhishthira.Types.identifier = id})
                      [0 ..]
                      (map (\cfg -> Lib.Yudhishthira.Types.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) driverPoolCfg)
              patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
              mbCfgs :: [Maybe (Lib.Yudhishthira.Types.Config DTD.DriverPoolConfig)] <-
                mapM
                  ( \resp ->
                      case (A.fromJSON resp.result :: A.Result (Lib.Yudhishthira.Types.Config DTD.DriverPoolConfig)) of
                        A.Success dpc -> pure $ Just dpc
                        A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to driver pool config. " <> show e
                  )
                  patchedConfigs
              when (any isNothing mbCfgs) $ throwError $ InvalidRequest "Error occurred: One or more configurations are missing in the patched configs."
              let sortedMbCfgs :: [Maybe (Lib.Yudhishthira.Types.Config DTD.DriverPoolConfig)] = sortOn (Lib.Yudhishthira.Types.identifier . Prelude.fromJust) mbCfgs
                  configsToUpdate :: [DTD.DriverPoolConfig] =
                    catMaybes $
                      zipWith
                        ( \cfg mbCfg ->
                            case mbCfg of
                              Just cfg' | cfg.identifier == cfg'.identifier && cfg /= cfg' -> Just cfg'.config
                              _ -> Nothing
                        )
                        configWrapper
                        sortedMbCfgs
              mapM_ SCMD.updateByPrimaryKey configsToUpdate
            Lib.Yudhishthira.Types.DRIVER_CONFIG Lib.Yudhishthira.Types.TransporterConfig -> do
              transporterCfg <- SCMT.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
              let configWrapper :: [Lib.Yudhishthira.Types.Config DTT.TransporterConfig] =
                    zipWith
                      (\id cfg -> cfg {Lib.Yudhishthira.Types.identifier = id})
                      [0 ..]
                      [(\cfg -> Lib.Yudhishthira.Types.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) transporterCfg]
              patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
              mbCfgs :: [Maybe (Lib.Yudhishthira.Types.Config DTT.TransporterConfig)] <-
                mapM
                  ( \resp ->
                      case (A.fromJSON resp.result :: A.Result (Lib.Yudhishthira.Types.Config DTT.TransporterConfig)) of
                        A.Success tc -> pure $ Just tc
                        A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to transporter config. " <> show e
                  )
                  patchedConfigs
              let configsToUpdate :: [DTT.TransporterConfig] =
                    catMaybes $
                      zipWith
                        ( \cfg mbCfg ->
                            case mbCfg of
                              Just cfg' | cfg /= cfg' -> Just cfg'.config
                              _ -> Nothing
                        )
                        configWrapper
                        mbCfgs
              mapM_ SCMT.update configsToUpdate
            _ -> throwError $ InvalidRequest $ "Logic Domain not supported" <> show concludeReq.domain
          let originalBasePercentage = baseRollout.percentageRollout
              updatedBaseRollout =
                baseRollout
                  { LYTADLR.isBaseVersion = Nothing,
                    LYTADLR.experimentStatus = Just Lib.Yudhishthira.Types.CONCLUDED,
                    LYTADLR.percentageRollout = 0
                  }
          LYSQADLR.updateByPrimaryKey updatedBaseRollout
          YudhishthiraFlow.postNammaTagConfigPilotActionChange (Just $ cast merchant.id) (cast merchantOpCityId) req (Just originalBasePercentage)
        _ -> YudhishthiraFlow.postNammaTagConfigPilotActionChange (Just $ cast merchant.id) (cast merchantOpCityId) req Nothing
    _ ->
      YudhishthiraFlow.postNammaTagConfigPilotActionChange (Just $ cast merchant.id) (cast merchantOpCityId) req Nothing
