{-# OPTIONS_GHC -Wno-deprecations #-}

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
  )
where

import qualified Data.Aeson as A
import Data.Default.Class (Default (..))
import Data.Singletons
import qualified Domain.Types.Merchant
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
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.Common as C
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.DriverPool.Config (Config (..))
import SharedLogic.DriverPool.Types
import SharedLogic.DynamicPricing
import qualified SharedLogic.KaalChakra.Chakras as Chakras
import SharedLogic.Merchant
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

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
      let jobData = Lib.Yudhishthira.Types.mkKaalChakraJobData req
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
