{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.Management.NammaTag
  ( postNammaTagTagCreate,
    postNammaTagTagUpdate,
    deleteNammaTagTagDelete,
    postNammaTagQueryCreate,
    postNammaTagAppDynamicLogicVerify,
    getNammaTagAppDynamicLogic,
    postNammaTagRunJob,
    Handle.kaalChakraHandle,
    postNammaTagTimeBoundsCreate,
    deleteNammaTagTimeBoundsDelete,
    getNammaTagAppDynamicLogicGetLogicRollout,
    postNammaTagAppDynamicLogicUpsertLogicRollout,
    getNammaTagTimeBounds,
  )
where

import qualified Data.Aeson as A
import Data.Default.Class (Default (..))
import Data.List (nub)
import qualified Data.List.NonEmpty as DLNE
import Data.OpenApi (ToSchema)
import Data.Singletons
import qualified Domain.Action.Dashboard.Management.NammaTag.Handle as Handle
import qualified Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DPerson
import Domain.Types.TimeBoundConfig
import qualified Environment
import EulerHS.Prelude hiding (id)
import JsonLogic
import qualified Kernel.Prelude as Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import qualified Lib.Scheduler.JobStorageType.DB.Queries as QDBJ
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import Lib.Scheduler.Types (AnyJob (..), Job (..))
import qualified Lib.Yudhishthira.Event.KaalChakra as KaalChakra
import qualified Lib.Yudhishthira.Flow.Dashboard as YudhishthiraFlow
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicElement as CADLE
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as CADLR
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElement as QADLE
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicElement as DTADLE
import Lib.Yudhishthira.Types.AppDynamicLogicRollout
import qualified Lib.Yudhishthira.Types.ChakraQueries
import Servant hiding (throwError)
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.DriverPool.Config (Config (..))
import SharedLogic.DriverPool.Types
import SharedLogic.DynamicPricing
import SharedLogic.Merchant
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.TimeBoundConfig as CQTBC
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.TimeBoundConfig as QTBC
import Tools.Auth

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.CreateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagCreate _merchantShortId _opCity req = YudhishthiraFlow.postTagCreate req

postNammaTagTagUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.UpdateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagUpdate _merchantShortId _opCity req = YudhishthiraFlow.postTagUpdate req

deleteNammaTagTagDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTagDelete _merchantShortId _opCity tagName = YudhishthiraFlow.deleteTag tagName

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate _merchantShortId _opCity req = YudhishthiraFlow.postQueryCreate req

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.AppDynamicLogicReq -> Environment.Flow Lib.Yudhishthira.Types.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  resp <-
    case req.domain of
      Lib.Yudhishthira.Types.POOLING -> do
        driversData :: [DriverPoolWithActualDistResult] <- mapM (createLogicData def . Just) req.inputData
        let logicData = TaggedDriverPoolInput driversData False
        YudhishthiraFlow.verifyDynamicLogic req.rules logicData
      Lib.Yudhishthira.Types.DYNAMIC_PRICING_UNIFIED -> do
        logicData :: DynamicPricingData <- createLogicData def (Prelude.listToMaybe req.inputData)
        YudhishthiraFlow.verifyDynamicLogic req.rules logicData
      Lib.Yudhishthira.Types.CONFIG Lib.Yudhishthira.Types.DriverPoolConfig -> do
        logicData :: Config <- createLogicData def (Prelude.listToMaybe req.inputData)
        YudhishthiraFlow.verifyDynamicLogic req.rules logicData
      _ -> throwError $ InvalidRequest "Logic Domain not supported"
  (isRuleUpdated, version) <-
    if fromMaybe False req.shouldUpdateRule
      then do
        if null resp.errors
          then do
            verifyPassword req.updatePassword transporterConfig.referralLinkPassword -- Using referralLinkPassword as updatePassword, could be changed to a new field in future
            updateDynamicLogic merchantOpCityId req.rules req.domain
          else throwError $ InvalidRequest $ "Errors found in the rules" <> show resp.errors
      else return (False, Nothing)
  return $ Lib.Yudhishthira.Types.AppDynamicLogicResp resp.result isRuleUpdated req.domain version resp.errors
  where
    createLogicData :: (FromJSON a, ToJSON a) => a -> Maybe A.Value -> Environment.Flow a
    createLogicData defaultVal Nothing = return defaultVal
    createLogicData defaultVal (Just inputValue) = do
      let defaultValue = A.toJSON defaultVal
          finalValue = deepMerge defaultValue inputValue
      case A.fromJSON finalValue of
        A.Success a -> return a
        A.Error err -> throwError $ InvalidRequest ("Not able to merge input data into default value. Getting error: " <> show err)

    verifyPassword :: Maybe Text -> Text -> Environment.Flow ()
    verifyPassword Nothing _ = throwError $ InvalidRequest "Password not provided"
    verifyPassword (Just updatePassword) referralLinkPassword =
      unless (updatePassword == referralLinkPassword) $ throwError $ InvalidRequest "Password does not match"

    updateDynamicLogic :: Kernel.Types.Id.Id MerchantOperatingCity -> [A.Value] -> Lib.Yudhishthira.Types.LogicDomain -> Environment.Flow (Bool, Maybe Int)
    updateDynamicLogic _ rules domain = do
      now <- getCurrentTime
      latestElement <- QADLE.findLatestVersion (Just 1) Nothing domain
      let version = maybe 1 ((+ 1) . (.version)) (Prelude.listToMaybe latestElement)
      let appDynamicLogics = zip rules [0 ..] <&> (\(rule, order) -> mkAppDynamicLogicElement version rule order now)
      CADLE.createMany appDynamicLogics
      CADLE.clearCache domain
      return (True, Just version)
      where
        mkAppDynamicLogicElement :: Int -> A.Value -> Int -> UTCTime -> DTADLE.AppDynamicLogicElement
        mkAppDynamicLogicElement version logic order now =
          DTADLE.AppDynamicLogicElement
            { createdAt = now,
              updatedAt = now,
              description = req.description,
              ..
            }

getNammaTagAppDynamicLogic :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Maybe Int -> Lib.Yudhishthira.Types.LogicDomain -> Environment.Flow [Lib.Yudhishthira.Types.GetLogicsResp]
getNammaTagAppDynamicLogic _merchantShortId _opCity mbVersion domain = do
  case mbVersion of
    Just version -> do
      logicsObject <- CADLE.findByDomainAndVersion domain version
      let logics = map (.logic) logicsObject
      let description = (Prelude.listToMaybe logicsObject) >>= (.description)
      return $ [Lib.Yudhishthira.Types.GetLogicsResp domain version description logics]
    Nothing -> do
      allDomainLogics <- CADLE.findByDomain domain
      let sortedLogics = sortByOrderAndVersion allDomainLogics
      let groupedLogics = groupBy ((==) `on` (.version)) sortedLogics
      let versionWithLogics = mapMaybe combineLogic groupedLogics
      return $ (versionWithLogics <&> (\(version, description, logics) -> Lib.Yudhishthira.Types.GetLogicsResp domain version description logics))
  where
    sortByOrderAndVersion :: [DTADLE.AppDynamicLogicElement] -> [DTADLE.AppDynamicLogicElement]
    sortByOrderAndVersion = sortBy compareData
      where
        compareData a b =
          case compare b.version a.version of -- Descending for version
            EQ -> compare a.order b.order -- Ascending for order
            cmp -> cmp

    combineLogic :: NonEmpty DTADLE.AppDynamicLogicElement -> Maybe (Int, Maybe Text, [A.Value])
    combineLogic arr =
      let logics = map (.logic) arr -- Combine all logic values into a list
          firstElement = DLNE.head arr
       in Just (firstElement.version, firstElement.description, DLNE.toList logics)

postNammaTagRunJob ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Lib.Yudhishthira.Types.RunKaalChakraJobReq ->
  Environment.Flow Lib.Yudhishthira.Types.RunKaalChakraJobRes
postNammaTagRunJob _merchantShortId _opCity req = do
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

  case req.action of
    Lib.Yudhishthira.Types.RUN -> YudhishthiraFlow.postRunKaalChakraJob Handle.kaalChakraHandle req
    Lib.Yudhishthira.Types.SCHEDULE scheduledTime -> do
      now <- getCurrentTime
      when (scheduledTime <= now) $
        throwError (InvalidRequest "Schedule job available only for future")
      case req.usersSet of
        Lib.Yudhishthira.Types.ALL_USERS -> pure ()
        _ -> throwError (InvalidRequest "Schedule job available only for all users")
      let jobData = Lib.Yudhishthira.Types.mkKaalChakraJobData req
      maxShards <- asks (.maxShards)
      case req.chakra of
        Lib.Yudhishthira.Types.Daily -> QAllJ.createJobByTime @_ @'Daily scheduledTime maxShards jobData
        Lib.Yudhishthira.Types.Weekly -> QAllJ.createJobByTime @_ @'Weekly scheduledTime maxShards jobData
        Lib.Yudhishthira.Types.Monthly -> QAllJ.createJobByTime @_ @'Monthly scheduledTime maxShards jobData
        Lib.Yudhishthira.Types.Quarterly -> QAllJ.createJobByTime @_ @'Quarterly scheduledTime maxShards jobData

      logInfo $ "Scheduled new " <> show req.chakra <> " job"
      pure $ Lib.Yudhishthira.Types.RunKaalChakraJobRes {eventId = Nothing, tags = Nothing, users = Nothing, chakraBatchState = Lib.Yudhishthira.Types.Completed}

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
  allTimeBounds <- QTBC.findByCityAndDomain merchantOpCityId domain
  return $ (\TimeBoundConfig {..} -> Lib.Yudhishthira.Types.CreateTimeBoundRequest {..}) <$> allTimeBounds

postNammaTagTimeBoundsCreate :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.CreateTimeBoundRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagTimeBoundsCreate merchantShortId opCity req = do
  when (req.timeBounds == Unbounded) $ throwError $ InvalidRequest "Unbounded time bounds not allowed"
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  allTimeBounds <- QTBC.findByCityAndDomain merchantOpCityId req.timeBoundDomain
  forM_ allTimeBounds $ \existingTimeBound -> do
    when (timeBoundsOverlap existingTimeBound.timeBounds req.timeBounds) $ do
      throwError (InvalidRequest $ "Time bounds overlap with existing time bound: " <> existingTimeBound.name)
  now <- getCurrentTime
  let timeBound = mkTimeBound merchantOpCityId now req
  QTBC.create timeBound
  CQTBC.clearCache merchantOpCityId timeBound.timeBoundDomain timeBound.name
  return Kernel.Types.APISuccess.Success
  where
    mkTimeBound :: Id MerchantOperatingCity -> UTCTime -> Lib.Yudhishthira.Types.CreateTimeBoundRequest -> TimeBoundConfig
    mkTimeBound merchantOperatingCityId now Lib.Yudhishthira.Types.CreateTimeBoundRequest {..} =
      TimeBoundConfig
        { createdAt = now,
          updatedAt = now,
          ..
        }

deleteNammaTagTimeBoundsDelete :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.LogicDomain -> Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteNammaTagTimeBoundsDelete merchantShortId opCity domain name = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  CQTBC.delete merchantOpCityId domain name
  CQTBC.clearCache merchantOpCityId domain name
  return Kernel.Types.APISuccess.Success

getNammaTagAppDynamicLogicGetLogicRollout :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Maybe Text -> Lib.Yudhishthira.Types.LogicDomain -> Environment.Flow [Lib.Yudhishthira.Types.LogicRolloutObject]
getNammaTagAppDynamicLogicGetLogicRollout merchantShortId opCity _ domain = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  allDomainRollouts <- CADLR.findByMerchantOpCityAndDomain (cast merchantOpCityId) domain
  let sortedDomainRollouts = sortBy (\a b -> compare b.timeBounds a.timeBounds) allDomainRollouts
  let groupedRollouts = groupBy ((==) `on` (.timeBounds)) sortedDomainRollouts
  return $ mapMaybe combineRollout groupedRollouts
  where
    combineRollout :: NonEmpty AppDynamicLogicRollout -> Maybe Lib.Yudhishthira.Types.LogicRolloutObject
    combineRollout arr =
      let rollout = map (\r -> Lib.Yudhishthira.Types.RolloutVersion r.version r.percentageRollout r.versionDescription) arr
          firstElement = DLNE.head arr
       in Just $ Lib.Yudhishthira.Types.LogicRolloutObject firstElement.domain firstElement.timeBounds (DLNE.toList rollout)

postNammaTagAppDynamicLogicUpsertLogicRollout :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> [Lib.Yudhishthira.Types.LogicRolloutObject] -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagAppDynamicLogicUpsertLogicRollout merchantShortId opCity rolloutReq = do
  unless (checkSameDomainDifferentTimeBounds rolloutReq) $ throwError $ InvalidRequest "Only domain and different time bounds are allowed"
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  domain <- getDomain & fromMaybeM (InvalidRequest "Domain not found")
  now <- getCurrentTime
  rolloutObjectsArr <- mapM (mkAppDynamicLogicRolloutDomain merchantOpCityId now) rolloutReq
  let rolloutObjects = concat rolloutObjectsArr
  CADLR.delete (cast merchantOpCityId) domain
  CADLR.createMany rolloutObjects
  CADLR.clearCache (cast merchantOpCityId) domain
  return Kernel.Types.APISuccess.Success
  where
    getDomain :: Maybe Lib.Yudhishthira.Types.LogicDomain
    getDomain = Prelude.listToMaybe $ map (.domain) rolloutReq

    mkAppDynamicLogicRolloutDomain :: Id MerchantOperatingCity -> UTCTime -> Lib.Yudhishthira.Types.LogicRolloutObject -> Environment.Flow [AppDynamicLogicRollout]
    mkAppDynamicLogicRolloutDomain merchantOperatingCityId now Lib.Yudhishthira.Types.LogicRolloutObject {..} = do
      when (timeBounds /= "Unbounded") $
        void $ CQTBC.findByPrimaryKey merchantOperatingCityId timeBounds domain >>= fromMaybeM (InvalidRequest $ "Time bound not found: " <> timeBounds)
      let rolloutSum = sum $ map (.rolloutPercentage) rollout
      when (rolloutSum /= 100) $ throwError $ InvalidRequest "Sum of rollout percentage should be 100"
      mapM (mkAppDynamicLogicRollout merchantOperatingCityId now domain timeBounds) rollout

    mkAppDynamicLogicRollout :: Id MerchantOperatingCity -> UTCTime -> Lib.Yudhishthira.Types.LogicDomain -> Text -> Lib.Yudhishthira.Types.RolloutVersion -> Environment.Flow AppDynamicLogicRollout
    mkAppDynamicLogicRollout merchantOperatingCityId now domain timeBounds Lib.Yudhishthira.Types.RolloutVersion {..} = do
      logicsObject <- CADLE.findByDomainAndVersion domain version
      let _versionDescription = Prelude.listToMaybe logicsObject >>= (.description)
      when (null logicsObject) $ throwError $ InvalidRequest $ "Logic not found for version: " <> show version
      return $
        AppDynamicLogicRollout
          { createdAt = now,
            updatedAt = now,
            percentageRollout = rolloutPercentage,
            merchantOperatingCityId = cast merchantOperatingCityId,
            versionDescription = versionDescription <|> _versionDescription,
            ..
          }

    checkSameDomainDifferentTimeBounds :: [Lib.Yudhishthira.Types.LogicRolloutObject] -> Bool
    checkSameDomainDifferentTimeBounds [] = True
    checkSameDomainDifferentTimeBounds (x : xs) =
      all (\obj -> obj.domain == x.domain) xs && length timeBoundsList == length (nub timeBoundsList)
      where
        timeBoundsList = map (.timeBounds) (x : xs)
