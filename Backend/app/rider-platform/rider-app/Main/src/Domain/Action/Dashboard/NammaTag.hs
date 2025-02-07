module Domain.Action.Dashboard.NammaTag
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
    getNammaTagAppDynamicLogicVersions,
    getNammaTagAppDynamicLogicDomains,
    getNammaTagQueryAll,
    postNammaTagUpdateCustomerTag,
  )
where

import qualified Dashboard.Common as Common
import Data.Singletons
import qualified Domain.Action.Dashboard.NammaTag.Handle as Handle
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
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
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.JobScheduler (RiderJobType (..))
import qualified SharedLogic.Scheduler.Jobs.Chakras as Chakras
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Person as QPerson
import Tools.Error

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.CreateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagCreate _merchantShortId _opCity req = YudhishthiraFlow.postTagCreate req

postNammaTagTagUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.UpdateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagUpdate _merchantShortId _opCity req = YudhishthiraFlow.postTagUpdate req

deleteNammaTagTagDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTagDelete _merchantShortId _opCity tagName = YudhishthiraFlow.deleteTag tagName

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ChakraQueriesAPIEntity -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate _merchantShortId _opCity req = YudhishthiraFlow.postQueryCreate req

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.AppDynamicLogicReq -> Environment.Flow LYT.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify merchantShortId opCity req = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantOpCityId = merchantOperatingCity.id
  _riderConfig <- QRC.findByMerchantOperatingCityId merchantOpCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
  case req.domain of
    -- LYT.CONFIG LYT.DriverPoolConfig -> do
    --   logicData :: Config <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
    --   YudhishthiraFlow.verifyAndUpdateDynamicLogic riderConfig.dynamicLogicUpdatePassword req logicData
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
    mbOldJob :: Maybe (AnyJob RiderJobType) <- QDBJ.findById oldJobId
    case mbOldJob of
      Nothing -> throwError (InvalidRequest "Job not found")
      Just (AnyJob oldJob) -> do
        let jobType = fromSing $ oldJob.jobInfo.jobType
        unless (castChakra jobType == Just req.chakra) do
          throwError (InvalidRequest "Invalid job type")
        QDBJ.markAsComplete oldJobId

  case req.action of
    LYT.RUN -> YudhishthiraFlow.postRunKaalChakraJob Handle.kaalChakraHandle req
    LYT.SCHEDULE scheduledTime -> do
      now <- getCurrentTime
      when (scheduledTime <= now) $
        throwError (InvalidRequest "Schedule job available only for future")
      case req.usersSet of
        LYT.ALL_USERS -> pure ()
        _ -> throwError (InvalidRequest "Schedule job available only for all users")
      let jobData = LYT.mkKaalChakraJobData req
      Chakras.createFetchUserDataJob mbMerchantId mbMerchantOpCityId req.chakra jobData scheduledTime

      logInfo $ "Scheduled new " <> show req.chakra <> " job"
      pure $ LYT.RunKaalChakraJobRes {eventId = Nothing, tags = Nothing, users = Nothing, chakraBatchState = LYT.Completed}
  where
    castChakra :: RiderJobType -> Maybe LYT.Chakra
    castChakra Daily = Just LYT.Daily
    castChakra Weekly = Just LYT.Weekly
    castChakra Monthly = Just LYT.Monthly
    castChakra Quarterly = Just LYT.Quarterly
    castChakra _ = Nothing

getNammaTagTimeBounds :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.LogicDomain -> Environment.Flow LYT.TimeBoundResp
getNammaTagTimeBounds merchantShortId opCity domain = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantOpCityId = merchantOperatingCity.id
  YudhishthiraFlow.getTimeBounds (cast merchantOpCityId) domain

postNammaTagTimeBoundsCreate :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.CreateTimeBoundRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagTimeBoundsCreate merchantShortId opCity req = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantOpCityId = merchantOperatingCity.id
  YudhishthiraFlow.createTimeBounds (cast merchantOpCityId) req

deleteNammaTagTimeBoundsDelete :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.LogicDomain -> Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteNammaTagTimeBoundsDelete merchantShortId opCity domain name = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantOpCityId = merchantOperatingCity.id
  YudhishthiraFlow.deleteTimeBounds (cast merchantOpCityId) domain name

getNammaTagAppDynamicLogicGetLogicRollout :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Maybe Text -> LYT.LogicDomain -> Environment.Flow [LYT.LogicRolloutObject]
getNammaTagAppDynamicLogicGetLogicRollout merchantShortId opCity timeBound domain = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantOpCityId = merchantOperatingCity.id
  YudhishthiraFlow.getLogicRollout (cast merchantOpCityId) timeBound domain

postNammaTagAppDynamicLogicUpsertLogicRollout :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> [LYT.LogicRolloutObject] -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagAppDynamicLogicUpsertLogicRollout merchantShortId opCity rolloutReq = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  YudhishthiraFlow.upsertLogicRollout (Just $ cast merchantOperatingCity.merchantId) (cast merchantOperatingCity.id) rolloutReq

getNammaTagAppDynamicLogicVersions :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Maybe Prelude.Int -> Prelude.Maybe Prelude.Int -> LYT.LogicDomain -> Environment.Flow LYT.AppDynamicLogicVersionResp
getNammaTagAppDynamicLogicVersions _merchantShortId _opCity = YudhishthiraFlow.getAppDynamicLogicVersions

getNammaTagAppDynamicLogicDomains :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow LYT.AppDynamicLogicDomainResp
getNammaTagAppDynamicLogicDomains _merchantShortId _opCity = return LYT.allValues

getNammaTagQueryAll :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.Chakra -> Environment.Flow LYT.ChakraQueryResp
getNammaTagQueryAll _merchantShortId _opCity = YudhishthiraFlow.getNammaTagQueryAll

postNammaTagUpdateCustomerTag :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Common.User -> LYT.UpdateTagReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagUpdateCustomerTag merchantShortId opCity userId req = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantOpCityId = merchantOperatingCity.id
  let personId = cast @Common.User @DP.Person userId
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  when (req.isAddingTag && maybe False (Yudhishthira.elemTagNameValue req.tag) person.customerNammaTags) $
    logInfo "Tag already exists, update expiry"
  -- merchant access checking
  unless (merchantOpCityId == person.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  mbNammTag <- YudhishthiraFlow.verifyTag req.tag
  now <- getCurrentTime
  let tag =
        if req.isAddingTag
          then do
            let reqCustomerTagWithExpiry = Yudhishthira.addTagExpiry req.tag (mbNammTag >>= (.validity)) now
            Yudhishthira.replaceTagNameValue person.customerNammaTags reqCustomerTagWithExpiry
          else Yudhishthira.removeTagNameValue person.customerNammaTags req.tag
  unless (Just (Yudhishthira.showRawTags tag) == (Yudhishthira.showRawTags <$> person.customerNammaTags)) $
    QPerson.updateCustomerTags (Just tag) personId
  pure Success
