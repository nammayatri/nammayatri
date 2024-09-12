{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.Management.NammaTag
  ( postNammaTagTagCreate,
    postNammaTagQueryCreate,
    postNammaTagAppDynamicLogicVerify,
    getNammaTagAppDynamicLogic,
  )
where

import qualified Data.Aeson as A
import Data.Default.Class (Default (..))
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Environment
import EulerHS.Prelude hiding (id)
import JsonLogic
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Flow.Dashboard as YudhishthiraFlow
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogic as CADL
import qualified Lib.Yudhishthira.Types
import Lib.Yudhishthira.Types.AppDynamicLogic
import qualified Lib.Yudhishthira.Types.ChakraQueries
import Servant hiding (throwError)
import SharedLogic.DriverPool.Types
import SharedLogic.Merchant
import Storage.Beam.Yudhishthira ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Auth

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.CreateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagCreate _merchantShortId _opCity req = YudhishthiraFlow.postTagCreate req

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
        driversData <- mapM createPoolingLogicData req.inputData
        let logicData = TaggedDriverPoolInput driversData False
        YudhishthiraFlow.verifyDynamicLogic req.rules logicData
      _ -> throwError $ InvalidRequest "Logic Domain not supported"
  isRuleUpdated <-
    if fromMaybe False req.shouldUpdateRule
      then do
        if null resp.errors
          then do
            verifyPassword req.updatePassword transporterConfig.referralLinkPassword -- Using referralLinkPassword as updatePassword, could be changed to a new field in future
            updateDynamicLogic merchantOpCityId req.rules req.domain req.timeBounds
          else throwError $ InvalidRequest "Errors found in the rules"
      else return False
  return $ Lib.Yudhishthira.Types.AppDynamicLogicResp resp.result isRuleUpdated resp.errors
  where
    createPoolingLogicData :: A.Value -> Environment.Flow DriverPoolWithActualDistResult
    createPoolingLogicData inputValue = do
      let defaultValue = A.toJSON (def :: DriverPoolWithActualDistResult)
          finalValue = deepMerge defaultValue inputValue
      case A.fromJSON finalValue :: A.Result DriverPoolWithActualDistResult of
        A.Success a -> pure a
        A.Error err -> throwError $ InvalidRequest ("Not able to merge input data into default value. Getting error: " <> show err)

    verifyPassword :: Maybe Text -> Text -> Environment.Flow ()
    verifyPassword Nothing _ = throwError $ InvalidRequest "Password not provided"
    verifyPassword (Just updatePassword) referralLinkPassword =
      unless (updatePassword == referralLinkPassword) $ throwError $ InvalidRequest "Password does not match"

    updateDynamicLogic :: Kernel.Types.Id.Id MerchantOperatingCity -> [A.Value] -> Lib.Yudhishthira.Types.LogicDomain -> Maybe TimeBound -> Environment.Flow Bool
    updateDynamicLogic merchantOpCityId rules domain mbTimeBounds = do
      let timeBounds = fromMaybe Unbounded mbTimeBounds
      now <- getCurrentTime
      let appDynamicLogics = (zip rules [0 ..]) <&> (\(rule, order) -> mkAppDynamicLogic timeBounds rule order now)
      CADL.delete (cast merchantOpCityId) domain timeBounds
      CADL.createMany appDynamicLogics
      CADL.clearCache (cast merchantOpCityId) domain
      return True
      where
        mkAppDynamicLogic :: TimeBound -> A.Value -> Int -> UTCTime -> AppDynamicLogic
        mkAppDynamicLogic timeBounds logic order now =
          AppDynamicLogic
            { description = "Rule for " <> show domain <> " order " <> show order,
              merchantOperatingCityId = (cast merchantOpCityId),
              name = "Rule" <> show order,
              createdAt = now,
              updatedAt = now,
              ..
            }

getNammaTagAppDynamicLogic :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.LogicDomain -> Environment.Flow [Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic]
getNammaTagAppDynamicLogic merchantShortId opCity domain = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  CADL.findByMerchantOpCityAndDomain (cast merchantOpCityId) domain
