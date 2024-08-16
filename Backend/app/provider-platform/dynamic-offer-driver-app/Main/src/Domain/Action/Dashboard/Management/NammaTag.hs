{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.Management.NammaTag
  ( postNammaTagTagCreate,
    postNammaTagQueryCreate,
    postNammaTagAppDynamicLogicVerify,
  )
where

import qualified Data.Aeson as A
import Data.Default.Class (Default (..))
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import JsonLogic
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Flow.Dashboard as YudhishthiraFlow
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.ChakraQueries
import Servant hiding (throwError)
import SharedLogic.DriverPool.Types
import Storage.Beam.Yudhishthira ()
import Tools.Auth

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.CreateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagCreate _merchantShortId _opCity req = YudhishthiraFlow.postTagCreate req

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate _merchantShortId _opCity req = YudhishthiraFlow.postQueryCreate req

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.AppDynamicLogicReq -> Environment.Flow Lib.Yudhishthira.Types.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify _merchantShortId _opCity req = do
  case req.domain of
    Lib.Yudhishthira.Types.POOLING -> do
      driversData <- mapM createPoolingLogicData req.inputData
      let logicData = TaggedDriverPoolInput driversData False
      YudhishthiraFlow.verifyDynamicLogic req.rules logicData
    _ -> throwError $ InvalidRequest "Logic Domain not supported"
  where
    createPoolingLogicData :: A.Value -> Environment.Flow DriverPoolWithActualDistResult
    createPoolingLogicData inputValue = do
      let defaultValue = A.toJSON (def :: DriverPoolWithActualDistResult)
          finalValue = deepMerge defaultValue inputValue
      case A.fromJSON finalValue :: A.Result DriverPoolWithActualDistResult of
        A.Success a -> pure a
        A.Error err -> throwError $ InvalidRequest ("Not able to merge input data into default value. Getting error: " <> show err)
