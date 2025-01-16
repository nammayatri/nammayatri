{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.FlaggedCategory where

import API.Types.UI.FlaggedCategory
import qualified API.Types.UI.Notification as Notification
import qualified "dashboard-helper-api" Dashboard.SafetyPlatform as Safety
import qualified Domain.Types.FlaggedCategory
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import qualified Storage.Queries.FlaggedCategory as SQFC
import qualified Storage.Queries.FlaggedCategoryExtra as SQFCE
import "lib-dashboard" Tools.Auth
import Tools.Error

buildTransaction ::
  ( MonadFlow m
  ) =>
  Safety.SafetyEndpoint ->
  TokenInfo ->
  Text ->
  m DT.Transaction
buildTransaction endpoint tokenInfo = T.buildTransactionForSafetyDashboard (DT.SafetyAPI endpoint) (Just tokenInfo)

postAddFlagCategory :: TokenInfo -> API.Types.UI.FlaggedCategory.AddFlagCategoryReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postAddFlagCategory tokenInfo req = do
  transaction <- buildTransaction Safety.AddFlagCategoryEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    isAlreadyPresent <- SQFC.findByName req.name
    when (isJust isAlreadyPresent) $ throwError FlagCategoryAlreadyExists
    flagCategory <- buildFlagCategory req
    SQFC.create flagCategory
    pure Kernel.Types.APISuccess.Success

postDeleteFlagCategory :: TokenInfo -> API.Types.UI.FlaggedCategory.DeleteFlagCategoryReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postDeleteFlagCategory tokenInfo req = do
  transaction <- buildTransaction Safety.DeleteFlagCategoryEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    SQFC.deleteById (Kernel.Types.Id.Id $ req.id)
    pure Kernel.Types.APISuccess.Success

getListFlagCategory :: TokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.UI.FlaggedCategory.FlagCategoryList
getListFlagCategory _ mbLimit mbOffset = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  list <- SQFCE.findAll limit offset
  total <- SQFCE.countAll
  let count = length list
      summary = Notification.Summary {totalCount = total, count = count}
  return $ FlagCategoryList {flagCategoryList = list, summary = summary}

buildFlagCategory :: API.Types.UI.FlaggedCategory.AddFlagCategoryReq -> Environment.Flow Domain.Types.FlaggedCategory.FlaggedCategory
buildFlagCategory req = do
  uid <- generateGUID
  now <- getCurrentTime
  return
    Domain.Types.FlaggedCategory.FlaggedCategory
      { id = uid,
        name = req.name,
        createdAt = now,
        updatedAt = now
      }
