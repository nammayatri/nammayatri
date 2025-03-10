{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.OperationHub (getOperationGetAllHubs, postOperationCreateRequest) where

import API.Types.UI.OperationHub
import Data.OpenApi (ToSchema)
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.OperationHub
import Domain.Types.OperationHubRequests
import Domain.Types.Person
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Queries.OperationHub
import qualified Storage.Queries.OperationHubRequests as SQOH
import Tools.Auth

getOperationGetAllHubs :: (Maybe (Id Person), Id Merchant, Id MerchantOperatingCity) -> Flow [OperationHub]
getOperationGetAllHubs (_, _, opCityId) = findAllByCityId opCityId

postOperationCreateRequest :: (Maybe (Id Person), Id Merchant, Id MerchantOperatingCity) -> DriverOperationHubRequest -> Flow APISuccess
postOperationCreateRequest (mbPersonId, merchantId, merchantOperatingCityId) req = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  Redis.whenWithLockRedis (opsHubDriverLockKey driverId.getId) 60 $ do
    id <- generateGUID
    now <- getCurrentTime
    opsHubReq <- SQOH.findByDriverStatusAndType driverId PENDING req.requestType
    unless (isNothing opsHubReq) $ Kernel.Utils.Common.throwError (InvalidRequest "Duplicate Request")
    let operationHubReq =
          OperationHubRequests
            { operationHubId = Id req.operationHubId, -- shall we validate?
              requestType = req.requestType,
              requestStatus = PENDING,
              createdAt = now,
              updatedAt = now,
              fulfilledAt = Nothing,
              operatorId = Nothing,
              remarks = Nothing,
              ..
            }
    void $ SQOH.create operationHubReq
  pure Success

opsHubDriverLockKey :: Text -> Text
opsHubDriverLockKey reqId = "opsHub:driver:Id-" <> reqId
