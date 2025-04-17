module Domain.Action.UI.OperationHub (getOperationGetAllHubs, postOperationCreateRequest, getOperationGetRequests) where

import API.Types.UI.OperationHub
import Data.Time (UTCTime (..))
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.OperationHub
import Domain.Types.OperationHubRequests
import Domain.Types.Person
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified Storage.Queries.OperationHub as QOH
import qualified Storage.Queries.OperationHubRequests as QOHR
import qualified Storage.Queries.OperationHubRequestsExtra as QOHRE
import Tools.Error

getOperationGetAllHubs :: (Maybe (Id Person), Id Merchant, Id MerchantOperatingCity) -> Flow [OperationHub]
getOperationGetAllHubs (_, _, opCityId) = QOH.findAllByCityId opCityId

postOperationCreateRequest :: (Maybe (Id Person), Id Merchant, Id MerchantOperatingCity) -> DriverOperationHubRequest -> Flow APISuccess
postOperationCreateRequest (mbPersonId, merchantId, merchantOperatingCityId) req = do
  runRequestValidation validateDriverOperationHubRequest req
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  Redis.whenWithLockRedis (opsHubDriverLockKey driverId.getId) 60 $ do
    id <- generateGUID
    now <- getCurrentTime
    opsHubReq <- QOHR.findByDriverStatusAndType driverId PENDING req.requestType
    unless (isNothing opsHubReq) $ Kernel.Utils.Common.throwError (InvalidRequest "Duplicate Request")
    void $ QOH.findByPrimaryKey req.operationHubId >>= fromMaybeM (OperationHubDoesNotExist req.operationHubId.getId)
    let operationHubReq =
          OperationHubRequests
            { operationHubId = req.operationHubId,
              registrationNo = req.registrationNo,
              requestType = req.requestType,
              requestStatus = PENDING,
              createdAt = now,
              updatedAt = now,
              fulfilledAt = Nothing,
              operatorId = Nothing,
              remarks = Nothing,
              creatorId = Id $ fromMaybe driverId.getId req.creatorId,
              ..
            }
    void $ QOHR.create operationHubReq
  pure Success

getOperationGetRequests ::
  (Maybe (Id Person), Id Merchant, Id MerchantOperatingCity) ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe RequestStatus ->
  Maybe RequestType ->
  Text ->
  Flow OperationHubRequestsResp
getOperationGetRequests (mbPersonId, _, _) mbFrom mbTo mbLimit mbOffset mbStatus mbType rcNo = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  now <- getCurrentTime
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
      defaultFrom = UTCTime (utctDay now) 0
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  requests <- QOHRE.findAllRequestsInRange from to limit offset Nothing mbStatus mbType (Just driverId.getId) Nothing (Just rcNo)
  pure (OperationHubRequestsResp (map fst requests))

opsHubDriverLockKey :: Text -> Text
opsHubDriverLockKey driverId = "opsHub:driver:Id-" <> driverId

validateDriverOperationHubRequest :: Validate DriverOperationHubRequest
validateDriverOperationHubRequest DriverOperationHubRequest {..} =
  sequenceA_
    [ validateField "registrationNo" registrationNo $
        LengthInRange 1 11 `And` star (P.latinUC \/ P.digit)
    ]
