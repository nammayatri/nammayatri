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
import Kernel.External.Encryption (decrypt)
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
  -- At least one of registrationNo or driverId must be provided
  unless (isJust req.registrationNo || isJust req.driverId) $
    throwError $ InvalidRequest "Either registrationNo or driverId must be provided"
  let creatorId = fromMaybe (Id req.creatorId) mbPersonId
  Redis.whenWithLockRedis (opsHubDriverLockKey creatorId.getId) 60 $ do
    id <- generateGUID
    now <- getCurrentTime
    -- Check for duplicate requests based on creator, status, type, and either registrationNo or driverId
    opsHubReqs <- QOHR.findByCreatorStatusAndType creatorId PENDING req.requestType
    let isDuplicate = case (req.registrationNo, req.driverId) of
          (Just rcNo, _) -> any (\r -> r.registrationNo == Just rcNo) opsHubReqs
          (_, Just driverId) -> any (\r -> r.driverId == Just driverId) opsHubReqs
          _ -> False
    when isDuplicate $ Kernel.Utils.Common.throwError (InvalidRequest "Duplicate Request")
    void $ QOH.findByPrimaryKey req.operationHubId >>= fromMaybeM (OperationHubDoesNotExist req.operationHubId.getId)
    let operationHubReq =
          OperationHubRequests
            { operationHubId = req.operationHubId,
              registrationNo = req.registrationNo,
              driverId = req.driverId,
              requestType = req.requestType,
              requestStatus = PENDING,
              createdAt = now,
              updatedAt = now,
              fulfilledAt = Nothing,
              operatorId = Nothing,
              remarks = Nothing,
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
  requests <- QOHRE.findAllRequestsInRange from to limit offset Nothing mbStatus mbType (Just driverId.getId) Nothing Nothing (Just rcNo)
  reqs <- mapM castHubRequests requests
  pure (OperationHubRequestsResp reqs)

opsHubDriverLockKey :: Text -> Text
opsHubDriverLockKey driverId = "opsHub:driver:Id-" <> driverId

validateDriverOperationHubRequest :: Validate DriverOperationHubRequest
validateDriverOperationHubRequest DriverOperationHubRequest {..} =
  sequenceA_
    [ -- Validate registrationNo if provided
      whenJust registrationNo $ \rcNo ->
        validateField "registrationNo" rcNo $
          LengthInRange 1 11 `And` star (P.latinUC \/ P.digit)
    ]

castHubRequests :: (OperationHubRequests, Person, OperationHub) -> Flow OperationHubDriverRequest
castHubRequests (hubReq, person, hub) = do
  driverPhoneNo <- mapM decrypt person.mobileNumber
  pure $
    OperationHubDriverRequest
      { id = hubReq.id.getId,
        operationHubId = hubReq.operationHubId,
        operationHubName = hub.name,
        registrationNo = hubReq.registrationNo,
        driverPhoneNo,
        requestStatus = hubReq.requestStatus,
        requestTime = hubReq.createdAt,
        requestType = hubReq.requestType
      }
