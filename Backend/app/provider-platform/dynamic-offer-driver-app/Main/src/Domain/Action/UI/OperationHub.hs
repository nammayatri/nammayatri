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
import Tools.Error

getOperationGetAllHubs :: (Maybe (Id Person), Id Merchant, Id MerchantOperatingCity) -> Flow [OperationHub]
getOperationGetAllHubs (_, _, opCityId) = QOH.findAllByCityId opCityId

postOperationCreateRequest :: (Maybe (Id Person), Id Merchant, Id MerchantOperatingCity) -> DriverOperationHubRequest -> Flow APISuccess
postOperationCreateRequest (mbPersonId, merchantId, merchantOperatingCityId) req = do
  runRequestValidation validateDriverOperationHubRequest req
  let creatorId = fromMaybe (Id req.creatorId) mbPersonId
  -- Lock and duplicate check by entity (driver or RC), not creator: so same driver/RC cannot have duplicate PENDING from any creator
  lockKey <- case req.requestType of
    DRIVER_ONBOARDING_INSPECTION -> req.driverId & fromMaybeM (InvalidRequest "driverId is required for DRIVER_ONBOARDING_INSPECTION") <&> (\d -> opsHubDriverLockKey d.getId)
    DRIVER_REGULAR_INSPECTION -> req.driverId & fromMaybeM (InvalidRequest "driverId is required for DRIVER_REGULAR_INSPECTION") <&> (\d -> opsHubDriverLockKey d.getId)
    ONBOARDING_INSPECTION -> req.registrationNo & fromMaybeM (InvalidRequest "registrationNo is required for ONBOARDING_INSPECTION") <&> opsHubVehicleLockKey
    REGULAR_INSPECTION -> req.registrationNo & fromMaybeM (InvalidRequest "registrationNo is required for REGULAR_INSPECTION") <&> opsHubVehicleLockKey
  Redis.whenWithLockRedis lockKey 60 $ do
    id <- generateGUID
    now <- getCurrentTime
    -- Duplicate: any PENDING request for this entity (driver or RC), regardless of request type or creator
    isDuplicate <- case req.requestType of
      DRIVER_ONBOARDING_INSPECTION -> maybe (pure False) (\d -> isJust <$> QOHR.findOneByRequestStatusAndDriverId PENDING (Just d)) req.driverId
      DRIVER_REGULAR_INSPECTION -> maybe (pure False) (\d -> isJust <$> QOHR.findOneByRequestStatusAndDriverId PENDING (Just d)) req.driverId
      ONBOARDING_INSPECTION -> maybe (pure False) (\rc -> isJust <$> QOHR.findOneByRequestStatusAndRegistrationNo PENDING (Just rc)) req.registrationNo
      REGULAR_INSPECTION -> maybe (pure False) (\rc -> isJust <$> QOHR.findOneByRequestStatusAndRegistrationNo PENDING (Just rc)) req.registrationNo
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
  Maybe Text ->
  Maybe (Id Person) ->
  Flow OperationHubRequestsResp
getOperationGetRequests (mbPersonId, _, _) mbFrom mbTo mbLimit mbOffset mbStatus mbType mbRcNo mbDriverId = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  now <- getCurrentTime
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
      defaultFrom = UTCTime (utctDay now) 0
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  -- At least one of mbRcNo or mbDriverId must be provided
  unless (isJust mbRcNo || isJust mbDriverId) $
    throwError $ InvalidRequest "Either rcNo or driverId must be provided"
  requests <- QOHR.findAllRequestsInRange from to limit offset Nothing mbStatus mbType (Just driverId.getId) Nothing Nothing mbRcNo mbDriverId
  reqs <- mapM castHubRequests requests
  pure (OperationHubRequestsResp reqs)

opsHubDriverLockKey :: Text -> Text
opsHubDriverLockKey driverId = "opsHub:driver:Id-" <> driverId

opsHubVehicleLockKey :: Text -> Text
opsHubVehicleLockKey rcNo = "opsHub:vehicle:rc-" <> rcNo

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
        operationHubAddress = hub.address,
        operationHubContact = hub.mobileNumber,
        driverPhoneNo,
        driverId = hubReq.driverId,
        registrationNo = hubReq.registrationNo,
        requestStatus = hubReq.requestStatus,
        requestTime = hubReq.createdAt,
        requestType = hubReq.requestType
      }
