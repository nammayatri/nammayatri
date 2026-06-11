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
import qualified Kernel.External.ChallanSearch.Interface.Types as CSIT
import Kernel.External.Encryption (decrypt, getDbHash)
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
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.VehicleRegistrationCertificate as QVRC
import qualified Tools.ChallanSearch as TCS
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
    when isDuplicate $ Kernel.Utils.Common.throwError (InvalidRequest "Inspection request already exists. Please proceed with the inspection.")
    void $ QOH.findByPrimaryKey req.operationHubId >>= fromMaybeM (OperationHubDoesNotExist req.operationHubId.getId)
    let (registrationNo', driverId') =
          case req.requestType of
            DRIVER_ONBOARDING_INSPECTION -> (Nothing, req.driverId)
            DRIVER_REGULAR_INSPECTION -> (Nothing, req.driverId)
            ONBOARDING_INSPECTION -> (req.registrationNo, Nothing)
            REGULAR_INSPECTION -> (req.registrationNo, Nothing)
        operationHubReq =
          OperationHubRequests
            { operationHubId = req.operationHubId,
              registrationNo = registrationNo',
              driverId = driverId',
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
  Maybe Bool ->
  Flow OperationHubRequestsResp
getOperationGetRequests (mbPersonId, merchantId, merchantOpCityId) mbFrom mbTo mbLimit mbOffset mbStatus mbType mbRcNo mbDriverId mbRefreshChallanCount = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  now <- getCurrentTime
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
      to = fromMaybe now mbTo
  from <- case (mbFrom, mbDriverId) of
    (Just f, _) -> pure f
    (Nothing, Just dId) -> do
      person <- QPerson.findById dId >>= fromMaybeM (PersonNotFound dId.getId)
      pure $ UTCTime (utctDay person.createdAt) 0
    (Nothing, Nothing) -> pure $ UTCTime (utctDay now) 0
  -- At least one of mbRcNo or mbDriverId must be provided
  unless (isJust mbRcNo || isJust mbDriverId) $
    throwError $ InvalidRequest "Either rcNo or driverId must be provided"
  -- Fetch pending challan count for the RC if provided
  mbChallanCount <- case mbRcNo of
    Just rcNo -> fetchPendingChallanCount merchantId merchantOpCityId rcNo (mbRefreshChallanCount == Just True)
    Nothing -> pure Nothing
  requests <- QOHR.findAllRequestsInRange from to limit offset Nothing Nothing mbStatus mbType (Just driverId.getId) Nothing Nothing mbRcNo mbDriverId
  reqs <- mapM (castHubRequests mbChallanCount) requests
  pure (OperationHubRequestsResp reqs)

fetchPendingChallanCount :: Id Merchant -> Id MerchantOperatingCity -> Text -> Bool -> Flow (Maybe Int)
fetchPendingChallanCount merchantId merchantOpCityId rcNo forceRefresh = do
  rcHash <- getDbHash rcNo
  mbVrc <- QVRC.findByCertificateNumberHash rcHash
  case mbVrc of
    Nothing -> pure Nothing
    Just vrc -> do
      case (vrc.pendingChallanCount, forceRefresh) of
        (Just count, False) -> pure (Just count)
        _ -> do
          resp <- try @_ @SomeException $ TCS.getPendingChallanCount merchantId merchantOpCityId (CSIT.PendingChallanReq {vehicleNumber = rcNo})
          case resp of
            Right challanResp -> do
              let count = challanResp.pendingChallanCount
              QVRC.updatePendingChallanCount (Just count) vrc.id
              pure (Just count)
            Left err -> do
              logError $ "Failed to fetch pending challan count from Signzy for rcNo " <> rcNo <> ": " <> show err
              pure vrc.pendingChallanCount

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

castHubRequests :: Maybe Int -> (OperationHubRequests, Person, OperationHub) -> Flow OperationHubDriverRequest
castHubRequests mbChallanCount (hubReq, person, hub) = do
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
        requestType = hubReq.requestType,
        pendingChallanCount = if isJust hubReq.registrationNo then mbChallanCount else Nothing
      }
