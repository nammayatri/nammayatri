module Domain.Action.UI.OperationHub
  ( getOperationGetAllHubs,
    postOperationCreateRequest,
    getOperationGetRequests,
    createOperationHubRequest,
    CreateOperationHubRequestParams (..),
    CreateOperationHubRequestData (..),
    mkCreateOperationHubRequestKey,
  )
where

import API.Types.UI.OperationHub
import Data.Time (UTCTime (..))
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.OperationHub
import Domain.Types.OperationHubRequests
import Domain.Types.Person as DP
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverRCAssociation as QDRCA
import qualified Storage.Queries.OperationHub as QOH
import qualified Storage.Queries.OperationHubRequests as QOHR
import qualified Storage.Queries.OperationHubRequestsExtra as QOHRE
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import Tools.Error

getOperationGetAllHubs :: (Maybe (Id Person), Id Merchant, Id MerchantOperatingCity) -> Flow [OperationHub]
getOperationGetAllHubs (_, _, opCityId) = QOH.findAllByCityId opCityId

postOperationCreateRequest :: (Maybe (Id Person), Id Merchant, Id MerchantOperatingCity) -> DriverOperationHubRequest -> Flow APISuccess
postOperationCreateRequest (mbPersonId, merchantId, merchantOperatingCityId) req = do
  runRequestValidation validateDriverOperationHubRequest req
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  vehicleRC <- RCQuery.findLastVehicleRCWrapper req.registrationNo >>= fromMaybeM (RCNotFound req.registrationNo)
  unless (vehicleRC.verificationStatus == Documents.VALID) $ throwError RcNotValid
  let params = mkCreateOperationHubRequestParams personId merchantId merchantOperatingCityId vehicleRC req

  checkDriverRcAssociation vehicleRC.id personId

  createOperationHubRequest params
  pure Success

checkDriverRcAssociation :: Id DVRC.VehicleRegistrationCertificate -> Id Person -> Flow ()
checkDriverRcAssociation personId rcId = do
  now <- getCurrentTime
  void $ QDRCA.findLinkedByRCIdAndDriverId rcId personId now >>= fromMaybeM (InvalidRequest "Vehicle is not associated with driver")

mkCreateOperationHubRequestParams ::
  Id Person ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  DVRC.VehicleRegistrationCertificate ->
  DriverOperationHubRequest ->
  CreateOperationHubRequestParams
mkCreateOperationHubRequestParams personId merchantId merchantOperatingCityId vehicleRC DriverOperationHubRequest {..} = do
  CreateOperationHubRequestParams
    { driverId = Just personId,
      creatorId = personId, -- driver is creator
      ..
    }

validateDriverOperationHubRequest :: Validate DriverOperationHubRequest
validateDriverOperationHubRequest DriverOperationHubRequest {..} =
  sequenceA_
    [ validateField "registrationNo" registrationNo $
        LengthInRange 1 11 `And` star (P.latinUC \/ P.digit)
    ]

data CreateOperationHubRequestParams = CreateOperationHubRequestParams
  { requestType :: RequestType,
    operationHubId :: Id OperationHub,
    vehicleRC :: DVRC.VehicleRegistrationCertificate,
    creatorId :: Id Person,
    driverId :: Maybe (Id Person),
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity
  }

createOperationHubRequest :: (EsqDBFlow m r, EncFlow m r, MonadFlow m, CacheFlow m r) => CreateOperationHubRequestParams -> m ()
createOperationHubRequest req = do
  -- TODO should we lock on driverId and registrationNo instead?
  Redis.whenWithLockRedis (opsHubDriverLockKey req.creatorId.getId) 60 $ do
    let merchantId = req.merchantId
        merchantOperatingCityId = req.merchantOperatingCityId
    let vehicleAlreadyApproved = fromMaybe False req.vehicleRC.approved
    driverAlreadyEnabled <- case req.driverId of
      Just driverId -> do
        driverInformation <- QDriverInfo.findById driverId >>= fromMaybeM DriverInfoNotFound
        pure driverInformation.enabled
      Nothing -> pure True
    when (req.requestType == ONBOARDING_INSPECTION && vehicleAlreadyApproved && driverAlreadyEnabled) $
      throwError (InvalidRequest "Vehicle already approved and driver already enabled")
    let creatorId = req.creatorId
    registrationNo <- decrypt req.vehicleRC.certificateNumber
    case req.driverId of
      Just driverId -> do
        opsHubReq <- QOHR.findByRcOrCreatorAndStatusAndType registrationNo driverId PENDING req.requestType
        unless (isNothing opsHubReq) $ do
          Kernel.Utils.Common.throwError (InvalidRequest "Duplicate Request")
      Nothing -> do
        opsHubReq <- QOHR.findByRcAndStatusAndType registrationNo PENDING req.requestType
        unless (isNothing opsHubReq) $
          Kernel.Utils.Common.throwError (InvalidRequest "Duplicate Request")
    void $ QOH.findByPrimaryKey req.operationHubId >>= fromMaybeM (OperationHubDoesNotExist req.operationHubId.getId)
    id <- generateGUID
    now <- getCurrentTime
    let operationHubReq =
          OperationHubRequests
            { operationHubId = req.operationHubId,
              registrationNo,
              requestType = req.requestType,
              requestStatus = PENDING,
              createdAt = now,
              updatedAt = now,
              fulfilledAt = Nothing,
              operatorId = Nothing,
              remarks = Nothing,
              ..
            }
    QOHR.create operationHubReq

data CreateOperationHubRequestData = CreateOperationHubRequestData
  { operationHubId :: Id OperationHub,
    creatorId :: Id Person
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

mkCreateOperationHubRequestKey :: Text -> Text
mkCreateOperationHubRequestKey registrationNo = "createOperationHubRequest:registrationNo-" <> registrationNo

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
