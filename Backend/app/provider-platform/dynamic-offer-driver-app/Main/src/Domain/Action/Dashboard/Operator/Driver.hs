module Domain.Action.Dashboard.Operator.Driver (getDriverOperatorFetchHubRequests, postDriverOperatorRespondHubRequest, opsHubRequestLockKey) where

import qualified API.Types.ProviderPlatform.Operator.Driver
import qualified Dashboard.Common as Common
import Data.Time hiding (getCurrentTime)
import Domain.Action.Dashboard.RideBooking.Driver
import qualified Domain.Types.Merchant
import qualified Domain.Types.OperationHub as DOH
import Domain.Types.OperationHubRequests
import qualified Domain.Types.Person as DP
import qualified Environment
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Cac.TransporterConfig (findByMerchantOpCityId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.OperationHubRequests as SQOHR
import qualified Storage.Queries.OperationHubRequestsExtra as SQOH
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle

getDriverOperatorFetchHubRequests ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe API.Types.ProviderPlatform.Operator.Driver.RequestStatus ->
  Maybe API.Types.ProviderPlatform.Operator.Driver.RequestType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe (Id Common.OperationHub) ->
  Maybe Text ->
  Environment.Flow API.Types.ProviderPlatform.Operator.Driver.OperationHubReqResp
getDriverOperatorFetchHubRequests _merchantShortId _opCity mbFrom mbTo mbStatus mbReqType mbLimit mbOffset mbDriverId mbMobileNumber mbReqOperationHubId mbRegistrationNo = do
  now <- getCurrentTime
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
      defaultFrom = UTCTime (utctDay now) 0
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
      mbOperationHubId = cast @Common.OperationHub @DOH.OperationHub <$> mbReqOperationHubId
  mbMobileNumberHash <- mapM getDbHash mbMobileNumber
  mbRegistrationNoHash <- mapM getDbHash mbRegistrationNo
  reqList <- SQOH.findAllRequestsInRange from to limit offset mbMobileNumberHash (castReqStatusToDomain <$> mbStatus) (castReqTypeToDomain <$> mbReqType) mbDriverId mbOperationHubId mbRegistrationNoHash
  operationHubDriverRequests <- mapM buildOperationHubDriverRequest reqList
  pure $ API.Types.ProviderPlatform.Operator.Driver.OperationHubReqResp {requests = operationHubDriverRequests}

postDriverOperatorRespondHubRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Operator.Driver.RespondHubRequest -> Environment.Flow APISuccess)
postDriverOperatorRespondHubRequest merchantShortId opCity req = do
  now <- getCurrentTime
  Redis.whenWithLockRedis (opsHubRequestLockKey req.operationHubRequestId) 60 $ do
    opHubReq <- SQOHR.findByPrimaryKey (Kernel.Types.Id.Id req.operationHubRequestId) >>= fromMaybeM (InternalError "Invalid operation hub request id")
    unless (opHubReq.requestStatus == PENDING) $ Kernel.Utils.Common.throwError (InvalidRequest "Request already responded")
    void $ SQOHR.updateStatusWithDetails (castReqStatusToDomain req.status) (Just req.remarks) (Just now) (Just (Kernel.Types.Id.Id req.operatorId)) (Kernel.Types.Id.Id req.operationHubRequestId)
    when (req.status == API.Types.ProviderPlatform.Operator.Driver.APPROVED && opHubReq.requestType == ONBOARDING_INSPECTION) $ do
      fork "enable driver after inspection" $ do
        let personId = opHubReq.driverId
        merchant <- findMerchantByShortId merchantShortId
        merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
        transporterConfig <- findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId) -- (Just (DriverId (cast personId)))
        person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
        let language = fromMaybe merchantOpCity.language person.language
        driverDocuments <- SStatus.fetchDriverDocuments personId merchantOpCity transporterConfig language
        vehicleDocumentsUnverified <- SStatus.fetchVehicleDocuments personId merchantOpCity transporterConfig language
        registrationNo <- decrypt opHubReq.registrationNo
        vehicleDoc <-
          find (\doc -> doc.registrationNo == registrationNo) vehicleDocumentsUnverified
            & fromMaybeM (InvalidRequest $ "Vehicle doc not found for driverId " <> personId.getId <> " with registartionNo " <> registrationNo)
        let makeSelfieAadhaarPanMandatory = Nothing
        allVehicleDocsVerified <- SStatus.checkAllVehicleDocsVerified merchantOpCity.id vehicleDoc makeSelfieAadhaarPanMandatory
        allDriverDocsVerified <- SStatus.checkAllDriverDocsVerified merchantOpCity.id driverDocuments vehicleDoc makeSelfieAadhaarPanMandatory
        when (allVehicleDocsVerified && allDriverDocsVerified) $
          void $ postDriverEnable merchantShortId opCity $ cast @DP.Person @Common.Driver personId
        mbVehicle <- QVehicle.findById personId
        when (isNothing mbVehicle && allVehicleDocsVerified && allDriverDocsVerified) $
          void $ try @_ @SomeException (SStatus.activateRCAutomatically personId merchantOpCity vehicleDoc.registrationNo)
  pure Success

opsHubRequestLockKey :: Text -> Text
opsHubRequestLockKey reqId = "opsHub:Request:Id-" <> reqId

buildOperationHubDriverRequest :: EncFlow m r => OperationHubRequests -> m API.Types.ProviderPlatform.Operator.Driver.OperationHubDriverRequest
buildOperationHubDriverRequest OperationHubRequests {..} = do
  registrationNoDec <- decrypt registrationNo
  pure $
    API.Types.ProviderPlatform.Operator.Driver.OperationHubDriverRequest
      { driverId = driverId.getId,
        id = id.getId,
        operationHubId = cast @DOH.OperationHub @Common.OperationHub operationHubId,
        registrationNo = registrationNoDec,
        requestStatus = castReqStatus requestStatus,
        requestTime = createdAt,
        requestType = castReqType requestType
      }

castReqStatusToDomain :: API.Types.ProviderPlatform.Operator.Driver.RequestStatus -> RequestStatus
castReqStatusToDomain = \case
  API.Types.ProviderPlatform.Operator.Driver.PENDING -> PENDING
  API.Types.ProviderPlatform.Operator.Driver.REJECTED -> REJECTED
  API.Types.ProviderPlatform.Operator.Driver.APPROVED -> APPROVED

castReqTypeToDomain :: API.Types.ProviderPlatform.Operator.Driver.RequestType -> RequestType
castReqTypeToDomain = \case
  API.Types.ProviderPlatform.Operator.Driver.ONBOARDING_INSPECTION -> ONBOARDING_INSPECTION
  API.Types.ProviderPlatform.Operator.Driver.REGULAR_INSPECTION -> REGULAR_INSPECTION

castReqStatus :: RequestStatus -> API.Types.ProviderPlatform.Operator.Driver.RequestStatus
castReqStatus = \case
  PENDING -> API.Types.ProviderPlatform.Operator.Driver.PENDING
  REJECTED -> API.Types.ProviderPlatform.Operator.Driver.REJECTED
  APPROVED -> API.Types.ProviderPlatform.Operator.Driver.APPROVED

castReqType :: RequestType -> API.Types.ProviderPlatform.Operator.Driver.RequestType
castReqType = \case
  ONBOARDING_INSPECTION -> API.Types.ProviderPlatform.Operator.Driver.ONBOARDING_INSPECTION
  REGULAR_INSPECTION -> API.Types.ProviderPlatform.Operator.Driver.REGULAR_INSPECTION
