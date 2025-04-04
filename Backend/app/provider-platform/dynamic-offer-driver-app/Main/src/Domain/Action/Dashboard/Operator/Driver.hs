module Domain.Action.Dashboard.Operator.Driver
  ( getDriverOperatorFetchHubRequests,
    postDriverOperatorRespondHubRequest,
    opsHubRequestLockKey,
    postDriverOperatorCreateRequest,
    getDriverOperatorList,
  )
where

import qualified API.Types.ProviderPlatform.Operator.Driver
import qualified API.Types.UI.OperationHub as DomainT
import qualified Dashboard.Common as Common
import Data.Time hiding (getCurrentTime)
import Domain.Action.Dashboard.Fleet.Onboarding (castStatusRes)
import Domain.Action.Dashboard.RideBooking.Driver
import qualified Domain.Action.UI.OperationHub as Domain
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
import Storage.Queries.DriverOperatorAssociationExtra (findAllByOperatorIdWithLimitOffset)
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
  reqList <- SQOH.findAllRequestsInRange from to limit offset mbMobileNumberHash (castReqStatusToDomain <$> mbStatus) (castReqTypeToDomain <$> mbReqType) mbDriverId mbOperationHubId mbRegistrationNo
  pure $ API.Types.ProviderPlatform.Operator.Driver.OperationHubReqResp {requests = map castHubRequests reqList}

postDriverOperatorCreateRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Operator.Driver.DriverOperationHubRequest -> Environment.Flow APISuccess)
postDriverOperatorCreateRequest merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let domainReq = castOpsHubReq req
  Domain.postOperationCreateRequest (Just (Id req.driverId), merchant.id, merchantOpCity.id) domainReq
  where
    castOpsHubReq :: API.Types.ProviderPlatform.Operator.Driver.DriverOperationHubRequest -> DomainT.DriverOperationHubRequest
    castOpsHubReq API.Types.ProviderPlatform.Operator.Driver.DriverOperationHubRequest {..} = DomainT.DriverOperationHubRequest {operationHubId = cast operationHubId, requestType = castReqTypeToDomain requestType, ..}

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
        driverDocuments <- SStatus.fetchDriverDocuments personId merchantOpCity transporterConfig language (Just True)
        vehicleDocumentsUnverified <- SStatus.fetchVehicleDocuments personId merchantOpCity transporterConfig language
        vehicleDoc <-
          find (\doc -> doc.registrationNo == req.registrationNo) vehicleDocumentsUnverified
            & fromMaybeM (InvalidRequest $ "Vehicle doc not found for driverId " <> personId.getId <> " with registartionNo " <> req.registrationNo)
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

castHubRequests :: OperationHubRequests -> API.Types.ProviderPlatform.Operator.Driver.OperationHubDriverRequest
castHubRequests OperationHubRequests {..} =
  API.Types.ProviderPlatform.Operator.Driver.OperationHubDriverRequest
    { driverId = driverId.getId,
      id = id.getId,
      operationHubId = cast @DOH.OperationHub @Common.OperationHub operationHubId,
      registrationNo,
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

getDriverOperatorList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Text ->
  Environment.Flow [API.Types.ProviderPlatform.Operator.Driver.DriverInfo]
getDriverOperatorList _merchantShortId _opCity mbIsActive mbLimit mbOffset requestorId = do
  person <- QPerson.findById (Id requestorId) >>= fromMaybeM (PersonNotFound requestorId)
  unless (person.role == DP.OPERATOR) $
    Kernel.Utils.Common.throwError (InvalidRequest "Requestor role is not OPERATOR")
  driverOperatorAssociationLs <-
    findAllByOperatorIdWithLimitOffset requestorId mbIsActive mbLimit mbOffset
  mapM createDriverInfo driverOperatorAssociationLs
  where
    createDriverInfo drvOpAsn = do
      let driverId = drvOpAsn.driverId
      person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      decryptedMobileNumber <-
        mapM decrypt person.mobileNumber
          >>= fromMaybeM
            ( InvalidRequest $
                "Person do not have a mobile number " <> person.id.getId
            )
      mblinkedVehicle <- QVehicle.findById driverId
      let merchantOpCityId = person.merchantOperatingCityId
      transporterConfig <-
        findByMerchantOpCityId merchantOpCityId Nothing
          >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      merchantOpCity <-
        CQMOC.findById merchantOpCityId
          >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
      statusRes <-
        castStatusRes
          <$> SStatus.statusHandler' person.id merchantOpCity transporterConfig Nothing Nothing Nothing Nothing Nothing (Just True) -- FXME: Need to change
      pure $
        API.Types.ProviderPlatform.Operator.Driver.DriverInfo
          { driverId = cast drvOpAsn.driverId,
            firstName = person.firstName,
            middleName = person.middleName,
            lastName = person.lastName,
            isActive = drvOpAsn.isActive,
            mobileCountryCode = fromMaybe "+91" person.mobileCountryCode,
            mobileNumber = decryptedMobileNumber,
            vehicle = (.model) <$> mblinkedVehicle,
            documents = statusRes
          }
