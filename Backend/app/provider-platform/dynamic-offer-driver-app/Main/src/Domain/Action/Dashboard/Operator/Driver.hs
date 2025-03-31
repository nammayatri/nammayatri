module Domain.Action.Dashboard.Operator.Driver
  ( getDriverOperatorFetchHubRequests,
    postDriverOperatorRespondHubRequest,
    opsHubRequestLockKey,
    postDriverOperatorSendJoiningOtp,
    postDriverOperatorVerifyJoiningOtp,
    postDriverOperatorAddDrivers,
  )
where

import EulerHS.Prelude (whenNothing_, (<|>))
import Kernel.Sms.Config
import qualified Domain.Types.RegistrationToken as SR
import Kernel.Beam.Functions as B
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as CommonFleet
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified API.Types.ProviderPlatform.Operator.Driver
import Data.Time hiding (getCurrentTime)
import Domain.Action.Dashboard.RideBooking.Driver
import qualified Domain.Action.Dashboard.RideBooking.DriverRegistration as DRBReg
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.DriverOperatorAssociation as DOA
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.OperationHub as DOH
import Domain.Types.OperationHubRequests
import qualified Domain.Types.Person as DP
import Environment
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.Beam.SystemConfigs ()
import Storage.Cac.TransporterConfig (findByMerchantOpCityId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.OperationHubRequests as SQOHR
import qualified Storage.Queries.OperationHubRequestsExtra as SQOH
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error
import Tools.SMS as Sms hiding (Success)

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

---------------------------------------------------------------------
postDriverOperatorSendJoiningOtp ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.AuthReq ->
  Flow Common.AuthRes
postDriverOperatorSendJoiningOtp merchantShortId opCity requestorId req = do
  operator <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
  unless (operator.role == DP.OPERATOR) $
    throwError AccessDenied

  merchant <- findMerchantByShortId merchantShortId
  smsCfg <- asks (.smsCfg)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  mobileNumberHash <- getDbHash req.mobileNumber
  mbPerson <- B.runInReplica $ QP.findByMobileNumberAndMerchantAndRole req.mobileCountryCode mobileNumberHash merchant.id DP.DRIVER
  case mbPerson of
    Nothing -> DRBReg.auth merchantShortId opCity req -------------- to onboard a driver that is not the part of the fleet
    Just person -> do
      withLogTag ("personId_" <> getId person.id) $ do
        let useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp
            phoneNumber = req.mobileCountryCode <> req.mobileNumber
        otpCode <- maybe generateOTPCode return useFakeOtpM
        whenNothing_ useFakeOtpM $ do
          let operatorName = operator.firstName <> maybe "" (" " <>) operator.lastName
          (mbSender, message) <-
            MessageBuilder.buildOperatorJoiningMessage merchantOpCityId $
              MessageBuilder.BuildOperatorJoiningMessageReq
                { otp = otpCode,
                  operatorName = operatorName
                }
          let sender = fromMaybe smsCfg.sender mbSender
          Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
        let key = makeOperatorDriverOtpKey phoneNumber
        Redis.setExp key otpCode 3600
      pure $ Common.AuthRes {authId = "ALREADY_USING_APPLICATION", attempts = 0}

---------------------------------------------------------------------

-- TODO reuse for fleet apis also
endActiveAssociationsIfAllowed ::
  DM.Merchant ->
  Id DP.Person ->
  Flow ()
endActiveAssociationsIfAllowed merchant personId = do
  existingFleetAssociations <- QFDA.findAllByDriverId personId True
  unless (null existingFleetAssociations) $ do
    if merchant.overwriteAssociation == Just True
      then forM_ existingFleetAssociations $ \existingAssociation -> do
        logInfo $ "End existing fleet driver association: fleetOwnerId: " <> existingAssociation.fleetOwnerId <> "driverId: " <> existingAssociation.driverId.getId
        QFDA.endFleetDriverAssociation existingAssociation.fleetOwnerId existingAssociation.driverId
      else throwError (InvalidRequest "Driver already associated with another fleet")

  existingOperatorAssociations <- QDOA.findAllByDriverId personId True
  unless (null existingOperatorAssociations) $ do
    if merchant.overwriteAssociation == Just True
      then forM_ existingOperatorAssociations $ \existingAssociation -> do
        logInfo $ "End existing operator driver association: operatorId: " <> existingAssociation.operatorId <> "driverId: " <> existingAssociation.driverId.getId
        QDOA.endOperatorDriverAssociation existingAssociation.operatorId existingAssociation.driverId
      else throwError (InvalidRequest "Driver already associated with another operator")

postDriverOperatorVerifyJoiningOtp ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Text ->
  API.Types.ProviderPlatform.Operator.Driver.VerifyOperatorJoiningOtpReq ->
  Flow APISuccess
postDriverOperatorVerifyJoiningOtp merchantShortId opCity mbAuthId requestorId req = do
  operator <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
  unless (operator.role == DP.OPERATOR) $
    throwError AccessDenied

  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  mobileNumberHash <- getDbHash req.mobileNumber
  person <- B.runInReplica $ QP.findByMobileNumberAndMerchantAndRole req.mobileCountryCode mobileNumberHash merchant.id DP.DRIVER >>= fromMaybeM (PersonNotFound req.mobileNumber)
  case mbAuthId of
    Just authId -> do
      smsCfg <- asks (.smsCfg)
      deviceToken <- fromMaybeM (DeviceTokenNotFound) $ req.deviceToken
      let regId = Id authId :: Id SR.RegistrationToken
      res <-
        DReg.verify
          regId
          DReg.AuthVerifyReq
            { otp = req.otp,
              deviceToken = deviceToken,
              whatsappNotificationEnroll = Nothing
            }

      checkAssocOperator <- B.runInReplica $ QDOA.findByDriverIdAndOperatorId res.person.id operator.id True
      when (isJust checkAssocOperator) $ throwError (InvalidRequest "Driver already associated with operator")

      endActiveAssociationsIfAllowed merchant person.id

      assoc <- DOA.makeDriverOperatorAssociation merchant.id merchantOpCityId res.person.id operator.id.getId (DomainRC.convertTextToUTC (Just "2099-12-12"))
      QDOA.create assoc

      let phoneNumber = req.mobileCountryCode <> req.mobileNumber
      withLogTag ("personId_" <> getId person.id) $ do
        (mbSender, message) <-
          MessageBuilder.buildOperatorJoinAndDownloadAppMessage merchantOpCityId $
            MessageBuilder.BuildOperatorJoinAndDownloadAppMessageReq
              { operatorName = operator.firstName
              }
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
          >>= Sms.checkSmsResult
    Nothing -> do
      let key = makeOperatorDriverOtpKey (req.mobileCountryCode <> req.mobileNumber)
      otp <- Redis.get key >>= fromMaybeM OtpNotFound
      when (otp /= req.otp) $ throwError InvalidOtp
      checkAssocOperator <- B.runInReplica $ QDOA.findByDriverIdAndOperatorId person.id operator.id True
      when (isJust checkAssocOperator) $ throwError (InvalidRequest "Driver already associated with operator")

      endActiveAssociationsIfAllowed merchant person.id

      assoc <- DOA.makeDriverOperatorAssociation merchant.id merchantOpCityId person.id operator.id.getId (DomainRC.convertTextToUTC (Just "2099-12-12"))
      QDOA.create assoc
  pure Success

makeOperatorDriverOtpKey :: Text -> Text
makeOperatorDriverOtpKey phoneNo = "Operator:Driver:PhoneNo" <> phoneNo

---------------------------------------------------------------------
postDriverOperatorAddDrivers ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  API.Types.ProviderPlatform.Operator.Driver.CreateDriversReq ->
  Flow CommonFleet.APISuccessWithUnprocessedEntities
postDriverOperatorAddDrivers _merchantShortId _opCity _requestorId _req = do
  error "TODO"
