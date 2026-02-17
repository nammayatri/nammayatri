module Domain.Action.Dashboard.Operator.Driver
  ( getDriverOperatorFetchHubRequests,
    postDriverOperatorRespondHubRequest,
    opsHubRequestLockKey,
    postDriverOperatorCreateRequest,
    getDriverOperationGetAllHubs,
    getDriverOperatorList,
    postDriverOperatorSendJoiningOtp,
    postDriverOperatorVerifyJoiningOtp,
    getDriverOperatorDashboardAnalyticsAllTime,
    getDriverOperatorDashboardAnalytics,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as CommonFleet
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified API.Types.ProviderPlatform.Operator.Driver
import qualified API.Types.ProviderPlatform.Operator.Endpoints.Driver as CommonDriver
import qualified API.Types.UI.OperationHub as DomainT
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.Dashboard.Fleet.Driver as DFDriver
import Domain.Action.Dashboard.Fleet.Onboarding (castStatusRes)
import Domain.Action.Dashboard.RideBooking.Driver
import qualified Domain.Action.Dashboard.RideBooking.DriverRegistration as DRBReg
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Action.UI.DriverOnboarding.Referral as DOR
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.OperationHub as Domain
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.DocumentReminderHistory as DRH
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.OperationHub as DOH
import Domain.Types.OperationHubRequests
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as SR
import Environment
import EulerHS.Prelude (whenNothing_, (<|>))
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import Kernel.Sms.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import SharedLogic.Analytics as Analytics
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import SharedLogic.Reminder.Helper (cancelRemindersForDriverByDocumentType, cancelRemindersForRCByDocumentType, recordDocumentCompletion)
import Storage.Beam.SystemConfigs ()
import Storage.Cac.TransporterConfig (findByMerchantOpCityId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.DriverRCAssociationExtra as QDRC
import qualified Storage.Queries.DriverRCAssociationExtra as SQDRA
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.OperationHub as QOH
import qualified Storage.Queries.OperationHubRequests as SQOHR
import qualified Storage.Queries.OperationHubRequestsExtra as SQOH
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as QVRC
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as QVRCE
import Tools.Error
import Tools.SMS as Sms hiding (Success)

getDriverOperationGetAllHubs ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow [CommonDriver.OperationHub]
getDriverOperationGetAllHubs merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  opsHub <- QOH.findAllByCityId merchantOpCity.id
  pure $ map castOpsHub opsHub
  where
    castOpsHub DOH.OperationHub {..} = CommonDriver.OperationHub {id = cast id, merchantId = merchantId.getId, merchantOperatingCityId = merchantOperatingCityId.getId, ..}

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
  Maybe (Id CommonDriver.OperationHub) ->
  Maybe Text ->
  Maybe Text ->
  Environment.Flow API.Types.ProviderPlatform.Operator.Driver.OperationHubReqResp
getDriverOperatorFetchHubRequests _merchantShortId _opCity mbFrom mbTo mbStatus mbReqType mbLimit mbOffset mbDriverId mbMobileNumber mbReqOperationHubId mbOperationHubName mbRegistrationNo = do
  now <- getCurrentTime
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
      defaultFrom = UTCTime (fromGregorian 2020 1 1) 0
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
      mbOperationHubId = cast @CommonDriver.OperationHub @DOH.OperationHub <$> mbReqOperationHubId
      mbDriverIdPerson = (Id :: Text -> Id DP.Person) <$> mbDriverId
  mbMobileNumberHash <- mapM getDbHash mbMobileNumber
  reqList <- SQOH.findAllRequestsInRange from to limit offset mbMobileNumberHash (castReqStatusToDomain <$> mbStatus) (castReqTypeToDomain <$> mbReqType) Nothing mbOperationHubId mbOperationHubName mbRegistrationNo mbDriverIdPerson
  logInfo $ "Driver Operator Fetch Hub Requests' params - mbFrom: " <> show mbFrom <> " from: " <> show from <> " to: " <> show to
  let summary = Common.Summary {totalCount = 10000, count = length reqList}
  requests <- mapM castHubRequests reqList
  pure $ API.Types.ProviderPlatform.Operator.Driver.OperationHubReqResp {..}

postDriverOperatorCreateRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Operator.Driver.DriverOperationHubRequest -> Environment.Flow APISuccess)
postDriverOperatorCreateRequest merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let domainReq = castOpsHubReq req
  Domain.postOperationCreateRequest (Nothing, merchant.id, merchantOpCity.id) domainReq
  where
    castOpsHubReq :: API.Types.ProviderPlatform.Operator.Driver.DriverOperationHubRequest -> DomainT.DriverOperationHubRequest
    castOpsHubReq API.Types.ProviderPlatform.Operator.Driver.DriverOperationHubRequest {..} =
      DomainT.DriverOperationHubRequest
        { operationHubId = cast operationHubId,
          requestType = castReqTypeToDomain requestType,
          registrationNo = registrationNo,
          driverId = cast <$> driverId,
          creatorId = creatorId
        }

postDriverOperatorRespondHubRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Operator.Driver.RespondHubRequest -> Environment.Flow APISuccess)
postDriverOperatorRespondHubRequest merchantShortId opCity req = withLogTag ("operationHubRequestId_" <> req.operationHubRequestId) $ do
  now <- getCurrentTime
  Redis.whenWithLockRedis (opsHubRequestLockKey req.operationHubRequestId) 60 $ do
    opHubReq <- SQOHR.findByPrimaryKey (Kernel.Types.Id.Id req.operationHubRequestId) >>= fromMaybeM (InvalidRequest "Invalid operation hub request id")
    when (opHubReq.requestStatus == APPROVED) $ Kernel.Utils.Common.throwError (InvalidRequest "Request already approved")

    -- Handle rejection for any request type
    when (req.status == API.Types.ProviderPlatform.Operator.Driver.REJECTED) $ do
      void $ SQOHR.updateStatusWithDetails (castReqStatusToDomain req.status) (Just req.remarks) (Just now) (Just (Kernel.Types.Id.Id req.operatorId)) (Kernel.Types.Id.Id req.operationHubRequestId)

    -- Handle approval based on request type
    when (req.status == API.Types.ProviderPlatform.Operator.Driver.APPROVED) $ do
      case opHubReq.requestType of
        ONBOARDING_INSPECTION -> handleVehicleInspectionApproval merchantShortId opCity req opHubReq now
        REGULAR_INSPECTION -> handleVehicleInspectionApproval merchantShortId opCity req opHubReq now
        DRIVER_ONBOARDING_INSPECTION -> handleDriverInspectionApproval merchantShortId opCity req opHubReq now
        DRIVER_REGULAR_INSPECTION -> handleDriverInspectionApproval merchantShortId opCity req opHubReq now
  pure Success
  where
    handleVehicleInspectionApproval mShortId city request opHubReq now = do
      creator <- runInReplica $ QPerson.findById opHubReq.creatorId >>= fromMaybeM (PersonNotFound opHubReq.creatorId.getId)
      registrationNo <- opHubReq.registrationNo & fromMaybeM (InvalidRequest "registrationNo is required for vehicle inspection")
      rc <- QVRCE.findLastVehicleRCWrapper registrationNo >>= fromMaybeM (RCNotFound registrationNo)
      personId <- case creator.role of
        DP.DRIVER -> pure creator.id
        _ -> do
          drc <- SQDRA.findAllActiveAssociationByRCId rc.id
          case drc of
            [] -> throwError (InvalidRequest "No driver exist with this RC")
            (assoc : _) -> pure assoc.driverId
      (merchantOpCity, transporterConfig, person, language) <- getMerchantAndPersonInfo mShortId city personId
      fork "enable driver after vehicle inspection" $ do
        -- Only check vehicle docs, not driver docs (vehicle and driver enablement are segregated)
        allVehicleDocsVerified <- SStatus.checkAllVehicleDocsVerifiedForRC person merchantOpCity transporterConfig language registrationNo
        when allVehicleDocsVerified $ do
          QVRC.updateApproved (Just True) rc.id
          -- Cancel pending vehicle inspection reminders for all drivers using this RC
          cancelRemindersForRCByDocumentType rc.id DVC.VehicleInspectionForm
          -- Record inspection completion for auto-trigger monitoring (per RC, not per driver)
          recordDocumentCompletion DVC.VehicleInspectionForm rc.id.getId DRH.RC Nothing merchantOpCity.merchantId merchantOpCity.id
        let reqUpdatedStatus = if allVehicleDocsVerified then castReqStatusToDomain request.status else PENDING
        void $ SQOHR.updateStatusWithDetails reqUpdatedStatus (Just request.remarks) (Just now) (Just (Kernel.Types.Id.Id request.operatorId)) (Kernel.Types.Id.Id request.operationHubRequestId)
        mbVehicle <- QVehicle.findById personId
        when (isNothing mbVehicle && allVehicleDocsVerified) $
          void $ withTryCatch "activateRCAutomatically:postDriverOperatorRespondHubRequest" (SStatus.activateRCAutomatically personId merchantOpCity registrationNo)

    handleDriverInspectionApproval mShortId city request opHubReq now = do
      personId <- opHubReq.driverId & fromMaybeM (InvalidRequest "driverId is required for driver inspection")
      (merchantOpCity, transporterConfig, person, language) <- getMerchantAndPersonInfo mShortId city personId
      fork "enable driver after driver inspection" $ do
        -- Only check driver docs, not vehicle docs (vehicle and driver enablement are segregated)
        allDriverDocsVerified <- SStatus.checkAllDriverDocsVerifiedForDriver person merchantOpCity transporterConfig language
        when allDriverDocsVerified $ do
          QDIExtra.updateApproved (Just True) personId
          -- Cancel pending driver inspection reminders
          cancelRemindersForDriverByDocumentType personId DVC.DriverInspectionForm
          -- Record driver inspection completion for auto-trigger monitoring
          recordDocumentCompletion DVC.DriverInspectionForm personId.getId DRH.DRIVER (Just personId) merchantOpCity.merchantId merchantOpCity.id
          void $ postDriverEnable mShortId city $ cast @DP.Person @Common.Driver personId
        let reqUpdatedStatus = if allDriverDocsVerified then castReqStatusToDomain request.status else PENDING
        void $ SQOHR.updateStatusWithDetails reqUpdatedStatus (Just request.remarks) (Just now) (Just (Kernel.Types.Id.Id request.operatorId)) (Kernel.Types.Id.Id request.operationHubRequestId)

    getMerchantAndPersonInfo mShortId city personId = do
      merchant <- findMerchantByShortId mShortId
      merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> mShortId.getShortId <> " ,city: " <> show city)
      transporterConfig <- findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
      person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      let language = fromMaybe merchantOpCity.language person.language
      pure (merchantOpCity, transporterConfig, person, language)

opsHubRequestLockKey :: Text -> Text
opsHubRequestLockKey reqId = "opsHub:Request:Id-" <> reqId

castHubRequests :: (OperationHubRequests, DP.Person, DOH.OperationHub) -> Environment.Flow API.Types.ProviderPlatform.Operator.Driver.OperationHubDriverRequest
castHubRequests (hubReq, creator, hub) = do
  creatorPhoneNo <- mapM decrypt creator.mobileNumber
  driverPhoneNo <- case hubReq.driverId of
    Just driverId -> do
      driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      mapM decrypt driver.mobileNumber
    Nothing -> pure Nothing
  rcId <- case hubReq.registrationNo of
    Just regNo -> do
      mbRc <- QVRCE.findLastVehicleRCWrapper regNo
      pure $ fmap (.id.getId) mbRc
    Nothing -> pure Nothing
  pure $
    API.Types.ProviderPlatform.Operator.Driver.OperationHubDriverRequest
      { id = hubReq.id.getId,
        operationHubId = cast @DOH.OperationHub @CommonDriver.OperationHub hubReq.operationHubId,
        operationHubName = hub.name,
        registrationNo = hubReq.registrationNo,
        rcId,
        creatorPhoneNo,
        driverPhoneNo,
        requestStatus = castReqStatus hubReq.requestStatus,
        requestTime = hubReq.createdAt,
        requestType = castReqType hubReq.requestType
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
  API.Types.ProviderPlatform.Operator.Driver.DRIVER_ONBOARDING_INSPECTION -> DRIVER_ONBOARDING_INSPECTION
  API.Types.ProviderPlatform.Operator.Driver.DRIVER_REGULAR_INSPECTION -> DRIVER_REGULAR_INSPECTION

castReqStatus :: RequestStatus -> API.Types.ProviderPlatform.Operator.Driver.RequestStatus
castReqStatus = \case
  PENDING -> API.Types.ProviderPlatform.Operator.Driver.PENDING
  REJECTED -> API.Types.ProviderPlatform.Operator.Driver.REJECTED
  APPROVED -> API.Types.ProviderPlatform.Operator.Driver.APPROVED

castReqType :: RequestType -> API.Types.ProviderPlatform.Operator.Driver.RequestType
castReqType = \case
  ONBOARDING_INSPECTION -> API.Types.ProviderPlatform.Operator.Driver.ONBOARDING_INSPECTION
  REGULAR_INSPECTION -> API.Types.ProviderPlatform.Operator.Driver.REGULAR_INSPECTION
  DRIVER_ONBOARDING_INSPECTION -> API.Types.ProviderPlatform.Operator.Driver.DRIVER_ONBOARDING_INSPECTION
  DRIVER_REGULAR_INSPECTION -> API.Types.ProviderPlatform.Operator.Driver.DRIVER_REGULAR_INSPECTION

getDriverOperatorList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Text ->
  Environment.Flow API.Types.ProviderPlatform.Operator.Driver.DriverInfoResp
getDriverOperatorList _merchantShortId _opCity mbIsActive mbLimit mbOffset mbVehicleNo mbSearchString onlyMandatoryDocs requestorId = do
  requestor <- QPerson.findById (Id requestorId) >>= fromMaybeM (PersonNotFound requestorId)
  unless (requestor.role == DP.OPERATOR) $
    Kernel.Utils.Common.throwError (InvalidRequest "Requestor role is not OPERATOR")
  now <- getCurrentTime
  driverOperatorInfoList <- case mbVehicleNo of
    Nothing -> do
      driverOperatorAssociationAndPersonLs <- QDOA.findAllByOperatorIdWithLimitOffsetSearch requestorId mbIsActive mbLimit mbOffset mbSearchString Nothing
      forM driverOperatorAssociationAndPersonLs \(drvOpAsn, person) -> do
        let driverId = drvOpAsn.driverId
        (vehicleModel, registrationNo, isRcActive) <- fetchVehicleDetailsByDriverId now driverId
        pure (drvOpAsn, person, vehicleModel, registrationNo, isRcActive)
    Just vehicleNo -> (maybeToList <$>) . runMaybeT $ do
      (vehicleModel, registrationNo, isRcActive, driverId) <- fetchVehicleDetailsByVehicleNo now vehicleNo
      (drvOpAsn, person) <- MaybeT $ listToMaybe <$> QDOA.findAllByOperatorIdWithLimitOffsetSearch requestorId mbIsActive mbLimit mbOffset mbSearchString (Just driverId)
      pure (drvOpAsn, person, vehicleModel, registrationNo, isRcActive)

  listItem <- mapM (buildDriverInfo now) driverOperatorInfoList
  let count = length listItem
  let summary = Common.Summary {totalCount = 10000, count}
  pure API.Types.ProviderPlatform.Operator.Driver.DriverInfoResp {..}
  where
    fetchVehicleDetailsByDriverId now driverId = do
      mbVehicle <- QVehicle.findById driverId
      case mbVehicle of
        Just vehicle -> pure (Just vehicle.model, Just vehicle.registrationNo, True)
        Nothing -> do
          latestAssociation <- QDRC.findLatestLinkedByDriverId driverId now
          case latestAssociation of
            Nothing -> pure (Nothing, Nothing, False)
            Just assoc -> do
              mbRc <- QVRC.findById assoc.rcId
              case mbRc of
                Nothing -> pure (Nothing, Nothing, False)
                Just rc -> pure (rc.vehicleModel, rc.unencryptedCertificateNumber, assoc.isRcActive)

    fetchVehicleDetailsByVehicleNo now vehicleNo = do
      mbVehicle <- lift $ QVehicle.findByRegistrationNo vehicleNo
      case mbVehicle of
        Just vehicle -> do
          pure (Just vehicle.model, Just vehicle.registrationNo, True, vehicle.driverId)
        Nothing -> do
          rc <- MaybeT $ QVRC.findLastVehicleRCWrapper vehicleNo
          assoc <- MaybeT $ QDRC.findLatestLinkedByRCId rc.id now
          pure (rc.vehicleModel, rc.unencryptedCertificateNumber, assoc.isRcActive, assoc.driverId)

    buildDriverInfo now (drvOpAsn, person, vehicleModel, registrationNo, isRcActive) = do
      let driverId = drvOpAsn.driverId
      decryptedMobileNumber <-
        mapM decrypt person.mobileNumber
          >>= fromMaybeM
            ( InvalidRequest $
                "Person do not have a mobile number " <> person.id.getId
            )
      let merchantOpCityId = person.merchantOperatingCityId
      transporterConfig <-
        findByMerchantOpCityId merchantOpCityId Nothing
          >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      merchantOpCity <-
        CQMOC.findById merchantOpCityId
          >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
      driverImages <- IQuery.findAllByPersonId transporterConfig driverId
      let driverImagesInfo = IQuery.DriverImagesInfo {driverId, merchantOperatingCity = merchantOpCity, driverImages, transporterConfig, now}
      driverInfo <- QDI.findById driverId >>= fromMaybeM DriverInfoNotFound
      let shouldActivateRc = False
      statusRes <-
        castStatusRes
          <$> SStatus.statusHandler' (Just person) driverImagesInfo Nothing Nothing Nothing Nothing (Just True) shouldActivateRc onlyMandatoryDocs -- FIXME: Need to change
      pure $
        API.Types.ProviderPlatform.Operator.Driver.DriverInfo
          { driverId = cast drvOpAsn.driverId,
            firstName = person.firstName,
            middleName = person.middleName,
            lastName = person.lastName,
            status = Just $ DFDriver.castDriverStatus driverInfo.mode,
            isActive = drvOpAsn.isActive,
            mobileCountryCode = fromMaybe "+91" person.mobileCountryCode,
            mobileNumber = decryptedMobileNumber,
            vehicle = vehicleModel,
            vehicleNo = registrationNo,
            isRcActive = isRcActive,
            documents = statusRes
          }

---------------------------------------------------------------------
postDriverOperatorSendJoiningOtp ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.AuthReq ->
  Flow Common.AuthRes
postDriverOperatorSendJoiningOtp merchantShortId opCity requestorId req = do
  let phoneNumber = req.mobileCountryCode <> req.mobileNumber
  sendOtpRateLimitOptions <- asks (.sendOtpRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeOperatorDriverHitsCountKey phoneNumber) sendOtpRateLimitOptions

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
        SA.checkForDriverAssociationOverwrite merchant person.id
        let useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp
        otpCode <- maybe generateOTPCode return useFakeOtpM
        whenNothing_ useFakeOtpM $ do
          let operatorName = operator.firstName <> maybe "" (" " <>) operator.lastName
          (mbSender, message, templateId, messageType) <-
            MessageBuilder.buildOperatorJoiningMessage merchantOpCityId $
              MessageBuilder.BuildOperatorJoiningMessageReq
                { otp = otpCode,
                  operatorName = operatorName
                }
          let sender = fromMaybe smsCfg.sender mbSender
          Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId messageType) >>= Sms.checkSmsResult
        let key = makeOperatorDriverOtpKey phoneNumber
        Redis.setExp key otpCode 3600
      pure $ Common.AuthRes {authId = "ALREADY_USING_APPLICATION", attempts = 0}

---------------------------------------------------------------------
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
  transporterConfig <- findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  mobileNumberHash <- getDbHash req.mobileNumber
  person <- B.runInReplica $ QP.findByMobileNumberAndMerchantAndRole req.mobileCountryCode mobileNumberHash merchant.id DP.DRIVER >>= fromMaybeM (PersonNotFound req.mobileNumber)
  case mbAuthId of
    Just authId -> do
      smsCfg <- asks (.smsCfg)

      SA.endDriverAssociationsIfAllowed merchant merchantOpCityId transporterConfig person

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

      verifyAndAssociateDriverWithOperator merchant merchantOpCityId operator res.person transporterConfig

      DOR.makeDriverReferredByOperator merchantOpCityId person.id operator.id

      let phoneNumber = req.mobileCountryCode <> req.mobileNumber
      withLogTag ("personId_" <> getId person.id) $ do
        (mbSender, message, templateId, messageType) <-
          MessageBuilder.buildOperatorJoinAndDownloadAppMessage merchantOpCityId $
            MessageBuilder.BuildOperatorJoinAndDownloadAppMessageReq
              { operatorName = operator.firstName
              }
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId messageType)
          >>= Sms.checkSmsResult
    Nothing -> do
      let key = makeOperatorDriverOtpKey (req.mobileCountryCode <> req.mobileNumber)
      otp <- Redis.get key >>= fromMaybeM OtpNotFound
      when (otp /= req.otp) $ throwError InvalidOtp

      SA.endDriverAssociationsIfAllowed merchant merchantOpCityId transporterConfig person

      verifyAndAssociateDriverWithOperator merchant merchantOpCityId operator person transporterConfig

  pure Success
  where
    verifyAndAssociateDriverWithOperator merchant merchantOpCityId operator person transporterConfig = do
      checkAssocOperator <-
        B.runInReplica $
          QDOA.findByDriverIdAndOperatorId person.id operator.id True
      when (isJust checkAssocOperator) $
        throwError (InvalidRequest "Driver already associated with operator")

      assoc <- SA.makeDriverOperatorAssociation merchant.id merchantOpCityId person.id operator.id.getId (DomainRC.convertTextToUTC (Just "2099-12-12"))
      QDOA.create assoc
      Analytics.handleDriverAnalyticsAndFlowStatus
        transporterConfig
        person.id
        Nothing
        ( \driverInfo -> do
            when driverInfo.enabled $ Analytics.incrementOperatorAnalyticsDriverEnabled transporterConfig operator.id.getId
            Analytics.incrementOperatorAnalyticsActiveDriver transporterConfig operator.id.getId
        )
        ( \driverInfo -> do
            DDriverMode.incrementFleetOperatorStatusKeyForDriver DP.OPERATOR operator.id.getId driverInfo.driverFlowStatus
        )

makeOperatorDriverOtpKey :: Text -> Text
makeOperatorDriverOtpKey phoneNo = "Operator:Driver:PhoneNo" <> phoneNo

makeOperatorDriverHitsCountKey :: Text -> Text
makeOperatorDriverHitsCountKey phoneNo = "Operator:Driver:PhoneNoHits" <> phoneNo <> ":hitsCount"

---------------------------------------------------------------------
getDriverOperatorDashboardAnalyticsAllTime ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Flow API.Types.ProviderPlatform.Operator.Driver.AllTimeOperatorAnalyticsRes
getDriverOperatorDashboardAnalyticsAllTime merchantShortId opCity requestorId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (not transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics) $ throwError (InvalidRequest "Analytics is not allowed for this merchant")
  operator <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
  unless (operator.role == DP.OPERATOR) $
    throwError AccessDenied

  -- Redis keys for fleet aggregates
  let allTimeKeysData = Analytics.allTimeKeys operator.id.getId
  logTagInfo "allTimeKeysData" (show allTimeKeysData)

  -- try redis first
  mbAllTimeKeysRes <- mapM (Redis.get @Int) allTimeKeysData
  logTagInfo "mbAllTimeKeysRes" (show mbAllTimeKeysRes)

  -- fallback to ClickHouse and populate cache when missing
  (totalRides, ratingSum, ratingCount, cancelCount, acceptationCount, totalRequestCount) <- do
    if all isJust mbAllTimeKeysRes
      then do
        let res = Analytics.convertToAllTimeFallbackRes (zip Analytics.allTimeMetrics (map (fromMaybe 0) mbAllTimeKeysRes))
        logTagInfo "AllTimeFallbackRes" (show res)
        Analytics.extractOperatorAnalyticsData res
      else do
        res <- Analytics.handleCacheMissForAnalyticsAllTimeCommon DP.OPERATOR operator.id.getId allTimeKeysData
        logTagInfo "AllTimeFallbackRes" (show res)
        Analytics.extractOperatorAnalyticsData res

  logTagInfo "Total rides" (show totalRides)
  logTagInfo "Rating sum" (show ratingSum)
  logTagInfo "Rating count" (show ratingCount)
  logTagInfo "Cancel count" (show cancelCount)
  logTagInfo "Acceptation count" (show acceptationCount)
  logTagInfo "Total request count" (show totalRequestCount)
  let rating =
        ratingSum >>= \rs ->
          ratingCount >>= \rc ->
            if rc > 0 then Just (fromIntegral rs / fromIntegral rc) else Nothing
      cancellationRate =
        cancelCount >>= \cc ->
          totalRides >>= \tr ->
            if tr > 0 then Just (fromIntegral cc / fromIntegral tr * 100) else Nothing
      acceptanceRate =
        acceptationCount >>= \ac ->
          totalRequestCount >>= \trc ->
            if trc > 0 then Just (fromIntegral ac / fromIntegral trc * 100) else Nothing
  pure $ API.Types.ProviderPlatform.Operator.Driver.AllTimeOperatorAnalyticsRes {rating, cancellationRate, acceptanceRate}

---------------------------------------------------------------------
getDriverOperatorDashboardAnalytics ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Day ->
  Day ->
  Flow API.Types.ProviderPlatform.Operator.Driver.FilteredOperatorAnalyticsRes
getDriverOperatorDashboardAnalytics merchantShortId opCity requestorId fromDay toDay = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (not transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics) $ throwError (InvalidRequest "Analytics is not allowed for this merchant")
  operator <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
  unless (operator.role == DP.OPERATOR) $ throwError AccessDenied

  now <- getCurrentTime
  let mbPeriod = Analytics.inferPeriod (utctDay now) fromDay toDay
  logTagInfo "mbPeriod:-" (show mbPeriod)

  case mbPeriod of
    Nothing -> do
      res <- Analytics.fallbackComputePeriodOperatorAnalytics operator.id.getId fromDay toDay
      logTagInfo "PeriodFallbackRes" (show res)
      pure $
        API.Types.ProviderPlatform.Operator.Driver.FilteredOperatorAnalyticsRes
          { activeDriver = res.activeDriver,
            driverEnabled = res.driverEnabled,
            greaterThanOneRide = res.greaterThanOneRide,
            greaterThanTenRide = res.greaterThanTenRide,
            greaterThanFiftyRide = res.greaterThanFiftyRide
          }
    Just period -> do
      let periodKeysData = Analytics.periodKeys operator.id.getId period
      logTagInfo "periodKeysData" (show periodKeysData)
      -- attempt to read cached values
      mbPeriodKeysRes <- mapM (Redis.get @Int) periodKeysData
      logTagInfo "mbPeriodKeysRes" (show mbPeriodKeysRes)

      (activeDriver, driverEnabled, greaterThanOneRide, greaterThanTenRide, greaterThanFiftyRide) <- do
        if all isJust mbPeriodKeysRes
          then do
            let res = Analytics.convertToPeriodFallbackRes (zip Analytics.periodMetrics (map (fromMaybe 0) mbPeriodKeysRes))
            logTagInfo "PeriodKeysRes" (show res)
            pure (res.activeDriver, res.driverEnabled, res.greaterThanOneRide, res.greaterThanTenRide, res.greaterThanFiftyRide)
          else do
            res <- Analytics.handleCacheMissForOperatorAnalyticsPeriod transporterConfig operator.id.getId periodKeysData period fromDay toDay
            logTagInfo "PeriodFallbackRes" (show res)
            pure (res.activeDriver, res.driverEnabled, res.greaterThanOneRide, res.greaterThanTenRide, res.greaterThanFiftyRide)

      pure $ API.Types.ProviderPlatform.Operator.Driver.FilteredOperatorAnalyticsRes {activeDriver, driverEnabled, greaterThanOneRide, greaterThanTenRide, greaterThanFiftyRide}
