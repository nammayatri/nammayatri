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
    inferPeriod,
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
import qualified SharedLogic.Analytics as Analytics
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import qualified SharedLogic.DriverFlowStatus as SDFStatus
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.Beam.SystemConfigs ()
import Storage.Cac.TransporterConfig (findByMerchantOpCityId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Clickhouse.DailyStats as CDaily
import qualified Storage.Clickhouse.DriverInformation as CDI
import qualified Storage.Queries.DriverInformation as QDI
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
  mbMobileNumberHash <- mapM getDbHash mbMobileNumber
  reqList <- SQOH.findAllRequestsInRange from to limit offset mbMobileNumberHash (castReqStatusToDomain <$> mbStatus) (castReqTypeToDomain <$> mbReqType) mbDriverId mbOperationHubId mbOperationHubName mbRegistrationNo
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
    castOpsHubReq API.Types.ProviderPlatform.Operator.Driver.DriverOperationHubRequest {..} = DomainT.DriverOperationHubRequest {operationHubId = cast operationHubId, requestType = castReqTypeToDomain requestType, ..}

postDriverOperatorRespondHubRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Operator.Driver.RespondHubRequest -> Environment.Flow APISuccess)
postDriverOperatorRespondHubRequest merchantShortId opCity req = withLogTag ("operationHubRequestId_" <> req.operationHubRequestId) $ do
  now <- getCurrentTime
  Redis.whenWithLockRedis (opsHubRequestLockKey req.operationHubRequestId) 60 $ do
    opHubReq <- SQOHR.findByPrimaryKey (Kernel.Types.Id.Id req.operationHubRequestId) >>= fromMaybeM (InvalidRequest "Invalid operation hub request id")
    when (opHubReq.requestStatus == APPROVED) $ Kernel.Utils.Common.throwError (InvalidRequest "Request already approved")
    when (req.status == API.Types.ProviderPlatform.Operator.Driver.REJECTED && opHubReq.requestType == ONBOARDING_INSPECTION) $ do
      void $ SQOHR.updateStatusWithDetails (castReqStatusToDomain req.status) (Just req.remarks) (Just now) (Just (Kernel.Types.Id.Id req.operatorId)) (Kernel.Types.Id.Id req.operationHubRequestId)
    when (req.status == API.Types.ProviderPlatform.Operator.Driver.APPROVED && opHubReq.requestType == ONBOARDING_INSPECTION) $ do
      creator <- runInReplica $ QPerson.findById opHubReq.creatorId >>= fromMaybeM (PersonNotFound opHubReq.creatorId.getId)
      rc <- QVRCE.findLastVehicleRCWrapper opHubReq.registrationNo >>= fromMaybeM (RCNotFound opHubReq.registrationNo)
      personId <- case creator.role of
        DP.DRIVER -> pure creator.id
        _ -> do
          drc <- SQDRA.findAllActiveAssociationByRCId rc.id
          case drc of
            [] -> throwError (InvalidRequest "No driver exist with this RC")
            (assoc : _) -> pure assoc.driverId
      merchant <- findMerchantByShortId merchantShortId
      merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
      transporterConfig <- findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId) -- (Just (DriverId (cast personId)))
      person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      let language = fromMaybe merchantOpCity.language person.language
      fork "enable driver after inspection" $ do
        allDriverVehicleDocsVerified <- SStatus.checkAllDriverVehicleDocsVerified person merchantOpCity transporterConfig language opHubReq.registrationNo
        when allDriverVehicleDocsVerified $ do
          QVRC.updateApproved (Just True) rc.id
          void $ postDriverEnable merchantShortId opCity $ cast @DP.Person @Common.Driver personId
        let reqUpdatedStatus = if allDriverVehicleDocsVerified then castReqStatusToDomain req.status else PENDING
        void $ SQOHR.updateStatusWithDetails reqUpdatedStatus (Just req.remarks) (Just now) (Just (Kernel.Types.Id.Id req.operatorId)) (Kernel.Types.Id.Id req.operationHubRequestId)
        mbVehicle <- QVehicle.findById personId
        when (isNothing mbVehicle && allDriverVehicleDocsVerified) $
          void $ try @_ @SomeException (SStatus.activateRCAutomatically personId merchantOpCity opHubReq.registrationNo)
  pure Success

opsHubRequestLockKey :: Text -> Text
opsHubRequestLockKey reqId = "opsHub:Request:Id-" <> reqId

castHubRequests :: (OperationHubRequests, DP.Person, DOH.OperationHub) -> Environment.Flow API.Types.ProviderPlatform.Operator.Driver.OperationHubDriverRequest
castHubRequests (hubReq, creator, hub) = do
  creatorPhoneNo <- mapM decrypt creator.mobileNumber
  mbRc <- QVRCE.findLastVehicleRCWrapper hubReq.registrationNo
  (driverPhoneNo, rcId) <- case mbRc of
    Just rc -> do
      drc <- SQDRA.findAllActiveAssociationByRCId rc.id
      case listToMaybe drc of
        Just assoc -> do
          QPerson.findById assoc.driverId >>= \case
            Just person -> do
              number <- mapM decrypt person.mobileNumber
              pure (number, Just rc.id.getId)
            Nothing -> pure (Nothing, Just rc.id.getId)
        Nothing -> pure (Nothing, Just rc.id.getId)
    Nothing -> pure (Nothing, Nothing)
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
          <$> SStatus.statusHandler' driverImagesInfo Nothing Nothing Nothing Nothing Nothing (Just True) shouldActivateRc onlyMandatoryDocs -- FIXME: Need to change
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
          (mbSender, message, templateId) <-
            MessageBuilder.buildOperatorJoiningMessage merchantOpCityId $
              MessageBuilder.BuildOperatorJoiningMessageReq
                { otp = otpCode,
                  operatorName = operatorName
                }
          let sender = fromMaybe smsCfg.sender mbSender
          Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId) >>= Sms.checkSmsResult
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

      verifyAndAssociateDriverWithOperator merchant merchantOpCityId operator res.person (transporterConfig.allowCacheDriverFlowStatus == Just True)

      DOR.makeDriverReferredByOperator merchantOpCityId person.id operator.id

      let phoneNumber = req.mobileCountryCode <> req.mobileNumber
      withLogTag ("personId_" <> getId person.id) $ do
        (mbSender, message, templateId) <-
          MessageBuilder.buildOperatorJoinAndDownloadAppMessage merchantOpCityId $
            MessageBuilder.BuildOperatorJoinAndDownloadAppMessageReq
              { operatorName = operator.firstName
              }
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId)
          >>= Sms.checkSmsResult
    Nothing -> do
      let key = makeOperatorDriverOtpKey (req.mobileCountryCode <> req.mobileNumber)
      otp <- Redis.get key >>= fromMaybeM OtpNotFound
      when (otp /= req.otp) $ throwError InvalidOtp

      SA.endDriverAssociationsIfAllowed merchant merchantOpCityId transporterConfig person

      verifyAndAssociateDriverWithOperator merchant merchantOpCityId operator person (transporterConfig.allowCacheDriverFlowStatus == Just True)

  pure Success
  where
    verifyAndAssociateDriverWithOperator merchant merchantOpCityId operator person allowCache = do
      checkAssocOperator <-
        B.runInReplica $
          QDOA.findByDriverIdAndOperatorId person.id operator.id True
      when (isJust checkAssocOperator) $
        throwError (InvalidRequest "Driver already associated with operator")

      assoc <- SA.makeDriverOperatorAssociation merchant.id merchantOpCityId person.id operator.id.getId (DomainRC.convertTextToUTC (Just "2099-12-12"))
      QDOA.create assoc
      when allowCache $ do
        driverInfo <- QDI.findById person.id >>= fromMaybeM (DriverNotFound person.id.getId)
        DDriverMode.incrementFleetOperatorStatusKeyForDriver DP.OPERATOR operator.id.getId driverInfo.driverFlowStatus

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
getDriverOperatorDashboardAnalyticsAllTime _merchantShortId _opCity requestorId = do
  operator <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
  unless (operator.role == DP.OPERATOR) $
    throwError AccessDenied

  -- Redis keys for fleet aggregates
  let totalRideKey = Analytics.makeOperatorAnalyticsKey operator.id.getId "totalRideCount"
      ratingSumKey = Analytics.makeOperatorAnalyticsKey operator.id.getId "ratingSum"
      cancelCountKey = Analytics.makeOperatorAnalyticsKey operator.id.getId "cancelCount"

  -- try redis first
  mbTotalRides <- Redis.get @Int totalRideKey
  mbRatingSum <- Redis.get @Int ratingSumKey
  mbCancelCount <- Redis.get @Int cancelCountKey

  -- fallback to ClickHouse and populate cache when missing
  (totalRides, ratingSum, cancelCount) <- do
    case (mbTotalRides, mbRatingSum, mbCancelCount) of
      (Just tr, Just rs, Just cc) -> pure (tr, rs, cc)
      _ -> Analytics.fallbackToClickHouseAndUpdateRedisForAllTime operator.id.getId totalRideKey ratingSumKey cancelCountKey

  let acceptationCount = 0 :: Int -- TODO: Implement this
  logTagInfo "Total rides" (show totalRides)
  logTagInfo "Rating sum" (show ratingSum)
  logTagInfo "Cancel count" (show cancelCount)
  logTagInfo "Acceptation count" (show acceptationCount)
  let (rating, cancellationRate, acceptanceRate) = if totalRides > 0 then (fromIntegral ratingSum / fromIntegral totalRides, (fromIntegral cancelCount / fromIntegral totalRides) * 100, (fromIntegral acceptationCount / fromIntegral totalRides) * 100) else (0, 0, 0)
  pure $ API.Types.ProviderPlatform.Operator.Driver.AllTimeOperatorAnalyticsRes {rating, cancellationRate, acceptanceRate}

---------------------------------------------------------------------
getDriverOperatorDashboardAnalytics ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  UTCTime ->
  UTCTime ->
  Flow API.Types.ProviderPlatform.Operator.Driver.FilteredOperatorAnalyticsRes
getDriverOperatorDashboardAnalytics _merchantShortId _opCity requestorId from to = do
  operator <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
  unless (operator.role == DP.OPERATOR) $ throwError AccessDenied

  now <- getCurrentTime
  let period = inferPeriod now from to
      kTotalApplicationCount = Analytics.makeOperatorPeriodicKey operator.id.getId "totalApplicationCount" (show period)
      kDriverEnabled = Analytics.makeOperatorPeriodicKey operator.id.getId "driverEnabled" (show period)
      kGtOne = Analytics.makeOperatorPeriodicKey operator.id.getId "greaterThanOneRide" (show period)
      kGtTen = Analytics.makeOperatorPeriodicKey operator.id.getId "greaterThanTenRide" (show period)
      kGtFifty = Analytics.makeOperatorPeriodicKey operator.id.getId "greaterThanFiftyRide" (show period)
  logTagInfo "kTotalApplicationCount:-" kTotalApplicationCount
  logTagInfo "kDriverEnabled:-" kDriverEnabled
  logTagInfo "kGtOne:-" kGtOne
  logTagInfo "kGtTen:-" kGtTen
  logTagInfo "kGtFifty:-" kGtFifty

  case period of
    Nothing -> do
      (appCount, enabledCount, gt1, gt10, gt50) <- fallbackCompute operator.id.getId
      pure $
        API.Types.ProviderPlatform.Operator.Driver.FilteredOperatorAnalyticsRes
          { totalApplicationCount = appCount,
            driverEnabled = enabledCount,
            greaterThanOneRide = gt1,
            greaterThanTenRide = gt10,
            greaterThanFiftyRide = gt50
          }
    Just _ -> do
      -- attempt to read cached values
      mbApp <- Redis.get @Int kTotalApplicationCount
      mbEnabled <- Redis.get @Int kDriverEnabled
      mbGt1 <- Redis.get @Int kGtOne
      mbGt10 <- Redis.get @Int kGtTen
      mbGt50 <- Redis.get @Int kGtFifty

      (totalApplicationCount, driverEnabled, greaterThanOneRide, greaterThanTenRide, greaterThanFiftyRide) <- do
        case (mbApp, mbEnabled, mbGt1, mbGt10, mbGt50) of
          (Just a, Just e, Just g1, Just g10, Just g50) -> pure (a, e, g1, g10, g50)
          _ -> do
            (appCount, enabledCount, gt1, gt10, gt50) <- fallbackCompute operator.id.getId
            -- write back to Redis
            void $ Redis.set kTotalApplicationCount appCount
            void $ Redis.set kDriverEnabled enabledCount
            void $ Redis.set kGtOne gt1
            void $ Redis.set kGtTen gt10
            void $ Redis.set kGtFifty gt50
            pure (appCount, enabledCount, gt1, gt10, gt50)

      pure $ API.Types.ProviderPlatform.Operator.Driver.FilteredOperatorAnalyticsRes {totalApplicationCount, driverEnabled, greaterThanOneRide, greaterThanTenRide, greaterThanFiftyRide}
  where
    fallbackCompute :: Text -> Flow (Int, Int, Int, Int, Int)
    fallbackCompute operatorId = do
      driverIds <- SDFStatus.getFleetDriverIdsAndDriverIdsByOperatorId operatorId
      -- totalApplicationCount = active driver-operator associations and active driver-fleet-operator associations
      appCount <- SDFStatus.getTotalFleetDriverAndDriverCountByOperatorIdInDateRange operatorId from to
      -- driverEnabled = count of driver_info.enabled == true among these drivers
      enabledCount <- CDI.getEnabledDriverCountByDriverIds driverIds from to
      -- thresholds via daily_stats (sum numRides in [from,to] on merchantLocalDate)
      let fromDay = utctDay from
          toDay = utctDay to
      gt1 <- CDaily.countDriversWithNumRidesGreaterThan1Between driverIds fromDay toDay
      gt10 <- CDaily.countDriversWithNumRidesGreaterThan10Between driverIds fromDay toDay
      gt50 <- CDaily.countDriversWithNumRidesGreaterThan50Between driverIds fromDay toDay
      pure (appCount, enabledCount, gt1, gt10, gt50)

inferPeriod :: UTCTime -> UTCTime -> UTCTime -> Maybe Period
inferPeriod now from to =
  let (y, m, d) = toGregorian (utctDay now)
      todayStart = UTCTime (fromGregorian y m d) 0
      -- week starts on Monday
      dow :: Int = case dayOfWeek (utctDay now) of
        Monday -> 0
        Tuesday -> 1
        Wednesday -> 2
        Thursday -> 3
        Friday -> 4
        Saturday -> 5
        Sunday -> 6
      thisWeekStart = addUTCTime (negate (fromIntegral dow * 86400)) todayStart
      firstOfMonth = UTCTime (fromGregorian y m 1) 0
      yesterdayStart = addUTCTime (-86400) todayStart
      lastWeekStart = addUTCTime (-7 * 86400) thisWeekStart
      lastMonthStart = UTCTime (addGregorianMonthsClip (-1) (fromGregorian y m 1)) 0
   in if from == todayStart && to <= now && to >= todayStart
        then Just Today
        else
          if from == thisWeekStart && to <= now && to >= todayStart
            then Just ThisWeek
            else
              if from == firstOfMonth && to <= now && to >= todayStart
                then Just ThisMonth
                else
                  if from == yesterdayStart && to == addUTCTime (-1) todayStart
                    then Just Yesterday
                    else
                      if from == lastWeekStart && to == addUTCTime (-1) thisWeekStart
                        then Just LastWeek
                        else
                          if from == lastMonthStart && to == addUTCTime (-1) firstOfMonth
                            then Just LastMonth
                            else Nothing

data Period = Today | ThisWeek | ThisMonth | Yesterday | LastWeek | LastMonth
  deriving (Show, Eq)
