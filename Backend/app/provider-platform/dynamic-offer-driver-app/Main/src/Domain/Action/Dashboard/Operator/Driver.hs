{-# OPTIONS_GHC -Wno-orphans #-}

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
    getDriverReviewQueueRequest,
    postDriverSubmitReviewRequest,
    reviewRequestLockKey,
    getDriverRequestReviewHistory,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as CommonFleet
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified API.Types.ProviderPlatform.Operator.Driver
import qualified API.Types.ProviderPlatform.Operator.Endpoints.Driver as CommonDriver
import qualified API.Types.UI.OperationHub as DomainT
import Dashboard.ProviderPlatform.Operator.Driver ()
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.Dashboard.Fleet.Driver as DFDriver
import Domain.Action.Dashboard.Fleet.Onboarding (castStatusRes)
import Domain.Action.Dashboard.Management.DriverRegistration (mapDocumentType, sendDocumentRejectionNotification)
import Domain.Action.Dashboard.RideBooking.Driver
import qualified Domain.Action.Dashboard.RideBooking.DriverRegistration as DRBReg
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Action.UI.DriverOnboarding.CourtRecordCheck as CourtRecordCheck
import qualified Domain.Action.UI.DriverOnboarding.Referral as DOR
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.OperationHub as Domain
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.AadhaarCard as DAadhaarCard
import qualified Domain.Types.Common as DC
import qualified Domain.Types.DocumentReminderHistory as DRH
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverGstin as DDGST
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.DriverLicense as DDL
import qualified Domain.Types.DriverPanCard as DDPC
import qualified Domain.Types.DriverSSN as DDSSN
import qualified Domain.Types.DriverUdyam as DDUDYAM
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.OperationHub as DOH
import Domain.Types.OperationHubRequests
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as SR
import qualified Domain.Types.ReviewRequest as DRR
import qualified Domain.Types.SubscriptionPurchase as DSP
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Environment
import EulerHS.Prelude (whenNothing_, (<|>))
import Kernel.Beam.Functions as B
import qualified Kernel.External.ChallanSearch.Interface.Types as ChallanSearchTypes
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import Kernel.Sms.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import SharedLogic.Analytics as Analytics
import SharedLogic.AnalyticsExtra as AnalyticsExtra
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import qualified SharedLogic.DriverOnboarding as SDO
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import qualified SharedLogic.DriverOnboarding.VehicleDocs as VDocs
import qualified SharedLogic.FleetOperatorStats as FleetOpStats
import SharedLogic.MediaFileDocument (finalizeInspectionMedia)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import SharedLogic.Reminder.Helper (cancelRemindersForDriverByDocumentType, cancelRemindersForRCByDocumentType, recordDocumentCompletion)
import Storage.Beam.SystemConfigs ()
import Storage.Cac.TransporterConfig (findByMerchantOpCityId)
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as CQFODVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.DocumentVerificationConfig (DocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.FleetOwnerDocumentVerificationConfig (FleetOwnerDocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DriverGstin as QDGST
import qualified Storage.Queries.DriverIdentityInfo as QDII
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.DriverLicense as DLQuery
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.DriverPanCard as QDPC
import qualified Storage.Queries.DriverRCAssociation as QRCAssoc
import qualified Storage.Queries.DriverRCAssociationExtra as QDRC
import qualified Storage.Queries.DriverRCAssociationExtra as SQDRA
import qualified Storage.Queries.DriverSSN as QDSSN
import qualified Storage.Queries.DriverUdyam as QUDYAM
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOIExtra
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.OperationHub as QOH
import qualified Storage.Queries.OperationHubRequests as SQOHR
import qualified Storage.Queries.OperationHubRequestsExtra as SQOH
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ReviewRequest as QRR
import qualified Storage.Queries.ReviewRequestExtra as SQRRExtra
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubscriptionPurchaseExtra
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as QVRC
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as QVRCE
import qualified Tools.ChallanSearch as ChallanSearch
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
  Maybe Text ->
  Maybe (Id CommonDriver.OperationHub) ->
  Maybe Text ->
  Maybe Text ->
  Environment.Flow API.Types.ProviderPlatform.Operator.Driver.OperationHubReqResp
getDriverOperatorFetchHubRequests _merchantShortId _opCity mbFrom mbTo mbStatus mbReqType mbLimit mbOffset mbDriverId mbMobileNumber mbDriverMobileNumber mbReqOperationHubId mbOperationHubName mbRegistrationNo = do
  now <- getCurrentTime
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
      defaultFrom = UTCTime (fromGregorian 2020 1 1) 0
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
      mbOperationHubId = cast @CommonDriver.OperationHub @DOH.OperationHub <$> mbReqOperationHubId
      mbDriverIdPerson = (Id :: Text -> Id DP.Person) <$> mbDriverId
  mbMobileNumberHash <- mapM getDbHash mbMobileNumber
  mbDriverMobileNumberHash <- mapM getDbHash mbDriverMobileNumber
  reqList <- SQOH.findAllRequestsInRange from to limit offset mbMobileNumberHash mbDriverMobileNumberHash (castReqStatusToDomain <$> mbStatus) (castReqTypeToDomain <$> mbReqType) Nothing mbOperationHubId mbOperationHubName mbRegistrationNo mbDriverIdPerson
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
    let reqDomainStatus = castReqStatusToDomain req.status
    case (opHubReq.requestStatus, reqDomainStatus) of
      (existing, target)
        | existing == target ->
          logInfo $ "Hub request already in status " <> show existing <> "; returning idempotent success"
      (APPROVED, _) ->
        Kernel.Utils.Common.throwError (InvalidRequest "Request has already been approved; approval is terminal")
      (REJECTED, _) ->
        Kernel.Utils.Common.throwError (InvalidRequest "Request has already been rejected; rejection is terminal")
      _ -> do
        (merchantOpCity, transporterConfig) <- getMerchantInfo merchantShortId opCity
        -- Persist the per-request inspection checklist snapshot (answers + media ids) for audit/history, and
        -- validate + finalize each referenced media (must be confirmed, present, in-size, unchanged) -> COMPLETED,
        -- so the cleanup job keeps it (un-referenced / un-confirmed uploads are swept from S3).
        whenJust req.responses $ \responses -> do
          forM_ responses $ \r -> whenJust r.mediaFileId $ \mfId -> finalizeInspectionMedia merchantOpCity.id (Kernel.Types.Id.Id mfId)
          let inspectionResponseItems =
                map
                  ( \r ->
                      InspectionResponseItem
                        { questionId = r.questionId,
                          question = r.question,
                          answer = r.answer,
                          mediaFileId = r.mediaFileId
                        }
                  )
                  responses
          void $ SQOHR.updateInspectionResponse (Just inspectionResponseItems) (Kernel.Types.Id.Id req.operationHubRequestId)
        -- Handle rejection based on request type
        when (req.status == API.Types.ProviderPlatform.Operator.Driver.REJECTED) $ do
          void $ SQOHR.updateStatusWithDetails reqDomainStatus (Just req.remarks) (Just now) (Just (Kernel.Types.Id.Id req.operatorId)) (Kernel.Types.Id.Id req.operationHubRequestId)
          when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
            let analyticsDriverId = maybe req.operatorId (.getId) opHubReq.driverId
            case opHubReq.requestType of
              ONBOARDING_INSPECTION ->
                void $
                  withTryCatch "incrementRejectedVehicleRequestsDaily" $
                    FleetOpStats.incrementRejectedVehicleRequestsDaily req.operatorId analyticsDriverId transporterConfig
              REGULAR_INSPECTION ->
                void $
                  withTryCatch "incrementRejectedVehicleRequestsDaily" $
                    FleetOpStats.incrementRejectedVehicleRequestsDaily req.operatorId analyticsDriverId transporterConfig
              DRIVER_ONBOARDING_INSPECTION -> do
                personId <- opHubReq.driverId & fromMaybeM (InvalidRequest "driverId is required for driver inspection")
                void $
                  withTryCatch "incrementRejectedDriverRequestsDaily" $
                    FleetOpStats.incrementRejectedDriverRequestsDaily req.operatorId personId.getId transporterConfig
              DRIVER_REGULAR_INSPECTION -> do
                personId <- opHubReq.driverId & fromMaybeM (InvalidRequest "driverId is required for driver inspection")
                void $
                  withTryCatch "incrementRejectedDriverRequestsDaily" $
                    FleetOpStats.incrementRejectedDriverRequestsDaily req.operatorId personId.getId transporterConfig

        when (req.status == API.Types.ProviderPlatform.Operator.Driver.APPROVED) $ do
          case opHubReq.requestType of
            ONBOARDING_INSPECTION -> handleVehicleInspectionApproval req opHubReq merchantOpCity transporterConfig now
            REGULAR_INSPECTION -> handleVehicleInspectionApproval req opHubReq merchantOpCity transporterConfig now
            DRIVER_ONBOARDING_INSPECTION -> do
              personId <- opHubReq.driverId & fromMaybeM (InvalidRequest "driverId is required for driver inspection")
              handleDriverInspectionApproval merchantShortId opCity req now personId merchantOpCity transporterConfig
            DRIVER_REGULAR_INSPECTION -> do
              personId <- opHubReq.driverId & fromMaybeM (InvalidRequest "driverId is required for driver inspection")
              handleDriverInspectionApproval merchantShortId opCity req now personId merchantOpCity transporterConfig
  pure Success
  where
    handleVehicleInspectionApproval request opHubReq merchantOpCity transporterConfig now = do
      creator <- runInReplica $ QPerson.findById opHubReq.creatorId >>= fromMaybeM (PersonNotFound opHubReq.creatorId.getId)
      registrationNo <- opHubReq.registrationNo & fromMaybeM (InvalidRequest "registrationNo is required for vehicle inspection")

      rc <- QVRCE.findLastVehicleRCWrapper registrationNo >>= fromMaybeM (RCNotFound registrationNo)
      mbPersonId <- case creator.role of
        DP.DRIVER -> pure $ Just creator.id
        _ -> do
          drc <- SQDRA.findAllActiveAssociationByRCId rc.id
          case drc of
            [] -> pure Nothing
            (assoc : _) -> pure $ Just assoc.driverId
      mbPerson <- forM mbPersonId $ \personId -> runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      language <- getPersonInfo merchantOpCity mbPerson
      let analyticsDriverId = maybe request.operatorId (.getId) mbPersonId
      -- Recording the inspection (→ InspectionHub VALID) runs for all cities. Under enableBotFlow the
      -- BOT owns `approved` and statusHandler owns RC activation, so those are skipped; non-BOT ops-hub
      -- cities keep them (ops-approve is their approval step).
      let enableBotFlow = transporterConfig.enableBotFlow == Just True
      -- BOT: approve once dependency docs are VALID (fetch once, reuse in the fork). Non-BOT: full check.
      (mbVehicleDocs, allVehicleDocsVerified) <-
        if enableBotFlow
          then do
            (vehicleDocItem, allDocumentVerificationConfigs) <- SStatus.fetchVehicleDocStatusesForRC rc merchantOpCity transporterConfig language registrationNo Nothing False True
            let vehicleCategory = fromMaybe vehicleDocItem.userSelectedVehicleCategory vehicleDocItem.verifiedVehicleCategory
            -- Vehicle docs: no fleet-driver/individual applicableTo split → Nothing, [].
            -- Throws (naming the offending docs) if any InspectionHub dependency doc isn't VALID.
            invalidInspectionDeps <- inspectionInvalidDependencyDocs merchantOpCity.id DVC.InspectionHub Nothing [] vehicleCategory vehicleDocItem.documents
            unless (null invalidInspectionDeps) $
              throwError (InvalidRequest $ "Cannot approve: InspectionHub dependency documents not valid: " <> T.intercalate ", " (map show invalidInspectionDeps))
            pure (Just (vehicleDocItem, allDocumentVerificationConfigs), True)
          else (Nothing,) <$> SStatus.fetchAndCheckVehicleDocsValidForEnabling rc merchantOpCity transporterConfig language registrationNo
      when allVehicleDocsVerified $ do
        if enableBotFlow
          then -- BOT: mark RC verified now, then reconcile in the fork.
          whenJust mbVehicleDocs $ \(vehicleDocItem, allDocumentVerificationConfigs) -> do
            SStatus.forkRecomputeVehicleVerified registrationNo vehicleDocItem allDocumentVerificationConfigs
          else QVRC.updateApproved (Just True) rc.id
        -- Cancel pending vehicle inspection reminders for all drivers using this RC
        cancelRemindersForRCByDocumentType rc.id DVC.InspectionHub
        -- Record inspection completion for auto-trigger monitoring (per RC, not per driver)
        recordDocumentCompletion DVC.InspectionHub rc.id.getId DRH.RC Nothing merchantOpCity.merchantId merchantOpCity.id
        when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $
          void $
            withTryCatch "incrementApprovedVehicleRequests" $
              FleetOpStats.incrementApprovedVehicleRequests request.operatorId analyticsDriverId transporterConfig
      let reqUpdatedStatus = if allVehicleDocsVerified then castReqStatusToDomain request.status else PENDING
      void $ SQOHR.updateStatusWithDetails reqUpdatedStatus (Just request.remarks) (Just now) (Just (Kernel.Types.Id.Id request.operatorId)) (Kernel.Types.Id.Id request.operationHubRequestId)

      -- BOT-flow only: the pending-challan result is consumed by the BOT, so don't call the paid 3P API
      when (enableBotFlow && allVehicleDocsVerified) $
        fork "fetchPendingChallanCount" $ do
          let challanReq = ChallanSearchTypes.PendingChallanReq {vehicleNumber = registrationNo}
          result <- withTryCatch "fetchPendingChallanCount" $ ChallanSearch.getPendingChallanCount merchantOpCity.id challanReq
          case result of
            Right challanResp -> do
              QVRC.updatePendingChallan (Just DVRC.PendingChallanResult {pendingChallanCount = Just challanResp.pendingChallanCount, errorMessage = Nothing}) rc.id
              logInfo $ "Pending challan count for RC " <> registrationNo <> ": " <> show challanResp.pendingChallanCount
            Left err ->
              QVRC.updatePendingChallan (Just DVRC.PendingChallanResult {pendingChallanCount = Nothing, errorMessage = Just (show err)}) rc.id

      -- RC auto-activation is legacy-only; under enableBotFlow the RC is activated by the BOT/statusHandler.
      unless enableBotFlow $
        whenJust mbPersonId $ \personId -> do
          mbVehicle <- QVehicle.findById personId
          when (isNothing mbVehicle && allVehicleDocsVerified) $
            void $ withTryCatch "activateRCAutomatically:postDriverOperatorRespondHubRequest" (SStatus.activateRCAutomatically personId merchantOpCity registrationNo)

    handleDriverInspectionApproval mShortId city request now personId merchantOpCity transporterConfig = do
      person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      language <- getPersonInfo merchantOpCity (Just person)
      -- Recording the inspection (→ DriverInspectionHub VALID) runs for all cities. Under enableBotFlow
      -- the BOT owns `approved` and statusHandler owns enablement, so those are skipped; non-BOT ops-hub
      -- cities keep them (ops-approve is their approval step).
      let enableBotFlow = transporterConfig.enableBotFlow == Just True
      -- BOT: approve once dependency docs are VALID (just marks APPROVED; verified/enabled is owned by
      -- the BOT/statusHandler flow). Non-BOT: full enabling check.
      allDriverDocsVerified <-
        if enableBotFlow
          then do
            (allDocVerificationConfigs, driverDocStatuses, vehicleCategory) <- SStatus.fetchDriverDocStatusesForPerson person merchantOpCity transporterConfig language Nothing
            isFleetDriver <- SStatus.hasActiveFleetAssociation person.id
            let driverConfigs = case allDocVerificationConfigs of Right cs -> cs; Left _ -> []
            -- Throws (naming the offending docs) if any DriverInspectionHub dependency doc isn't VALID.
            invalidInspectionDeps <- inspectionInvalidDependencyDocs merchantOpCity.id DVC.DriverInspectionHub (Just isFleetDriver) driverConfigs vehicleCategory driverDocStatuses
            unless (null invalidInspectionDeps) $
              throwError (InvalidRequest $ "Cannot approve: DriverInspectionHub dependency documents not valid: " <> T.intercalate ", " (map show invalidInspectionDeps))
            pure True
          else SStatus.fetchAndCheckDriverDocsValidForEnabling person merchantOpCity transporterConfig language
      when allDriverDocsVerified $ do
        unless enableBotFlow $ do
          QDIExtra.updateApproved (Just True) personId
          void $ postDriverEnable mShortId city $ cast @DP.Person @Common.Driver personId
        -- Cancel pending driver inspection reminders
        cancelRemindersForDriverByDocumentType personId DVC.DriverInspectionHub
        -- Record driver inspection completion for auto-trigger monitoring
        recordDocumentCompletion DVC.DriverInspectionHub personId.getId DRH.DRIVER (Just personId) merchantOpCity.merchantId merchantOpCity.id
        -- Court Record Check (CRC) — submit on inspection approval; Idfy pushes the result to the webhook.
        -- BOT-flow only: the CRC result is consumed by the BOT, so it is pointless to call/store this
        -- paid Idfy check when the BOT is off. Also gated per-merchant via enableCourtRecordCheck (MSIL).
        -- Forked so it never blocks or fails the approval/enable path.
        when (enableBotFlow && transporterConfig.enableCourtRecordCheck == Just True) $
          fork "courtRecordCheck" $
            void $
              withTryCatch "courtRecordCheck" $
                CourtRecordCheck.runCourtRecordCheck person merchantOpCity
        when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $
          void $
            withTryCatch "incrementApprovedDriverRequestsDaily" $
              FleetOpStats.incrementApprovedDriverRequestsDaily request.operatorId personId.getId transporterConfig
      let reqUpdatedStatus = if allDriverDocsVerified then castReqStatusToDomain request.status else PENDING
      void $ SQOHR.updateStatusWithDetails reqUpdatedStatus (Just request.remarks) (Just now) (Just (Kernel.Types.Id.Id request.operatorId)) (Kernel.Types.Id.Id request.operationHubRequestId)

    -- Returns the inspection-hub config's dependency docs that are NOT VALID (empty ⇒ all valid). Drives APPROVED vs PENDING.
    -- On the driver side a dep counts only if it applies per dvc `applicableTo` (a fleet driver skips
    -- INDIVIDUAL-only deps like OperatorPartnerCode); pass Nothing/[] for vehicle docs (no applicableTo split).
    inspectionInvalidDependencyDocs merchantOpCityId inspectionHubDocType mbIsFleetDriver driverConfigs vehicleCategory docStatuses = do
      mbInspectionCfg <- getOneConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Just inspectionHubDocType, vehicleCategory = Just vehicleCategory}) (Just (maybeToList <$> CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId inspectionHubDocType vehicleCategory Nothing))
      pure $ case mbInspectionCfg of
        Nothing -> []
        Just inspectionCfg -> SStatus.invalidDependencyDocs mbIsFleetDriver driverConfigs inspectionCfg.dependencyDocumentType docStatuses

    getMerchantInfo mShortId city = do
      merchant <- findMerchantByShortId mShortId
      merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> mShortId.getShortId <> " ,city: " <> show city)
      transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCity.id Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
      pure (merchantOpCity, transporterConfig)

    getPersonInfo merchantOpCity mbPerson = do
      let language = fromMaybe merchantOpCity.language $ mbPerson >>= (.language)
      pure (language)

opsHubRequestLockKey :: Text -> Text
opsHubRequestLockKey reqId = "opsHub:Request:Id-" <> reqId

reviewRequestLockKey :: Text -> Text
reviewRequestLockKey reqId = "ops:reviewQueue:Id-" <> reqId

castHubRequests :: (OperationHubRequests, Maybe DP.Person, DOH.OperationHub) -> Environment.Flow API.Types.ProviderPlatform.Operator.Driver.OperationHubDriverRequest
castHubRequests (hubReq, mbCreator, hub) = do
  creatorPhoneNo <- maybe (pure Nothing) (\creator -> mapM decrypt creator.mobileNumber) mbCreator
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
        driverId = fmap (cast @DP.Person @Common.Driver) hubReq.driverId,
        requestStatus = castReqStatus hubReq.requestStatus,
        requestTime = hubReq.createdAt,
        requestType = castReqType hubReq.requestType,
        remarks = hubReq.remarks,
        inspectionResponse =
          map
            ( \r ->
                API.Types.ProviderPlatform.Operator.Driver.InspectionResponseItem
                  { questionId = r.questionId,
                    question = r.question,
                    answer = r.answer,
                    mediaFileId = r.mediaFileId
                  }
            )
            <$> hubReq.inspectionResponse
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
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe CommonFleet.DriverMode ->
  Kernel.Prelude.Text ->
  Environment.Flow API.Types.ProviderPlatform.Operator.Driver.DriverInfoResp
getDriverOperatorList _merchantShortId _opCity mbIsActive mbLimit mbOffset mbVehicleNo mbSearchString mbIncludeDocuments onlyMandatoryDocs mbStatus requestorId = do
  requestor <- QPerson.findById (Id requestorId) >>= fromMaybeM (PersonNotFound requestorId)
  unless (requestor.role == DP.OPERATOR) $
    Kernel.Utils.Common.throwError (InvalidRequest "Requestor role is not OPERATOR")
  now <- getCurrentTime
  let mbMode = castApiDriverMode <$> mbStatus
  driverOperatorInfoList <- case mbVehicleNo of
    Nothing -> do
      driverOperatorAssociationAndPersonLs <- QDOA.findAllByOperatorIdWithLimitOffsetSearch requestorId mbIsActive mbLimit mbOffset mbSearchString Nothing mbMode
      forM driverOperatorAssociationAndPersonLs \(drvOpAsn, person, driverMode) -> do
        let driverId = drvOpAsn.driverId
        (vehicleModel, registrationNo, isRcActive) <- fetchVehicleDetailsByDriverId now driverId
        pure (drvOpAsn, person, vehicleModel, registrationNo, isRcActive, driverMode)
    Just vehicleNo -> (maybeToList <$>) . runMaybeT $ do
      (vehicleModel, registrationNo, isRcActive, driverId) <- fetchVehicleDetailsByVehicleNo now vehicleNo
      (drvOpAsn, person, driverMode) <- MaybeT $ listToMaybe <$> QDOA.findAllByOperatorIdWithLimitOffsetSearch requestorId mbIsActive mbLimit mbOffset mbSearchString (Just driverId) mbMode
      pure (drvOpAsn, person, vehicleModel, registrationNo, isRcActive, driverMode)

  driverInfoList <- QDI.findAllByDriverIds (map (\(drvOpAsn, _, _, _, _, _) -> drvOpAsn.driverId.getId) driverOperatorInfoList)
  let driverInfoMap = HashMap.fromList [(driverInfo.driverId.getId, driverInfo) | driverInfo <- driverInfoList]
  listItem <- mapM (buildDriverInfo now mbIncludeDocuments driverInfoMap) driverOperatorInfoList
  let count = length listItem
  let summary = Common.Summary {totalCount = 10000, count}
  pure API.Types.ProviderPlatform.Operator.Driver.DriverInfoResp {..}
  where
    castApiDriverMode :: CommonFleet.DriverMode -> DC.DriverMode
    castApiDriverMode CommonFleet.ONLINE = DC.ONLINE
    castApiDriverMode CommonFleet.OFFLINE = DC.OFFLINE
    castApiDriverMode CommonFleet.SILENT = DC.SILENT

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

    buildDriverInfo now mbIncDocs driverInfoMap (drvOpAsn, person, vehicleModel, registrationNo, isRcActive, driverMode) = do
      decryptedMobileNumber <-
        mapM decrypt person.mobileNumber
          >>= fromMaybeM
            ( InvalidRequest $
                "Person do not have a mobile number " <> person.id.getId
            )
      let merchantOpCityId = person.merchantOperatingCityId
      transporterConfig <-
        getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing))
          >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      merchantOpCity <-
        CQMOC.findById merchantOpCityId
          >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)

      let shouldIncludeDocs = fromMaybe True mbIncDocs
      statusRes <-
        if shouldIncludeDocs
          then do
            let entity = IQuery.PersonEntity person
            entityImages <- IQuery.findAllByEntityId transporterConfig entity
            let entityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity = merchantOpCity, entityImages, transporterConfig, now, enableDocumentMetadata = False}
            let shouldActivateRc = False
                skipMessages = False -- Need translations for API response
            Just . castStatusRes <$> SStatus.statusHandler' person entityImagesInfo Nothing Nothing Nothing Nothing (Just True) shouldActivateRc onlyMandatoryDocs skipMessages
          else pure Nothing

      driverInfo <- HashMap.lookup drvOpAsn.driverId.getId driverInfoMap & fromMaybeM DriverInfoNotFound

      pure $
        API.Types.ProviderPlatform.Operator.Driver.DriverInfo
          { driverId = cast drvOpAsn.driverId,
            firstName = person.firstName,
            middleName = person.middleName,
            lastName = person.lastName,
            status = Just $ DFDriver.castDriverStatus driverMode,
            isActive = drvOpAsn.isActive,
            mobileCountryCode = fromMaybe "+91" person.mobileCountryCode,
            mobileNumber = decryptedMobileNumber,
            vehicle = vehicleModel,
            vehicleNo = registrationNo,
            isRcActive = isRcActive,
            verified = Just driverInfo.verified,
            approved = driverInfo.approved,
            enabled = Just driverInfo.enabled,
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
      transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      if transporterConfig.overrideOperatorDriverJoiningWithDeepLink == Just True
        then do
          withLogTag ("personId_" <> getId person.id) $ do
            activeAssociations <- B.runInReplica $ QDOA.findAllByDriverId person.id True
            activeFleetAssociations <- B.runInReplica $ QFDA.findAllByDriverId person.id True
            unless (null activeFleetAssociations) $
              throwError (InvalidRequest "Driver already associated with a fleet")
            case find (\assoc -> assoc.operatorId /= operator.id.getId) activeAssociations of
              Just _ -> throwError (InvalidRequest "Already existing with another operations partner")
              Nothing -> do
                when (any (\assoc -> assoc.operatorId == operator.id.getId) activeAssociations) $
                  throwError (InvalidRequest "Driver already associated with operator")
                mbExistingInactiveAssoc <- B.runInReplica $ QDOA.findByDriverIdAndOperatorId person.id operator.id False
                whenNothing_ mbExistingInactiveAssoc $ do
                  assoc <- SA.makeDriverOperatorAssociation merchant.id merchantOpCityId person.id operator.id.getId DomainRC.defaultAssociationEnd False
                  QDOA.create assoc
                (mbSender, message, templateId, messageType) <-
                  MessageBuilder.buildOperatorJoinAndDownloadAppMessage merchantOpCityId $
                    MessageBuilder.BuildOperatorJoinAndDownloadAppMessageReq
                      { operatorName = operator.firstName
                      }
                let sender = fromMaybe smsCfg.sender mbSender
                Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId messageType)
                  >>= Sms.checkSmsResult
          pure $ Common.AuthRes {authId = "DIRECTLY_ASSOCIATED", attempts = 0}
        else do
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
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  mobileNumberHash <- getDbHash req.mobileNumber
  person <- B.runInReplica $ QP.findByMobileNumberAndMerchantAndRole req.mobileCountryCode mobileNumberHash merchant.id DP.DRIVER >>= fromMaybeM (PersonNotFound req.mobileNumber)
  case mbAuthId of
    Just authId -> do
      smsCfg <- asks (.smsCfg)
      SA.endDriverAssociationsIfAllowed merchant merchantOpCityId transporterConfig person
      when (merchant.overwriteAssociation == Just True) $
        QDRC.endAllRCAssociationsForDriver person.id

      deviceToken <- fromMaybeM (DeviceTokenNotFound) $ req.deviceToken
      let regId = Id authId :: Id SR.RegistrationToken
      _ <-
        DReg.verify
          regId
          DReg.AuthVerifyReq
            { otp = req.otp,
              deviceToken = deviceToken,
              whatsappNotificationEnroll = Nothing
            }

      verifyAndAssociateDriverWithOperator merchant merchantOpCityId operator person transporterConfig

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
      when (merchant.overwriteAssociation == Just True) $
        QDRC.endAllRCAssociationsForDriver person.id

      verifyAndAssociateDriverWithOperator merchant merchantOpCityId operator person transporterConfig

  pure Success

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
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (not transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics) $ throwError (InvalidRequest "Analytics is not allowed for this merchant")
  operator <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
  unless (operator.role == DP.OPERATOR) $
    throwError AccessDenied

  -- Redis keys for fleet aggregates
  let allTimeKeysData = AnalyticsExtra.allTimeKeys operator.id.getId
  logTagInfo "allTimeKeysData" (show allTimeKeysData)

  -- try redis first
  mbAllTimeKeysRes <- mapM (Redis.get @Int) allTimeKeysData
  logTagInfo "mbAllTimeKeysRes" (show mbAllTimeKeysRes)

  -- fallback to ClickHouse and populate cache when missing
  (totalRides, ratingSum, ratingCount, cancelCount, acceptationCount, totalRequestCount, mbTotalAssociatedDriver, mbTotalActiveDrivers, mbTotalEnabledDrivers) <- do
    if all isJust mbAllTimeKeysRes
      then do
        let res = AnalyticsExtra.convertToAllTimeFallbackRes (zip AnalyticsExtra.allTimeMetrics (map (fromMaybe 0) mbAllTimeKeysRes))
        logTagInfo "AllTimeFallbackRes" (show res)
        Analytics.extractOperatorAnalyticsData res
      else do
        res <- AnalyticsExtra.handleCacheMissForAnalyticsAllTimeCommon transporterConfig DP.OPERATOR operator.id.getId allTimeKeysData
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
  pure $
    API.Types.ProviderPlatform.Operator.Driver.AllTimeOperatorAnalyticsRes
      { rating,
        cancellationRate,
        acceptanceRate,
        totalActiveDrivers = mbTotalActiveDrivers,
        totalAssociatedDriver = mbTotalAssociatedDriver,
        totalEnabledDriver = mbTotalEnabledDrivers
      }

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
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (not transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics) $ throwError (InvalidRequest "Analytics is not allowed for this merchant")
  operator <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
  unless (operator.role == DP.OPERATOR) $ throwError AccessDenied

  let buildRes res =
        let totalInspectionCompleted =
              API.Types.ProviderPlatform.Operator.Driver.TotalInspectionCompletedRes
                { approvedVehicleInspection = res.approvedVehicleInspection,
                  approvedDriverInspection = res.approvedDriverInspection,
                  rejectedVehicleInspection = res.rejectedVehicleInspection,
                  rejectedDriverInspection = res.rejectedDriverInspection
                }
         in API.Types.ProviderPlatform.Operator.Driver.FilteredOperatorAnalyticsRes
              { activeDriver = res.activeDriver,
                driverEnabled = res.driverEnabled,
                greaterThanOneRide = res.greaterThanOneRide,
                greaterThanTenRide = res.greaterThanTenRide,
                greaterThanFiftyRide = res.greaterThanFiftyRide,
                totalInspectionCompleted
              }

  res <- Analytics.computePeriodOperatorAnalytics transporterConfig operator.id.getId fromDay toDay
  logTagInfo "PeriodFallbackRes" (show res)
  pure $ buildRes res

---------------------------------------------------------------------
getDriverReviewQueueRequest ::
  ShortId DM.Merchant ->
  Context.City ->
  API.Types.ProviderPlatform.Operator.Driver.EntityType ->
  API.Types.ProviderPlatform.Operator.Driver.ReviewRequestType ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Flow API.Types.ProviderPlatform.Operator.Driver.ReviewQueueResp
getDriverReviewQueueRequest merchantShortId opCity entityType reviewRequestType mbFrom mbTo mbLimit mbOffset mbMobileNumber mbPersonId mbRcNo mbRcId mbApproved = do
  unless (reviewRequestType == API.Types.ProviderPlatform.Operator.Driver.BOT_REVIEW) $ throwError (InvalidRequest "Review Request Type not supported")
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
      maxLimit = 20
      defaultLimit = 10
  reviewQueue <- case entityType of
    API.Types.ProviderPlatform.Operator.Driver.DRIVER -> do
      finalPersonIds <- getFinalPersonIds merchant.id
      case finalPersonIds of
        Just [] -> pure []
        _ -> do
          driverInfoList <- QDIExtra.findByVerifiedAndApprovedAndEnabled merchantOpCity.id True mbApproved (Just False) mbFrom mbTo limit offset finalPersonIds
          let driverIds = map (.driverId) driverInfoList
          if null driverIds
            then pure []
            else do
              persons <- runInReplica $ QPerson.getDriversByIdIn [Id dId.getId | dId <- driverIds]
              let personMap = HashMap.fromList [(p.id.getId, p) | p <- persons]
              identityInfos <- QDII.findAllByDriverIds [Id dId.getId | dId <- driverIds]
              let crcMap = HashMap.fromList [(ii.driverId.getId, ii) | ii <- identityInfos]
              let pairedList = [(di, p) | di <- driverInfoList, Just p <- [HashMap.lookup di.driverId.getId personMap]]
              mapM (buildDriverReviewItem crcMap) pairedList
    API.Types.ProviderPlatform.Operator.Driver.FLEET_OWNER -> do
      finalPersonIds <- getFinalPersonIds merchant.id
      case finalPersonIds of
        Just [] -> pure []
        _ -> do
          fleetOwnerList <- QFOIExtra.findByVerifiedAndEnabled merchantOpCity.id True False mbFrom mbTo limit offset finalPersonIds
          let ownerIds = map (.fleetOwnerPersonId) fleetOwnerList
          if null ownerIds
            then pure []
            else do
              persons <- runInReplica $ QPerson.getDriversByIdIn [Id oId.getId | oId <- ownerIds]
              let personMap = HashMap.fromList [(p.id.getId, p) | p <- persons]
              let pairedList = [(foi, p) | foi <- fleetOwnerList, Just p <- [HashMap.lookup foi.fleetOwnerPersonId.getId personMap]]
              mapM buildFleetOwnerReviewItem pairedList
    API.Types.ProviderPlatform.Operator.Driver.VEHICLE -> do
      rcList <- QVRCE.findVerifiedAndApproved merchantOpCity.id True (fromMaybe False mbApproved) mbFrom mbTo limit offset mbRcNo mbRcId
      mapM buildVehicleReviewItem rcList
  pure API.Types.ProviderPlatform.Operator.Driver.ReviewQueueResp {reviewQueue}
  where
    getFinalPersonIds merchantId = do
      mbMobileNumberHash <- mapM getDbHash mbMobileNumber
      mbPersonIdsForMobile <- case mbMobileNumberHash of
        Nothing -> pure Nothing
        Just hash -> do
          persons <- runInReplica $ QPerson.findAllByMobileNumberAndMerchant hash merchantId
          pure $ Just $ map (\p -> p.id.getId) persons

      pure $ case (mbPersonIdsForMobile, mbPersonId) of
        (Nothing, Nothing) -> Nothing
        (Just ids, Nothing) -> Just ids
        (Nothing, Just eId) -> Just [eId]
        (Just ids, Just eId) -> Just (filter (== eId) ids)

    buildDriverReviewItem crcMap (driverInfo, person) = do
      mobileNumber <- mapM decrypt person.mobileNumber
      let courtRecord = mkApiCourtRecord <$> (HashMap.lookup driverInfo.driverId.getId crcMap >>= (.courtRecord))
      pure $
        API.Types.ProviderPlatform.Operator.Driver.ReviewQueue
          { entityDetails =
              API.Types.ProviderPlatform.Operator.Driver.EntityDetails
                { entityType = API.Types.ProviderPlatform.Operator.Driver.DRIVER,
                  name = Just $ T.strip (person.firstName <> maybe "" (" " <>) person.middleName <> maybe "" (" " <>) person.lastName),
                  mobileNumber,
                  entityId = driverInfo.driverId.getId,
                  rcNo = Nothing,
                  courtRecord,
                  pendingChallan = Nothing,
                  fleetType = Nothing
                },
            createdAt = driverInfo.updatedAt,
            updatedAt = driverInfo.updatedAt,
            verified = Just driverInfo.verified,
            enabled = Just driverInfo.enabled,
            approved = driverInfo.approved
          }

    buildFleetOwnerReviewItem (fleetOwnerInfo, person) = do
      mobileNumber <- mapM decrypt person.mobileNumber
      pure $
        API.Types.ProviderPlatform.Operator.Driver.ReviewQueue
          { entityDetails =
              API.Types.ProviderPlatform.Operator.Driver.EntityDetails
                { entityType = API.Types.ProviderPlatform.Operator.Driver.FLEET_OWNER,
                  name = Just $ T.strip (person.firstName <> maybe "" (" " <>) person.middleName <> maybe "" (" " <>) person.lastName),
                  mobileNumber,
                  entityId = fleetOwnerInfo.fleetOwnerPersonId.getId,
                  rcNo = Nothing,
                  courtRecord = Nothing,
                  pendingChallan = Nothing,
                  fleetType = Just $ castToApiFleetType fleetOwnerInfo.fleetType
                },
            createdAt = fleetOwnerInfo.updatedAt,
            updatedAt = fleetOwnerInfo.updatedAt,
            verified = Just fleetOwnerInfo.verified,
            enabled = Just fleetOwnerInfo.enabled,
            approved = Nothing
          }

    buildVehicleReviewItem rc = do
      pure $
        API.Types.ProviderPlatform.Operator.Driver.ReviewQueue
          { entityDetails =
              API.Types.ProviderPlatform.Operator.Driver.EntityDetails
                { entityType = API.Types.ProviderPlatform.Operator.Driver.VEHICLE,
                  name = Nothing,
                  mobileNumber = Nothing,
                  entityId = rc.id.getId,
                  rcNo = rc.unencryptedCertificateNumber,
                  courtRecord = Nothing,
                  pendingChallan = mkApiPendingChallan <$> rc.pendingChallan,
                  fleetType = Nothing
                },
            createdAt = rc.updatedAt,
            updatedAt = rc.updatedAt,
            verified = rc.verified,
            enabled = Nothing,
            approved = rc.approved
          }

    mkApiCourtRecord cr =
      API.Types.ProviderPlatform.Operator.Driver.CourtRecordResult {result = cr.result, errorMessage = cr.errorMessage}

    mkApiPendingChallan pc =
      API.Types.ProviderPlatform.Operator.Driver.PendingChallanResult {pendingChallanCount = pc.pendingChallanCount, errorMessage = pc.errorMessage}

castToApiFleetType :: DFOI.FleetType -> API.Types.ProviderPlatform.Operator.Driver.FleetType
castToApiFleetType DFOI.RENTAL_FLEET = API.Types.ProviderPlatform.Operator.Driver.RENTAL_FLEET
castToApiFleetType DFOI.NORMAL_FLEET = API.Types.ProviderPlatform.Operator.Driver.NORMAL_FLEET
castToApiFleetType DFOI.BUSINESS_FLEET = API.Types.ProviderPlatform.Operator.Driver.BUSINESS_FLEET

postDriverSubmitReviewRequest ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  API.Types.ProviderPlatform.Operator.Driver.SubmitReviewRequest ->
  Environment.Flow APISuccess
postDriverSubmitReviewRequest merchantShortId opCity requestorId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  now <- getCurrentTime

  let reqId = req.entityId
  Redis.whenWithLockRedis (reviewRequestLockKey reqId) 60 $ do
    let isDocReqEmpty = null req.rejectDocumentUpdateReq

    validatedDocs <- if isDocReqEmpty then pure [] else validateDocs merchantOpCity.id req.entityType
    let docDetails = map (\(_, _, d) -> d) validatedDocs

    (status, mbRegNo, mbPersonToNotify) <- case req.entityType of
      API.Types.ProviderPlatform.Operator.Driver.DRIVER -> do
        let driverId = Kernel.Types.Id.Id reqId
        driverInfo <- QDI.findByPrimaryKey driverId >>= fromMaybeM (PersonNotFound reqId)
        unless driverInfo.verified $ throwError (InvalidRequest "Driver is not verified")
        if isDocReqEmpty
          then do
            -- Two-phase: phase 1 durably commits `approved`; phase 2 (approved == True) does the fast
            -- enabling-docs check synchronously and forks the heavy recompute (which performs the actual
            -- enable + alerts). Splitting the two calls avoids a stale read of `approved`.
            let runReconcile = do
                  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound reqId)
                  transporterConfig <- findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
                  -- Throws (naming the offending docs) if any BotApproval dependency doc isn't VALID.
                  SStatus.botApproveAndReconcile merchantOpCity person transporterConfig
                  QDIExtra.updateEnabledVerifiedState driverId True Nothing Nothing
                  pure (DRR.COMPLETED, Nothing, Nothing)
            if driverInfo.approved == Just True
              then runReconcile
              else do
                QDI.updateByPrimaryKey driverInfo {DDI.approved = Just True}
                pure (DRR.IN_PROGRESS, Nothing, Nothing)
          else do
            applyDocRejections req.entityType reqId validatedDocs
            QDI.updateByPrimaryKey driverInfo {DDI.approved = Just False, DDI.verified = False}
            person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound reqId)
            pure (DRR.REJECTED, Nothing, Just person)
      API.Types.ProviderPlatform.Operator.Driver.FLEET_OWNER -> do
        let fleetOwnerId = Kernel.Types.Id.Id reqId
        fleetOwnerInfo <- QFOI.findByPrimaryKey fleetOwnerId >>= fromMaybeM (PersonNotFound reqId)
        unless fleetOwnerInfo.verified $ throwError (InvalidRequest "Fleet Owner is not verified")
        if isDocReqEmpty
          then do
            -- Sync dependency-docs check; the heavy fleet recompute (sets `enabled` + cascades) is forked.
            fleetPerson <- QPerson.findById fleetOwnerInfo.fleetOwnerPersonId >>= fromMaybeM (PersonNotFound fleetOwnerInfo.fleetOwnerPersonId.getId)
            transporterConfig <- findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
            -- Throws (naming the offending docs) if any BotApproval dependency doc isn't VALID.
            SStatus.botApproveAndReconcile merchantOpCity fleetPerson transporterConfig
            QFOIExtra.updateFleetOwnerApprovedAndEnabledStatus (Just True) True fleetOwnerInfo.fleetOwnerPersonId
            pure (DRR.COMPLETED, Nothing, Nothing)
          else do
            applyDocRejections req.entityType reqId validatedDocs
            QFOI.updateByPrimaryKey fleetOwnerInfo {DFOI.enabled = False, DFOI.verified = False, DFOI.approved = Just False}
            person <- QPerson.findById fleetOwnerInfo.fleetOwnerPersonId >>= fromMaybeM (PersonNotFound fleetOwnerInfo.fleetOwnerPersonId.getId)
            pure (DRR.REJECTED, Nothing, Just person)
      API.Types.ProviderPlatform.Operator.Driver.VEHICLE -> do
        let rcId = Kernel.Types.Id.Id reqId
        rcInfo <- QVRC.findByPrimaryKey rcId >>= fromMaybeM (VehicleNotFound reqId)
        when (rcInfo.verified /= Just True) $ throwError (InvalidRequest "RC is not verified")
        if isDocReqEmpty
          then do
            unless (rcInfo.approved == Just True) $ QVRC.updateApproved (Just True) rcId
            pure (DRR.COMPLETED, rcInfo.unencryptedCertificateNumber, Nothing)
          else do
            applyDocRejections req.entityType reqId validatedDocs
            QVRCE.updateApprovedAndVerifiedById (Just False) (Just False) rcId
            mbPersonId <- resolvePersonIdViaRc rcId
            mbPerson <- case mbPersonId of
              Just pId -> QPerson.findById pId
              Nothing -> pure Nothing
            pure (DRR.REJECTED, rcInfo.unencryptedCertificateNumber, mbPerson)

    when (not isDocReqEmpty) $ do
      forM_ mbPersonToNotify $ \person -> do
        forM_ validatedDocs $ \(_images, _rejectedReason, docDetail) -> do
          sendDocumentRejectionNotification merchantOpCity.id (show docDetail.documentType) docDetail.rejectedReason person

    let domainEntityType = case req.entityType of
          API.Types.ProviderPlatform.Operator.Driver.DRIVER -> DRR.DRIVER
          API.Types.ProviderPlatform.Operator.Driver.FLEET_OWNER -> DRR.FLEET_OWNER
          API.Types.ProviderPlatform.Operator.Driver.VEHICLE -> DRR.VEHICLE

    existingRecords <- QRR.findByEntityIdAndEntityTypeAndRequestTypeAndStatusAndRcNo reqId domainEntityType DRR.BOT_REVIEW status mbRegNo

    inProgressRecords <-
      if domainEntityType == DRR.DRIVER && status == DRR.COMPLETED
        then QRR.findByEntityIdAndEntityTypeAndRequestTypeAndStatusAndRcNo reqId domainEntityType DRR.BOT_REVIEW DRR.IN_PROGRESS mbRegNo
        else pure []

    if not (null inProgressRecords)
      then do
        forM_ inProgressRecords $ \record -> do
          QRR.updateByPrimaryKey record {DRR.requestStatus = status, DRR.updatedAt = now, DRR.reviewerId = Just (Kernel.Types.Id.Id requestorId)}
      else when (null existingRecords) $ do
        reqUUID <- generateGUID
        QRR.create
          DRR.ReviewRequest
            { id = reqUUID,
              createdAt = now,
              updatedAt = now,
              requestType = DRR.BOT_REVIEW,
              entityType = domainEntityType,
              entityId = reqId,
              rcNo = mbRegNo,
              requestStatus = status,
              reviewerId = Just (Kernel.Types.Id.Id requestorId),
              documentDetails = if isDocReqEmpty then Nothing else Just docDetails,
              merchantId = merchant.id,
              merchantOperatingCityId = merchantOpCity.id
            }

  pure Success
  where
    resolvePersonIdViaRc rcId = do
      mbDriverAssoc <- QRCAssoc.findActiveAssociationByRC rcId True
      case mbDriverAssoc of
        Just assoc -> pure $ Just assoc.driverId
        Nothing -> do
          mbRc <- QVRC.findById rcId
          case mbRc of
            Just rcRecord -> do
              mbImage <- IQuery.findById rcRecord.documentImageId
              case mbImage of
                Just image -> pure $ Just image.personId
                Nothing -> pure Nothing
            Nothing -> pure Nothing
    validateDocs mocId entityType = do
      allFODVC <- case entityType of
        API.Types.ProviderPlatform.Operator.Driver.FLEET_OWNER -> getConfig (FleetOwnerDocumentVerificationConfigDimensions {merchantOperatingCityId = mocId.getId, documentType = Nothing, role = Nothing}) (Just (CQFODVC.findAllByMerchantOpCityId mocId Nothing))
        _ -> pure []
      allDVC <- case entityType of
        API.Types.ProviderPlatform.Operator.Driver.FLEET_OWNER -> pure []
        _ -> getConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = mocId.getId, documentType = Nothing, vehicleCategory = Nothing}) (Just (CQDVC.findAllByMerchantOpCityId mocId Nothing))

      -- Resolve the fleet owner's role once (entityId is the fleet owner's person id) so the per-doc FODVC
      -- lookup can match the right role's row instead of being role-blind.
      mbFleetOwnerRole <- case entityType of
        API.Types.ProviderPlatform.Operator.Driver.FLEET_OWNER ->
          Just . (.role) <$> (QPerson.findById (Kernel.Types.Id.Id req.entityId) >>= fromMaybeM (PersonNotFound req.entityId))
        _ -> pure Nothing

      forM req.rejectDocumentUpdateReq $ \doc -> do
        let domainDocType = mapDocumentType doc.documentType
        let imageIds = catMaybes [Kernel.Types.Id.cast <$> doc.imageId1, Kernel.Types.Id.cast <$> doc.imageId2]
        images <- IQuery.findImagesByIds imageIds
        let retrievedIds = map (.id) images
        unless (all (`elem` retrievedIds) imageIds) $ throwError (InvalidRequest "Image ID not found")

        when (domainDocType == DVC.ProfilePhoto) $ throwError (InvalidRequest "Cannot reject Profile Photo")

        isMandatory <- case entityType of
          API.Types.ProviderPlatform.Operator.Driver.FLEET_OWNER -> do
            -- Role-aware: match this fleet owner's role, falling back to any fleet-role row on drift.
            let isFleetRoleCfg r = r `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS]
                mbConfig =
                  (mbFleetOwnerRole >>= \role -> find (\c -> c.documentType == domainDocType && c.role == role) allFODVC)
                    <|> find (\c -> c.documentType == domainDocType && isFleetRoleCfg c.role) allFODVC
            config <- mbConfig & fromMaybeM (InvalidRequest "Document Verification Config not found")
            pure config.isMandatory
          _ -> do
            case filter (\c -> c.documentType == domainDocType) allDVC of
              (dvc : _) -> pure dvc.isMandatory
              [] -> throwError (InvalidRequest "Document Verification Config not found")

        unless isMandatory $ throwError (InvalidRequest "Cannot reject optional document")
        let docDetail =
              DRR.DocumentDetail
                { documentType = domainDocType,
                  documentDescription = Nothing,
                  rejectedReason = doc.rejectedReason,
                  remarks = doc.remarks,
                  imageId1 = fmap Kernel.Types.Id.cast doc.imageId1,
                  imageId2 = fmap Kernel.Types.Id.cast doc.imageId2,
                  mediaId = Nothing
                }
        pure (images, doc.rejectedReason, docDetail)
    applyDocRejections entityType reqId validatedDocs =
      forM_ validatedDocs $ \(images, rejectedReason, docDetail) -> do
        let imageIds = map (.id) images
        IQuery.updateVerificationStatusAndFailureReasonForIds (Just Documents.INVALID) (Just $ ImageNotValid rejectedReason) imageIds
        invalidateSpecificDocument entityType reqId docDetail.documentType (Just rejectedReason)

    invalidateSpecificDocument entityType entityIdTxt docType rejectReason = do
      now <- getCurrentTime
      case entityType of
        API.Types.ProviderPlatform.Operator.Driver.VEHICLE -> do
          let rcId = Kernel.Types.Id.Id entityIdTxt
          case docType of
            DVC.VehicleRegistrationCertificate -> do
              mbDoc <- QVRC.findByPrimaryKey rcId
              forM_ mbDoc $ \doc -> QVRC.updateByPrimaryKey doc {DVRC.verificationStatus = Documents.INVALID, DVRC.rejectReason = rejectReason}
            DVC.VehicleInspectionForm ->
              IQuery.updateVerificationStatusByRcIdAndImageTypes Documents.INVALID rcId VDocs.vehicleDocsByRcIdList
            DVC.InspectionHub -> do
              mbRc <- QVRC.findByPrimaryKey rcId
              forM_ mbRc $ \rc -> do
                case rc.unencryptedCertificateNumber of
                  Just rcNo -> do
                    mbReq <- SQOH.findLatestByRegistrationNoAndRequestType rcNo ONBOARDING_INSPECTION
                    forM_ mbReq $ \hubReq -> SQOHR.updateByPrimaryKey hubReq {requestStatus = REJECTED, updatedAt = now, remarks = rejectReason}
                  Nothing -> pure ()
            _ -> pure ()
        _ -> do
          let personId = Kernel.Types.Id.Id entityIdTxt
          case docType of
            DVC.AadhaarCard -> do
              mbDoc <- QAadhaarCard.findByPrimaryKey personId
              forM_ mbDoc $ \doc -> QAadhaarCard.updateByPrimaryKey doc {DAadhaarCard.verificationStatus = Documents.INVALID, DAadhaarCard.rejectReason = rejectReason}
            DVC.PanCard -> do
              mbDoc <- QDPC.findByDriverId personId
              forM_ mbDoc $ \doc -> QDPC.updateByPrimaryKey doc {DDPC.verificationStatus = Documents.INVALID, DDPC.rejectReason = rejectReason}
            DVC.DriverLicense -> do
              mbDoc <- DLQuery.findByDriverId personId
              forM_ mbDoc $ \doc -> DLQuery.updateByPrimaryKey doc {DDL.verificationStatus = Documents.INVALID, DDL.rejectReason = rejectReason}
            DVC.SocialSecurityNumber -> do
              mbDoc <- QDSSN.findByDriverId personId
              forM_ mbDoc $ \doc -> QDSSN.updateByPrimaryKey doc {DDSSN.verificationStatus = Documents.INVALID, DDSSN.rejectReason = rejectReason}
            DVC.GSTCertificate -> do
              mbDoc <- QDGST.findByDriverId personId
              forM_ mbDoc $ \doc -> QDGST.updateByPrimaryKey doc {DDGST.verificationStatus = Documents.INVALID, DDGST.rejectReason = rejectReason}
            DVC.UDYAMCertificate -> do
              mbDoc <- QUDYAM.findByDriverId personId
              forM_ mbDoc $ \doc -> QUDYAM.updateByPrimaryKey doc {DDUDYAM.verificationStatus = Documents.INVALID, DDUDYAM.rejectReason = rejectReason}
            DVC.DriverInspectionHub -> do
              mbReq <- SQOH.findLatestByDriverIdAndRequestType personId DRIVER_ONBOARDING_INSPECTION
              forM_ mbReq $ \hubReq -> SQOHR.updateByPrimaryKey hubReq {requestStatus = REJECTED, updatedAt = now, remarks = rejectReason}
            DVC.LocalResidenceProof -> do
              proofImages <- IQuery.findRecentByPersonIdAndImageType personId DVC.LocalResidenceProof
              forM_ proofImages $ \img ->
                whenJust rejectReason $ \r -> IQuery.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid r) img.id
              case entityType of
                API.Types.ProviderPlatform.Operator.Driver.FLEET_OWNER -> QFOI.updateLocalAddressDetails Nothing Nothing Nothing personId
                _ -> QDII.updateLocalAddressDetails Nothing Nothing Nothing personId
            _ -> pure ()

getDriverRequestReviewHistory ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.ProviderPlatform.Operator.Driver.EntityType ->
  API.Types.ProviderPlatform.Operator.Driver.ReviewRequestType ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe API.Types.ProviderPlatform.Operator.Driver.ReviewRequestStatus ->
  Maybe Text ->
  Flow API.Types.ProviderPlatform.Operator.Driver.ReviewRequestHistoryList
getDriverRequestReviewHistory merchantShortId opCity apiEntityType reviewRequestType mbFrom mbTo mbLimit mbOffset mbMobileCountryCode mbMobileNumber mbPersonId mbRcNo mbRequestStatus mbRcId = do
  unless (reviewRequestType == API.Types.ProviderPlatform.Operator.Driver.BOT_REVIEW) $ throwError (InvalidRequest "Review Request Type not supported")
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
      maxLimit = 20
      defaultLimit = 10
      mbStatusDomain = fmap castFromApiReqStatus mbRequestStatus
      entityTypeDomain = castFromApiEntityType apiEntityType
      mbReqTypeDomain = Just (castFromApiReqType reviewRequestType)

  mbPersonIdsForMobile <- case mbMobileNumber of
    Nothing -> pure Nothing
    Just mob -> do
      mobHash <- getDbHash mob
      let countryCode = fromMaybe "+91" mbMobileCountryCode
      persons <- B.runInReplica $ QPerson.findAllByMobileNumberAndMerchantAndRoles countryCode mobHash merchant.id [DP.DRIVER, DP.FLEET_OWNER, DP.FLEET_BUSINESS]
      pure $ Just $ map (\p -> p.id.getId) persons

  case mbPersonIdsForMobile of
    Just [] -> pure API.Types.ProviderPlatform.Operator.Driver.ReviewRequestHistoryList {reviewHistory = []}
    _ -> do
      let mbEntityIds = case (mbPersonIdsForMobile, mbPersonId) of
            (Nothing, Nothing) -> Nothing
            (Just ids, Nothing) -> Just ids
            (Nothing, Just eId) -> Just [eId]
            (Just ids, Just eId) -> Just (filter (== eId) ids)

      let finalMbEntityIds = case entityTypeDomain of
            DRR.VEHICLE -> case (mbEntityIds, mbRcId) of
              (Nothing, Nothing) -> Nothing
              (Just ids, Nothing) -> Just ids
              (Nothing, Just rId) -> Just [rId]
              (Just ids, Just rId) -> Just (filter (== rId) ids)
            _ -> mbEntityIds

      historyRecords <- SQRRExtra.findAllByFilters merchant.id merchantOpCity.id mbFrom mbTo limit offset entityTypeDomain finalMbEntityIds mbRcNo mbStatusDomain mbReqTypeDomain

      let personIds = [Id req.entityId | req <- historyRecords, req.entityType /= DRR.VEHICLE]
      persons <- if null personIds then pure [] else B.runInReplica $ QPerson.getDriversByIdIn personIds
      let personByEntityId = HashMap.fromList [(p.id.getId, p) | p <- persons]

      let fleetOwnerIds = [Id req.entityId | req <- historyRecords, req.entityType == DRR.FLEET_OWNER]
      fleetOwnerInfos <- if null fleetOwnerIds then pure [] else B.runInReplica $ QFOI.findAllByPrimaryKeys fleetOwnerIds
      let fleetTypeByEntityId = HashMap.fromList [(f.fleetOwnerPersonId.getId, f.fleetType) | f <- fleetOwnerInfos]

      reviewHistory <- mapM (buildReviewHistoryItem personByEntityId fleetTypeByEntityId) historyRecords
      pure API.Types.ProviderPlatform.Operator.Driver.ReviewRequestHistoryList {reviewHistory}
  where
    buildReviewHistoryItem personByEntityId fleetTypeByEntityId req = do
      let mbPerson = if req.entityType == DRR.VEHICLE then Nothing else HashMap.lookup req.entityId personByEntityId
      mbPersonMobileNumber <- maybe (pure Nothing) (\p -> mapM decrypt p.mobileNumber) mbPerson
      let mbPersonName = (\p -> T.strip (p.firstName <> maybe "" (" " <>) p.middleName <> maybe "" (" " <>) p.lastName)) <$> mbPerson
      let mbFleetType = if req.entityType == DRR.FLEET_OWNER then castToApiFleetType <$> HashMap.lookup req.entityId fleetTypeByEntityId else Nothing
      pure $
        API.Types.ProviderPlatform.Operator.Driver.ReviewRequestHistory
          { id = req.id.getId,
            requestType = castToApiReqType req.requestType,
            entityDetails =
              API.Types.ProviderPlatform.Operator.Driver.EntityDetails
                { entityType = castToApiEntityType req.entityType,
                  entityId = req.entityId,
                  name = mbPersonName,
                  mobileNumber = mbPersonMobileNumber,
                  rcNo = req.rcNo,
                  courtRecord = Nothing,
                  pendingChallan = Nothing,
                  fleetType = mbFleetType
                },
            requestStatus = castToApiReqStatus req.requestStatus,
            reviewerId = fmap (.getId) req.reviewerId,
            documentDetails = fmap (map castToApiDocumentDetail) req.documentDetails,
            createdAt = req.createdAt,
            updatedAt = req.updatedAt
          }

    castToApiReqType DRR.BOT_REVIEW = API.Types.ProviderPlatform.Operator.Driver.BOT_REVIEW

    castToApiEntityType DRR.DRIVER = API.Types.ProviderPlatform.Operator.Driver.DRIVER
    castToApiEntityType DRR.VEHICLE = API.Types.ProviderPlatform.Operator.Driver.VEHICLE
    castToApiEntityType DRR.FLEET_OWNER = API.Types.ProviderPlatform.Operator.Driver.FLEET_OWNER

    castToApiReqStatus DRR.IN_PROGRESS = API.Types.ProviderPlatform.Operator.Driver.RR_IN_PROGRESS
    castToApiReqStatus DRR.COMPLETED = API.Types.ProviderPlatform.Operator.Driver.RR_COMPLETED
    castToApiReqStatus DRR.REJECTED = API.Types.ProviderPlatform.Operator.Driver.RR_REJECTED

    castToApiDocumentDetail doc =
      API.Types.ProviderPlatform.Operator.Driver.DocumentDetail
        { documentType = SDO.castDocumentType doc.documentType,
          documentDescription = doc.documentDescription,
          rejectedReason = doc.rejectedReason,
          remarks = doc.remarks,
          imageId1 = fmap Kernel.Types.Id.cast doc.imageId1,
          imageId2 = fmap Kernel.Types.Id.cast doc.imageId2,
          mediaId = doc.mediaId
        }

    castFromApiReqStatus API.Types.ProviderPlatform.Operator.Driver.RR_IN_PROGRESS = DRR.IN_PROGRESS
    castFromApiReqStatus API.Types.ProviderPlatform.Operator.Driver.RR_COMPLETED = DRR.COMPLETED
    castFromApiReqStatus API.Types.ProviderPlatform.Operator.Driver.RR_REJECTED = DRR.REJECTED

    castFromApiEntityType API.Types.ProviderPlatform.Operator.Driver.DRIVER = DRR.DRIVER
    castFromApiEntityType API.Types.ProviderPlatform.Operator.Driver.VEHICLE = DRR.VEHICLE
    castFromApiEntityType API.Types.ProviderPlatform.Operator.Driver.FLEET_OWNER = DRR.FLEET_OWNER

    castFromApiReqType API.Types.ProviderPlatform.Operator.Driver.BOT_REVIEW = DRR.BOT_REVIEW

---------------------------------------------------------------------
-- Associates an already-onboarded driver with an operator. Shared by the OTP verify flow
-- (postDriverOperatorVerifyJoiningOtp) and the OTP-bypass flow (postDriverOperatorSendJoiningOtp
-- when transporterConfig.overrideOperatorDriverJoiningWithDeepLink is enabled).
verifyAndAssociateDriverWithOperator ::
  DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DP.Person ->
  DP.Person ->
  TransporterConfig ->
  Flow ()
verifyAndAssociateDriverWithOperator merchant merchantOpCityId operator person transporterConfig = do
  checkAssocOperator <-
    B.runInReplica $
      QDOA.findByDriverIdAndOperatorId person.id operator.id True
  when (isJust checkAssocOperator) $
    throwError (InvalidRequest "Driver already associated with operator")

  assoc <- SA.makeDriverOperatorAssociation merchant.id merchantOpCityId person.id operator.id.getId DomainRC.defaultAssociationEnd True
  QDOA.create assoc
  Analytics.handleDriverAnalyticsAndFlowStatus
    transporterConfig
    person.id
    Nothing
    ( \driverInfo -> do
        activeSubCount <- QSubscriptionPurchaseExtra.countActiveSubscriptionsForOwner person.id.getId DSP.DRIVER
        AnalyticsExtra.adjustOperatorDriverAssociationAnalytics transporterConfig operator.id.getId 1 activeSubCount driverInfo.enabled
    )
    ( \driverInfo -> do
        DDriverMode.incrementFleetOperatorStatusKeyForDriver DP.OPERATOR operator.id.getId driverInfo.driverFlowStatus
    )
