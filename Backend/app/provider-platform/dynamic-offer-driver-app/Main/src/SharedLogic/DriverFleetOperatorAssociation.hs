module SharedLogic.DriverFleetOperatorAssociation
  ( checkForDriverAssociationOverwrite,
    guardDriverNotAssociated,
    endDriverAssociations,
    endFleetAssociations,
    makeFleetOperatorAssociation,
    makeDriverOperatorAssociation,
    isAssociationBetweenTwoPerson,
    checkFleetDriverAssociation,
    checkFleetDriverAssociationIgnoringActive,
    checkFleetOperatorAssociation,
    checkDriverOperatorAssociation,
    ensureDriverNotActiveElsewhere,
    resolveOperatorByCode,
    associateDriverWithOperator,
    linkDriverToOperator,
    endAssociationsForOperatorLink,
    sendOperatorDeepLinkForAuth,
    adjustOperatorAnalytics,
    AssociationChangeType (..),
    performAssociationChange,
  )
where

import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverOperatorAssociation as DDOA
import qualified Domain.Types.FleetOperatorAssociation as DFOA
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SubscriptionPurchase as DSP
import Domain.Types.TransporterConfig
import qualified Domain.Types.VehicleCategory as DVC
import Domain.Utils (defaultAssociationEnd)
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification as Notification
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.Analytics as Analytics
import SharedLogic.AnalyticsExtra as AnalyticsExtra
import qualified SharedLogic.Association.Change as AC
import qualified SharedLogic.DriverOnboarding.OnboardingComms as SOnboardingComms
import qualified SharedLogic.DriverOnboarding.OnboardingFlags.Guard as SGuard
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubscriptionPurchaseExtra
import qualified Tools.Notifications as TN
import Tools.SMS as Sms hiding (Success)

checkForDriverAssociationOverwrite ::
  DM.Merchant ->
  Id DP.Person ->
  Flow ()
checkForDriverAssociationOverwrite merchant driverId = do
  -- Skip the fleet read entirely when overwrite is enabled (the guard is a no-op then).
  hasActiveFleetAssoc <-
    if merchant.overwriteAssociation == Just True
      then pure False
      else not . null <$> QFDA.findAllByDriverId driverId True
  guardDriverNotAssociated merchant driverId hasActiveFleetAssoc

-- Same "driver must not already belong to a fleet/operator" guard, but the caller passes a
-- pre-fetched flag for the active fleet association so we don't re-read fleet_driver_association.
-- No-op when overwriteAssociation is enabled.
guardDriverNotAssociated :: DM.Merchant -> Id DP.Person -> Bool -> Flow ()
guardDriverNotAssociated merchant driverId hasActiveFleetAssoc =
  unless (merchant.overwriteAssociation == Just True) $ do
    when hasActiveFleetAssoc $
      throwError (InvalidRequest "Driver already associated with a fleet")
    existingDOAssociations <- QDOA.findAllByDriverId driverId True
    unless (null existingDOAssociations) $
      throwError (InvalidRequest "Driver is already associated with an operator")

resolveOperatorByCode :: Id DMOC.MerchantOperatingCity -> Text -> Flow DP.Person
resolveOperatorByCode merchantOpCityId code = do
  dr <- QDR.findByReferralCodeAndMerchantCityId (Just merchantOpCityId) (Id code) >>= fromMaybeM (InvalidRequest $ "Unknown operator code: " <> code)
  operator <- QP.findById dr.driverId >>= fromMaybeM (PersonDoesNotExist dr.driverId.getId)
  unless (operator.role == DP.OPERATOR) $ throwError (InvalidRequest "Operator code does not belong to an operator")
  pure operator

endDriverAssociations ::
  Id DMOC.MerchantOperatingCity ->
  TransporterConfig ->
  DP.Person ->
  Flow ()
endDriverAssociations merchantOpCityId transporterConfig driver = do
  let driverFullName = driver.firstName <> maybe "" (" " <>) driver.lastName
  driverMobile <- maybe (pure "unknown") decrypt driver.mobileNumber
  existingFDAssociations <- QFDA.findAllByDriverId driver.id True
  unless (null existingFDAssociations) $ do
    forM_ existingFDAssociations $ \existingAssociation -> do
      logInfo $ "End existing fleet driver association: fleetOwnerId: " <> existingAssociation.fleetOwnerId <> "driverId: " <> existingAssociation.driverId.getId
      QFDA.endFleetDriverAssociation existingAssociation.fleetOwnerId existingAssociation.driverId
      Analytics.handleDriverAnalyticsAndFlowStatus
        transporterConfig
        existingAssociation.driverId
        Nothing
        ( \_ -> do
            Analytics.decrementFleetOwnerAnalyticsActiveDriverCount transporterConfig (Just existingAssociation.fleetOwnerId) existingAssociation.driverId
        )
        ( \driverInfo -> do
            DDriverMode.decrementFleetOperatorStatusKeyForDriver DP.FLEET_OWNER existingAssociation.fleetOwnerId driverInfo.driverFlowStatus
        )

      fork "Driver fleet unlink notification" $
        SOnboardingComms.notifyOnDriverFleetUnlink merchantOpCityId driver existingAssociation.fleetOwnerId SOnboardingComms.ByFleetOwner

  existingDOAssociations <- QDOA.findAllByDriverId driver.id True
  unless (null existingDOAssociations) $ do
    forM_ existingDOAssociations $ \existingAssociation -> do
      logInfo $ "End existing operator driver association: operatorId: " <> existingAssociation.operatorId <> "driverId: " <> existingAssociation.driverId.getId
      QDOA.endOperatorDriverAssociation existingAssociation.operatorId existingAssociation.driverId
      Analytics.handleDriverAnalyticsAndFlowStatus
        transporterConfig
        existingAssociation.driverId
        Nothing
        ( \driverInfo -> do
            activeSubscriptions <- QSubscriptionPurchaseExtra.countActiveSubscriptionsForOwner existingAssociation.driverId.getId DSP.DRIVER
            AnalyticsExtra.adjustOperatorDriverAssociationAnalytics transporterConfig existingAssociation.operatorId (-1) activeSubscriptions driverInfo.enabled
        )
        ( \driverInfo -> do
            DDriverMode.decrementFleetOperatorStatusKeyForDriver DP.OPERATOR existingAssociation.operatorId driverInfo.driverFlowStatus
        )

      -- send notification to existing operator about driver unlink
      mbExistingOperator <- B.runInReplica $ QP.findById (Id existingAssociation.operatorId :: Id DP.Person)
      whenJust mbExistingOperator $ \existingOperator -> do
        mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId (show Notification.DRIVER_UNLINK_FROM_OPERATOR) Nothing Nothing existingOperator.language Nothing
        whenJust mbMerchantPN $ \merchantPN -> do
          let title = T.replace "{#driverName#}" driverFullName . T.replace "{#driverNo#}" driverMobile $ merchantPN.title
          let body = T.replace "{#driverName#}" driverFullName . T.replace "{#driverNo#}" driverMobile $ merchantPN.body
          TN.notifyWithGRPCProvider merchantOpCityId Notification.DRIVER_UNLINK_FROM_OPERATOR title body (Id existingAssociation.operatorId) Nothing ()

endFleetAssociations ::
  Id DMOC.MerchantOperatingCity ->
  TransporterConfig ->
  DP.Person ->
  Flow ()
endFleetAssociations merchantOpCityId transporterConfig fleetOwner = do
  let fleetOwnerFullName = fleetOwner.firstName <> maybe "" (" " <>) fleetOwner.lastName
  fleetOwnerMobile <- maybe (pure "unknown") decrypt fleetOwner.mobileNumber
  existingFOAssociations <- QFOA.findAllByFleetOwnerId fleetOwner.id True
  -- Always end all existing fleet-operator associations before creating a new one.
  forM_ existingFOAssociations $ \existingAssociation -> do
    logInfo $ "End existing fleet operator association: fleetOwnerId: " <> existingAssociation.fleetOwnerId <> "operatorId: " <> existingAssociation.operatorId
    QFOA.endFleetOperatorAssociation (Id existingAssociation.fleetOwnerId) (Id existingAssociation.operatorId)
    let allowCacheDriverFlowStatus = transporterConfig.analyticsConfig.allowCacheDriverFlowStatus
    when allowCacheDriverFlowStatus $ do
      DDriverMode.decrementOperatorStatusKeyForFleetOwner existingAssociation.operatorId existingAssociation.fleetOwnerId

    -- send notification to existing operator about fleet unlink
    mbExistingOperator <- B.runInReplica $ QP.findById (Id existingAssociation.operatorId :: Id DP.Person)
    whenJust mbExistingOperator $ \existingOperator -> do
      mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId (show Notification.FLEET_UNLINK_FROM_OPERATOR) Nothing Nothing existingOperator.language Nothing
      whenJust mbMerchantPN $ \merchantPN -> do
        let title = T.replace "{#fleetName#}" fleetOwnerFullName . T.replace "{#fleetNo#}" fleetOwnerMobile $ merchantPN.title
        let body = T.replace "{#fleetName#}" fleetOwnerFullName . T.replace "{#fleetNo#}" fleetOwnerMobile $ merchantPN.body

        TN.notifyWithGRPCProvider merchantOpCityId Notification.FLEET_UNLINK_FROM_OPERATOR title body (Id existingAssociation.operatorId) Nothing ()

makeFleetOperatorAssociation ::
  (MonadFlow m) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  Maybe UTCTime ->
  m DFOA.FleetOperatorAssociation
makeFleetOperatorAssociation merchantId merchantOpCityId fleetOwnerId operatorId end = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    DFOA.FleetOperatorAssociation
      { id = id,
        operatorId = operatorId,
        isActive = True,
        fleetOwnerId = fleetOwnerId,
        associatedOn = Just now,
        associatedTill = end,
        createdAt = now,
        updatedAt = now,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOpCityId
      }

makeDriverOperatorAssociation ::
  (MonadFlow m) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Text ->
  Maybe UTCTime ->
  Bool ->
  m DDOA.DriverOperatorAssociation
makeDriverOperatorAssociation merchantId merchantOperatingCityId driverId operatorId end isActive = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    DDOA.DriverOperatorAssociation
      { id = id,
        driverId = driverId,
        isActive = isActive,
        operatorId = operatorId,
        associatedOn = Just now,
        associatedTill = end,
        onboardingVehicleCategory = Nothing,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }

isAssociationBetweenTwoPerson :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DP.Person -> DP.Person -> m Bool
isAssociationBetweenTwoPerson requestedPersonDetails personDetails = do
  case (requestedPersonDetails.role, personDetails.role) of
    (DP.OPERATOR, DP.DRIVER) -> checkFleetDriverAndDriverOperatorAssociation personDetails.id requestedPersonDetails.id
    (DP.OPERATOR, DP.FLEET_OWNER) -> checkFleetOperatorAssociation personDetails.id requestedPersonDetails.id
    (DP.OPERATOR, DP.FLEET_BUSINESS) -> checkFleetOperatorAssociation personDetails.id requestedPersonDetails.id
    (DP.OPERATOR, DP.OPERATOR) -> pure $ requestedPersonDetails.id.getId == personDetails.id.getId
    (DP.FLEET_OWNER, DP.DRIVER) -> checkFleetDriverAssociationIgnoringActive requestedPersonDetails.id personDetails.id
    (DP.FLEET_OWNER, DP.FLEET_OWNER) -> pure $ requestedPersonDetails.id.getId == personDetails.id.getId
    (DP.FLEET_OWNER, DP.FLEET_BUSINESS) -> pure $ requestedPersonDetails.id.getId == personDetails.id.getId
    (DP.FLEET_BUSINESS, DP.DRIVER) -> checkFleetDriverAssociationIgnoringActive requestedPersonDetails.id personDetails.id
    (DP.FLEET_BUSINESS, DP.FLEET_OWNER) -> pure $ requestedPersonDetails.id.getId == personDetails.id.getId
    (DP.FLEET_BUSINESS, DP.FLEET_BUSINESS) -> pure $ requestedPersonDetails.id.getId == personDetails.id.getId
    (DP.ADMIN, _) -> return True
    _ -> return True

checkFleetDriverAssociation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m Bool
checkFleetDriverAssociation fleetId driverId = do
  mbAssoc <- QFDA.findByDriverIdAndFleetOwnerId driverId fleetId.getId True
  return $ isJust mbAssoc

checkFleetDriverAssociationIgnoringActive :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m Bool
checkFleetDriverAssociationIgnoringActive fleetId driverId = do
  mbAssoc <- QFDA.findByDriverIdAndFleetOwnerIdWithStatus driverId fleetId.getId
  return $ isJust mbAssoc

-- | For a requestor holding the driver only through a pending invite: moving the driver would end
--   another fleet's or operator's active link without its consent, so it is refused.
ensureDriverNotActiveElsewhere :: Id DP.Person -> Flow ()
ensureDriverNotActiveElsewhere driverId = do
  hasActiveFleetLink <- not . null <$> QFDA.findAllByDriverId driverId True
  isActiveElsewhere <- if hasActiveFleetLink then pure True else isJust <$> QDOA.findByDriverId driverId True
  when isActiveElsewhere $ throwError (InvalidRequest "Driver is associated with another fleet or operator")

checkFleetOperatorAssociation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m Bool
checkFleetOperatorAssociation fleetId operatorId = do
  mbAssoc <- QFOA.findByFleetIdAndOperatorId fleetId.getId operatorId.getId True
  return $ isJust mbAssoc

checkDriverOperatorAssociation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m Bool
checkDriverOperatorAssociation driverId operatorId = do
  mbAssoc <- QDOA.findByDriverIdAndOperatorId driverId operatorId True
  return $ isJust mbAssoc

checkFleetDriverAndDriverOperatorAssociation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m Bool
checkFleetDriverAndDriverOperatorAssociation driverId operatorId = do
  isDriverOperatorAssociated <- checkDriverOperatorAssociation driverId operatorId
  if isDriverOperatorAssociated
    then return True
    else do
      QFDA.findByDriverId driverId True >>= \case
        Just fleetDriverAssoc -> checkFleetOperatorAssociation (Id fleetDriverAssoc.fleetOwnerId) operatorId
        Nothing -> return False

associateDriverWithOperator :: DM.Merchant -> DMOC.MerchantOperatingCity -> DP.Person -> DP.Person -> Bool -> Text -> Maybe DVC.VehicleCategory -> Flow ()
associateDriverWithOperator merchant moc person operator isNew driverMobile mbVehicleCategory = do
  unless isNew $ checkForDriverAssociationOverwrite merchant person.id
  QDOA.createDriverOperatorAssociationIfNotExists moc person.id operator.id (fromMaybe DVC.CAR mbVehicleCategory) False
  fork "Sending Operator Consent SMS to Driver" $
    sendOperatorDeepLinkForAuth person driverMobile moc.merchantId moc.id moc.country operator

-- | Links the driver to the operator as ACTIVE, without the consent step: what postOperatorConsent
--   does, minus the driver notification. Remaining active fleet/operator links and pending links to
--   other operators end; a pending link to this operator is activated in place. Caller holds the
--   driver's onboarding lock and owns the new operator's +1 analytics and the referral.
linkDriverToOperator :: DM.Merchant -> DMOC.MerchantOperatingCity -> TransporterConfig -> DP.Person -> DP.Person -> Flow ()
linkDriverToOperator merchant moc transporterConfig driver operator = do
  checkForDriverAssociationOverwrite merchant driver.id
  endAssociationsForOperatorLink moc.id transporterConfig driver
  pendingAssocs <- QDOA.findAllByDriverId driver.id False
  let isForOperator assoc = assoc.operatorId == operator.id.getId
  forM_ (nub $ (.operatorId) <$> filter (not . isForOperator) pendingAssocs) $ \otherOperatorId ->
    QDOA.endOperatorDriverAssociation otherOperatorId driver.id
  mbVehicleCategory <- (>>= (.onboardingVehicleCategory)) <$> QDI.findById (cast driver.id)
  case find isForOperator pendingAssocs of
    Just pending -> QDOA.activateById pending.id mbVehicleCategory
    Nothing -> do
      assoc <- makeDriverOperatorAssociation merchant.id moc.id driver.id operator.id.getId defaultAssociationEnd True
      QDOA.create assoc {DDOA.onboardingVehicleCategory = mbVehicleCategory}
  SOnboardingComms.setOnboardingAsWithoutLock transporterConfig driver DI.INDIVIDUAL
  -- Same as postOperatorConsent: cities without the unified recompute enable on linking.
  unless (transporterConfig.requiresOnboardingInspection == Just True || transporterConfig.unifiedOnboardingFlagsRecompute == Just True) $
    Analytics.updateEnabledVerifiedStateWithAnalytics Nothing transporterConfig driver.id True (Just True)

-- | Ends the driver's active fleet/operator links before an operator link. The vehicle goes only
--   when the driver leaves a fleet (it is the fleet's); from an operator or from no link it stays.
endAssociationsForOperatorLink :: Id DMOC.MerchantOperatingCity -> TransporterConfig -> DP.Person -> Flow ()
endAssociationsForOperatorLink merchantOpCityId transporterConfig driver = do
  leavesFleet <- not . null <$> QFDA.findAllByDriverId driver.id True
  endDriverAssociations merchantOpCityId transporterConfig driver
  when leavesFleet $
    DomainRC.endAllRCAssociationsAndRemoveVehicle transporterConfig driver.id

sendOperatorDeepLinkForAuth :: DP.Person -> Text -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Context.Country -> DP.Person -> Flow ()
sendOperatorDeepLinkForAuth person mobileNumber merchantId merchantOpCityId country operator = do
  let countryCode = fromMaybe (P.getCountryMobileCode country) person.mobileCountryCode
      phoneNumber = countryCode <> mobileNumber
  smsCfg <- asks (.smsCfg)
  withLogTag ("sending Operator Deeplink Auth SMS" <> getId person.id) $ do
    (mbSender, message, templateId, messageType) <-
      MessageBuilder.buildOperatorDeepLinkAuthMessage merchantOpCityId $
        MessageBuilder.BuildOperatorDeepLinkAuthMessage
          { operatorName = operator.firstName
          }
    let sender = fromMaybe smsCfg.sender mbSender
    Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId messageType) >>= Sms.checkSmsResult

adjustOperatorAnalytics :: TransporterConfig -> Id DP.Person -> Text -> Integer -> Flow ()
adjustOperatorAnalytics transporterConfig personId operatorId delta =
  fork ("Adjust Operator Analytics: " <> operatorId) $
    Analytics.handleDriverAnalyticsAndFlowStatus
      transporterConfig
      personId
      Nothing
      ( \driverInfo -> do
          activeSubscriptions <- QSubscriptionPurchaseExtra.countActiveSubscriptionsForOwner personId.getId DSP.DRIVER
          AnalyticsExtra.adjustOperatorDriverAssociationAnalytics transporterConfig operatorId delta activeSubscriptions driverInfo.enabled
      )
      ( \driverInfo ->
          if delta < (0 :: Integer)
            then DDriverMode.decrementFleetOperatorStatusKeyForDriver DP.OPERATOR operatorId driverInfo.driverFlowStatus
            else DDriverMode.incrementFleetOperatorStatusKeyForDriver DP.OPERATOR operatorId driverInfo.driverFlowStatus
      )

data AssociationChangeType
  = ChangeDriverOperator
  | ChangeFleetOperator

driverOperatorAssociationLockKey :: Id DP.Person -> Text
driverOperatorAssociationLockKey personId = "DriverOperatorAssociationChange:" <> personId.getId

fleetOperatorAssociationLockKey :: Text -> Text
fleetOperatorAssociationLockKey fleetOwnerId = "FleetOperatorAssociationChange:" <> fleetOwnerId

-- Mandatory operatorCode, mirroring the admin-triggered association-change requirements:
-- new operator must be specified up front, old deactivation + new activation happen
-- in the same logical (lock-guarded) block, and same-operator requests are a no-op.
-- Returns the operator a driver was newly linked to, for the caller's referral attribution.
performAssociationChange ::
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  Text ->
  AssociationChangeType ->
  Flow (Maybe DP.Person)
performAssociationChange merchant merchantOpCity requestorId subjectId operatorCode changeType = do
  let merchantOpCityId = merchantOpCity.id
  persons <- QP.findAllByPersonIdsAndMerchantOpsCityId [Id subjectId, Id requestorId] merchantOpCityId
  subject <- find (\p -> p.id == Id subjectId) persons & fromMaybeM (PersonDoesNotExist subjectId)
  unless (merchant.id == subject.merchantId && merchantOpCityId == subject.merchantOperatingCityId) $
    throwError (PersonDoesNotExist subjectId)
  let mbRequestor = find (\p -> p.id == Id requestorId) persons
  -- A driver change runs this under the driver's onboarding lock instead (changeDriverOperator).
  case changeType of
    ChangeFleetOperator -> ensureRequestorMayChange mbRequestor subject
    ChangeDriverOperator -> pure ()
  newOperator <- resolveOperatorByCode merchantOpCityId operatorCode
  unless (subject.role `elem` allowedSubjectRoles changeType) $
    throwError (InvalidRequest $ "Subject role " <> show subject.role <> " is not allowed for this association change")
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  case changeType of
    ChangeDriverOperator -> changeDriverOperator merchant merchantOpCity transporterConfig mbRequestor subject newOperator
    ChangeFleetOperator -> Nothing <$ changeFleetOperator merchant merchantOpCity transporterConfig subject newOperator
  where
    -- The requestor must be associated with the subject; admins (not persons here) and self-changes skip it.
    ensureRequestorMayChange mbRequestor subject =
      when (subjectId /= requestorId) $
        whenJust mbRequestor $ \requestor -> do
          isValid <- isAssociationBetweenTwoPerson requestor subject
          unless isValid $ throwError AccessDenied

    -- isAssociationBetweenTwoPerson lets a fleet act on a driver it only invited; such a fleet can't
    -- move a driver who is active elsewhere. Operators need an active link there already.
    ensurePendingFleetMayMove mbRequestor personId =
      when (subjectId /= requestorId) $
        whenJust mbRequestor $ \requestor ->
          when (requestor.role `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS]) $ do
            isActiveInRequestorFleet <- checkFleetDriverAssociation requestor.id personId
            unless isActiveInRequestorFleet $ ensureDriverNotActiveElsewhere personId

    -- Only driver -> operator and fleet -> operator links exist. isAssociationBetweenTwoPerson checks who may
    -- make the change; this checks what gets linked (no operator -> operator, no fleet owner as a driver).
    allowedSubjectRoles = \case
      ChangeDriverOperator -> [DP.DRIVER]
      ChangeFleetOperator -> [DP.FLEET_OWNER, DP.FLEET_BUSINESS]

    -- Moves the driver as a whole, like the fleet/operator removal APIs: active fleet and operator
    -- links end, and the new operator link is created active (no consent). Ownership and the current
    -- link are read under the onboarding lock, so a concurrent consent or move can't change them after.
    changeDriverOperator merchant' moc' transporterConfig mbRequestor subject newOperator = do
      let personId = subject.id
      Redis.withWaitAndLockRedis (driverOperatorAssociationLockKey personId) 10 50000 $ do
        -- Nothing: already with this operator. Just mbOld: moved, from mbOld's operator if any.
        mbMove <- SGuard.withOnboardingAction transporterConfig SGuard.None SGuard.UnlinkFromOperator (SGuard.TargetDriver personId) $ do
          ensureRequestorMayChange mbRequestor subject
          mbActiveAssociation <- QDOA.findByDriverId personId True
          case mbActiveAssociation of
            Just old | old.operatorId == newOperator.id.getId -> pure Nothing
            _ -> do
              ensurePendingFleetMayMove mbRequestor personId
              whenJust mbActiveAssociation $ \old -> QDOA.endOperatorDriverAssociation old.operatorId personId
              linkDriverToOperator merchant' moc' transporterConfig subject newOperator
              pure (Just mbActiveAssociation)
        -- Analytics after the guard's recompute, as before.
        forM mbMove $ \mbOld -> do
          whenJust mbOld $ \old -> adjustOperatorAnalytics transporterConfig personId old.operatorId (-1)
          adjustOperatorAnalytics transporterConfig personId newOperator.id.getId 1
          pure newOperator

    changeFleetOperator merchant' moc' transporterConfig subject newOperator = do
      let fleetOwnerId = subject.id.getId
      Redis.withWaitAndLockRedis (fleetOperatorAssociationLockKey fleetOwnerId) 10 50000 $ do
        mbActiveAssociation <- QFOA.findActiveByFleetOwnerId subject.id
        case mbActiveAssociation of
          Just old | old.operatorId == newOperator.id.getId -> pure ()
          _ -> do
            AC.withAssociation (AC.guardNoLiveRideInFleet fleetOwnerId) $ do
              whenJust mbActiveAssociation $ \old -> QFOA.endFleetOperatorAssociation (Id old.fleetOwnerId) (Id old.operatorId)
              newAssoc <- makeFleetOperatorAssociation merchant'.id moc'.id fleetOwnerId newOperator.id.getId defaultAssociationEnd
              QFOA.create newAssoc
            let allowCacheDriverFlowStatus = transporterConfig.analyticsConfig.allowCacheDriverFlowStatus
            when allowCacheDriverFlowStatus $ do
              whenJust mbActiveAssociation $ \old -> DDriverMode.decrementOperatorStatusKeyForFleetOwner old.operatorId fleetOwnerId
              DDriverMode.incrementOperatorStatusKeyForFleetOwner newOperator.id.getId fleetOwnerId
