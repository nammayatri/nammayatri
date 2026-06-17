module SharedLogic.DriverFleetOperatorAssociation
  ( checkForDriverAssociationOverwrite,
    guardDriverNotAssociated,
    endDriverAssociationsIfAllowed,
    endFleetAssociationsIfAllowed,
    makeFleetOperatorAssociation,
    makeDriverOperatorAssociation,
    isAssociationBetweenTwoPerson,
    checkFleetDriverAssociation,
    checkFleetOperatorAssociation,
    checkDriverOperatorAssociation,
    resolveOperatorByCode,
  )
where

import Data.List.Extra (notNull)
import qualified Data.Text as T
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Types.DriverOperatorAssociation as DDOA
import qualified Domain.Types.FleetOperatorAssociation as DFOA
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SubscriptionPurchase as DSP
import Domain.Types.TransporterConfig
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification as Notification
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Analytics as Analytics
import SharedLogic.AnalyticsExtra as AnalyticsExtra
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubscriptionPurchaseExtra
import qualified Tools.Notifications as TN

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

endDriverAssociationsIfAllowed ::
  DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TransporterConfig ->
  DP.Person ->
  Flow ()
endDriverAssociationsIfAllowed merchant merchantOpCityId transporterConfig driver = do
  let driverFullName = driver.firstName <> maybe "" (" " <>) driver.lastName
  driverMobile <- maybe (pure "unknown") decrypt driver.mobileNumber
  existingFDAssociations <- QFDA.findAllByDriverId driver.id True
  unless (null existingFDAssociations) $ do
    if merchant.overwriteAssociation == Just True
      then forM_ existingFDAssociations $ \existingAssociation -> do
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

        -- send notification to existing fleet owner about driver unlink
        mbExistingFleetOwner <- B.runInReplica $ QP.findById (Id existingAssociation.fleetOwnerId :: Id DP.Person)
        whenJust mbExistingFleetOwner $ \existingFleetOwner -> do
          mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId (show Notification.DRIVER_UNLINK_FROM_FLEET) Nothing Nothing existingFleetOwner.language Nothing
          whenJust mbMerchantPN $ \merchantPN -> do
            let title = T.replace "{#driverName#}" driverFullName . T.replace "{#driverNo#}" driverMobile $ merchantPN.title
            let body = T.replace "{#driverName#}" driverFullName . T.replace "{#driverNo#}" driverMobile $ merchantPN.body
            TN.notifyWithGRPCProvider merchantOpCityId Notification.DRIVER_UNLINK_FROM_FLEET title body (Id existingAssociation.fleetOwnerId) Nothing ()
      else throwError (InvalidRequest "Driver is already associated with a fleet")

  existingDOAssociations <- QDOA.findAllByDriverId driver.id True
  unless (null existingDOAssociations) $ do
    if merchant.overwriteAssociation == Just True
      then forM_ existingDOAssociations $ \existingAssociation -> do
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
      else throwError (InvalidRequest "Driver already associated with another operator")

endFleetAssociationsIfAllowed ::
  DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  TransporterConfig ->
  DP.Person ->
  Flow ()
endFleetAssociationsIfAllowed merchant merchantOpCityId transporterConfig fleetOwner = do
  let fleetOwnerFullName = fleetOwner.firstName <> maybe "" (" " <>) fleetOwner.lastName
  fleetOwnerMobile <- maybe (pure "unknown") decrypt fleetOwner.mobileNumber
  existingFOAssociations <- QFOA.findAllByFleetOwnerId fleetOwner.id True
  -- If overwriteAssociation is enabled, end all existing associations
  -- Otherwise, allow multiple operators (no action needed)
  when (merchant.overwriteAssociation == Just True && notNull existingFOAssociations) $ do
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
  m DDOA.DriverOperatorAssociation
makeDriverOperatorAssociation merchantId merchantOperatingCityId driverId operatorId end = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    DDOA.DriverOperatorAssociation
      { id = id,
        driverId = driverId,
        isActive = True,
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
    (DP.FLEET_OWNER, DP.DRIVER) -> checkFleetDriverAssociation requestedPersonDetails.id personDetails.id
    (DP.FLEET_OWNER, DP.FLEET_OWNER) -> pure $ requestedPersonDetails.id.getId == personDetails.id.getId
    (DP.FLEET_OWNER, DP.FLEET_BUSINESS) -> pure $ requestedPersonDetails.id.getId == personDetails.id.getId
    (DP.FLEET_BUSINESS, DP.DRIVER) -> checkFleetDriverAssociation requestedPersonDetails.id personDetails.id
    (DP.FLEET_BUSINESS, DP.FLEET_OWNER) -> pure $ requestedPersonDetails.id.getId == personDetails.id.getId
    (DP.FLEET_BUSINESS, DP.FLEET_BUSINESS) -> pure $ requestedPersonDetails.id.getId == personDetails.id.getId
    (DP.ADMIN, _) -> return True
    _ -> return True

checkFleetDriverAssociation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m Bool
checkFleetDriverAssociation fleetId driverId = do
  mbAssoc <- QFDA.findByDriverIdAndFleetOwnerId driverId fleetId.getId True
  return $ isJust mbAssoc

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
