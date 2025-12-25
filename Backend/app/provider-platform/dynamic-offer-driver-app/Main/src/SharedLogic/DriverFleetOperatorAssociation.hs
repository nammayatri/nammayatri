module SharedLogic.DriverFleetOperatorAssociation
  ( checkForDriverAssociationOverwrite,
    endDriverAssociationsIfAllowed,
    endFleetAssociationsIfAllowed,
    makeFleetOperatorAssociation,
    makeDriverOperatorAssociation,
  )
where

import qualified Data.Text as T
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Types.DriverOperatorAssociation as DDOA
import qualified Domain.Types.FleetOperatorAssociation as DFOA
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
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
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import qualified Storage.Queries.Person as QP
import qualified Tools.Notifications as TN

checkForDriverAssociationOverwrite ::
  DM.Merchant ->
  Id DP.Person ->
  Flow ()
checkForDriverAssociationOverwrite merchant driverId = do
  unless (merchant.overwriteAssociation == Just True) $ do
    existingFDAssociations <- QFDA.findAllByDriverId driverId True
    unless (null existingFDAssociations) $ do
      throwError (InvalidRequest "Driver already associated with a fleet")

    existingDOAssociations <- QDOA.findAllByDriverId driverId True
    unless (null existingDOAssociations) $ do
      throwError (InvalidRequest "Driver is already associated with a fleet")

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
          ( \driverInfo -> do
              Analytics.decrementFleetOwnerAnalyticsActiveDriverCount (Just existingAssociation.fleetOwnerId) existingAssociation.driverId
              mbOperator <- QFOA.findByFleetOwnerId existingAssociation.fleetOwnerId True
              when (isNothing mbOperator) $ logTagError "AnalyticsRemoveDriver" "Operator not found for fleet owner"
              whenJust mbOperator $ \operator -> do
                when driverInfo.enabled $ Analytics.decrementOperatorAnalyticsDriverEnabled transporterConfig operator.operatorId
                Analytics.decrementOperatorAnalyticsActiveDriver transporterConfig operator.operatorId
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
            TN.notifyWithGRPCProvider merchantOpCityId Notification.DRIVER_UNLINK_FROM_FLEET title body (Id existingAssociation.fleetOwnerId) ()
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
              when driverInfo.enabled $ Analytics.decrementOperatorAnalyticsDriverEnabled transporterConfig existingAssociation.operatorId
              Analytics.decrementOperatorAnalyticsActiveDriver transporterConfig existingAssociation.operatorId
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
            TN.notifyWithGRPCProvider merchantOpCityId Notification.DRIVER_UNLINK_FROM_OPERATOR title body (Id existingAssociation.operatorId) ()
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
  unless (null existingFOAssociations) $ do
    if merchant.overwriteAssociation == Just True
      then forM_ existingFOAssociations $ \existingAssociation -> do
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

            TN.notifyWithGRPCProvider merchantOpCityId Notification.FLEET_UNLINK_FROM_OPERATOR title body (Id existingAssociation.operatorId) ()
      else throwError (InvalidRequest "Fleet already associated with another operator")

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
