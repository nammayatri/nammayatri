module SharedLogic.DriverFleetOperatorAssociation
  ( endDriverAssociationsIfAllowed,
    endFleetAssociationsIfAllowed,
    makeFleetOperatorAssociation,
    makeDriverOperatorAssociation,
  )
where

import qualified Domain.Types.DriverOperatorAssociation as DDOA
import qualified Domain.Types.FleetOperatorAssociation as DFOA
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOperatorAssociation as QFOA

endDriverAssociationsIfAllowed ::
  DM.Merchant ->
  Id DP.Person ->
  Flow ()
endDriverAssociationsIfAllowed merchant personId = do
  existingFDAssociations <- QFDA.findAllByDriverId personId True
  unless (null existingFDAssociations) $ do
    if merchant.overwriteAssociation == Just True
      then forM_ existingFDAssociations $ \existingAssociation -> do
        logInfo $ "End existing fleet driver association: fleetOwnerId: " <> existingAssociation.fleetOwnerId <> "driverId: " <> existingAssociation.driverId.getId
        QFDA.endFleetDriverAssociation existingAssociation.fleetOwnerId existingAssociation.driverId
      else throwError (InvalidRequest "Driver already associated with another fleet")

  existingDOAssociations <- QDOA.findAllByDriverId personId True
  unless (null existingDOAssociations) $ do
    if merchant.overwriteAssociation == Just True
      then forM_ existingDOAssociations $ \existingAssociation -> do
        logInfo $ "End existing operator driver association: operatorId: " <> existingAssociation.operatorId <> "driverId: " <> existingAssociation.driverId.getId
        QDOA.endOperatorDriverAssociation existingAssociation.operatorId existingAssociation.driverId
      else throwError (InvalidRequest "Driver already associated with another operator")

endFleetAssociationsIfAllowed ::
  DM.Merchant ->
  Text ->
  Flow ()
endFleetAssociationsIfAllowed merchant fleetOwnerId = do
  existingFOAssociations <- QFOA.findAllByFleetOwnerId (Id fleetOwnerId) True
  unless (null existingFOAssociations) $ do
    if merchant.overwriteAssociation == Just True
      then forM_ existingFOAssociations $ \existingAssociation -> do
        logInfo $ "End existing fleet operator association: fleetOwnerId: " <> existingAssociation.fleetOwnerId <> "operatorId: " <> existingAssociation.operatorId
        QFOA.endFleetOperatorAssociation (Id existingAssociation.fleetOwnerId) (Id existingAssociation.operatorId)
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
