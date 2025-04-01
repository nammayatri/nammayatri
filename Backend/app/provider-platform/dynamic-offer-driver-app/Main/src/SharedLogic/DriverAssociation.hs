module SharedLogic.DriverAssociation (endActiveAssociationsIfAllowed) where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.FleetDriverAssociation as QFDA

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
