{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module SharedLogic.DriverOnboarding.OnboardingComms
  ( setOnboardingAs,
    notifyOnFleetOwnerChange,
    UnlinkInitiator (..),
    notifyOnDriverFleetUnlink,
    notifyOnVehicleFleetUnlink,
  )
where

import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.Interface.Types as Notification
import Kernel.Prelude
import Kernel.Types.Forkable (Forkable, fork)
import Kernel.Types.Id
import qualified SharedLogic.DriverOnboarding.OnboardingFlags.Guard as SGuard
import SharedLogic.DriverOnboarding.OnboardingFlags.Types (OnboardingFlow)
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as TN

driverOnboardingAsChangedKey :: Text
driverOnboardingAsChangedKey = "DRIVER_ONBOARDING_AS_CHANGED"

fleetOwnerDriverOnboardingAsChangedKey :: Text
fleetOwnerDriverOnboardingAsChangedKey = "FLEET_OWNER_DRIVER_ONBOARDING_AS_CHANGED"

driverFleetOwnerChangedKey :: Text
driverFleetOwnerChangedKey = "DRIVER_FLEET_OWNER_CHANGED"

fleetOwnerDriverAddedKey :: Text
fleetOwnerDriverAddedKey = "FLEET_OWNER_DRIVER_ADDED"

fleetOwnerDriverRemovedKey :: Text
fleetOwnerDriverRemovedKey = "FLEET_OWNER_DRIVER_REMOVED"

driverUnlinkFromFleetKey :: Text
driverUnlinkFromFleetKey = "DRIVER_UNLINK_FROM_FLEET"

fleetOwnerDriverLeftKey :: Text
fleetOwnerDriverLeftKey = "FLEET_OWNER_DRIVER_LEFT"

driverRemovedFromFleetKey :: Text
driverRemovedFromFleetKey = "DRIVER_REMOVED_FROM_FLEET"

driverLeftFleetKey :: Text
driverLeftFleetKey = "DRIVER_LEFT_FLEET"

fleetOwnerVehicleUnlinkedKey :: Text
fleetOwnerVehicleUnlinkedKey = "FLEET_OWNER_VEHICLE_UNLINKED"

fleetVehicleUnlinkedKey :: Text
fleetVehicleUnlinkedKey = "FLEET_VEHICLE_UNLINKED"

driverDeclinedFleetInviteKey :: Text
driverDeclinedFleetInviteKey = "DRIVER_DECLINED_FLEET_INVITE"

fleetOwnerDriverDeclinedInviteKey :: Text
fleetOwnerDriverDeclinedInviteKey = "FLEET_OWNER_DRIVER_DECLINED_INVITE"

setOnboardingAs :: (OnboardingFlow m r, Forkable m) => DTC.TransporterConfig -> DP.Person -> DI.OnboardingAs -> m ()
setOnboardingAs transporterConfig driver onboardingAs = do
  endFleetAssociationsOnLeavingFleetDriver
  SGuard.withOnboardingAction transporterConfig SGuard.None SGuard.SetOnboardingAs (SGuard.TargetDriver driver.id) $
    QDIExtra.updateOnboardingAs (Just onboardingAs) (cast driver.id)
  fork "Onboarding as changed notification" $ do
    notifyOnOnboardingAsChange driver onboardingAs
  where
    endFleetAssociationsOnLeavingFleetDriver =
      when (onboardingAs /= DI.FLEET_DRIVER) $ do
        mbDriverInfo <- QDIExtra.findById (cast driver.id)
        when (((.onboardingAs) =<< mbDriverInfo) == Just DI.FLEET_DRIVER) $ do
          activeAssociations <- QFDA.findAllByDriverId driver.id True
          forM_ activeAssociations $ \association ->
            SGuard.withOnboardingAction transporterConfig (SGuard.ActorFleetAndDriver (Id association.fleetOwnerId) driver.id) SGuard.UnlinkFromFleet (SGuard.TargetDriver driver.id) $ do
              QFDA.endFleetDriverAssociation association.fleetOwnerId association.driverId
              fork "Driver fleet unlink notification" $
                notifyOnDriverFleetUnlink driver.merchantOperatingCityId driver association.fleetOwnerId ByDriver

notifyOnOnboardingAsChange :: OnboardingFlow m r => DP.Person -> DI.OnboardingAs -> m ()
notifyOnOnboardingAsChange driver onboardingAs = do
  let merchantOpCityId = driver.merchantOperatingCityId
      driverName = mkDriverName driver
      dynamicParams = [("driverName", driverName), ("onboardingAs", show onboardingAs)]
      entityData =
        TN.OnboardingChangeEntityData
          { driverId = driver.id.getId,
            driverName = driverName,
            fleetOwnerId = Nothing,
            onboardingAs = Just (show onboardingAs)
          }
  mbDriverInfo <- QDIExtra.findById (cast driver.id)
  mbFleetOwner <- findActiveFleetOwner driver.id
  when (maybe False (.enabled) mbDriverInfo || isJust mbFleetOwner) $ do
    TN.notifyDriverOnMerchantPN merchantOpCityId driver driverOnboardingAsChangedKey Notification.DRIVER_NOTIFY dynamicParams entityData
    whenJust mbFleetOwner $ \fleetOwner ->
      TN.notifyFleetOwnerOnMerchantPN merchantOpCityId fleetOwner fleetOwnerDriverOnboardingAsChangedKey Notification.DRIVER_NOTIFY dynamicParams entityData

notifyOnFleetOwnerChange :: OnboardingFlow m r => Id DMOC.MerchantOperatingCity -> DP.Person -> DP.Person -> Maybe DP.Person -> m ()
notifyOnFleetOwnerChange merchantOpCityId driver newFleetOwner mbOldFleetOwner = do
  let driverName = mkDriverName driver
      dynamicParams =
        [ ("driverName", driverName),
          ("newFleetOwnerName", newFleetOwner.firstName),
          ("oldFleetOwnerName", maybe "" (.firstName) mbOldFleetOwner)
        ]
      entityData =
        TN.OnboardingChangeEntityData
          { driverId = driver.id.getId,
            driverName = driverName,
            fleetOwnerId = Just newFleetOwner.id.getId,
            onboardingAs = Nothing
          }
  whenJust mbOldFleetOwner $ \oldFleetOwner -> do
    TN.notifyDriverOnMerchantPN merchantOpCityId driver driverFleetOwnerChangedKey Notification.DRIVER_NOTIFY dynamicParams entityData
    TN.notifyFleetOwnerOnMerchantPN merchantOpCityId newFleetOwner fleetOwnerDriverAddedKey Notification.DRIVER_NOTIFY dynamicParams entityData
    TN.notifyFleetOwnerOnMerchantPN merchantOpCityId oldFleetOwner fleetOwnerDriverRemovedKey Notification.DRIVER_NOTIFY dynamicParams entityData

data UnlinkInitiator = ByFleetOwner | ByDriver | ByAdmin | ByDriverConsentDecline
  deriving (Show, Eq)

notifyOnDriverFleetUnlink :: OnboardingFlow m r => Id DMOC.MerchantOperatingCity -> DP.Person -> Text -> UnlinkInitiator -> m ()
notifyOnDriverFleetUnlink merchantOpCityId driver fleetOwnerId initiator = do
  mbFleetOwner <- QPerson.findById (Id fleetOwnerId)
  driverNo <- maybe (pure "unknown") decrypt driver.mobileNumber
  let driverName = mkDriverName driver
      dynamicParams =
        [ ("driverName", driverName),
          ("driverNo", driverNo),
          ("fleetName", maybe "" mkDriverName mbFleetOwner)
        ]
      entityData =
        TN.FleetUnlinkEntityData
          { driverId = Just driver.id.getId,
            driverName = Just driverName,
            fleetOwnerId = Just fleetOwnerId,
            vehicleNo = Nothing
          }
      (driverKey, fleetOwnerKey) = case initiator of
        ByDriver -> (driverLeftFleetKey, fleetOwnerDriverLeftKey)
        ByDriverConsentDecline -> (driverDeclinedFleetInviteKey, fleetOwnerDriverDeclinedInviteKey)
        _ -> (driverRemovedFromFleetKey, driverUnlinkFromFleetKey)
  TN.notifyDriverOnMerchantPN merchantOpCityId driver driverKey Notification.DRIVER_NOTIFY dynamicParams entityData
  whenJust mbFleetOwner $ \fleetOwner ->
    TN.notifyFleetOwnerOnMerchantPN merchantOpCityId fleetOwner fleetOwnerKey Notification.DRIVER_UNLINK_FROM_FLEET dynamicParams entityData

notifyOnVehicleFleetUnlink :: OnboardingFlow m r => Id DMOC.MerchantOperatingCity -> Text -> Maybe Text -> [DP.Person] -> m ()
notifyOnVehicleFleetUnlink merchantOpCityId vehicleNo mbFleetOwnerId drivers = do
  mbFleetOwner <- maybe (pure Nothing) (QPerson.findById . Id) mbFleetOwnerId
  let mkParams mbDriver =
        [ ("vehicleNo", vehicleNo),
          ("driverName", maybe "" mkDriverName mbDriver),
          ("fleetName", maybe "" mkDriverName mbFleetOwner)
        ]
      mkEntityData mbDriver =
        TN.FleetUnlinkEntityData
          { driverId = (.id.getId) <$> mbDriver,
            driverName = mkDriverName <$> mbDriver,
            fleetOwnerId = mbFleetOwnerId,
            vehicleNo = Just vehicleNo
          }
  whenJust mbFleetOwner $ \fleetOwner ->
    TN.notifyFleetOwnerOnMerchantPN merchantOpCityId fleetOwner fleetOwnerVehicleUnlinkedKey Notification.DRIVER_NOTIFY (mkParams Nothing) (mkEntityData Nothing)
  forM_ drivers $ \driver ->
    TN.notifyDriverOnMerchantPN merchantOpCityId driver fleetVehicleUnlinkedKey Notification.DRIVER_NOTIFY (mkParams $ Just driver) (mkEntityData $ Just driver)

mkDriverName :: DP.Person -> Text
mkDriverName driver = driver.firstName <> maybe "" (" " <>) driver.lastName

findActiveFleetOwner :: OnboardingFlow m r => Id DP.Person -> m (Maybe DP.Person)
findActiveFleetOwner driverId = do
  mbAssoc <- QFDA.findByDriverId driverId True
  maybe (pure Nothing) (QPerson.findById . Id . (.fleetOwnerId)) mbAssoc
