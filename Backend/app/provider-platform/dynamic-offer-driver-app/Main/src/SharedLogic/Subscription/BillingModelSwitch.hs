{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Moving a driver between ride billing models (`driver_information.ride_billing_model`).
--
-- Once a driver has a model (set at registration), it moves only here -- by the driver
-- (preview, then switch) or by the dashboard's bulk catalogue update enrolling drivers into
-- prepaid -- with an audit row each time (`billing_model_switch_history`). The one exception
-- is a city that does not bill rides by model, where a prepaid purchase still moves the
-- driver onto prepaid, as it always has.
--
-- A switch is refused whenever it could lose revenue or leave a ride billed under the
-- wrong model; see 'SwitchBlocker'. It never touches `driver_plan` or the mandate: a
-- postpaid plan is left as it was, so moving back resumes it.
module SharedLogic.Subscription.BillingModelSwitch
  ( SwitchBlocker (..),
    SwitchPreview (..),
    previewSwitch,
    switchByDriver,
    enrollInPrepaidByDashboard,
    hasSwitchedOutOf,
    postpaidChargedCategories,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.BillingModelSwitchHistory as DBMSH
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.Extra.Plan (ServiceNames (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SubscriptionPurchase as DSP
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleVariant as DVV
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.Subscription.BillingModel (isRideBillingModelEnabled, modelOrCatalogue, rideBillingModelLockKey)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.BillingModelSwitchHistory as QBMSH
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SubscriptionPurchase as QSP
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

-- | Why a switch cannot happen now.
data SwitchBlocker
  = -- | This city does not bill rides by model ('isRideBillingModelEnabled').
    SwitchUnavailable
  | AlreadyOnTarget
  | -- | The driver is in a fleet; the fleet's billing decides how their rides are paid.
    FleetBillingManaged
  | -- | The target is not in the driver's catalogue, not live in the city, or not running
    -- at the merchant (prepaid off, or postpaid billing off).
    TargetNotOffered
  | DriverOnline
  | -- | A new, scheduled or in-progress ride: it keeps the model it was assigned under.
    ActiveRideExists
  | -- | A prepaid purchase is awaiting payment.
    PaymentInProgress
  | SwitchCooldown
  | -- | Another switch, or a ride assignment, holds the driver's billing-model lock.
    SwitchInProgress
  | -- | Postpaid dues are owed or may still be (YATRI_SUBSCRIPTION -> PREPAID_SUBSCRIPTION).
    PostpaidDuesOutstanding
  | -- | The driver holds prepaid credits that go unused on postpaid and has not confirmed it.
    AcknowledgementRequired
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data SwitchPreview = SwitchPreview
  { currentModel :: ServiceNames,
    targetModel :: ServiceNames,
    canSwitch :: Bool,
    -- | Every reason the switch would be refused now, 'AcknowledgementRequired' included.
    blockers :: [SwitchBlocker],
    holdsPrepaidCredits :: Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Who is switching. The dashboard enrols drivers in bulk, so it is not held to the
-- checks that only make sense for a driver acting on their own account.
data Requester = RequestedByDriver | RequestedByDashboard
  deriving (Eq)

data SwitchContext = SwitchContext
  { person :: DP.Person,
    driverInfo :: DI.DriverInformation,
    transporterConfig :: TransporterConfig,
    currentModel :: ServiceNames,
    holdsPrepaidCredits :: Bool
  }

-- | Postpaid fee statuses that mean money is owed, or may yet be.
outstandingDueStatuses :: [DDF.DriverFeeStatus]
outstandingDueStatuses = [DDF.ONGOING, DDF.PAYMENT_PENDING, DDF.PAYMENT_OVERDUE, DDF.MANUAL_REVIEW_NEEDED, DDF.IN_DISPUTE_WINDOW]

-- | How long a pending prepaid purchase counts as a payment in progress. A payment link
-- left unpaid past this no longer holds the driver back.
pendingPurchaseWindow :: NominalDiffTime
pendingPurchaseWindow = 30 * 60

defaultCooldownHours :: Int
defaultCooldownHours = 24

loadContext :: Id DP.Person -> Flow SwitchContext
loadContext personId = do
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  driverInfo <- QDI.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  loadContextFor person driverInfo

loadContextFor :: DP.Person -> DI.DriverInformation -> Flow SwitchContext
loadContextFor person driverInfo = do
  let personId = person.id
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  activePurchases <- QSP.findAllByOwnerAndStatus personId.getId DSP.DRIVER DSP.ACTIVE
  pure
    SwitchContext
      { person,
        driverInfo,
        transporterConfig,
        currentModel = fromMaybe YATRI_SUBSCRIPTION (modelOrCatalogue driverInfo.rideBillingModel driverInfo.servicesEnabledForSubscription),
        holdsPrepaidCredits = any ((== PREPAID_SUBSCRIPTION) . (.serviceName)) activePurchases
      }

-- | All reasons the switch is refused now. Empty means it may go ahead.
evaluateBlockers :: Requester -> Bool -> ServiceNames -> SwitchContext -> Flow [SwitchBlocker]
evaluateBlockers requester acknowledged target ctx = do
  if not (isRideBillingModelEnabled ctx.transporterConfig.subscriptionConfig)
    then pure [SwitchUnavailable]
    else
      if ctx.currentModel == target
        then pure [AlreadyOnTarget]
        else do
          now <- getCurrentTime
          let driverId = ctx.person.id
          fleetManaged <-
            if ctx.person.role `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS]
              then pure True
              else isJust <$> QFDA.findByDriverId driverId True
          targetOffered <- isTargetOffered requester target ctx
          activeRide <- isJust <$> QRide.getUpcomingOrActiveByDriverId driverId
          pendingPurchases <- QSP.findAllByOwnerAndStatus driverId.getId DSP.DRIVER DSP.PENDING
          let paymentInProgress = any (\p -> diffUTCTime now p.createdAt < pendingPurchaseWindow) pendingPurchases
          inCooldown <-
            if requester == RequestedByDriver
              then do
                let cooldown = fromIntegral (fromMaybe defaultCooldownHours ctx.transporterConfig.subscriptionConfig.rideBillingModelSwitchCooldownHours) * 3600
                maybe False (\h -> diffUTCTime now h.createdAt < cooldown) . listToMaybe <$> QBMSH.findLatestByDriverId (Just 1) Nothing driverId
              else pure False
          duesOutstanding <-
            if target == PREPAID_SUBSCRIPTION
              then
                if not ctx.driverInfo.subscribed
                  then pure True
                  else isJust <$> QDF.findOneOutstandingPostpaidDue driverId outstandingDueStatuses
              else pure False
          let isOnline = case ctx.driverInfo.mode of
                Just mode -> mode `elem` [DCommon.ONLINE, DCommon.SILENT]
                Nothing -> ctx.driverInfo.active
          pure $
            [FleetBillingManaged | fleetManaged]
              <> [TargetNotOffered | not targetOffered]
              <> [DriverOnline | requester == RequestedByDriver && isOnline]
              <> [ActiveRideExists | activeRide]
              <> [PaymentInProgress | paymentInProgress]
              <> [SwitchCooldown | inCooldown]
              <> [PostpaidDuesOutstanding | duesOutstanding]
              <> [AcknowledgementRequired | target == YATRI_SUBSCRIPTION && ctx.holdsPrepaidCredits && not acknowledged]

-- | The target must be one the driver may be on, and one the merchant and city run. The
-- dashboard adds the target to the catalogue itself, so its catalogue is not checked.
-- Postpaid also has to charge the driver's vehicle category; otherwise the driver would
-- ride uncharged.
isTargetOffered :: Requester -> ServiceNames -> SwitchContext -> Flow Bool
isTargetOffered requester target ctx = do
  merchant <- CQM.findById ctx.person.merchantId >>= fromMaybeM (MerchantNotFound ctx.person.merchantId.getId)
  mbSubscriptionConfig <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName ctx.person.merchantOperatingCityId Nothing target
  let inCatalogue = requester == RequestedByDashboard || target `elem` ctx.driverInfo.servicesEnabledForSubscription
      liveInCity = maybe False (.isUIEnabled) mbSubscriptionConfig
  runByMerchant <- case target of
    PREPAID_SUBSCRIPTION -> pure $ fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
    YATRI_SUBSCRIPTION -> do
      mbVehicle <- QVehicle.findById ctx.person.id
      chargedCategories <- postpaidChargedCategories ctx.person.merchantOperatingCityId ctx.transporterConfig Nothing
      pure $ maybe False (\v -> fromMaybe (DVV.castVehicleVariantToVehicleCategory v.variant) v.category `elem` chargedCategories) mbVehicle
    _ -> pure False
  pure $ inCatalogue && liveInCity && runByMerchant

-- | Vehicle categories whose rides the city charges postpaid: postpaid billing is on and
-- the YATRI_SUBSCRIPTION config charges the category, the two conditions end-ride's
-- createDriverFee checks before charging a ride. Empty when postpaid is off.
postpaidChargedCategories :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> TransporterConfig -> Maybe [LYT.ConfigVersionMap] -> m [DVC.VehicleCategory]
postpaidChargedCategories merchantOpCityId transporterConfig mbConfigVersions =
  if not transporterConfig.subscription
    then pure []
    else maybe [] (fromMaybe [] . (.executionEnabledForVehicleCategories)) <$> CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId mbConfigVersions YATRI_SUBSCRIPTION

previewSwitch :: Id DP.Person -> ServiceNames -> Flow SwitchPreview
previewSwitch personId target = do
  ctx <- loadContext personId
  switchLocked <- isSwitchLocked personId
  blockers <- (<> [SwitchInProgress | switchLocked]) <$> evaluateBlockers RequestedByDriver False target ctx
  pure
    SwitchPreview
      { currentModel = ctx.currentModel,
        targetModel = target,
        canSwitch = null blockers,
        blockers,
        holdsPrepaidCredits = ctx.holdsPrepaidCredits
      }

-- | The driver switches their own model.
--
-- Holds the billing-model lock, which ride assignment also takes, and the go-online lock,
-- so the driver can neither be assigned a ride nor go online between the checks and the
-- write. Every check is made again under the locks.
switchByDriver :: Id DP.Person -> ServiceNames -> Bool -> Flow ()
switchByDriver personId target acknowledged =
  withSwitchLock personId $
    withGoOnlineLock personId $ do
      ctx <- loadContext personId
      blockers <- evaluateBlockers RequestedByDriver acknowledged target ctx
      unless (null blockers) $ throwError $ RideBillingModelSwitchBlocked (map show blockers)
      applySwitch DBMSH.DRIVER (Just personId.getId) acknowledged target ctx

-- | The dashboard's bulk catalogue update enrols drivers into prepaid. Where rides are
-- billed by model, the whole batch is refused if any driver cannot be moved now, with each
-- driver's reasons, and nothing changes; otherwise the catalogue is updated and each driver
-- moved. Drivers already on prepaid are left alone. Elsewhere it enrols everyone, as the
-- catalogue alone used to. Unknown driver ids are skipped, as the catalogue update does.
enrollInPrepaidByDashboard :: Maybe Text -> [Text] -> Flow () -> Flow ()
enrollInPrepaidByDashboard mbRequestedBy driverIds updateCatalogue = do
  contexts <-
    fmap catMaybes . forM driverIds $ \driverId -> do
      mbPerson <- QP.findById (Id driverId)
      mbDriverInfo <- QDI.findById (Id driverId)
      case (mbPerson, mbDriverInfo) of
        (Just person, Just driverInfo) -> Just <$> loadContextFor person driverInfo
        _ -> pure Nothing
  let toMove = filter ((/= PREPAID_SUBSCRIPTION) . (.currentModel)) contexts
  refusals <-
    fmap catMaybes . forM toMove $ \ctx ->
      if isRideBillingModelEnabled ctx.transporterConfig.subscriptionConfig
        then do
          switchLocked <- isSwitchLocked ctx.person.id
          blockers <- (<> [SwitchInProgress | switchLocked]) <$> evaluateBlockers RequestedByDashboard False PREPAID_SUBSCRIPTION ctx
          pure $ if null blockers then Nothing else Just (ctx.person.id.getId <> ": " <> T.intercalate " " (map show blockers))
        else pure Nothing
  unless (null refusals) $ throwError $ RideBillingModelSwitchBlocked refusals
  updateCatalogue
  forM_ toMove $ \ctx ->
    withSwitchLock ctx.person.id $
      applySwitch DBMSH.DASHBOARD mbRequestedBy False PREPAID_SUBSCRIPTION ctx

-- | Whether the driver has ever been moved off the given model.
hasSwitchedOutOf :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => ServiceNames -> Id DP.Person -> m Bool
hasSwitchedOutOf model personId =
  any ((== model) . (.fromModel)) <$> QBMSH.findLatestByDriverId Nothing Nothing personId

applySwitch :: DBMSH.BillingModelSwitchSource -> Maybe Text -> Bool -> ServiceNames -> SwitchContext -> Flow ()
applySwitch source mbRequestedBy acknowledged target ctx = do
  now <- getCurrentTime
  historyId <- generateGUID
  QDI.updateRideBillingModel (Just target) ctx.person.id
  QBMSH.create
    DBMSH.BillingModelSwitchHistory
      { id = Id historyId,
        driverId = ctx.person.id,
        fromModel = ctx.currentModel,
        toModel = target,
        source,
        requestedBy = mbRequestedBy,
        acknowledgedUnusedCredits = acknowledged,
        merchantId = ctx.person.merchantId,
        merchantOperatingCityId = ctx.person.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }
  logInfo $ "Ride billing model of driver " <> ctx.person.id.getId <> " switched from " <> show ctx.currentModel <> " to " <> show target <> " by " <> show source

withSwitchLock :: Id DP.Person -> Flow a -> Flow a
withSwitchLock personId action = do
  locked <- Redis.tryLockRedis (rideBillingModelLockKey personId.getId) 5
  unless locked $ throwError $ RideBillingModelSwitchBlocked [show SwitchInProgress]
  finally action (Redis.unlockRedis (rideBillingModelLockKey personId.getId))

-- | The lock setActivity takes (Domain.Action.UI.Driver); the key must match it exactly.
withGoOnlineLock :: Id DP.Person -> Flow a -> Flow a
withGoOnlineLock personId action = do
  let key = "Driver:SetActivity:" <> show personId
  locked <- Redis.tryLockRedis key 5
  unless locked $ throwError $ RideBillingModelSwitchBlocked [show SwitchInProgress]
  finally action (Redis.unlockRedis key)

isSwitchLocked :: Id DP.Person -> Flow Bool
isSwitchLocked personId =
  isJust <$> (Redis.get (Redis.buildLockResourceName (rideBillingModelLockKey personId.getId)) :: Flow (Maybe ()))
