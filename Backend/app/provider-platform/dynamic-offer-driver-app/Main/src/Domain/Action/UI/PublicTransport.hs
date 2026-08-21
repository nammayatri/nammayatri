module Domain.Action.UI.PublicTransport
  ( getPublicTransportBlockedVehicles,
    postPublicTransportVehicleDataBlock,
    isVehicleBlocked,
  )
where

import qualified API.Types.UI.PublicTransport as APIT
import qualified BecknV2.FRFS.Enums as BecknSpec
import qualified Domain.Types.DepotManager as DDM
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleActionHistory as DVAH
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.Queries.DepotManager as QDM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.VehicleActionHistory as QVAH
import Tools.Auth ()
import Tools.Error

-- | Default TTL (10 min) for a per-vehicle block entry when the IBPP config
-- does not specify one. Matches rider-app behaviour.
defaultBusBlockExpiry :: Seconds
defaultBusBlockExpiry = Seconds 600

-- | Default cap on concurrently-blocked vehicles per checker.
defaultBusBlockMaxLimit :: Int
defaultBusBlockMaxLimit = 1

mkBusBlockKey :: Kernel.Types.Id.Id DIBC.IntegratedBPPConfig -> Text -> Text
mkBusBlockKey cfgId vehicleNumber = cfgId.getId <> ":blocked:" <> vehicleNumber

mkCheckerBlockedListKey :: Kernel.Types.Id.Id DP.Person -> Text
mkCheckerBlockedListKey personId = "blocked:checker:" <> personId.getId

-- | Resolve caller → (Person, DepotManager) and enforce the BUS_DISPATCHER
-- role gate + isBlockAllowed flag before any block/unblock mutation.
requireBlockPermittedDispatcher ::
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person) ->
  Flow (DP.Person, DDM.DepotManager)
requireBlockPermittedDispatcher mbPersonId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (person.role == DP.BUS_DISPATCHER) $ throwError AccessDenied
  depotManager <- B.runInReplica $ QDM.findByPersonId personId >>= fromMaybeM (BusBlockNotAllowed personId.getId)
  unless (depotManager.isBlockAllowed == Just True) $ throwError (BusBlockNotAllowed personId.getId)
  pure (person, depotManager)

blockVehicle ::
  (CacheFlow m r, MonadFlow m) =>
  Kernel.Types.Id.Id DP.Person ->
  [DIBC.IntegratedBPPConfig] ->
  Text ->
  m ()
blockVehicle personId configs vehicleNumber = do
  mapM_ (\cfg -> Hedis.setExp (mkBusBlockKey cfg.id vehicleNumber) personId.getId defaultBusBlockExpiry.getSeconds) configs
  addToCheckerBlockedList personId vehicleNumber

unblockVehicle ::
  (CacheFlow m r, MonadFlow m) =>
  Kernel.Types.Id.Id DP.Person ->
  [DIBC.IntegratedBPPConfig] ->
  Text ->
  m ()
unblockVehicle personId configs vehicleNumber = do
  mapM_ (\cfg -> Hedis.del (mkBusBlockKey cfg.id vehicleNumber)) configs
  removeFromCheckerBlockedList personId vehicleNumber

isVehicleBlocked ::
  (CacheFlow m r, MonadFlow m) =>
  [DIBC.IntegratedBPPConfig] ->
  Text ->
  m Bool
isVehicleBlocked configs vehicleNumber = do
  flags <-
    mapM
      ( \cfg -> do
          mbVal <- Hedis.safeGet (mkBusBlockKey cfg.id vehicleNumber)
          pure (isJust (mbVal :: Maybe Text))
      )
      configs
  pure (Kernel.Prelude.or flags)

addToCheckerBlockedList ::
  (CacheFlow m r, MonadFlow m) =>
  Kernel.Types.Id.Id DP.Person ->
  Text ->
  m ()
addToCheckerBlockedList personId vehicleNumber = do
  let key = mkCheckerBlockedListKey personId
  existing <- fromMaybe [] <$> Hedis.safeGet key
  unless (vehicleNumber `elem` existing) $
    Hedis.setExp key (vehicleNumber : existing) defaultBusBlockExpiry.getSeconds

removeFromCheckerBlockedList ::
  (CacheFlow m r, MonadFlow m) =>
  Kernel.Types.Id.Id DP.Person ->
  Text ->
  m ()
removeFromCheckerBlockedList personId vehicleNumber = do
  let key = mkCheckerBlockedListKey personId
  existing <- fromMaybe [] <$> Hedis.safeGet key
  case filter (/= vehicleNumber) existing of
    [] -> void $ Hedis.del key
    remaining -> Hedis.setExp key remaining defaultBusBlockExpiry.getSeconds

reconcileCheckerBlockedList ::
  (CacheFlow m r, MonadFlow m) =>
  Kernel.Types.Id.Id DP.Person ->
  [DIBC.IntegratedBPPConfig] ->
  m [Text]
reconcileCheckerBlockedList personId configs = do
  let key = mkCheckerBlockedListKey personId
  vehicles <- fromMaybe [] <$> Hedis.safeGet key
  flagged <-
    mapM
      ( \vn -> do
          blocked <- isVehicleBlocked configs vn
          pure (vn, blocked)
      )
      vehicles
  let stillBlocked = map fst (filter snd flagged)
  when (stillBlocked /= vehicles) $
    case stillBlocked of
      [] -> void $ Hedis.del key
      _ -> Hedis.setExp key stillBlocked defaultBusBlockExpiry.getSeconds
  pure stillBlocked

postPublicTransportVehicleDataBlock ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Kernel.Prelude.Bool ->
    Flow APIT.BlockedVehiclesResp
  )
postPublicTransportVehicleDataBlock (mbPersonId, _merchantId, _merchantOpCityId) vehicleNumber isBlock = do
  (person, _depotManager) <- requireBlockPermittedDispatcher mbPersonId
  let personId = person.id
  configs <- SIBC.findAllIntegratedBPPConfig person.merchantOperatingCityId (show BecknSpec.BUS) DIBC.MULTIMODAL
  blockedVehicleNumbers <-
    if isBlock
      then do
        currentlyBlocked <- reconcileCheckerBlockedList personId configs
        when (not (vehicleNumber `elem` currentlyBlocked) && length currentlyBlocked >= defaultBusBlockMaxLimit) $
          throwError (BusBlockLimitExceeded defaultBusBlockMaxLimit)
        blockVehicle personId configs vehicleNumber
        reconcileCheckerBlockedList personId configs
      else do
        let key = mkCheckerBlockedListKey personId
        vehicles <- fromMaybe [] <$> Hedis.safeGet key
        when (vehicleNumber `Kernel.Prelude.notElem` vehicles) $
          throwError (BusBlockNotAllowed personId.getId)
        unblockVehicle personId configs vehicleNumber
        reconcileCheckerBlockedList personId configs
  historyId <- generateGUID
  now <- getCurrentTime
  QVAH.create
    DVAH.VehicleActionHistory
      { id = historyId,
        dispatcherId = personId,
        action = DVAH.BLOCKER,
        currentVehicle = vehicleNumber,
        replacedVehicle = Nothing,
        driverCode = Nothing,
        conductorCode = Nothing,
        merchantId = person.merchantId,
        merchantOperatingCityId = person.merchantOperatingCityId,
        depotId = Nothing,
        reasonTag = if isBlock then "BLOCK" else "UNBLOCK",
        reasonContent = Nothing,
        createdAt = now,
        updatedAt = now,
        waybillNo = Nothing
      }
  pure $ APIT.BlockedVehiclesResp blockedVehicleNumbers

getPublicTransportBlockedVehicles ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Flow APIT.BlockedVehiclesResp
  )
getPublicTransportBlockedVehicles (mbPersonId, _merchantId, _merchantOpCityId) = do
  (person, _depotManager) <- requireBlockPermittedDispatcher mbPersonId
  configs <- SIBC.findAllIntegratedBPPConfig person.merchantOperatingCityId (show BecknSpec.BUS) DIBC.MULTIMODAL
  blockedVehicleNumbers <- reconcileCheckerBlockedList person.id configs
  pure $ APIT.BlockedVehiclesResp blockedVehicleNumbers
