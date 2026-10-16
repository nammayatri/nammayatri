module Domain.Action.UI.PublicTransport
  ( getPublicTransportBlockedVehicles,
    postPublicTransportVehicleDataBlock,
  )
where

import qualified API.Types.UI.PublicTransport as APIT
import qualified BecknV2.FRFS.Enums as BecknSpec
import Data.List (nub)
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

getBusBlockExpiry :: DIBC.ProviderConfig -> Seconds
getBusBlockExpiry = \case
  DIBC.DIRECT c -> fromMaybe defaultBusBlockExpiry c.busBlockExpiryTime
  DIBC.ONDC c -> fromMaybe defaultBusBlockExpiry c.busBlockExpiryTime

getBusBlockMaxLimit :: DIBC.ProviderConfig -> Int
getBusBlockMaxLimit = \case
  DIBC.DIRECT c -> fromMaybe defaultBusBlockMaxLimit c.busBlockMaxLimit
  DIBC.ONDC c -> fromMaybe defaultBusBlockMaxLimit c.busBlockMaxLimit

-- Total over an empty list, and the checker list must outlive every block key it tracks
-- or the limit check reads an empty list and over-admits.
maxBusBlockExpiry :: [DIBC.IntegratedBPPConfig] -> Seconds
maxBusBlockExpiry configs = case map (getBusBlockExpiry . (.providerConfig)) configs of
  [] -> defaultBusBlockExpiry
  e : es -> foldr max e es

-- Scoped by GTFS feed, not our config row id: rider-app's ids differ, so it could never construct that key.
mkBusBlockKey :: Text -> Text -> Text
mkBusBlockKey feedKey vehicleNumber = "shared:busBlock:" <> feedKey <> ":" <> vehicleNumber

-- Several configs (ONDC + DIRECT) can share one feed; dedupe so we touch each key once.
feedKeysOf :: [DIBC.IntegratedBPPConfig] -> [Text]
feedKeysOf = nub . map (.feedKey)

-- One key per feed, but its TTL must cover every config sharing that feed.
feedGroupsOf :: [DIBC.IntegratedBPPConfig] -> [(Text, [DIBC.IntegratedBPPConfig])]
feedGroupsOf configs = map (\feedKey -> (feedKey, filter ((== feedKey) . (.feedKey)) configs)) (feedKeysOf configs)

mkCheckerBlockedListKey :: Kernel.Types.Id.Id DP.Person -> Text
mkCheckerBlockedListKey personId = "blocked:checker:" <> personId.getId

-- No config means blockVehicle writes no key and reconcile then wipes the tracker, so a block would
-- report success having done nothing.
requireBusConfigs :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Flow [DIBC.IntegratedBPPConfig]
requireBusConfigs merchantOperatingCityId = do
  configs <- SIBC.findAllIntegratedBPPConfig merchantOperatingCityId (show BecknSpec.BUS) DIBC.MULTIMODAL
  when (null configs) $ throwError IntegratedBPPConfigNotFound
  pure configs

-- Block is a checker capability; allow if ANY enabled depot_manager row grants it (a person may hold several).
requireBlockPermittedOperator ::
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person) ->
  Flow DP.Person
requireBlockPermittedOperator mbPersonId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (person.role == DP.PT_PROVIDER) $ throwError AccessDenied
  depotRows <- B.runInReplica $ QDM.findAllByPersonId personId
  let allowed = any (\dm -> dm.enabled && dm.isBlockAllowed == Just True) depotRows
  unless allowed $ throwError (BusBlockNotAllowed personId.getId)
  pure person

blockVehicle ::
  (CacheFlow m r, MonadFlow m) =>
  Kernel.Types.Id.Id DP.Person ->
  [DIBC.IntegratedBPPConfig] ->
  Text ->
  m ()
blockVehicle personId configs vehicleNumber = do
  Hedis.withCrossAppRedis $
    mapM_
      (\(feedKey, cfgs) -> Hedis.setExp (mkBusBlockKey feedKey vehicleNumber) personId.getId (maxBusBlockExpiry cfgs).getSeconds)
      (feedGroupsOf configs)
  addToCheckerBlockedList personId configs vehicleNumber

unblockVehicle ::
  (CacheFlow m r, MonadFlow m) =>
  Kernel.Types.Id.Id DP.Person ->
  [DIBC.IntegratedBPPConfig] ->
  Text ->
  m ()
unblockVehicle personId configs vehicleNumber = do
  Hedis.withCrossAppRedis $
    mapM_ (\feedKey -> Hedis.del (mkBusBlockKey feedKey vehicleNumber)) (feedKeysOf configs)
  removeFromCheckerBlockedList personId configs vehicleNumber

isVehicleBlocked ::
  (CacheFlow m r, MonadFlow m) =>
  [DIBC.IntegratedBPPConfig] ->
  Text ->
  m Bool
isVehicleBlocked configs vehicleNumber = do
  flags <-
    Hedis.withCrossAppRedis $
      mapM
        ( \feedKey -> do
            mbVal <- Hedis.safeGet (mkBusBlockKey feedKey vehicleNumber)
            pure (isJust (mbVal :: Maybe Text))
        )
        (feedKeysOf configs)
  pure (Kernel.Prelude.or flags)

addToCheckerBlockedList ::
  (CacheFlow m r, MonadFlow m) =>
  Kernel.Types.Id.Id DP.Person ->
  [DIBC.IntegratedBPPConfig] ->
  Text ->
  m ()
addToCheckerBlockedList personId configs vehicleNumber = do
  let key = mkCheckerBlockedListKey personId
  existing <- fromMaybe [] <$> Hedis.safeGet key
  unless (vehicleNumber `elem` existing) $
    Hedis.setExp key (vehicleNumber : existing) (maxBusBlockExpiry configs).getSeconds

removeFromCheckerBlockedList ::
  (CacheFlow m r, MonadFlow m) =>
  Kernel.Types.Id.Id DP.Person ->
  [DIBC.IntegratedBPPConfig] ->
  Text ->
  m ()
removeFromCheckerBlockedList personId configs vehicleNumber = do
  let key = mkCheckerBlockedListKey personId
  existing <- fromMaybe [] <$> Hedis.safeGet key
  case filter (/= vehicleNumber) existing of
    [] -> void $ Hedis.del key
    remaining -> Hedis.setExp key remaining (maxBusBlockExpiry configs).getSeconds

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
      _ -> Hedis.setExp key stillBlocked (maxBusBlockExpiry configs).getSeconds
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
  person <- requireBlockPermittedOperator mbPersonId
  let personId = person.id
  configs <- requireBusConfigs person.merchantOperatingCityId
  blockedVehicleNumbers <-
    if isBlock
      then do
        currentlyBlocked <- reconcileCheckerBlockedList personId configs
        let maxLimit = maybe defaultBusBlockMaxLimit (getBusBlockMaxLimit . (.providerConfig)) (Kernel.Prelude.listToMaybe configs)
        when (not (vehicleNumber `elem` currentlyBlocked) && length currentlyBlocked >= maxLimit) $
          throwError (BusBlockLimitExceeded maxLimit)
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
  person <- requireBlockPermittedOperator mbPersonId
  configs <- requireBusConfigs person.merchantOperatingCityId
  blockedVehicleNumbers <- reconcileCheckerBlockedList person.id configs
  pure $ APIT.BlockedVehiclesResp blockedVehicleNumbers
