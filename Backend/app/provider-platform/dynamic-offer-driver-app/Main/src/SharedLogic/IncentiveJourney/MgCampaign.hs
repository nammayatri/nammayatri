{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Driver-app sync + hot-path resolve for MG campaign perks on driver_information.mg_campaign.
-- Resolution:
--   Nothing on DI          → UseFleet (existing fleet path)
--   Just snap              → MgMember (never fleet for these three perks)
--     perks active iff optedIn && validTill && milestone timeBounds
module SharedLogic.IncentiveJourney.MgCampaign
  ( syncMgCampaignSnapshot,
    clearMgOptedInOnDriverInfo,
    MgPerkSource (..),
    resolveMgPerkSource,
    resolveMgPerkSourceAt,
    resolveMgPerkSourceFromCampaign,
    mgForbidsDriverCancellation,
    mgPoolingPriorityValue,
    mgAutoAssignValue,
    mgAutoAssignConfig,
  )
where

import qualified Data.Aeson as A
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.FleetConfig as DFC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import "config-pilot" Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as IJCommon
import qualified Lib.IncentiveJourney.MgCampaign as Mg
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformationExtra as QDIExtra

data MgPerkSource
  = -- | DI.mgCampaign is null — use existing fleet config path.
    UseFleet
  | -- | DI has MG membership marker. 'perksActive' gates cancel/pooling/auto-assign.
    MgMember {snapshot :: Mg.MgCampaignSnapshot, perksActive :: Bool}
  deriving (Show)

syncMgCampaignSnapshot ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Id IJCommon.Person ->
  Maybe Mg.MgCampaignSnapshot ->
  m ()
syncMgCampaignSnapshot personId mbSnap =
  QDIExtra.updateMgCampaign (cast personId) (Mg.encodeSnapshot <$> mbSnap)

-- | After OptIn disqualify: keep MG membership marker, clear optedIn so perks stay off (no fleet).
clearMgOptedInOnDriverInfo ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Id IJCommon.Person ->
  Id DCJM.CohortJourneyMapping ->
  m ()
clearMgOptedInOnDriverInfo personId cjmId = do
  mbInfo <- QDI.findById (cast personId)
  case mbInfo >>= (.mgCampaign) >>= Mg.decodeSnapshot of
    Just snap
      | snap.cohortJourneyMappingId == cjmId.getId ->
        QDIExtra.updateMgCampaign (cast personId) (Just $ Mg.encodeSnapshot (Mg.clearOptedIn snap))
    _ -> pure ()

-- | Pure resolve from LTS / DI.mg_campaign JSON when city-local time is already known.
resolveMgPerkSourceFromCampaign :: UTCTime -> Maybe A.Value -> MgPerkSource
resolveMgPerkSourceFromCampaign localTime mbCampaign =
  case mbCampaign >>= Mg.decodeSnapshot of
    Nothing -> UseFleet
    Just snap ->
      MgMember
        { snapshot = snap,
          perksActive = Mg.isMgPerksActive localTime snap
        }

-- | Pure resolve from a loaded DriverInformation row.
resolveMgPerkSourceAt :: UTCTime -> Maybe DI.DriverInformation -> MgPerkSource
resolveMgPerkSourceAt localTime mbInfo =
  resolveMgPerkSourceFromCampaign localTime (mbInfo >>= (.mgCampaign))

-- | Loads transporterConfig for local time. Prefer 'resolveMgPerkSourceFromCampaign' on the allocator hot path.
resolveMgPerkSource ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe DI.DriverInformation ->
  m MgPerkSource
resolveMgPerkSource merchantOpCityId mbInfo = do
  case mbInfo >>= (.mgCampaign) of
    Nothing -> pure UseFleet
    Just _ -> do
      transporterConfig <-
        getOneConfig
          (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
          Nothing
      localTime <- case transporterConfig of
        Just tc -> getLocalCurrentTime tc.timeDiffFromUtc
        Nothing -> getCurrentTime
      pure $ resolveMgPerkSourceAt localTime mbInfo

-- | True when MG forbids cancel (active perks + allowCancellationByDriver=False).
-- Nothing → caller should use fleet. Just False → do not forbid via MG (and do not ask fleet).
mgForbidsDriverCancellation :: MgPerkSource -> Maybe Bool
mgForbidsDriverCancellation = \case
  UseFleet -> Nothing
  MgMember {perksActive, snapshot} ->
    Just $ perksActive && not snapshot.allowCancellationByDriver

mgPoolingPriorityValue :: MgPerkSource -> Maybe A.Value
mgPoolingPriorityValue = \case
  UseFleet -> Nothing
  MgMember {perksActive, snapshot}
    | perksActive -> snapshot.poolingPriority
    | otherwise -> Nothing -- MG member but perks off: no pooling boost (no fleet)

mgAutoAssignValue :: MgPerkSource -> Maybe A.Value
mgAutoAssignValue = \case
  UseFleet -> Nothing
  MgMember {perksActive, snapshot}
    | perksActive -> snapshot.autoAssign
    | otherwise -> Nothing

-- | Decoded FleetConfig.AutoAssignConfig from an active MG snapshot. Nothing for UseFleet
-- or inactive MG (caller must not fall back to fleet when source is MgMember).
mgAutoAssignConfig :: MgPerkSource -> Maybe DFC.AutoAssignConfig
mgAutoAssignConfig src = do
  val <- mgAutoAssignValue src
  case A.fromJSON val of
    A.Success (cfg :: DFC.AutoAssignConfig) -> Just cfg
    A.Error _ -> Nothing
