{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Campaign membership via IJCampaign person tag + CJM controlFeatures (in-mem cache).
-- No driver_information / LTS campaign snapshot — rules stay on CJM; tag carries cjmId + expiry.
module SharedLogic.IncentiveJourney.MgCampaign
  ( syncCampaignMembership,
    syncCampaignMembershipBatch,
    clearMgOptedInOnDriverInfo,
    MgPerkSource (..),
    resolveMgPerkSource,
    resolveMgPerkSourceAt,
    resolveMgPerkSourceFromDriverTags,
    extractIjCampaignMemberships,
    mgForbidsDriverCancellation,
    mgPoolingPriorityValue,
    mgAutoAssignValue,
    mgAutoAssignConfig,
    mgAutoAssignConfigFromSnapshot,
    -- Legacy alias used by older call sites during transition.
    syncMgCampaignSnapshot,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.FleetConfig as DFC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import "config-pilot" Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.IncentiveJourney.CampaignSync as CampaignSync
import qualified Lib.IncentiveJourney.CampaignTag as CampaignTag
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as IJCommon
import qualified Lib.IncentiveJourney.MgCampaign as Mg
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Tools.Utils as YUtils
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.IncentiveJourney.CampaignControlsCache (CampaignControls, getCampaignControls)
import Storage.Beam.IncentiveJourney ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonExtra as QPersonExtra

data MgPerkSource
  = UseNoCampaign -- no active IJ campaign membership; callers use fleet/driver defaults
  | -- | Membership occupies the perk slot (incl. behaviour-only / empty controlFeatures):
    -- suppress fleet AA / cancel / pooling. Empty controls → driver defaults, not fleet.
    CampaignMember {snapshot :: Mg.MgCampaignSnapshot, perksActive :: Bool}
  deriving (Show)

-- | Sync / clear IJCampaign tag(s). Just payload upserts one CJM membership; Nothing clears all.
syncCampaignMembership ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Id IJCommon.Person ->
  Maybe CampaignSync.CampaignHotPathSync ->
  m ()
syncCampaignMembership personId mbPayload = do
  let driverId = cast personId :: Id Person.Person
  case mbPayload of
    Nothing -> clearIjCampaignTag driverId
    Just payload -> setIjCampaignTag driverId payload.membership payload.validTill

-- | Upsert several Assigned memberships in one person.driverTag write (list ensure).
syncCampaignMembershipBatch ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Id IJCommon.Person ->
  [CampaignSync.CampaignHotPathSync] ->
  m ()
syncCampaignMembershipBatch _ [] = pure ()
syncCampaignMembershipBatch personId payloads = do
  let driverId = cast personId :: Id Person.Person
      items = [(p.membership, p.validTill) | p <- payloads]
  setIjCampaignTags driverId items

-- | Legacy name: treat Just snap as membership with that snapshot's fields.
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
  case mbSnap of
    Nothing -> syncCampaignMembership personId Nothing
    Just snap ->
      let membership =
            CampaignTag.CampaignMembership
              { cjmId = snap.cohortJourneyMappingId,
                linkType = if snap.optedIn then DCJM.OptIn else DCJM.Assigned,
                timeBoundKeys = [CampaignTag.timeBoundKeyDaily]
              }
          payload =
            CampaignSync.CampaignHotPathSync
              { membership = membership,
                optedIn = snap.optedIn,
                validTill = snap.validTill,
                snapshot = snap
              }
       in syncCampaignMembership personId (Just payload)

setIjCampaignTag ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Id Person.Person ->
  CampaignTag.CampaignMembership ->
  Maybe UTCTime ->
  m ()
setIjCampaignTag driverId membership mbValidTill =
  setIjCampaignTags driverId [(membership, mbValidTill)]

setIjCampaignTags ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Id Person.Person ->
  [(CampaignTag.CampaignMembership, Maybe UTCTime)] ->
  m ()
setIjCampaignTags _ [] = pure ()
setIjCampaignTags driverId items = do
  mbPerson <- QPerson.findById driverId
  let startTags = mbPerson >>= (.driverTag)
      updated =
        foldl'
          ( \mbTags (membership, mbValidTill) ->
              let tagName = CampaignTag.ijCampaignTagNameFor membership.cjmId
                  tagValue = LYT.TextValue (CampaignTag.encodeMembershipValue membership)
                  newTag = YUtils.mkTagNameValueExpiryAt (LYT.TagName tagName) tagValue mbValidTill
                  withoutLegacy = stripMembershipFromLegacyTag membership.cjmId mbTags
               in Just (YUtils.replaceTagNameValue withoutLegacy newTag)
          )
          startTags
          items
  when (updated /= startTags) $
    QPersonExtra.updateDriverTag updated driverId

clearIjCampaignTag ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Id Person.Person ->
  m ()
clearIjCampaignTag driverId = do
  mbPerson <- QPerson.findById driverId
  case mbPerson >>= (.driverTag) of
    Nothing -> pure ()
    Just tags -> do
      let remaining = filter (not . isIjCampaignTag) tags
      QPersonExtra.updateDriverTag (if null remaining then Nothing else Just remaining) driverId

-- | Remove one CJM tag (keeps other campaigns). Returns True if it was present.
clearMgOptedInOnDriverInfo ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Id IJCommon.Person ->
  Id DCJM.CohortJourneyMapping ->
  m Bool
clearMgOptedInOnDriverInfo personId cjmId = do
  let driverId = cast personId :: Id Person.Person
  mbPerson <- QPerson.findById driverId
  case mbPerson >>= (.driverTag) of
    Nothing -> pure False
    Just tags -> do
      let memberships = membershipsFromTags tags
          matched = any (\m -> m.cjmId == cjmId.getId) memberships
      when matched $ do
        let perCjmName = CampaignTag.ijCampaignTagNameFor cjmId.getId
            withoutPerCjm = YUtils.removeTagName (Just tags) (LYT.TagNameValue perCjmName)
            withoutLegacy = fromMaybe [] $ stripMembershipFromLegacyTag cjmId.getId (Just withoutPerCjm)
        QPersonExtra.updateDriverTag (if null withoutLegacy then Nothing else Just withoutLegacy) driverId
      pure matched

isIjCampaignTag :: LYT.TagNameValueExpiry -> Bool
isIjCampaignTag tag =
  case T.splitOn "#" (LYT.getTagNameValueExpiry tag) of
    (name : _) -> CampaignTag.isIjCampaignTagName name
    _ -> False

stripMembershipFromLegacyTag :: Text -> Maybe [LYT.TagNameValueExpiry] -> Maybe [LYT.TagNameValueExpiry]
stripMembershipFromLegacyTag cjmId mbTags =
  case mbTags of
    Nothing -> Nothing
    Just tags ->
      Just $
        concatMap
          ( \tag ->
              case T.splitOn "#" (LYT.getTagNameValueExpiry tag) of
                (name : value : rest)
                  | name == CampaignTag.ijCampaignTagName ->
                    let kept = filter (\m -> m.cjmId /= cjmId) (CampaignTag.decodeMembershipsFromTagValue value)
                     in if null kept
                          then []
                          else
                            let newValue = T.intercalate "&" (map CampaignTag.encodeLegacyMembershipValue kept)
                                expirySuffix = case rest of
                                  (e : _) | not (T.null e) -> "#" <> e
                                  _ -> ""
                             in [LYT.TagNameValueExpiry (name <> "#" <> newValue <> expirySuffix)]
                _ -> [tag]
          )
          tags

membershipsFromTags :: [LYT.TagNameValueExpiry] -> [CampaignTag.CampaignMembership]
membershipsFromTags =
  concatMap
    ( \tag ->
        case T.splitOn "#" (LYT.getTagNameValueExpiry tag) of
          (name : value : _)
            | CampaignTag.isIjCampaignTagName name ->
              CampaignTag.decodeMembershipsFromTag name value
          _ -> []
    )

-- | Resolve from converted driverTags JSON (allocator hot path). Merges multi-campaign tags.
resolveMgPerkSourceFromDriverTags ::
  (BeamFlow m r, EsqDBFlow m r, CacheFlow m r, MonadFlow m) =>
  UTCTime ->
  A.Value ->
  m MgPerkSource
resolveMgPerkSourceFromDriverTags localTime driverTagsJson = do
  let memberships = extractIjCampaignMemberships driverTagsJson
  sources <- forM memberships $ \membership -> do
    mbControls <- getCampaignControls (Id membership.cjmId)
    pure $ resolveFromControls localTime membership True mbControls
  pure $ foldl' mergePerkSources UseNoCampaign sources

extractIjCampaignMemberships :: A.Value -> [CampaignTag.CampaignMembership]
extractIjCampaignMemberships = \case
  A.Object o ->
    concatMap
      ( \(k, v) ->
          let name = AKey.toText k
           in if CampaignTag.isIjCampaignTagName name
                then case v of
                  A.String s -> CampaignTag.decodeMembershipsFromTag name s
                  A.Array xs ->
                    concatMap
                      ( \case
                          A.String s -> CampaignTag.decodeMembershipsFromTag name s
                          _ -> []
                      )
                      (toList xs)
                  _ -> []
                else []
      )
      (AKM.toList o)
  _ -> []

resolveFromControls ::
  UTCTime ->
  CampaignTag.CampaignMembership ->
  Bool -> -- optedIn: tag presence (unexpired) implies membership active
  Maybe CampaignControls ->
  MgPerkSource
resolveFromControls localTime membership optedIn = \case
  Nothing -> UseNoCampaign
  Just controls
    | not controls.enabled -> UseNoCampaign
    -- Behaviour-only (no controlFeatures): still CampaignMember so fleet AA/cancel/pooling
    -- are suppressed. Snapshot controls are empty defaults — restore fleet by unassigning.
    | otherwise ->
      let snap =
            Mg.MgCampaignSnapshot
              { journeyId = "",
                cohortJourneyMappingId = membership.cjmId,
                optedIn = optedIn,
                -- Tag presence is post filterExpiredTags'; Assigned tags must carry
                -- min(UCM.validTill, streakEnd) so Unbounded timeBounds cannot outlive the campaign.
                validTill = Nothing,
                timeBounds = controls.timeBounds,
                allowCancellationByDriver = controls.allowCancellationByDriver,
                autoAssign = controls.autoAssign,
                poolingPriority = controls.poolingPriority
              }
          -- Match live controlFeatures slot: perks only once startDate <= now (tag may exist early for OptIn).
          perksActive =
            Mg.isMgPerksActive localTime snap
              && localTime >= controls.campaignStartDate
       in CampaignMember {snapshot = snap, perksActive = perksActive}

-- | Merge multi-campaign perk sources: cancel forbidden if any forbids; first AA / pooling wins.
mergePerkSources :: MgPerkSource -> MgPerkSource -> MgPerkSource
mergePerkSources UseNoCampaign other = other
mergePerkSources other UseNoCampaign = other
mergePerkSources (CampaignMember a activeA) (CampaignMember b activeB) =
  let active = activeA || activeB
      allowCancel =
        case (activeA, activeB) of
          (True, True) -> a.allowCancellationByDriver && b.allowCancellationByDriver
          (True, False) -> a.allowCancellationByDriver
          (False, True) -> b.allowCancellationByDriver
          (False, False) -> True
      snap =
        a
          { Mg.allowCancellationByDriver = allowCancel,
            Mg.autoAssign = (if activeA then a.autoAssign else Nothing) <|> (if activeB then b.autoAssign else Nothing) <|> a.autoAssign <|> b.autoAssign,
            Mg.poolingPriority = (if activeA then a.poolingPriority else Nothing) <|> (if activeB then b.poolingPriority else Nothing) <|> a.poolingPriority <|> b.poolingPriority
          }
   in CampaignMember {snapshot = snap, perksActive = active}

resolveMgPerkSourceAt :: UTCTime -> Maybe DI.DriverInformation -> MgPerkSource
resolveMgPerkSourceAt _ _ = UseNoCampaign

resolveMgPerkSource ::
  ( MonadFlow m,
    BeamFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe DI.DriverInformation ->
  m MgPerkSource
resolveMgPerkSource merchantOpCityId mbInfo = do
  case mbInfo of
    Nothing -> pure UseNoCampaign
    Just info -> do
      mbPerson <- QPerson.findById info.driverId
      transporterConfig <-
        getOneConfig
          (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
          Nothing
      now <- getCurrentTime
      localTime <- case transporterConfig of
        Just tc -> getLocalCurrentTime tc.timeDiffFromUtc
        Nothing -> pure now
      case mbPerson >>= (.driverTag) of
        Just tags -> do
          -- Drop expired IJCampaign tags (Assigned must be written with UCM/streak expiry).
          let memberships = membershipsFromTags (YUtils.filterExpiredTags' now tags)
          sources <- forM memberships $ \membership -> do
            mbControls <- getCampaignControls (Id membership.cjmId)
            pure $ resolveFromControls localTime membership True mbControls
          pure $ foldl' mergePerkSources UseNoCampaign sources
        Nothing -> pure UseNoCampaign

mgForbidsDriverCancellation :: MgPerkSource -> Maybe Bool
mgForbidsDriverCancellation = \case
  UseNoCampaign -> Nothing
  CampaignMember {perksActive, snapshot} ->
    Just $ perksActive && not snapshot.allowCancellationByDriver

mgPoolingPriorityValue :: MgPerkSource -> Maybe A.Value
mgPoolingPriorityValue = \case
  UseNoCampaign -> Nothing
  CampaignMember {perksActive, snapshot}
    | perksActive -> snapshot.poolingPriority
    | otherwise -> Nothing

mgAutoAssignValue :: MgPerkSource -> Maybe A.Value
mgAutoAssignValue = \case
  UseNoCampaign -> Nothing
  CampaignMember {perksActive, snapshot}
    | perksActive -> snapshot.autoAssign
    | otherwise -> Nothing

-- | Decode AutoAssign from snapshot regardless of perk window. Used when *applying*
-- driver tags/tiers at opt-in reconcile so settings are ready when the window opens.
-- Runtime silent-assign still uses 'mgAutoAssignConfig' (perksActive-gated).
mgAutoAssignConfigFromSnapshot :: Mg.MgCampaignSnapshot -> Maybe DFC.AutoAssignConfig
mgAutoAssignConfigFromSnapshot snap = do
  val <- snap.autoAssign
  case A.fromJSON val of
    A.Success (cfg :: DFC.AutoAssignConfig) -> Just cfg
    _ -> Nothing

mgAutoAssignConfig :: MgPerkSource -> Maybe DFC.AutoAssignConfig
mgAutoAssignConfig src = do
  val <- mgAutoAssignValue src
  case A.fromJSON val of
    A.Success (cfg :: DFC.AutoAssignConfig) -> Just cfg
    A.Error _ -> Nothing
