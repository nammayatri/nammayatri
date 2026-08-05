{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Capability where

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Domain.Types.Capability as DC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.AccessAudit as QAudit
import qualified Storage.Queries.Capability as QCap
import qualified Storage.Queries.CapabilityEndpoint as QCE
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonCapability as QPC
import qualified Storage.Queries.PersonTier as QPT
import qualified Storage.Queries.Role as QRole
import qualified Storage.Queries.RoleCapability as QRC
import Tools.Auth
import Tools.Error

-- Access Control framework admin surface (dashboard unification Phase 4).
-- Guard rules (PLAN.md admin tiering):
--   * SUPER_ADMIN is never mintable here — DB seed only. All SUPER_ADMIN
--     checks are EXISTENCE-GUARDED: until a SUPER_ADMIN row exists, legacy
--     DASHBOARD_ADMIN behavior applies unchanged (backward compatibility).
--   * No self-escalation: an actor can only grant capabilities they
--     themselves effectively hold (SUPER_ADMIN exempt).
--   * is_system capabilities move only via SUPER_ADMIN, unconditionally.
--   * Nobody edits their own grants.
--   * Every mutation writes an access_audit row.

--------------------------------------------------------------------- types

data UserCapabilitiesRes = UserCapabilitiesRes
  { capabilities :: [Text],
    adminTier :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CapabilityAPIEntity = CapabilityAPIEntity
  { id :: Text,
    domain :: Text,
    description :: Text,
    isSystem :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ListCapabilitiesRes = ListCapabilitiesRes
  { list :: [CapabilityAPIEntity]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data EndpointAPIEntity = EndpointAPIEntity
  { serverName :: Text,
    endpointId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CapabilityEndpointsRes = CapabilityEndpointsRes
  { capabilityId :: Text,
    endpoints :: [EndpointAPIEntity]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data RoleCapabilitiesRes = RoleCapabilitiesRes
  { roleId :: Id DRole.Role,
    roleName :: Text,
    capabilities :: [Text]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UpdateRoleCapabilitiesReq = UpdateRoleCapabilitiesReq
  { add :: [Text],
    remove :: [Text],
    reason :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data OverrideAPIEntity = OverrideAPIEntity
  { capabilityId :: Text,
    mode :: DC.CapabilityMode,
    reason :: Text,
    expiresAt :: Maybe UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PersonCapabilitiesRes = PersonCapabilitiesRes
  { personId :: Id DP.Person,
    roleId :: Id DRole.Role,
    roleName :: Text,
    adminTier :: Text,
    fromRole :: [Text],
    overrides :: [OverrideAPIEntity],
    effective :: [Text]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UpsertPersonCapabilityReq = UpsertPersonCapabilityReq
  { capabilityId :: Text,
    mode :: DC.CapabilityMode,
    reason :: Text,
    expiresAt :: Maybe UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

--------------------------------------------------------------- shared logic

-- | Effective set = (role capabilities ∪ live GRANTs) − live DENYs.
computeEffective :: BeamFlow m r => Id DP.Person -> Id DRole.Role -> m [Text]
computeEffective personId roleId = do
  now <- getCurrentTime
  roleCaps <- QRC.findAllByRoleId roleId
  overrides <- QPC.findAllByPersonId personId
  let live pc = maybe True (> now) pc.expiresAt
      grants = [pc.capabilityId.getId | pc <- overrides, pc.mode == DC.GRANT, live pc]
      denies = Set.fromList [pc.capabilityId.getId | pc <- overrides, pc.mode == DC.DENY, live pc]
      base = map (.capabilityId.getId) roleCaps
  pure $ Set.toList $ Set.fromList (base <> grants) `Set.difference` denies

adminTierOf :: BeamFlow m r => Id DP.Person -> m Text
adminTierOf personId = maybe DC.userTier (.adminTier) <$> QPT.findByPersonId personId

isSuperAdmin :: BeamFlow m r => Id DP.Person -> m Bool
isSuperAdmin personId = (== DC.superAdminTier) <$> adminTierOf personId

-- | Existence-guarded SUPER_ADMIN requirement: enforced only once a
-- SUPER_ADMIN has been seeded. Until then legacy admin behavior applies.
guardAdminMutation :: BeamFlow m r => Id DP.Person -> DRole.DashboardAccessType -> m ()
guardAdminMutation actorId targetAccessType =
  when (targetAccessType `elem` [DRole.DASHBOARD_ADMIN, DRole.DASHBOARD_RELEASE_ADMIN]) $ do
    exists <- QPT.superAdminExists
    when exists $
      unlessM (isSuperAdmin actorId) $
        throwError AccessDenied

-- | No-self-escalation: every capability being handed out must already be
-- effectively held by the actor (SUPER_ADMIN exempt). is_system capabilities
-- require SUPER_ADMIN unconditionally.
guardGrantable :: BeamFlow m r => TokenInfo -> [Text] -> m ()
guardGrantable tokenInfo capIds = do
  actorSuper <- isSuperAdmin tokenInfo.personId
  caps <- mapM (\cid -> QCap.findById (Id cid) >>= fromMaybeM (InvalidRequest $ "Unknown capability: " <> cid)) capIds
  let systemCaps = [c.id.getId | c <- caps, c.isSystem]
  unless (null systemCaps || actorSuper) $
    throwError $ InvalidRequest $ "Only a super admin can move system capabilities: " <> show systemCaps
  unless actorSuper $ do
    actor <- QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
    actorCaps <- Set.fromList <$> computeEffective tokenInfo.personId actor.roleId
    let missing = filter (`Set.notMember` actorCaps) capIds
    unless (null missing) $
      throwError $ InvalidRequest $ "Cannot grant capabilities you do not hold: " <> show missing

audit :: BeamFlow m r => TokenInfo -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> m ()
audit tokenInfo action targetType targetId beforeValue afterValue reason = do
  now <- getCurrentTime
  auditId <- generateGUID
  QAudit.create $
    DC.AccessAudit
      { id = auditId,
        actorId = Just tokenInfo.personId,
        action = action,
        targetType = targetType,
        targetId = Just targetId,
        beforeValue = beforeValue,
        afterValue = afterValue,
        reason = reason,
        createdAt = now
      }

------------------------------------------------------------------ handlers

getUserCapabilities :: BeamFlow m r => TokenInfo -> m UserCapabilitiesRes
getUserCapabilities tokenInfo = do
  person <- QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  effective <- computeEffective tokenInfo.personId person.roleId
  tier <- adminTierOf tokenInfo.personId
  pure UserCapabilitiesRes {capabilities = effective, adminTier = tier}

listCapabilities :: BeamFlow m r => TokenInfo -> m ListCapabilitiesRes
listCapabilities _ = do
  caps <- QCap.findAll
  pure $
    ListCapabilitiesRes $
      map (\c -> CapabilityAPIEntity {id = c.id.getId, domain = c.domain, description = c.description, isSystem = c.isSystem}) caps

getCapabilityEndpoints :: BeamFlow m r => TokenInfo -> Text -> m CapabilityEndpointsRes
getCapabilityEndpoints _ capabilityId = do
  void $ QCap.findById (Id capabilityId) >>= fromMaybeM (InvalidRequest $ "Unknown capability: " <> capabilityId)
  endpoints <- QCE.findAllByCapabilityId (Id capabilityId)
  pure $
    CapabilityEndpointsRes
      { capabilityId,
        endpoints = map (\e -> EndpointAPIEntity {serverName = e.serverName, endpointId = e.endpointId}) endpoints
      }

getRoleCapabilities :: BeamFlow m r => TokenInfo -> Id DRole.Role -> m RoleCapabilitiesRes
getRoleCapabilities _ roleId = do
  role <- QRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  roleCaps <- QRC.findAllByRoleId roleId
  pure RoleCapabilitiesRes {roleId, roleName = role.name, capabilities = map (.capabilityId.getId) roleCaps}

updateRoleCapabilities :: BeamFlow m r => TokenInfo -> Id DRole.Role -> UpdateRoleCapabilitiesReq -> m APISuccess
updateRoleCapabilities tokenInfo roleId req = do
  when (null req.add && null req.remove) $ throwError (InvalidRequest "Nothing to change")
  when (blank req.reason) $ throwError (InvalidRequest "reason is mandatory")
  role <- QRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  guardGrantable tokenInfo req.add
  existing <- map (.capabilityId.getId) <$> QRC.findAllByRoleId roleId
  now <- getCurrentTime
  forM_ (filter (`notElem` existing) req.add) $ \cid ->
    QRC.create DC.RoleCapability {roleId, capabilityId = Id cid, createdAt = now}
  forM_ req.remove $ \cid ->
    QRC.deleteByRoleIdAndCapabilityId roleId (Id cid)
  audit
    tokenInfo
    "ROLE_CAPABILITY_UPDATE"
    "role"
    (roleId.getId <> " (" <> role.name <> ")")
    (Just $ show existing)
    (Just $ "add: " <> show req.add <> ", remove: " <> show req.remove)
    (Just req.reason)
  pure Success

getPersonCapabilities :: BeamFlow m r => TokenInfo -> Id DP.Person -> m PersonCapabilitiesRes
getPersonCapabilities _ personId = do
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  role <- QRole.findById person.roleId >>= fromMaybeM (RoleDoesNotExist person.roleId.getId)
  roleCaps <- QRC.findAllByRoleId person.roleId
  overrides <- QPC.findAllByPersonId personId
  effective <- computeEffective personId person.roleId
  tier <- adminTierOf personId
  pure
    PersonCapabilitiesRes
      { personId,
        roleId = person.roleId,
        roleName = role.name,
        adminTier = tier,
        fromRole = map (.capabilityId.getId) roleCaps,
        overrides =
          map
            (\o -> OverrideAPIEntity {capabilityId = o.capabilityId.getId, mode = o.mode, reason = o.reason, expiresAt = o.expiresAt, createdAt = o.createdAt})
            overrides,
        effective = effective
      }

upsertPersonCapability :: BeamFlow m r => TokenInfo -> Id DP.Person -> UpsertPersonCapabilityReq -> m APISuccess
upsertPersonCapability tokenInfo personId req = do
  when (tokenInfo.personId == personId) $
    throwError $ InvalidRequest "Cannot edit your own capability grants"
  when (blank req.reason) $ throwError (InvalidRequest "reason is mandatory")
  void $ QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  when (req.mode == DC.GRANT) $ guardGrantable tokenInfo [req.capabilityId]
  now <- getCurrentTime
  -- upsert = delete + create (composite PK on person_id, capability_id)
  QPC.deleteByPersonIdAndCapabilityId personId (Id req.capabilityId)
  QPC.create $
    DC.PersonCapability
      { personId,
        capabilityId = Id req.capabilityId,
        mode = req.mode,
        reason = req.reason,
        grantedBy = Just tokenInfo.personId,
        expiresAt = req.expiresAt,
        createdAt = now
      }
  audit tokenInfo "PERSON_CAPABILITY_UPSERT" "person" personId.getId
    Nothing
    (Just $ req.capabilityId <> " " <> show req.mode <> maybe "" ((" until " <>) . show) req.expiresAt)
    (Just req.reason)
  pure Success

deletePersonCapability :: BeamFlow m r => TokenInfo -> Id DP.Person -> Text -> m APISuccess
deletePersonCapability tokenInfo personId capabilityId = do
  when (tokenInfo.personId == personId) $
    throwError $ InvalidRequest "Cannot edit your own capability grants"
  QPC.deleteByPersonIdAndCapabilityId personId (Id capabilityId)
  audit tokenInfo "PERSON_CAPABILITY_DELETE" "person" personId.getId
    (Just capabilityId)
    Nothing
    Nothing
  pure Success

blank :: Text -> Bool
blank = T.null . T.strip
