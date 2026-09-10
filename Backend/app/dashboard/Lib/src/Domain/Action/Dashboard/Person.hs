{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Person where

import qualified API.Types.UnifiedDashboard.Management.Person as BPPPerson
import Dashboard.Common
import Data.Char (isDigit, isLower, isUpper)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (groupBy, nub, sortOn, (\\))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Capability as DCap
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.DeletedUser as DDU
import qualified Domain.Types.Entity as DE
import qualified Domain.Types.EntityAccess as DEA
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DAccess
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.API as AP
import qualified Domain.Types.Person.Type as DPT
import qualified Domain.Types.Person.Type as SP
import qualified Domain.Types.Role as DRole
import qualified Domain.Types.ServerName as DTServer
import qualified Domain.Types.Transaction as DTransaction
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (DbHash, EncKind (..), EncryptedHashedField, decrypt, encrypt, getDbHash, unEncrypted)
import qualified Kernel.External.Types as KET
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Kernel.Utils.Validation
import qualified SharedLogic.Transaction as STransaction
import Storage.Beam.BeamFlow
import qualified Storage.Queries.AccessMatrix as QMatrix
import qualified Storage.Queries.DeletedUser as QDeletedUser
import qualified Storage.Queries.Entity as QEntity
import qualified Storage.Queries.EntityAccess as QEntityAccess
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonCapability as QPC
import qualified Storage.Queries.PersonResourceAccess as QPRA
import qualified Storage.Queries.RegistrationToken as QReg
import qualified Storage.Queries.Role as QRole
import qualified Storage.Queries.Transaction as QT
import Tools.Auth
import qualified Tools.Auth.Api as ApiAuth
-- isSuperAdmin lives here rather than in Domain.Action.Dashboard.Capability: that module has no
-- export list, so it re-exports only what it defines, not what it imports.
import Tools.Auth.Capability (isSuperAdmin)
import qualified Tools.Auth.Capability as AuthCap
import qualified Tools.Auth.Common as Auth
import Tools.Auth.Merchant
import Tools.Error
import qualified Tools.InternalClient as InternalClient

data ListPersonRes = ListPersonRes
  { list :: [DP.PersonAPIEntity],
    summary :: Summary
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype MerchantAccessReq = MerchantAccessReq
  { merchantId :: ShortId DMerchant.Merchant
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MerchantCityAccessReq = MerchantCityAccessReq
  { merchantId :: ShortId DMerchant.Merchant,
    operatingCity :: City.City
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type MerchantAccessRes = MerchantCityAccessReq

data ChangePasswordReq = ChangePasswordReq
  { oldPassword :: Text,
    newPassword :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ChangePasswordAfterExpiryReq = ChangePasswordAfterExpiryReq
  { email :: Text,
    oldPassword :: Text,
    newPassword :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CreatePersonReq = CreatePersonReq
  { firstName :: Text,
    lastName :: Text,
    roleId :: Id DRole.Role,
    email :: Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    password :: Text,
    dashboardType :: Maybe DPT.DashboardType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ChangeEmailByAdminReq = ChangeEmailByAdminReq
  { newEmail :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ChangeMobileNumberByAdminReq = ChangeMobileNumberByAdminReq
  { newMobileNumber :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ChangePasswordByAdminReq = ChangePasswordByAdminReq
  { newPassword :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ReleaseRegisterReq = ReleaseRegisterReq
  {token :: Text}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype ChangeEnabledStatusReq = ChangeEnabledStatusReq
  { enabled :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ReleaseRegisterRes = ReleaseRegisterRes
  { username :: Text,
    token :: Text,
    otpEnabled :: Bool,
    merchantId :: Maybe Text,
    email :: Text,
    context :: Text,
    acl :: Maybe Text,
    merchantTrack :: Maybe Text,
    clientConfig :: Maybe Text,
    resellerId :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data GetProductSpecInfoResp = GetProductSpecInfoResp
  { merchant_id :: Text,
    client_id :: Text,
    platform :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

registerRelease ::
  ( BeamFlow m r,
    EncFlow m r
  ) =>
  TokenInfo ->
  ReleaseRegisterReq ->
  m ReleaseRegisterRes
registerRelease _ ReleaseRegisterReq {..} = do
  return
    ReleaseRegisterRes
      { username = "Sidharth",
        token = token,
        otpEnabled = False,
        merchantId = Just "merchantId",
        email = "sidharth.sethu@juspay.in",
        context = "JUSPAY",
        acl = Just "{\"mjos_manager\":\"RW\"}",
        merchantTrack = Nothing,
        clientConfig = Nothing,
        resellerId = Nothing
      }

getProductSpecInfo ::
  BeamFlow m r =>
  Maybe Text ->
  m GetProductSpecInfoResp
getProductSpecInfo _ = do
  return
    GetProductSpecInfoResp
      { merchant_id = "nammayatriconsumer",
        client_id = "nammayatriconsumer",
        platform = "android"
      }

validateCreatePerson :: Validate CreatePersonReq
validateCreatePerson CreatePersonReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ MinLength 3 `And` P.name,
      validateField "lastName" lastName $ NotEmpty `And` P.name,
      validateField "email" email P.email,
      validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

-- | Access tiers that carry administrative privilege and therefore must live on a
-- managed organizational email domain. Written as an exhaustive case rather than a
-- membership list so that adding a constructor to DashboardAccessType is a compile error
-- here instead of a silently uncovered tier.
isAdminTier :: DRole.DashboardAccessType -> Bool
isAdminTier = \case
  DRole.DASHBOARD_ADMIN -> True
  DRole.DASHBOARD_RELEASE_ADMIN -> True
  DRole.MERCHANT_ADMIN -> True
  DRole.TICKET_DASHBOARD_ADMIN -> True
  DRole.DASHBOARD_USER -> False
  DRole.DASHBOARD_OPERATOR -> False
  DRole.FLEET_OWNER -> False
  DRole.RENTAL_FLEET_OWNER -> False
  DRole.MERCHANT_MAKER -> False
  DRole.MERCHANT_SERVER -> False
  DRole.TICKET_DASHBOARD_USER -> False
  DRole.TICKET_DASHBOARD_MERCHANT -> False
  DRole.TICKET_DASHBOARD_APPROVER -> False

-- | Invariant: an account holding an admin-tier role must sit on one of its merchant's approved
-- email domains. Enforced at every path that can produce that pairing — person creation, role
-- assignment, and email change — not just at creation.
--
-- The allow-list is per-merchant (merchant.adminEmailDomains); an empty list leaves the
-- restriction switched off. Matching is exact: an allow-list entry of "maruti.co.in" admits
-- "user@maruti.co.in" but not "user@mail.maruti.co.in". Subdomains must be listed explicitly.
assertAdminEmailDomain ::
  BeamFlow m r =>
  Id DMerchant.Merchant ->
  DRole.Role ->
  Maybe Text ->
  m ()
assertAdminEmailDomain merchantId role mbEmail =
  when (isAdminTier role.dashboardAccessType) $ do
    merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
    let allowedDomains = merchant.adminEmailDomains
    unless (null allowedDomains) $ do
      -- An admin-tier account with no email at all cannot satisfy the restriction.
      email <- mbEmail & fromMaybeM (InvalidRequest adminEmailDomainError)
      let domain = T.toLower . T.drop 1 . T.dropWhile (/= '@') $ email
      unless (any (\allowed -> domain == T.toLower allowed) allowedDomains) $
        throwError $ InvalidRequest adminEmailDomainError

-- Deliberately does not echo the allow-list back to the caller.
adminEmailDomainError :: Text
adminEmailDomainError = "Administrator accounts must use an approved organizational email domain."

-- | Merchants whose admin-email policy a given person is subject to.
--
-- One person row carries one email across every merchant that person can reach, so checking only
-- the caller's merchant is too weak: an admin of a permissive merchant could set an address that
-- violates a stricter merchant the same person administers. Reachability is defined exactly as in
-- 'assertPersonInCallerMerchant' — access rows when there are any, the provisioning merchant
-- otherwise — so the two guards cannot drift apart. The caller's own merchant is always included,
-- which is what preserves today's behaviour for a person with neither access rows nor a
-- provisioning merchant.
--
-- Merchants with an empty allow-list impose nothing, so this only tightens where a policy is
-- actually configured. Two merchants with disjoint non-empty lists will reject every address for
-- a person they share, which is the correct outcome: such an account cannot satisfy both.
policyMerchantsForPerson ::
  BeamFlow m r =>
  TokenInfo ->
  SP.Person ->
  m [Id DMerchant.Merchant]
policyMerchantsForPerson tokenInfo person = do
  allAccess <- QAccess.findAllMerchantAccessByPersonId person.id
  let reachable =
        if null allAccess
          then maybe [] (: []) person.merchantId
          else map (.merchantId) allAccess
  pure . nub $ tokenInfo.merchantId : reachable

-- | 'assertAdminEmailDomain' against every merchant the person is subject to, not just the
-- caller's. Use this for mutations on an existing person; 'assertAdminEmailDomain' alone is right
-- only at creation, where there is no person row and so nothing else to be subject to.
assertAdminEmailDomainForPerson ::
  BeamFlow m r =>
  TokenInfo ->
  SP.Person ->
  DRole.Role ->
  Maybe Text ->
  m ()
assertAdminEmailDomainForPerson tokenInfo person role mbEmail =
  when (isAdminTier role.dashboardAccessType) $ do
    merchants <- policyMerchantsForPerson tokenInfo person
    forM_ merchants $ \merchantId -> assertAdminEmailDomain merchantId role mbEmail

-- | Admin mutations that address a person directly by id must not reach across merchants.
-- Without this an admin of any merchant could act on an arbitrary person id.
--
-- merchant_access rows are authoritative whenever the person has any: those merchants, and only
-- those, may act on them. A person shared across merchants therefore stays manageable by each.
--
-- The access rows cannot be the whole story though, because they are deletable. An earlier
-- version of this function used them alone and was bypassable: resetMerchantAccess and
-- resetMerchantCityAccess delete access rows and leave the person row alive, so an attacker could
-- empty a victim's rows and then claim them as "unowned". That state is also reachable with no
-- attack at all — a merchant revoking its own user's last access produces it.
--
-- person.merchantId, the merchant the person was provisioned under, closes that. It is written
-- once at creation, never updated, and not settable from any admin endpoint, so it holds the
-- claim when the rows are gone. It is deliberately consulted ONLY as a fallback rather than
-- unioned in: unioning would leave the provisioning merchant with authority forever, including
-- over a person whose access has since moved entirely to somebody else. Access, once granted,
-- decides; provisioning only decides when there is no access to speak of.
--
-- No claimant at all is still permitted, and now means one of two things: the person was created
-- moments ago and has not been granted access yet (createPerson writes no access row;
-- createUserForMerchant grants it on the next line), so rejecting would strand an admin who
-- typo'd an email at creation; or the row predates this column and the backfill found no access
-- row to derive one from. Neither is forgeable by a caller.
assertPersonInCallerMerchant ::
  BeamFlow m r =>
  TokenInfo ->
  Id DP.Person ->
  m ()
assertPersonInCallerMerchant tokenInfo personId = do
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  allAccess <- QAccess.findAllMerchantAccessByPersonId personId
  let claimants =
        if null allAccess
          then maybe [] (: []) person.merchantId
          else map (.merchantId) allAccess
  unless (null claimants) $
    unless (tokenInfo.merchantId `elem` claimants) $
      throwError (PersonDoesNotExist personId.getId)

-- | Granting a person access to a merchant is how somebody becomes a user of that merchant, so
-- leaving this open undoes every other cross-merchant guard: an admin of B could grant their own
-- user access to merchant A. Callers are held to their own merchant, with an escape hatch for a
-- SUPER_ADMIN, who legitimately provisions across merchants.
--
-- Unconditional, matching DCap.guardAdminMutation: the SUPER_ADMIN tier is seeded (seed-migration
-- 0018), so the existence guard that once kept these rules dormant no longer has anything to wait
-- for. Returns True when this is a cross-merchant grant that was permitted.
assertMayGrantAccessToMerchant :: BeamFlow m r => TokenInfo -> Id DMerchant.Merchant -> m Bool
assertMayGrantAccessToMerchant tokenInfo targetMerchantId
  | targetMerchantId == tokenInfo.merchantId = pure False
  | otherwise = do
    unlessM (isSuperAdmin tokenInfo.personId) $
      throwError AccessDenied
    pure True

-- | Record an admin-initiated mutation against another person. Mirrors the shape deletePerson
-- already uses: who did it (requestorId), to whom (request), and when. The target's id is the
-- only payload — request bodies here carry credentials and must never reach the audit log.
recordAdminActionOnPerson ::
  BeamFlow m r =>
  DTransaction.Endpoint ->
  TokenInfo ->
  Id DP.Person ->
  m ()
recordAdminActionOnPerson endpoint tokenInfo personId = do
  transaction <- STransaction.buildDashboardAuthTransaction endpoint tokenInfo.personId tokenInfo.merchantId
  QT.create transaction {DTransaction.request = Just personId.getId}

validateChangeMobileNumberReq :: Validate ChangeMobileNumberByAdminReq
validateChangeMobileNumberReq ChangeMobileNumberByAdminReq {..} =
  sequenceA_
    [ validateField "mobileNumber" newMobileNumber P.mobileNumber
    ]

newtype CreatePersonRes = CreatePersonRes
  {person :: AP.PersonAPIEntity}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createPerson ::
  ( BeamFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["enforceStrongPasswordPolicy" ::: Bool],
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  TokenInfo ->
  CreatePersonReq ->
  m CreatePersonRes
createPerson tokenInfo personEntity = do
  runRequestValidation validateCreatePerson personEntity
  enforceStrongPasswordPolicy <- asks (.enforceStrongPasswordPolicy)
  when enforceStrongPasswordPolicy $
    validateStrongPassword personEntity.password
  unlessM
    ( isNothing
        <$> DPT.withDashboardType personEntity.dashboardType
          (\(_ :: Proxy t) -> QP.findByEmailWithType @t personEntity.email)
    )
    $ throwError (InvalidRequest "Email already registered")
  unlessM
    ( isNothing
        <$> DPT.withDashboardType personEntity.dashboardType
          (\(_ :: Proxy t) -> QP.findByMobileNumberWithType @t personEntity.mobileNumber personEntity.mobileCountryCode)
    )
    $ throwError (InvalidRequest "Phone already registered")
  let roleId = personEntity.roleId
  role <- QRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  assertAdminEmailDomain tokenInfo.merchantId role (Just personEntity.email)
  -- Admin tiering (existence-guarded): once a SUPER_ADMIN is seeded, only a
  -- SUPER_ADMIN can create admin-tier persons. Legacy behavior until then.
  DCap.guardAdminMutation tokenInfo.personId role.dashboardAccessType
  personId <-
    if DRole.isBppSyncRole role
      then do
        merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantDoesNotExist tokenInfo.merchantId.getId)
        roleName <- driverRoleName role.dashboardAccessType
        let createReq =
              BPPPerson.CreatePersonReq
                { email = Just personEntity.email,
                  firstName = personEntity.firstName,
                  lastName = personEntity.lastName,
                  mobileCountryCode = personEntity.mobileCountryCode,
                  mobileNumber = personEntity.mobileNumber,
                  password = Nothing,
                  roleName = roleName
                }
        res <- InternalClient.callBPPInternalCreatePerson (getShortId merchant.shortId) tokenInfo.city createReq
        pure $ cast res.personId
      else generateGUID
  person <- buildPerson personId personEntity (role.dashboardAccessType) tokenInfo.merchantId
  decPerson <- decrypt person
  let personAPIEntity = AP.makePersonAPIEntity decPerson role [] Nothing [] Nothing
  QP.create person
  return $ CreatePersonRes personAPIEntity

driverRoleName :: MonadFlow m => DRole.DashboardAccessType -> m Text
driverRoleName role = case role of
  DRole.FLEET_OWNER -> pure "FLEET_OWNER"
  DRole.RENTAL_FLEET_OWNER -> pure "FLEET_OWNER"
  DRole.DASHBOARD_OPERATOR -> pure "OPERATOR"
  DRole.DASHBOARD_ADMIN -> pure "ADMIN"
  DRole.MERCHANT_ADMIN -> pure "ADMIN"
  other -> throwError $ InternalError $ "Role is marked bpp-sync but has no driver-app mapping: " <> show other

listPerson ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe (Id DP.Person) ->
  m ListPersonRes
listPerson _ mbSearchString mbLimit mbOffset mbPersonId = do
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString
  personAndRoleList <- B.runInReplica $ QP.findAllWithLimitOffset mbSearchString mbSearchStrDBHash mbLimit mbOffset mbPersonId
  res <- forM personAndRoleList $ \(encPerson, role, merchantAccessList, merchantCityAccessList) -> do
    decPerson <- decrypt encPerson
    let availableCitiesForMerchant = makeAvailableCitiesForMerchant merchantAccessList merchantCityAccessList
    pure $ AP.makePersonAPIEntity decPerson role (nub merchantAccessList) (Just availableCitiesForMerchant) [] Nothing
  let count = length res
  let summary = Summary {totalCount = 10000, count}
  pure $ ListPersonRes {list = res, summary = summary}

maxPTPageSize :: Integer
maxPTPageSize = 100

data ListPTEmployeeRes = ListPTEmployeeRes
  { list :: [AP.PTEmployeeAPIEntity],
    summary :: Summary
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- Depot-operations view: PT staff of the caller's merchant with the credentials and depot
-- assignments that screen needs. entityShortId is resolved against the caller's merchant, so an
-- unknown or foreign depot is rejected rather than silently returning everyone.
ptList ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  m ListPTEmployeeRes
ptList tokenInfo mbSearchString mbRoleName mbEntityShortId mbLimit mbOffset = do
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString
  mbEntityId <- forM mbEntityShortId $ \shortId ->
    QEntity.findByMerchantAndShortId tokenInfo.merchantId (ShortId shortId)
      >>= fmap (.id) . fromMaybeM (InvalidRequest $ "Entity " <> shortId <> " does not exist for this merchant")
  -- Every returned row costs a passetto round-trip to decrypt tokenNo and vpa, so the caller
  -- cannot widen the page arbitrarily.
  -- Only the upper bound was capped, so a negative limit or offset reached Postgres as LIMIT/OFFSET -1.
  let cappedLimit = max 0 $ min maxPTPageSize (fromMaybe maxPTPageSize mbLimit)
      safeOffset = max 0 <$> mbOffset
  (personAndRoleList, totalCount) <- B.runInReplica $ QP.findAllPTWithLimitOffset tokenInfo.merchantId mbSearchString mbSearchStrDBHash mbRoleName mbEntityId (Just cappedLimit) safeOffset
  entityGrants <- B.runInReplica $ QEntityAccess.findAllByPersonIdsAndMerchantId (map ((.id) . fst) personAndRoleList) tokenInfo.merchantId
  -- Retired depots stay visible here, unlike profile: staff on one must be findable to reassign.
  let grantedEntityIds = nub $ entityGrants <&> (.entityId)
  entities <- B.runInReplica $ QEntity.findAllByIds grantedEntityIds
  let missingEntityIds = filter (`notElem` (entities <&> (.id))) grantedEntityIds
  unless (null missingEntityIds) $
    logError $ "Dangling entity grants in ptList: " <> T.intercalate ", " (missingEntityIds <&> (.getId))
  let entityIdsByPerson = M.fromListWith (flip (<>)) [(grant.personId, [grant.entityId]) | grant <- entityGrants]
      entityById = M.fromList [(entity.id, entity) | entity <- entities]
  res <- forM personAndRoleList $ \(encPerson, role) -> do
    decPerson <- decrypt encPerson
    let personEntities = mapMaybe (`M.lookup` entityById) (M.findWithDefault [] encPerson.id entityIdsByPerson)
    pure $ AP.makePTEmployeeAPIEntity decPerson role personEntities
  pure $ ListPTEmployeeRes {list = res, summary = Summary {totalCount = totalCount, count = length res}}

makeAvailableCitiesForMerchant :: [ShortId DMerchant.Merchant] -> [City.City] -> [DP.AvailableCitiesForMerchant]
makeAvailableCitiesForMerchant merchantAccessList merchantCityAccessList = do
  let merchantCityList = sortOn fst $ zip merchantAccessList merchantCityAccessList
  let groupedByMerchant = groupBy ((==) `on` fst) merchantCityList
  if null groupedByMerchant
    then []
    else do
      let merchantAccesslistWithCity = map (\group -> DP.AvailableCitiesForMerchant (fst (head group)) (map snd group)) groupedByMerchant
      merchantAccesslistWithCity

assignRole ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  Id DP.Person ->
  Id DRole.Role ->
  m APISuccess
assignRole tokenInfo personId roleId = do
  assertPersonInCallerMerchant tokenInfo personId
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  oldRole <- QRole.findById person.roleId >>= fromMaybeM (RoleDoesNotExist person.roleId.getId)
  newRole <- QRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  -- Promotion into an admin tier must satisfy the same domain restriction as creating one.
  -- Guarded on the tier so that ordinary role changes don't pay a passetto round trip to decrypt
  -- an email whose value would then be discarded.
  when (isAdminTier newRole.dashboardAccessType) $ do
    decPerson <- decrypt person
    assertAdminEmailDomainForPerson tokenInfo person newRole decPerson.email
  when (DRole.isBppSyncRole oldRole || DRole.isBppSyncRole newRole) $
    throwError RoleConversionNotAllowed
  -- Admin tiering (existence-guarded): promoting into (or demoting out of) an
  -- admin-tier role requires SUPER_ADMIN once one is seeded; also nobody
  -- reassigns their own role.
  when (tokenInfo.personId == personId) $
    throwError $ InvalidRequest "Cannot change your own role"
  DCap.guardAdminMutation tokenInfo.personId newRole.dashboardAccessType
  DCap.guardAdminMutation tokenInfo.personId oldRole.dashboardAccessType
  QP.updatePersonRole personId newRole
  recordAdminActionOnPerson DTransaction.DashboardUserRoleAssign tokenInfo personId
  pure Success

assignMerchantCityAccess ::
  ( BeamFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]]
  ) =>
  TokenInfo ->
  Id DP.Person ->
  MerchantCityAccessReq ->
  m APISuccess
assignMerchantCityAccess tokenInfo personId req = do
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  merchantServerAccessCheck merchant
  isCrossMerchantGrant <- assertMayGrantAccessToMerchant tokenInfo merchant.id
  -- A same-merchant grant must not adopt another merchant's user. One person row means one
  -- password across every merchant they can reach, so adopting merchant A's user and then
  -- resetting their password would hand the caller a working session on A. Cross-merchant grants
  -- skip this because they are already SUPER_ADMIN-gated above.
  unless isCrossMerchantGrant $ assertPersonInCallerMerchant tokenInfo personId
  let isSupportedCity = req.operatingCity `elem` (merchant.supportedOperatingCities)
  unless isSupportedCity $
    throwError $ InvalidRequest "Server does not support this city"
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- Granting admin-tier access into a merchant must satisfy that merchant's domain policy.
  -- Without this the control is bypassable in one hop: provision the admin under a merchant with
  -- no allow-list, then grant them access to the merchant that has one. Checked against
  -- merchant.id, the merchant being joined, rather than the caller's.
  role <- QRole.findById person.roleId >>= fromMaybeM (RoleDoesNotExist person.roleId.getId)
  when (isAdminTier role.dashboardAccessType) $ do
    decPerson <- decrypt person
    assertAdminEmailDomain merchant.id role decPerson.email
  mbMerchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity personId merchant.id req.operatingCity
  whenJust mbMerchantAccess $ \_ -> do
    throwError $ InvalidRequest "Merchant access already assigned."
  merchantAccess <- buildMerchantAccess personId merchant.id merchant.shortId req.operatingCity
  QAccess.create merchantAccess
  pure Success

resetMerchantAccess ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  Id DP.Person ->
  MerchantAccessReq ->
  m APISuccess
resetMerchantAccess tokenInfo personId req = do
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  merchantServerAccessCheck merchant
  -- Revoking access is a mutation on somebody else's user like any other, and it used to be the
  -- one that let a caller manufacture an "unowned" person for assertPersonInCallerMerchant.
  assertPersonInCallerMerchant tokenInfo personId
  _person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchantAccesses <- QAccess.findByPersonIdAndMerchantId personId merchant.id
  case merchantAccesses of
    [] -> throwError $ InvalidRequest "Server access already denied."
    (x : _) -> do
      -- this function uses tokens from db, so should be called before transaction
      Auth.cleanCachedTokensByMerchantId personId merchant.id
      QAccess.deleteById x.id
      QReg.deleteAllByPersonIdAndMerchantId personId merchant.id
      pure Success

resetMerchantCityAccess ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  Id DP.Person ->
  MerchantCityAccessReq ->
  m APISuccess
resetMerchantCityAccess tokenInfo personId req = do
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  merchantServerAccessCheck merchant
  assertPersonInCallerMerchant tokenInfo personId
  _person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mbMerchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity personId merchant.id req.operatingCity
  case mbMerchantAccess of
    Nothing -> throwError $ InvalidRequest "Server access already denied."
    Just merchantAccess -> do
      -- this function uses tokens from db, so should be called before transaction
      Auth.cleanCachedTokensByMerchantIdAndCity personId merchant.id req.operatingCity
      QAccess.deleteById merchantAccess.id
      QReg.deleteAllByPersonIdAndMerchantIdAndCity personId merchant.id req.operatingCity
      pure Success

changePassword ::
  ( BeamFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["enforceStrongPasswordPolicy" ::: Bool]
  ) =>
  TokenInfo ->
  ChangePasswordReq ->
  m APISuccess
changePassword tokenInfo req = do
  encPerson <- QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  enforceStrongPasswordPolicy <- asks (.enforceStrongPasswordPolicy)
  when enforceStrongPasswordPolicy $
    validateStrongPassword req.newPassword

  newHash <- getDbHash req.newPassword
  let oldActual = encPerson.passwordHash
  oldProvided <- getDbHash req.oldPassword
  unless (oldActual == Just oldProvided) . throwError $ InvalidRequest "Old password is incorrect."
  QP.updatePersonPassword tokenInfo.personId newHash
  -- Cleanup: delete auth token from cache and db to enforce re-authentication after password change
  Auth.cleanCachedTokensByMerchantIdAndCity tokenInfo.personId tokenInfo.merchantId tokenInfo.city
  QReg.deleteAllByPersonIdAndMerchantIdAndCity tokenInfo.personId tokenInfo.merchantId tokenInfo.city
  pure Success

-- | Rate-limit bucket keyed on email. Deliberately shared with login (Registration.login) so an
-- attacker cannot get a fresh budget by switching between the two endpoints that both resolve
-- credentials via findByEmailAndPassword.
--
-- Normalized the same way the lookup is: findByEmailAndPasswordWithType hashes the lower-cased
-- email, so a mixed-case and a lower-case spelling resolve to one account. Keying on the raw string
-- gave each casing its own budget, which is a fresh set of guesses per variant against one account.
-- Stripping surrounding whitespace only ever merges buckets further, so it cannot widen the budget.
makeEmailHitsCountKey :: Maybe Text -> Text
makeEmailHitsCountKey email = "Email:" <> maybe "" normalizeEmailForKey email <> ":hitsCount"
  where
    normalizeEmailForKey = T.toLower . T.strip

changePasswordAfterExpiry ::
  ( BeamFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["loginRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["enforceStrongPasswordPolicy" ::: Bool]
  ) =>
  ChangePasswordAfterExpiryReq ->
  m APISuccess
changePasswordAfterExpiry req = do
  -- Unauthenticated and resolves credentials, so it is a password oracle unless limited exactly
  -- as login is. This is also the sole recovery path for an admin-reset account.
  loginRateLimitOptions <- asks (.loginRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeEmailHitsCountKey (Just req.email)) loginRateLimitOptions
  encPerson <- QP.findByEmailAndPassword req.email req.oldPassword >>= fromMaybeM (PersonDoesNotExist req.email)
  enforceStrongPasswordPolicy <- asks (.enforceStrongPasswordPolicy)
  when enforceStrongPasswordPolicy $
    validateStrongPassword req.newPassword
  newHash <- getDbHash req.newPassword
  QP.updatePersonPassword encPerson.id newHash
  pure Success

validateStrongPassword :: (BeamFlow m r) => Text -> m ()
validateStrongPassword password = do
  let pwd = T.unpack password
      specialChars :: [Char]
      specialChars = "!@#$%^&*()-_=+[]{}|;:',.<>?/`~"

  unless (length pwd >= 10) $
    throwError $ InvalidRequest "Password must be at least 10 characters long."

  unless (any isUpper pwd) $
    throwError $ InvalidRequest "Password must contain at least one uppercase letter."

  unless (any isLower pwd) $
    throwError $ InvalidRequest "Password must contain at least one lowercase letter."

  unless (any isDigit pwd) $
    throwError $ InvalidRequest "Password must contain at least one number."

  unless (any (`elem` specialChars) pwd) $
    throwError $ InvalidRequest "Password must contain at least one special character."

buildMerchantAccess :: BeamFlow m r => Id DP.Person -> Id DMerchant.Merchant -> ShortId DMerchant.Merchant -> City.City -> m DAccess.MerchantAccess
buildMerchantAccess personId merchantId merchantShortId city = do
  uid <- generateGUID
  now <- getCurrentTime
  return $
    DAccess.MerchantAccess
      { id = Id uid,
        personId = personId,
        merchantId = merchantId,
        merchantShortId = merchantShortId,
        createdAt = now,
        operatingCity = city
      }

profile ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  m DP.PersonAPIEntity
profile tokenInfo = do
  encPerson <- B.runInReplica $ QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  role <- B.runInReplica $ QRole.findById encPerson.roleId >>= fromMaybeM (RoleNotFound encPerson.roleId.getId)
  merchantAccessList <- B.runInReplica $ QAccess.findAllMerchantAccessByPersonId tokenInfo.personId
  decPerson <- decrypt encPerson
  entityGrants <- B.runInReplica $ QEntityAccess.findAllByPersonIdsAndMerchantId [tokenInfo.personId] tokenInfo.merchantId
  let grantedEntityIds = entityGrants <&> (.entityId)
  personEntities <- B.runInReplica $ QEntity.findAllByIdsOrdered grantedEntityIds
  -- entity_access carries no FK, so a deleted depot leaves a dangling grant. Profile is a
  -- login-critical read: log the integrity break rather than locking the person out over it.
  let missingEntityIds = filter (`notElem` (personEntities <&> (.id))) grantedEntityIds
  unless (null missingEntityIds) $
    logError $
      "Dangling entity grants for person " <> tokenInfo.personId.getId <> ": "
        <> T.intercalate ", " (missingEntityIds <&> (.getId))
  -- A retired depot keeps its grants (soft delete is reversible) but must not render as current.
  let livePersonEntities = filter (not . (.deleted)) personEntities
  case merchantAccessList of
    [] -> throwError (InvalidRequest "No access to any merchant")
    merchantAccessList' -> do
      let sortedMerchantAccessList = sortOn DAccess.merchantId merchantAccessList'
      let groupedByMerchant = groupBy ((==) `on` DAccess.merchantId) sortedMerchantAccessList
      let merchantAccesslistWithCity = map (\group -> AP.AvailableCitiesForMerchant ((.merchantShortId) (head group)) (map (.operatingCity) group)) groupedByMerchant
      pure $ AP.makePersonAPIEntity decPerson role (merchantAccesslistWithCity <&> (.merchantShortId)) (Just merchantAccesslistWithCity) livePersonEntities (AP.presentableTokenNo decPerson.tokenNo)

updateProfile ::
  BeamFlow m r =>
  TokenInfo ->
  UpdateProfileReq ->
  m APISuccess
updateProfile tokenInfo req = do
  whenJust req.language $ \lang ->
    QP.updateLanguage tokenInfo.personId lang
  pure Success

getCurrentMerchant ::
  BeamFlow m r =>
  TokenInfo ->
  m MerchantAccessRes
getCurrentMerchant tokenInfo = do
  merchant <-
    B.runInReplica $
      QMerchant.findById tokenInfo.merchantId
        >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
  pure $ MerchantCityAccessReq merchant.shortId tokenInfo.city

getAccessMatrix ::
  BeamFlow m r =>
  TokenInfo ->
  m DMatrix.AccessMatrixRowAPIEntity
getAccessMatrix tokenInfo = do
  encPerson <- B.runInReplica $ QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  role <- B.runInReplica $ QRole.findById encPerson.roleId >>= fromMaybeM (RoleNotFound encPerson.roleId.getId)
  accessMatrixItems <- B.runInReplica $ QMatrix.findAllByRoleId encPerson.roleId
  pure $ DMatrix.mkAccessMatrixRowAPIEntity accessMatrixItems role

changePasswordByAdmin ::
  ( BeamFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["enforceStrongPasswordPolicy" ::: Bool]
  ) =>
  TokenInfo ->
  Id DP.Person ->
  ChangePasswordByAdminReq ->
  m APISuccess
changePasswordByAdmin tokenInfo personId req = do
  assertPersonInCallerMerchant tokenInfo personId
  void $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  enforceStrongPasswordPolicy <- asks (.enforceStrongPasswordPolicy)
  when enforceStrongPasswordPolicy $
    validateStrongPassword req.newPassword
  newHash <- getDbHash req.newPassword
  QP.updatePersonPasswordByAdmin personId newHash
  recordAdminActionOnPerson DTransaction.DashboardUserPasswordResetByAdmin tokenInfo personId
  -- An admin reset is also the remedy for a compromised account, so any session established
  -- with the old credential must die with it.
  Auth.cleanCachedTokens personId
  QReg.deleteAllByPersonId personId
  pure Success

changeMobileNumberByAdmin ::
  (BeamFlow m r, EncFlow m r, HasFlowEnv m r '["updateRestrictedBppRoles" ::: [Text]]) =>
  TokenInfo ->
  Id DP.Person ->
  ChangeMobileNumberByAdminReq ->
  m APISuccess
changeMobileNumberByAdmin tokenInfo personId req = do
  assertPersonInCallerMerchant tokenInfo personId
  runRequestValidation validateChangeMobileNumberReq req
  mobileDbHash <- getDbHash req.newMobileNumber
  result <- QP.findByIdWithRoleAndCheckMobileHash personId (Just mobileDbHash)
  let (mbPersonAndRole, isDuplicateNumber) = result
  unless (null isDuplicateNumber) $ throwError (InvalidRequest "Phone already registered")
  (_person, role) <- fromMaybeM (PersonNotFound personId.getId) mbPersonAndRole
  updateRestrictedBppRoles <- asks (.updateRestrictedBppRoles)
  when (role.name `elem` updateRestrictedBppRoles) $
    throwError $ InvalidRequest $ "Cannot update phone number for role: " <> role.name
  encMobileNum <- encrypt req.newMobileNumber
  QP.updatePersonMobile personId encMobileNum
  recordAdminActionOnPerson DTransaction.DashboardUserMobileChangeByAdmin tokenInfo personId
  pure Success

changeEnabledStatus ::
  (BeamFlow m r, EncFlow m r, HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]) =>
  TokenInfo ->
  Id DP.Person ->
  ChangeEnabledStatusReq ->
  m APISuccess
changeEnabledStatus tokenInfo personId req = do
  -- Writes here are already merchant+city scoped, so a cross-merchant call is inert rather than
  -- harmful. Guarding anyway turns a silent no-op into an explicit error and keeps every
  -- person-id-addressed admin endpoint consistent.
  assertPersonInCallerMerchant tokenInfo personId
  void $ B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Auth.cleanCachedTokensByMerchantIdAndCity personId tokenInfo.merchantId tokenInfo.city
  QReg.updateEnabledStatusByPersonIdAndMerchantIdAndCity personId tokenInfo.merchantId tokenInfo.city req.enabled
  pure Success

changeEmailByAdmin ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  Id DP.Person ->
  ChangeEmailByAdminReq ->
  m APISuccess
changeEmailByAdmin tokenInfo personId req = do
  -- Authorization first, and specifically before the uniqueness probe below: that probe reports
  -- whether an address is already registered, so running it for an unauthorized caller would turn
  -- this endpoint into an account-enumeration oracle over the whole person table.
  assertPersonInCallerMerchant tokenInfo personId
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  runRequestValidation validateChangeEmailReq req
  let newEmail = T.toLower req.newEmail
  mbExistingPerson <- QP.findByEmail newEmail
  whenJust mbExistingPerson $ \existingPerson ->
    when (existingPerson.id /= personId) $
      throwError (InvalidRequest $ "Email already registered with another user: " <> req.newEmail)
  -- Changing an existing admin's email must not move them off an approved domain.
  role <- QRole.findById person.roleId >>= fromMaybeM (RoleDoesNotExist person.roleId.getId)
  assertAdminEmailDomainForPerson tokenInfo person role (Just newEmail)
  encEmail <- encrypt newEmail
  QP.updatePersonEmail personId encEmail
  recordAdminActionOnPerson DTransaction.DashboardUserEmailChangeByAdmin tokenInfo personId
  pure Success

validateChangeEmailReq :: Validate ChangeEmailByAdminReq
validateChangeEmailReq ChangeEmailByAdminReq {..} =
  sequenceA_
    [validateField "newEmail" newEmail P.email]

deletePerson ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  Id DP.Person ->
  Maybe Text ->
  m APISuccess
deletePerson tokenInfo personId mbDeleteReason = do
  -- Every write below is keyed on personId alone and none is merchant-scoped, so without this
  -- guard any dashboard admin could hard-delete an arbitrary person in another merchant.
  assertPersonInCallerMerchant tokenInfo personId
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  -- Audit log: record who deleted which user before the deletion happens
  transaction <- STransaction.buildDashboardAuthTransaction DTransaction.DashboardUserDelete tokenInfo.personId tokenInfo.merchantId
  QT.create transaction{DTransaction.request = Just personId.getId}
  -- Snapshot the user into deleted_user (tombstone) before removing them, so the
  -- deletion leaves a resolvable record (also lets orphaned granted_by ids be
  -- traced back to who was deleted).
  now <- getCurrentTime
  deletedUserId <- generateGUID
  QDeletedUser.create
    DDU.DeletedUser
      { id = Id deletedUserId,
        personId = person.id,
        firstName = person.firstName,
        lastName = person.lastName,
        roleId = person.roleId,
        emailEncrypted = person.email <&> (unEncrypted . (.encrypted)),
        deleteReason = mbDeleteReason,
        deletedBy = tokenInfo.personId,
        deletedAt = now
      }
  QAccess.deleteAllByPersonId personId
  QEntityAccess.deleteAllByPersonId personId
  Auth.cleanCachedTokens personId
  QReg.deleteAllByPersonId personId
  -- person_capability.person_id (subject) is a NOT NULL FK to person, so delete the
  -- user's own overrides before the person delete. granted_by rows (capabilities this
  -- user granted to others) are kept as history — that FK was dropped in migration
  -- 0097, so the id survives and stays resolvable via deleted_user.
  QPC.deleteAllByPersonId personId
  -- person_resource_access.person_id is likewise a NOT NULL FK to person; clear
  -- the departing user's Layer C assignments before the person delete.
  QPRA.deleteAllByPersonId personId
  QP.deletePerson personId
  pure Success

buildPerson :: (EncFlow m r) => Id SP.Person -> CreatePersonReq -> DRole.DashboardAccessType -> Id DMerchant.Merchant -> m SP.Person
buildPerson pid req dashboardAccessType merchantId = do
  now <- getCurrentTime
  mobileNumber <- encrypt req.mobileNumber
  --TODO write query to make existing email in person table to lower case
  email <- encrypt (T.toLower req.email)
  passwordHash <- getDbHash req.password
  return
    SP.Person
      { id = pid,
        firstName = req.firstName,
        lastName = req.lastName,
        roleId = req.roleId,
        email = Just email,
        mobileNumber = mobileNumber,
        mobileCountryCode = req.mobileCountryCode,
        passwordHash = Just passwordHash,
        dashboardAccessType = Just dashboardAccessType,
        dashboardType = fromMaybe DPT.DEFAULT_DASHBOARD req.dashboardType,
        receiveNotification = Nothing,
        createdAt = now,
        updatedAt = now,
        verified = Nothing,
        rejectionReason = Nothing,
        rejectedAt = Nothing,
        passwordUpdatedAt = Just now,
        forcePasswordChange = Nothing,
        merchantId = Just merchantId,
        approvedBy = Nothing,
        rejectedBy = Nothing,
        language = Nothing,
        secretKey = Nothing,
        is2faEnabled = False,
        tokenNo = Nothing,
        vpa = Nothing
      }

data UpdatePersonReq = UpdatePersonReq
  { firstName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UpdateProfileReq = UpdateProfileReq
  { language :: Maybe KET.Language
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

updatePerson :: (BeamFlow m r, EncFlow m r) => Id SP.Person -> UpdatePersonReq -> m APISuccess
updatePerson personId req = do
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  encryptedEmail <- case req.email of
    Just email -> do
      emailExists <- B.runInReplica $ QP.findByEmail email
      when (isJust emailExists) $ throwError (InvalidRequest "Email already registered")
      res <- encrypt (T.toLower email)
      return $ Just res
    Nothing -> pure person.email
  encryptedMobileNumber <- case req.mobileNumber of
    Just mobileNumber -> do
      mobileNumberExists <- B.runInReplica $ QP.findByMobileNumber mobileNumber person.mobileCountryCode
      when (isJust mobileNumberExists) $ throwError (InvalidRequest "Phone already registered")
      encrypt mobileNumber
    Nothing -> pure person.mobileNumber
  let updatedPerson =
        person
          { SP.firstName = fromMaybe person.firstName req.firstName,
            SP.lastName = fromMaybe person.lastName req.lastName,
            SP.email = encryptedEmail,
            SP.mobileNumber = encryptedMobileNumber,
            SP.mobileCountryCode = fromMaybe person.mobileCountryCode req.mobileCountryCode
          }
  QP.updatePerson personId updatedPerson
  pure Success

data BulkUpsertPerson = BulkUpsertPerson
  { firstName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    email :: Maybe Text,
    -- | Deprecated per-row role. The upload screen picks one role for the whole batch, so prefer
    -- the request-level roleId; this stays for CSVs written against the old bulkCreate contract.
    roleName :: Maybe Text,
    -- | Deprecated single-depot field, still accepted so existing CSV payloads keep working.
    entityId :: Maybe Text,
    -- | Three-state: absent leaves grants alone, [] revokes all under this merchant, non-empty replaces.
    entityIds :: Maybe [Text],
    tokenNo :: Maybe Text,
    vpa :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data BulkUpsertPersonReq = BulkUpsertPersonReq
  { operatingCity :: City.City,
    roleId :: Maybe (Id DRole.Role),
    persons :: [BulkUpsertPerson]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data BulkUpsertPersonResp = BulkUpsertPersonResp
  { totalCount :: Int,
    createdPersonIds :: [Id DPT.Person],
    updatedPersonIds :: [Id DPT.Person]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- The entity payload is the desired grant set; Nothing means the row said nothing about depots.
data PersonOp
  = InsertNewPerson DPT.Person DAccess.MerchantAccess [Id DE.Entity]
  | UpdateExistingPerson DPT.Person MerchantAccessAction (Maybe [Id DE.Entity])

data MerchantAccessAction
  = AccessCreate DAccess.MerchantAccess
  | AccessUnchanged

maxBulkPersons :: Int
maxBulkPersons = 500

nonBlank :: Text -> Maybe Text
nonBlank t = case T.strip t of
  "" -> Nothing
  s -> Just s

-- email is lowercased so storage-side hash (raw bytes) and findByEmail (lowercases before hash) agree.
sanitizeBulkPerson :: BulkUpsertPerson -> BulkUpsertPerson
sanitizeBulkPerson p =
  BulkUpsertPerson
    { firstName = p.firstName >>= nonBlank,
      lastName = p.lastName >>= nonBlank,
      mobileNumber = p.mobileNumber >>= nonBlank,
      mobileCountryCode = p.mobileCountryCode >>= nonBlank,
      email = p.email >>= nonBlank <&> T.toLower,
      roleName = p.roleName >>= nonBlank,
      entityId = p.entityId >>= nonBlank,
      entityIds = p.entityIds >>= sanitizeEntityIds,
      tokenNo = p.tokenNo >>= nonBlank,
      vpa = p.vpa >>= nonBlank
    }

-- A row whose depot cell was blank said nothing about depots; only a genuinely empty list revokes.
sanitizeEntityIds :: [Text] -> Maybe [Text]
sanitizeEntityIds ids = case nubOrd (mapMaybe nonBlank ids) of
  [] | not (null ids) -> Nothing
  cleaned -> Just cleaned

requireBulkPersonFields :: MonadFlow m => Int -> BulkUpsertPerson -> m ()
requireBulkPersonFields idx p = do
  let rowTag = "Row " <> T.pack (show idx) <> ": "
      require label = maybe (throwError (InvalidRequest $ rowTag <> label <> " is missing or blank")) (const (pure ()))
  require "mobileNumber" p.mobileNumber
  require "mobileCountryCode" p.mobileCountryCode

bulkUpsert ::
  ( BeamFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  ) =>
  TokenInfo ->
  ShortId DMerchant.Merchant ->
  BulkUpsertPersonReq ->
  m BulkUpsertPersonResp
bulkUpsert tokenInfo merchantShortId req = do
  let actorPersonId = tokenInfo.personId
      accessLevel =
        DMatrix.ApiAccessLevel
          { serverName = DTServer.APP_BACKEND_MANAGEMENT,
            apiEntity = DMatrix.DSL,
            userActionType = DMatrix.DASHBOARD_USER_BULK_CREATE
          }
      endpointId = AuthCap.mkEndpointId accessLevel
  actorPerson <- ApiAuth.verifyAccessLevel accessLevel actorPersonId
  -- The route is DashboardAuth, which never runs verifyApi, so the capability gate is enforced
  -- here; verifyAccessLevel alone no longer checks it.
  actorAccessCaps <- AuthCap.resolveAccess actorPerson.id actorPerson.roleId
  endpointCaps <- AuthCap.endpointCapabilities endpointId
  AuthCap.enforce actorAccessCaps endpointCaps actorPerson endpointId
  let total = length req.persons
  when (total == 0) $
    throwError (InvalidRequest "persons array is empty")
  when (total > maxBulkPersons) $
    throwError (InvalidRequest $ "persons exceeds per-request cap of " <> T.pack (show maxBulkPersons) <> " rows; split the CSV")
  -- CSV upload wire: every field is Maybe; sanitize collapses `Just ""` -> Nothing so required-field checks work.
  persons <- forM (zip [0 :: Int ..] req.persons) $ \(idx, p) -> do
    let p' = sanitizeBulkPerson p
    requireBulkPersonFields idx p'
    pure p'
  merchant <-
    QMerchant.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  ApiAuth.verifyCity merchant req.operatingCity
  actorAccess <- QAccess.findByPersonIdAndMerchantId actorPersonId merchant.id
  when (null actorAccess) $
    throwError AccessDenied
  actorRole <- QRole.findById actorPerson.roleId >>= fromMaybeM (RoleDoesNotExist actorPerson.roleId.getId)
  mbBatchRole <- forM req.roleId $ \rid -> QRole.findById rid >>= fromMaybeM (RoleDoesNotExist rid.getId)
  -- Batch role lookup: one findAllByName instead of 2*N (was called both here and inside resolvePersonOp per row).
  let requestedRoleNames = HS.toList $ HS.fromList $ mapMaybe (.roleName) persons
  foundRoles <- QRole.findAllByName requestedRoleNames
  let rolesByName = M.fromList [(r.name, r) | r <- foundRoles]
  -- Admin bypasses accessibleRoles gate; enumerating every assignable role doesn't scale (same policy as Roles.listV2).
  let isAdmin = actorRole.dashboardAccessType == DRole.DASHBOARD_ADMIN
      allowedRoleIds = actorRole.accessibleRoles
  rowRoles <- forM (zip [0 :: Int ..] persons) $ \(idx, p) -> do
    let rowTag = "Row " <> T.pack (show idx) <> ": "
    r <- case p.roleName of
      Just rn -> M.lookup rn rolesByName & fromMaybeM (InvalidRequest $ rowTag <> "role " <> rn <> " does not exist")
      Nothing -> mbBatchRole & fromMaybeM (InvalidRequest $ rowTag <> "roleName is missing or blank and the request set no roleId")
    unless (isAdmin || r.id `elem` allowedRoleIds) $
      throwError (InvalidRequest $ rowTag <> "role " <> r.name <> " is not assignable by your account")
    pure r
  -- HashSet dedup: O(N) vs Data.List.nub's O(N^2).
  let phoneKeys = mapMaybe (\p -> (,) <$> p.mobileCountryCode <*> p.mobileNumber) persons
      emails = mapMaybe (.email) persons
      tokenNos = mapMaybe (.tokenNo) persons
  when (length phoneKeys /= HS.size (HS.fromList phoneKeys)) $
    throwError (InvalidRequest "Duplicate mobileCountryCode+mobileNumber within the batch")
  when (length emails /= HS.size (HS.fromList emails)) $
    throwError (InvalidRequest "Duplicate email within the batch")
  when (length tokenNos /= HS.size (HS.fromList tokenNos)) $
    throwError (InvalidRequest "Duplicate tokenNo within the batch")
  -- Encrypt before the lock: a passetto failure mid-batch would strand it until the TTL expires.
  encryptedTokenNos <- forM persons $ \p -> forM p.tokenNo encrypt
  encryptedVpas <- forM persons $ \p -> forM p.vpa encrypt
  -- Keeps validation+write in one critical section across replicas.
  let lockKey = QP.bulkUpsertLockKey merchantShortId
  gotLock <- Redis.withCrossAppRedis $ Redis.tryLockRedis lockKey QP.bulkUpsertLockTtl
  unless gotLock $
    throwError (InvalidRequest "Another bulkUpsert for this merchant is in progress; retry shortly")
  ops <-
    finally
      ( do
          now <- getCurrentTime
          conflictRows <- QP.findTokenNoConflictsForMerchant merchant.id $ mapMaybe (fmap (.hash)) encryptedTokenNos
          let conflicts = M.fromListWith (<>) [(h, [personId]) | (h, personId) <- conflictRows]
          -- Batched: resolving per row issued a findById per (row, depot), all inside the merchant lock.
          let requestedEntityIds = nubOrd $ concatMap (\p -> maybeToList p.entityId <> fromMaybe [] p.entityIds) persons
          requestedEntities <- QEntity.findAllByIds (Id <$> requestedEntityIds)
          let entityById = M.fromList [(e.id.getId, e) | e <- requestedEntities]
          builtOps <- forM (zip3 [0 :: Int ..] (zip persons rowRoles) (zip encryptedTokenNos encryptedVpas)) $ \(idx, (p, role), (mbTokenEnc, mbVpaEnc)) ->
            resolvePersonOp merchant role conflicts entityById req.operatingCity now idx p mbTokenEnc mbVpaEnc
          -- Held grants, loaded once: they decide which retired depots are re-sends rather than new
          -- attachments, and the sync below reuses them instead of re-querying.
          heldGrants <- QEntityAccess.findAllByPersonIdsAndMerchantId (opPersonId <$> builtOps) merchant.id
          let heldByPerson = M.fromListWith (<>) [(g.personId, [g.entityId]) | g <- heldGrants]
          assertNoNewRetiredGrants entityById heldByPerson builtOps
          let inserts = [(pers, acc) | InsertNewPerson pers acc _ <- builtOps]
          QP.createPersonsWithAccessAtomic inserts
          forM_ builtOps $ \case
            InsertNewPerson _ _ _ -> pure ()
            UpdateExistingPerson pers accAction _ -> do
              QP.updatePersonUpsertableFields pers
              case accAction of
                AccessCreate acc -> QAccess.create acc
                AccessUnchanged -> pure ()
          syncEntityGrants merchant.id now heldByPerson builtOps
          pure builtOps
      )
      (Redis.withCrossAppRedis $ Redis.unlockRedis lockKey)
  let createdIds = [pers.id | InsertNewPerson pers _ _ <- ops]
      updatedIds = [pers.id | UpdateExistingPerson pers _ _ <- ops]
  logInfo $
    "[Person.bulkUpsert] actor=" <> actorPersonId.getId
      <> " merchant="
      <> merchantShortId.getShortId
      <> " created="
      <> T.pack (show (length createdIds))
      <> " updated="
      <> T.pack (show (length updatedIds))
  pure BulkUpsertPersonResp {totalCount = length createdIds + length updatedIds, createdPersonIds = createdIds, updatedPersonIds = updatedIds}

resolvePersonOp ::
  (BeamFlow m r, EncFlow m r) =>
  DMerchant.Merchant ->
  DRole.Role ->
  M.Map DbHash [Id DPT.Person] ->
  M.Map Text DE.Entity ->
  City.City ->
  UTCTime ->
  Int ->
  BulkUpsertPerson ->
  Maybe (EncryptedHashedField 'AsEncrypted Text) ->
  Maybe (EncryptedHashedField 'AsEncrypted Text) ->
  m PersonOp
resolvePersonOp merchant role conflicts entityById reqCity now idx p mbTokenEncrypted mbVpaEncrypted = do
  let rowTag = "Row " <> T.pack (show idx) <> ": "
      require label = fromMaybeM (InvalidRequest $ rowTag <> label <> " is missing or blank")
  mobileNumber <- require "mobileNumber" p.mobileNumber
  mobileCountryCode <- require "mobileCountryCode" p.mobileCountryCode
  -- Legacy entityId first, so it stays the primary entity behind the deprecated scalars.
  let mbRequestedEntityIds = case (p.entityId, p.entityIds) of
        (Nothing, mbIds) -> mbIds
        (Just eid, Nothing) -> Just [eid]
        -- Explicit [] is the documented revoke; a stale deprecated scalar must not resurrect the grant.
        (Just _, Just []) -> Just []
        (Just eid, Just ids) -> Just (nubOrd (eid : ids))
  mbEntityIdsTyped <- forM mbRequestedEntityIds (resolveEntities rowTag)
  -- findByMobileNumber is global; tenant check below prevents a caller from silently mutating another merchant's user.
  mbExistingByMobile <- QP.findByMobileNumber mobileNumber mobileCountryCode
  case mbExistingByMobile of
    Just existing -> do
      existingAccess <- QAccess.findByPersonIdAndMerchantId existing.id merchant.id
      when (null existingAccess) $
        throwError
          ( InvalidRequest $
              rowTag <> "mobileNumber " <> mobileNumber
                <> " is already registered under a different merchant"
          )
      whenJust p.email $ \email -> do
        mbEmailOwner <- QP.findByEmail email
        whenJust mbEmailOwner $ \owner ->
          when (owner.id /= existing.id) $
            throwError (InvalidRequest (rowTag <> "email " <> email <> " is registered to a different person"))
      whenJust mbTokenEncrypted $ \tokenEnc ->
        QP.requireTokenNoFree conflicts tokenEnc.hash (Just existing.id) rowTag
      encryptedEmail <- forM p.email encrypt
      let updated =
            existing
              { DPT.firstName = fromMaybe existing.firstName p.firstName,
                DPT.lastName = fromMaybe existing.lastName p.lastName,
                DPT.roleId = role.id,
                DPT.email = maybe existing.email Just encryptedEmail,
                DPT.dashboardAccessType = Just role.dashboardAccessType,
                DPT.tokenNo = maybe existing.tokenNo Just mbTokenEncrypted,
                DPT.vpa = maybe existing.vpa Just mbVpaEncrypted,
                DPT.verified = Just True,
                DPT.updatedAt = now
              }
      accessAction <- resolveAccessAction existing.id reqCity
      pure (UpdateExistingPerson updated accessAction mbEntityIdsTyped)
    Nothing -> do
      whenJust p.email $ \email -> do
        mbEmailOwner <- QP.findByEmail email
        whenJust mbEmailOwner $ \_ ->
          throwError (InvalidRequest (rowTag <> "email " <> email <> " is already registered"))
      whenJust mbTokenEncrypted $ \tokenEnc ->
        QP.requireTokenNoFree conflicts tokenEnc.hash Nothing rowTag
      personId <- generateGUID
      encryptedMobileNumber <- encrypt mobileNumber
      encryptedEmail <- forM p.email encrypt
      let fresh =
            DPT.Person
              { id = personId,
                firstName = fromMaybe "" p.firstName,
                lastName = fromMaybe "" p.lastName,
                roleId = role.id,
                email = encryptedEmail,
                mobileNumber = encryptedMobileNumber,
                mobileCountryCode = mobileCountryCode,
                passwordHash = Nothing,
                dashboardAccessType = Just role.dashboardAccessType,
                dashboardType = DPT.DEFAULT_DASHBOARD,
                receiveNotification = Nothing,
                createdAt = now,
                updatedAt = now,
                verified = Just True,
                rejectionReason = Nothing,
                rejectedAt = Nothing,
                passwordUpdatedAt = Nothing,
                forcePasswordChange = Nothing,
                merchantId = Just merchant.id,
                approvedBy = Nothing,
                rejectedBy = Nothing,
                language = Nothing,
                secretKey = Nothing,
                is2faEnabled = False,
                tokenNo = mbTokenEncrypted,
                vpa = mbVpaEncrypted
              }
      access <- buildMerchantAccess fresh.id merchant.id merchant.shortId reqCity
      pure (InsertNewPerson fresh access (fromMaybe [] mbEntityIdsTyped))
  where
    -- An unknown, foreign or retired depot must fail the row rather than be silently dropped.
    resolveEntities rowTag = mapM $ \eid -> do
      let entityIdTyped = Id eid :: Id DE.Entity
      entity <-
        M.lookup eid entityById
          & fromMaybeM (InvalidRequest (rowTag <> "entity " <> eid <> " does not exist"))
      unless (entity.merchantId == merchant.id) $
        throwError (InvalidRequest (rowTag <> "entity " <> eid <> " does not belong to merchant " <> merchant.shortId.getShortId))
      pure entityIdTyped
    -- Grant is per (person, merchant, city): a person may hold access to multiple cities on the same merchant.
    resolveAccessAction existingPersonId reqCity' = do
      mbExistingAccess <- QAccess.findByPersonIdAndMerchantIdAndCity existingPersonId merchant.id reqCity'
      case mbExistingAccess of
        Just _ -> pure AccessUnchanged
        Nothing -> do
          acc <- buildMerchantAccess existingPersonId merchant.id merchant.shortId reqCity'
          pure (AccessCreate acc)

-- Scoped to THIS merchant, and run inside the per-merchant lock so the read-modify-write cannot lose a grant.
-- A retired depot may stay on staff who already hold it (soft delete is reversible via update),
-- but must not take on anyone new.
assertNoNewRetiredGrants :: MonadFlow m => M.Map Text DE.Entity -> M.Map (Id DPT.Person) [Id DE.Entity] -> [PersonOp] -> m ()
assertNoNewRetiredGrants entityById heldByPerson ops =
  forM_ (zip [0 :: Int ..] ops) $ \(idx, op) ->
    whenJust (desiredGrants op) $ \(personId, wanted) -> do
      let held = M.findWithDefault [] personId heldByPerson
          rowTag = "Row " <> T.pack (show idx) <> ": "
      forM_ (wanted \\ held) $ \entityId ->
        whenJust (M.lookup entityId.getId entityById) $ \entity ->
          when entity.deleted $
            throwError (InvalidRequest (rowTag <> "entity " <> entityId.getId <> " is soft-deleted; cannot attach new persons to a retired depot"))

-- Nothing means the row said nothing about depots, so existing grants stand.
desiredGrants :: PersonOp -> Maybe (Id DPT.Person, [Id DE.Entity])
desiredGrants = \case
  InsertNewPerson pers _ entityIds -> Just (pers.id, entityIds)
  UpdateExistingPerson pers _ mbEntityIds -> (pers.id,) <$> mbEntityIds

opPersonId :: PersonOp -> Id DPT.Person
opPersonId = \case
  InsertNewPerson pers _ _ -> pers.id
  UpdateExistingPerson pers _ _ -> pers.id

syncEntityGrants :: BeamFlow m r => Id DMerchant.Merchant -> UTCTime -> M.Map (Id DPT.Person) [Id DE.Entity] -> [PersonOp] -> m ()
syncEntityGrants merchantId now heldByPerson ops = do
  let targets = mapMaybe desiredGrants ops
      revocations = [(personId, held' \\ desired) | (personId, desired) <- targets, let held' = M.findWithDefault [] personId heldByPerson]
  QEntityAccess.deleteManyByPersonIdAndEntityIds merchantId revocations
  forM_ targets $ \(personId, desired) -> do
    let held = M.findWithDefault [] personId heldByPerson
    forM_ (desired \\ held) $ \entityId -> do
      grantId <- generateGUID
      QEntityAccess.create DEA.EntityAccess {id = grantId, personId = personId, entityId = entityId, merchantId = merchantId, createdAt = now}
