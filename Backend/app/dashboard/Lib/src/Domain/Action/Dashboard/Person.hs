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
import qualified Data.HashMap.Strict as HM
import Data.List (groupBy, nub, sortOn)
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Capability as DCap
import qualified Domain.Types.AccessMatrix as DMatrix
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
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
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
import qualified Storage.Queries.Entity as QEntity
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QReg
import qualified Storage.Queries.Role as QRole
import qualified Storage.Queries.Transaction as QT
import Tools.Auth
-- isSuperAdmin lives here rather than in Domain.Action.Dashboard.Capability: that module has no
-- export list, so it re-exports only what it defines, not what it imports.
import Tools.Auth.Capability (isSuperAdmin)
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

-- | Admin mutations that address a person directly by id must not reach across merchants.
-- Without this an admin of any merchant could act on an arbitrary person id.
--
-- Authority comes from two sources, unioned:
--
--   * person.merchantId — the merchant the person was provisioned under. Written once at
--     creation, never updated, and not reachable from any admin endpoint. This is the anchor.
--   * merchant_access rows — merchants the person has since been granted access to. Included
--     so that a person shared across merchants stays manageable by each of them.
--
-- The access rows alone are not a safe basis, and an earlier version of this function that used
-- only them was bypassable: resetMerchantAccess and resetMerchantCityAccess delete access rows
-- and leave the person row alive, so an attacker could empty a victim's rows and then claim them
-- as "unowned". That is also reachable without any attack — a merchant revoking its own user's
-- last access produces exactly that state. person.merchantId survives both, which is the point:
-- authorization must not derive from state the unauthorized party can modify.
--
-- No claimants at all is still permitted, and now means only two things. Either the person was
-- created moments ago and has not been granted access yet (createPerson does not write an access
-- row; createUserForMerchant grants it on the next line) — rejecting that would strand an admin
-- who typo'd an email at creation, unable to fix or delete the account. Or the row predates the
-- merchantId column and was never backfilled. Neither is forgeable by a caller.
assertPersonInCallerMerchant ::
  BeamFlow m r =>
  TokenInfo ->
  Id DP.Person ->
  m ()
assertPersonInCallerMerchant tokenInfo personId = do
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  allAccess <- QAccess.findAllMerchantAccessByPersonId personId
  let claimants = maybe [] (: []) person.merchantId <> map (.merchantId) allAccess
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
  let personAPIEntity = AP.makePersonAPIEntity decPerson role [] Nothing Nothing Nothing
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
    pure $ DP.makePersonAPIEntity decPerson role (nub merchantAccessList) (Just availableCitiesForMerchant) Nothing Nothing
  let count = length res
  let summary = Summary {totalCount = 10000, count}
  pure $ ListPersonRes {list = res, summary = summary}

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
    assertAdminEmailDomain tokenInfo.merchantId newRole decPerson.email
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
  _person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
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
  mbEntity <- case encPerson.entityId of
    Just eId -> Just <$> (QEntity.findById eId >>= fromMaybeM (InvalidRequest $ "Entity " <> eId.getId <> " referenced by person " <> tokenInfo.personId.getId <> " does not exist"))
    Nothing -> pure Nothing
  let mbEntityId = mbEntity <&> (.id)
      mbEntityName = mbEntity <&> (.entityName)
  case merchantAccessList of
    [] -> throwError (InvalidRequest "No access to any merchant")
    merchantAccessList' -> do
      let sortedMerchantAccessList = sortOn DAccess.merchantId merchantAccessList'
      let groupedByMerchant = groupBy ((==) `on` DAccess.merchantId) sortedMerchantAccessList
      let merchantAccesslistWithCity = map (\group -> DP.AvailableCitiesForMerchant ((.merchantShortId) (head group)) (map (.operatingCity) group)) groupedByMerchant
      pure $ DP.makePersonAPIEntity decPerson role (merchantAccesslistWithCity <&> (.merchantShortId)) (Just merchantAccesslistWithCity) mbEntityId mbEntityName

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
  assertPersonInCallerMerchant tokenInfo personId
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  runRequestValidation validateChangeEmailReq req
  -- Changing an existing admin's email must not move them off an approved domain.
  role <- QRole.findById person.roleId >>= fromMaybeM (RoleDoesNotExist person.roleId.getId)
  let newEmail = T.toLower req.newEmail
  assertAdminEmailDomain tokenInfo.merchantId role (Just newEmail)
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
  m APISuccess
deletePerson tokenInfo personId = do
  -- Every write below is keyed on personId alone and none is merchant-scoped, so without this
  -- guard any dashboard admin could hard-delete an arbitrary person in another merchant.
  assertPersonInCallerMerchant tokenInfo personId
  void $ B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  -- Audit log: record who deleted which user before the deletion happens
  transaction <- STransaction.buildDashboardAuthTransaction DTransaction.DashboardUserDelete tokenInfo.personId tokenInfo.merchantId
  QT.create transaction{DTransaction.request = Just personId.getId}
  QAccess.deleteAllByPersonId personId
  Auth.cleanCachedTokens personId
  QReg.deleteAllByPersonId personId
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
        tokenNoHash = Nothing,
        entityId = Nothing
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
