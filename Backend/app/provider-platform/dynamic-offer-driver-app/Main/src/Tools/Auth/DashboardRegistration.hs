{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The dashboard's half of fleet and operator onboarding.
--
-- These routes were never a simple proxy hop: provider-dashboard called the
-- application server and then created or updated rows in the DASHBOARD
-- database with the result -- the person a fleet owner logs in as, their
-- merchant access, their verified flag, and the session token itself.
--
-- Serving the routes directly means doing that work here. Everything in this
-- module runs inside 'runInDashboardDb'; nothing here touches
-- atlas_driver_offer_bpp.
module Tools.Auth.DashboardRegistration
  ( issueFleetOwnerAuthToken,
    dashboardPersonIdByMobile,
    createFleetOwnerDashboardPerson,
    markDashboardPersonVerifiedOnOnboarding,
    adminApprovalRequiredForDriver,
    verifyFleetWhileLogin,
    fleetOnboardingEnabled,
    requestorDashboardAccessType,
    deleteDashboardPerson,
    assertDashboardEmailAvailable,
    updateDashboardPersonProfile,
    dashboardPersonApprovedBy,
    updateDashboardPersonLanguage,
    listUnverifiedDashboardAccounts,
    assertOperatorRegistrable,
    registerOperatorDashboardPerson,
  )
where

import qualified API.Types.ProviderPlatform.Management.Account as CommonAccount
import qualified Dashboard.Common as CommonDashboard
import qualified Data.Text as T
import qualified "lib-dashboard" Domain.Action.Dashboard.Registration as DRegistration
import qualified "lib-dashboard" Domain.Types.Merchant as DDashboardMerchant
import qualified "lib-dashboard" Domain.Types.Person as DDashboardPerson
import qualified "lib-dashboard" Domain.Types.Role as DDashboardRole
import qualified "lib-dashboard" Domain.Types.ServerName as DTServer
import Kernel.Beam.Functions (runInDashboardDb)
import Kernel.External.Encryption (decrypt, encrypt)
import qualified Kernel.External.Types as KET
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error (GenericError (InvalidRequest), MerchantError (MerchantDoesNotExist), PersonError (PersonDoesNotExist))
import Kernel.Types.Id (Id (..), ShortId (..))
import Kernel.Utils.Common
import "lib-dashboard" Storage.Beam.BeamFlow (BeamFlow)
import "lib-dashboard" Storage.Beam.SchemaInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QDashboardMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QDashboardAccess
import qualified "lib-dashboard" Storage.Queries.Person as QDashboardPerson
import qualified "lib-dashboard" Storage.Queries.RegistrationToken as QDashboardRegToken
import qualified "lib-dashboard" Storage.Queries.Role as QDashboardRole
import Tools.Auth.DashboardUser (DashboardAuthFlow, DashboardUser (..))
import qualified "lib-dashboard" Tools.Error as DashboardError

-- | What the dashboard-side half of these flows needs from the environment.
--
-- @dataServers@ and @authTokenCacheKeyPrefix@ are lib-dashboard's; driver-app's
-- AppEnv carries both already, which is what makes serving these routes here
-- possible at all.
type RegistrationFlow m r =
  ( DashboardAuthFlow m r,
    BeamFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]]
  )

merchantByShortId :: BeamFlow m r => Text -> m DDashboardMerchant.Merchant
merchantByShortId merchantShortId =
  QDashboardMerchant.findByShortId (ShortId merchantShortId)
    >>= fromMaybeM (MerchantDoesNotExist merchantShortId)

-- | Mint the dashboard session token a fleet owner logs in with, and clear
-- their pending-verification flag when the merchant verifies on login.
--
-- provider-dashboard did this in @postRegistrationV2VerifyOtp@ and returned the
-- token; the application server's own response carries no token, so without
-- this the fleet owner has nothing to authenticate with afterwards.
issueFleetOwnerAuthToken :: RegistrationFlow m r => Text -> City.City -> Text -> Text -> m Text
issueFleetOwnerAuthToken merchantShortId opCity mobileNumber mobileCountryCode = runInDashboardDb $ do
  person <-
    QDashboardPerson.findByMobileNumber mobileNumber mobileCountryCode
      >>= fromMaybeM (PersonDoesNotExist mobileNumber)
  merchant <- merchantByShortId merchantShortId
  unless (opCity `elem` merchant.supportedOperatingCities) $
    throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  token <- DRegistration.generateToken person.id merchant opCity
  when
    ( person.verified /= Just True
        && merchant.verifyFleetWhileLogin == Just True
        && not (fromMaybe False merchant.requireAdminApprovalForFleetOnboarding)
    )
    $ QDashboardPerson.updatePersonVerifiedStatus person.id True
  pure token

-- | The dashboard person id for a mobile number, if one exists.
--
-- The fleet login and fleet-create flows send it to the application server so
-- it can reuse the same id, and use its absence to decide whether a dashboard
-- person still has to be created.
dashboardPersonIdByMobile :: RegistrationFlow m r => Text -> Text -> m (Maybe Text)
dashboardPersonIdByMobile mobileNumber mobileCountryCode =
  runInDashboardDb $
    fmap (.id.getId) <$> QDashboardPerson.findByMobileNumber mobileNumber mobileCountryCode

-- | Create the dashboard-side fleet owner, mirroring
-- @createFleetOwnerDashboardOnly@ as provider-dashboard called it after the
-- application server allocated the person id.
--
-- Called only when no dashboard person exists for the number yet.
createFleetOwnerDashboardPerson ::
  RegistrationFlow m r =>
  Text ->
  City.City ->
  Text ->
  Text ->
  Text ->
  m ()
createFleetOwnerDashboardPerson merchantShortId opCity mobileNumber mobileCountryCode personId = runInDashboardDb $ do
  merchant <- merchantByShortId merchantShortId
  fleetOwnerRole <-
    QDashboardRole.findByDashboardAccessType DDashboardRole.FLEET_OWNER
      >>= fromMaybeM (DashboardError.RoleNotFound $ show DDashboardRole.FLEET_OWNER)
  -- the shape provider-dashboard built in buildFleetOwnerRegisterReqV2: a
  -- placeholder name that the register step later overwrites
  let req =
        DRegistration.FleetRegisterReq
          { firstName = "FLEET",
            lastName = "OWNER",
            mobileNumber,
            mobileCountryCode,
            merchantId = merchant.shortId,
            fleetType = Nothing,
            city = Just opCity,
            email = Nothing
          }
  DRegistration.createFleetOwnerDashboardOnly fleetOwnerRole merchant req (Id personId)

-- | Whether the merchant still needs an admin to approve this fleet owner.
--
-- provider-dashboard computed it from the DASHBOARD person's @approvedBy@ and
-- passed it to the onboarding verify call.
adminApprovalRequiredForDriver :: RegistrationFlow m r => Text -> Text -> m Bool
adminApprovalRequiredForDriver merchantShortId driverId = runInDashboardDb $ do
  merchant <- merchantByShortId merchantShortId
  mbPerson <- QDashboardPerson.findById (Id driverId)
  pure $ case (mbPerson, merchant.requireAdminApprovalForFleetOnboarding) of
    (Just p, Just True) -> isNothing p.approvedBy
    _ -> False

-- | Mark the dashboard person verified once onboarding enabled the fleet owner.
markDashboardPersonVerifiedOnOnboarding :: RegistrationFlow m r => Text -> m ()
markDashboardPersonVerifiedOnOnboarding driverId =
  runInDashboardDb $ QDashboardPerson.updatePersonVerifiedStatus (Id driverId) True

-- | @verifyFleetWhileLogin@ for this merchant, which the login-OTP call passes
-- through to the application server as its @enabled@ flag.
verifyFleetWhileLogin :: RegistrationFlow m r => Text -> City.City -> m Bool
verifyFleetWhileLogin merchantShortId _opCity =
  runInDashboardDb $ fromMaybe False . (.verifyFleetWhileLogin) <$> merchantByShortId merchantShortId

-- | Whether a fleet created by an operator starts enabled, i.e. the merchant
-- does not require an admin to approve fleet onboarding.
fleetOnboardingEnabled :: RegistrationFlow m r => Text -> City.City -> m Bool
fleetOnboardingEnabled merchantShortId _opCity =
  runInDashboardDb $ not . fromMaybe False . (.requireAdminApprovalForFleetOnboarding) <$> merchantByShortId merchantShortId

-- | The caller's dashboard access type in the shape the onboarding endpoints
-- take it, mirroring provider-dashboard's @castDashboardAccessType@.
requestorDashboardAccessType :: DashboardUser -> Maybe CommonAccount.DashboardAccessType
requestorDashboardAccessType dashboardUser = castAccessType <$> dashboardUser.person.dashboardAccessType

castAccessType :: DDashboardRole.DashboardAccessType -> CommonAccount.DashboardAccessType
castAccessType = \case
  DDashboardRole.DASHBOARD_USER -> CommonAccount.DASHBOARD_USER
  DDashboardRole.DASHBOARD_ADMIN -> CommonAccount.DASHBOARD_ADMIN
  DDashboardRole.FLEET_OWNER -> CommonAccount.FLEET_OWNER
  DDashboardRole.DASHBOARD_RELEASE_ADMIN -> CommonAccount.DASHBOARD_RELEASE_ADMIN
  DDashboardRole.MERCHANT_ADMIN -> CommonAccount.MERCHANT_ADMIN
  DDashboardRole.RENTAL_FLEET_OWNER -> CommonAccount.RENTAL_FLEET_OWNER
  DDashboardRole.MERCHANT_MAKER -> CommonAccount.MERCHANT_MAKER
  DDashboardRole.MERCHANT_SERVER -> CommonAccount.MERCHANT_SERVER
  DDashboardRole.DASHBOARD_OPERATOR -> CommonAccount.DASHBOARD_OPERATOR
  DDashboardRole.TICKET_DASHBOARD_USER -> CommonAccount.TICKET_DASHBOARD_USER
  DDashboardRole.TICKET_DASHBOARD_MERCHANT -> CommonAccount.TICKET_DASHBOARD_MERCHANT
  DDashboardRole.TICKET_DASHBOARD_ADMIN -> CommonAccount.TICKET_DASHBOARD_ADMIN
  DDashboardRole.TICKET_DASHBOARD_APPROVER -> CommonAccount.TICKET_DASHBOARD_APPROVER

-- | Remove a dashboard person and everything hanging off them.
--
-- provider-dashboard did this after the application server accepted a
-- permanent driver deletion; skipping it leaves an orphaned dashboard login
-- and stale merchant-access rows behind.
deleteDashboardPerson :: RegistrationFlow m r => Text -> m ()
deleteDashboardPerson personId = runInDashboardDb $ do
  mbPerson <- QDashboardPerson.findById (Id personId)
  whenJust mbPerson $ \_ -> do
    QDashboardAccess.deleteAllByPersonId (Id personId)
    QDashboardRegToken.deleteAllByPersonId (Id personId)
    QDashboardPerson.deletePerson (Id personId)
    logTagInfo "PermanentlyDelete" $ "removed dashboard person " <> personId

-- | Reject an e-mail already registered to a different dashboard person.
assertDashboardEmailAvailable :: RegistrationFlow m r => Text -> Text -> m ()
assertDashboardEmailAvailable email personId = runInDashboardDb $ do
  mbExisting <- QDashboardPerson.findByEmail (T.toLower email)
  whenJust mbExisting $ \existing ->
    when (existing.id.getId /= personId) $
      throwError (InvalidRequest $ "Email already registered with another user: " <> email)

-- | Keep the dashboard person's contact details in step with a fleet-driver
-- update, as provider-dashboard did after the application server accepted it.
updateDashboardPersonProfile ::
  RegistrationFlow m r =>
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  m ()
updateDashboardPersonProfile personId mbFirstName mbLastName mbEmail mbMobileNo mbMobileCountryCode =
  when (isJust mbFirstName || isJust mbLastName || isJust mbEmail || isJust mbMobileNo || isJust mbMobileCountryCode) $
    runInDashboardDb $ do
      mbPerson <- QDashboardPerson.findById (Id personId)
      whenJust mbPerson $ \person -> do
        newMobileNumber <- maybe (pure person.mobileNumber) encrypt mbMobileNo
        newEmail <- maybe (pure person.email) (fmap Just . encrypt . T.toLower) mbEmail
        let updatedPerson =
              person
                { DDashboardPerson.firstName = fromMaybe person.firstName mbFirstName,
                  DDashboardPerson.lastName = fromMaybe person.lastName mbLastName,
                  DDashboardPerson.email = newEmail,
                  DDashboardPerson.mobileNumber = newMobileNumber,
                  DDashboardPerson.mobileCountryCode = fromMaybe person.mobileCountryCode mbMobileCountryCode
                } ::
                DDashboardPerson.Person
        QDashboardPerson.updatePerson (Id personId) updatedPerson

-- | Who approved this dashboard person, if anyone. The operator-info response
-- prefers the dashboard's answer over the application server's.
dashboardPersonApprovedBy :: RegistrationFlow m r => Text -> m (Maybe Text)
dashboardPersonApprovedBy personId =
  runInDashboardDb $ do
    mbPerson <- QDashboardPerson.findById (Id personId)
    pure $ mbPerson >>= (.approvedBy) <&> (.getId)

-- | Mirror a fleet owner's language choice onto their dashboard person.
updateDashboardPersonLanguage :: RegistrationFlow m r => Text -> KET.Language -> m ()
updateDashboardPersonLanguage personId language =
  runInDashboardDb $ QDashboardPerson.updateLanguage (Id personId) language

-- | The unverified fleet-owner accounts awaiting admin approval.
--
-- This route never reached an application server: provider-dashboard answered
-- it entirely from the dashboard database. Forwarding it to driver-app returns
-- driver-app's own people, which is a different set of rows altogether.
-- | Map the provider API wire type onto lib-dashboard's domain equivalent.
castFleetOwnerStatus :: CommonAccount.FleetOwnerStatus -> DDashboardPerson.FleetOwnerStatus
castFleetOwnerStatus CommonAccount.Approved = DDashboardPerson.Approved
castFleetOwnerStatus CommonAccount.Rejected = DDashboardPerson.Rejected

listUnverifiedDashboardAccounts ::
  forall m r.
  RegistrationFlow m r =>
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe CommonAccount.FleetOwnerStatus ->
  Maybe Int ->
  Maybe Int ->
  m CommonAccount.UnverifiedAccountsResp
listUnverifiedDashboardAccounts mbFromDate mbToDate mbMobileNumber mbStatus mbLimit mbOffset = runInDashboardDb $ do
  people <-
    QDashboardPerson.findAllByFromDateAndToDateAndMobileNumberAndStatusWithLimitOffset
      mbFromDate
      mbToDate
      mbMobileNumber
      (castFleetOwnerStatus <$> mbStatus)
      mbLimit
      mbOffset
  listItems <- traverse toPersonAPIEntity people
  pure
    CommonAccount.UnverifiedAccountsResp
      { listItems,
        summary = CommonDashboard.Summary {totalCount = 10000, count = length listItems}
      }
  where
    -- the signature is needed: without it the binding is generalised and
    -- decrypt's encTools constraint can no longer be resolved against r
    toPersonAPIEntity :: DDashboardPerson.Person -> m CommonAccount.PersonAPIEntity
    toPersonAPIEntity person = do
      role <-
        QDashboardRole.findById person.roleId
          >>= fromMaybeM (DashboardError.RoleDoesNotExist person.roleId.getId)
      mobileNumber <- decrypt person.mobileNumber
      email <- traverse decrypt person.email
      pure
        CommonAccount.PersonAPIEntity
          { id = Id person.id.getId,
            firstName = person.firstName,
            lastName = person.lastName,
            roleAPIEntity =
              CommonAccount.RoleAPIEntity
                { id = Id role.id.getId,
                  name = role.name,
                  dashboardAccessType = castAccessType role.dashboardAccessType,
                  description = role.description
                },
            email,
            mobileNumber,
            mobileCountryCode = person.mobileCountryCode,
            dashboardAccessType = castAccessType <$> person.dashboardAccessType,
            createdAt = person.createdAt,
            receiveNotification = person.receiveNotification,
            updatedAt = person.updatedAt,
            verified = person.verified,
            rejectionReason = person.rejectionReason,
            rejectedAt = person.rejectedAt
          }

-- | Pre-flight every deterministic precondition of the dashboard-side operator
-- write, before the application server allocates the person.
--
-- provider-dashboard only checked the phone/e-mail here; the merchant and role
-- lookups happened after the forwarded call had already created the application
-- person, so a missing role left that person orphaned -- registered on the
-- application server, unable to log in, and impossible to re-register because
-- the retry then failed with @USER_ALREADY_EXISTS@.
--
-- Running the same lookups up front turns those failures into a clean rejection
-- with nothing written on either side. Only an infrastructure failure between
-- the two writes can still orphan a person, which no ordering can prevent
-- without a distributed transaction.
assertOperatorRegistrable :: RegistrationFlow m r => Text -> Maybe Text -> Text -> Text -> Maybe Text -> m ()
assertOperatorRegistrable merchantShortId mbEmail mobileNumber mobileCountryCode mbRoleId = runInDashboardDb $ do
  existing <- QDashboardPerson.findByEmailOrMobile mbEmail mobileNumber mobileCountryCode
  unless (null existing) $
    throwError (InvalidRequest "Phone or Email already registered")
  void $ merchantByShortId merchantShortId
  void $ case mbRoleId of
    Just roleId -> QDashboardRole.findById (Id roleId) >>= fromMaybeM (DashboardError.RoleNotFound roleId)
    Nothing -> QDashboardRole.findByDashboardAccessType DDashboardRole.DASHBOARD_OPERATOR >>= fromMaybeM (DashboardError.RoleNotFound "DASHBOARD_OPERATOR")

-- | Create the dashboard-side operator once the application server has
-- allocated the person id, so the operator can actually log in.
--
-- The implementation is lib-dashboard's, shared with provider-dashboard.
registerOperatorDashboardPerson ::
  RegistrationFlow m r =>
  Text ->
  City.City ->
  Maybe Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  Text ->
  Maybe Text ->
  m ()
registerOperatorDashboardPerson merchantShortId opCity email mobileNumber mobileCountryCode firstName lastName password operatorId mbRoleId = runInDashboardDb $ do
  merchant <- merchantByShortId merchantShortId
  DRegistration.registerOperatorDashboardOnly
    opCity
    email
    mobileNumber
    mobileCountryCode
    firstName
    lastName
    password
    (Id operatorId)
    merchant
    mbRoleId
