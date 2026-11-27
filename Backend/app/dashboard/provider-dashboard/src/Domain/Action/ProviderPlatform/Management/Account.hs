module Domain.Action.ProviderPlatform.Management.Account
  ( getAccountFetchUnverifiedAccounts,
    postAccountVerifyAccount,
    putAccountUpdateRole,
    castDashboardAccessType,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified "dynamic-offer-driver-app" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified API.Types.ProviderPlatform.Management.Account as Common
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Action.Dashboard.AssignmentSync as AssignSync
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Person.Type as DP
import qualified Domain.Types.Role as DRole
import qualified "lib-dashboard" Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified "lib-dashboard" SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Person
  ( findAllByFromDateAndToDateAndMobileNumberAndStatusWithLimitOffset,
    findById,
    softDeletePerson,
    updatePersonApprovedBy,
    updatePersonRejectedBy,
    updatePersonVerifiedStatus,
  )
import qualified "lib-dashboard" Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Role as QRole
import qualified Tools.Auth.Common as Auth
import qualified "lib-dashboard" Tools.Auth.FleetScope as FleetScope
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error
  ( PersonError (PersonDoesNotExist),
    RoleError (RoleConversionNotAllowed, RoleDoesNotExist),
  )

-- | The wire type is owned by the provider API package; lib-dashboard has its
-- own domain equivalent so it stays independent of the app packages.
castFleetOwnerStatus :: Common.FleetOwnerStatus -> DP.FleetOwnerStatus
castFleetOwnerStatus Common.Approved = DP.Approved
castFleetOwnerStatus Common.Rejected = DP.Rejected

getAccountFetchUnverifiedAccounts ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Common.FleetOwnerStatus ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Environment.Flow Common.UnverifiedAccountsResp
getAccountFetchUnverifiedAccounts _merchantShortId _opCity _apiTokenInfo mbFromDate mbToDate mbMobileNumber mbStatus mbLimit mbOffset = do
  encryptPersonLs <- findAllByFromDateAndToDateAndMobileNumberAndStatusWithLimitOffset mbFromDate mbToDate mbMobileNumber (castFleetOwnerStatus <$> mbStatus) mbLimit mbOffset
  res <- traverse convertPersonToPersonAPIEntity encryptPersonLs
  let summary = Common.Summary {totalCount = 10000, count = length res}
  pure $ Common.UnverifiedAccountsResp {listItems = res, summary = summary}
  where
    convertPersonToPersonAPIEntity DP.Person {..} = do
      role <- QRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
      mobileNumber' <- decrypt mobileNumber
      email' <- traverse decrypt email
      pure $
        Common.PersonAPIEntity
          { id = Kernel.Types.Id.cast id,
            roleAPIEntity = convertRoleToRoleAPIEntity role,
            email = email',
            mobileNumber = mobileNumber',
            dashboardAccessType = castDashboardAccessType <$> dashboardAccessType,
            ..
          }
    convertRoleToRoleAPIEntity DRole.Role {..} =
      Common.RoleAPIEntity
        { id = Kernel.Types.Id.cast id,
          name = name,
          dashboardAccessType = castDashboardAccessType dashboardAccessType,
          description = description
        }

castDashboardAccessType :: DRole.DashboardAccessType -> Common.DashboardAccessType
castDashboardAccessType = \case
  DRole.DASHBOARD_USER -> Common.DASHBOARD_USER
  DRole.DASHBOARD_ADMIN -> Common.DASHBOARD_ADMIN
  DRole.FLEET_OWNER -> Common.FLEET_OWNER
  DRole.DASHBOARD_RELEASE_ADMIN -> Common.DASHBOARD_RELEASE_ADMIN
  DRole.MERCHANT_ADMIN -> Common.MERCHANT_ADMIN
  DRole.RENTAL_FLEET_OWNER -> Common.RENTAL_FLEET_OWNER
  DRole.MERCHANT_MAKER -> Common.MERCHANT_MAKER
  DRole.MERCHANT_SERVER -> Common.MERCHANT_SERVER
  DRole.DASHBOARD_OPERATOR -> Common.DASHBOARD_OPERATOR
  DRole.TICKET_DASHBOARD_USER -> Common.TICKET_DASHBOARD_USER
  DRole.TICKET_DASHBOARD_MERCHANT -> Common.TICKET_DASHBOARD_MERCHANT
  DRole.TICKET_DASHBOARD_ADMIN -> Common.TICKET_DASHBOARD_ADMIN
  DRole.TICKET_DASHBOARD_APPROVER -> Common.TICKET_DASHBOARD_APPROVER

postAccountVerifyAccount ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Common.VerifyAccountReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postAccountVerifyAccount merchantShortId opCity apiTokenInfo req = do
  runRequestValidation validateVerifyAccountReq req
  -- Check the caller's merchant and city, and that the account is theirs, before anything is written.
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let personId = Kernel.Types.Id.cast req.fleetOwnerId
  -- A fleet created as a scope has no dashboard person; only a fleet that was someone's login does.
  mbPerson <- findById personId
  -- Older accounts have no merchant; the driver-app side checks the fleet's own merchant.
  whenJust mbPerson $ \person ->
    whenJust person.merchantId $ \personMerchantId ->
      unless (personMerchantId == apiTokenInfo.merchant.id) $ throwError (PersonDoesNotExist personId.getId)
  case req.status of
    Common.Rejected -> do
      whenJust mbPerson $ \_ -> do
        Auth.cleanCachedTokens personId
        QR.deleteAllByPersonId personId
        softDeletePerson personId req.reason
      -- Rejecting a fleet also closes the access others hold on it through role assignments.
      AssignSync.revokeAllOnFleet apiTokenInfo.merchant.id personId
      whenJust mbPerson $ \_ -> updatePersonRejectedBy personId apiTokenInfo.personId
    Common.Approved -> do
      whenJust mbPerson $ \person -> do
        unless (person.verified == Just True) $ updatePersonVerifiedStatus personId True
        updatePersonApprovedBy personId apiTokenInfo.personId
      -- Bring back the owner's own access to the fleet, which a rejection closed.
      AssignSync.syncPersonAssignments personId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.Management.callManagementAPI
      checkedMerchantId
      opCity
      (.accountDSL.postAccountVerifyAccount)
      req

-- Validate the reason field in VerifyAccountReq
validateVerifyAccountReq :: Validate Common.VerifyAccountReq
validateVerifyAccountReq Common.VerifyAccountReq {..} =
  sequenceA_
    [ validateField "reason" reason $ InMaybe P.inputName
    ]

putAccountUpdateRole ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo UserActionType ->
  Kernel.Types.Id.Id Dashboard.Common.Person ->
  Kernel.Types.Id.Id Dashboard.Common.Role ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putAccountUpdateRole merchantShortId opCity apiTokenInfo personId' roleId' = do
  let personId = Kernel.Types.Id.cast personId'
      roleId = Kernel.Types.Id.cast roleId'
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  oldRole <- QRole.findById person.roleId >>= fromMaybeM (RoleDoesNotExist person.roleId.getId)
  newRole <- QRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  when (DRole.isBppSyncRole oldRole || DRole.isBppSyncRole newRole) $
    throwError RoleConversionNotAllowed
  _ <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  rolesBefore <- FleetScope.rolesHeldBy person
  QP.updatePersonRole personId newRole
  FleetScope.endSessionsOnTwoFaRoleGrant personId rolesBefore newRole
  AssignSync.syncPersonAssignments personId
  pure Kernel.Types.APISuccess.Success
