module Domain.Action.ProviderPlatform.Management.Account
  ( getAccountFetchUnverifiedAccounts,
    postAccountVerifyAccount,
    putAccountUpdateRole,
    castDashboardAccessType,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified API.Types.ProviderPlatform.Management.Account as Common
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Person.Type as DP
import qualified Domain.Types.Role as DRole
import qualified Domain.Types.Transaction
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
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.CachedQueries.Role as CQRole
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
import Tools.Auth.Api
import qualified Tools.Auth.Common as Auth
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error
  ( PersonError (PersonDoesNotExist),
    RoleError (RoleDoesNotExist),
  )

getAccountFetchUnverifiedAccounts ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Common.FleetOwnerStatus ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Environment.Flow Common.UnverifiedAccountsResp
getAccountFetchUnverifiedAccounts _merchantShortId _opCity _apiTokenInfo mbFromDate mbToDate mbMobileNumber mbStatus mbLimit mbOffset = do
  encryptPersonLs <- findAllByFromDateAndToDateAndMobileNumberAndStatusWithLimitOffset mbFromDate mbToDate mbMobileNumber mbStatus mbLimit mbOffset
  res <- traverse convertPersonToPersonAPIEntity encryptPersonLs
  let summary = Common.Summary {totalCount = 10000, count = length res}
  pure $ Common.UnverifiedAccountsResp {listItems = res, summary = summary}
  where
    convertPersonToPersonAPIEntity DP.Person {..} = do
      role <- CQRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
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
  ApiTokenInfo ->
  Common.VerifyAccountReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postAccountVerifyAccount merchantShortId opCity apiTokenInfo req = do
  runRequestValidation validateVerifyAccountReq req
  let personId = Kernel.Types.Id.cast req.fleetOwnerId
  case req.status of
    Common.Rejected -> do
      Auth.cleanCachedTokens personId
      QR.deleteAllByPersonId personId
      softDeletePerson personId req.reason
      updatePersonRejectedBy personId apiTokenInfo.personId
    Common.Approved -> do
      person <- findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      unless (person.verified == Just True) $ updatePersonVerifiedStatus personId True
      updatePersonApprovedBy personId apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
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
  ApiTokenInfo ->
  Kernel.Types.Id.Id Dashboard.Common.Person ->
  Kernel.Types.Id.Id Dashboard.Common.Role ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putAccountUpdateRole merchantShortId opCity apiTokenInfo personId' roleId' = do
  let personId = Kernel.Types.Id.cast personId'
      roleId = Kernel.Types.Id.cast roleId'
  _person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  role <- CQRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  QP.updatePersonRole personId role
  let mbAccessType =
        case role.dashboardAccessType of
          r | r `elem` [DRole.RENTAL_FLEET_OWNER, DRole.FLEET_OWNER] -> Just Common.FLEET_OWNER
          DRole.DASHBOARD_OPERATOR -> Just Common.DASHBOARD_OPERATOR
          _ -> Nothing
  whenJust mbAccessType \accessType -> do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
    _ <-
      SharedLogic.Transaction.withTransactionStoring transaction $
        API.Client.ProviderPlatform.Management.callManagementAPI
          checkedMerchantId
          opCity
          (.accountDSL.putAccountUpdateRole)
          personId'
          accessType
    pure ()
  pure Kernel.Types.APISuccess.Success
