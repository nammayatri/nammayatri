{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.Account
  ( getAccountFetchUnverifiedAccounts,
    postAccountVerifyAccount,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Account as Common
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
import Kernel.Types.Error (MerchantError (MerchantDoesNotExist))
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Merchant (findByShortId)
import "lib-dashboard" Storage.Queries.Person
  ( findAllByFromDateAndToDateAndMobileNumberAndStatusWithLimitOffset,
    findById,
    softDeletePerson,
    updatePersonVerifiedStatus,
  )
import qualified Storage.Queries.Role as QRole
import Tools.Auth.Api
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error
  ( GenericError (InvalidRequest),
    PersonError (PersonDoesNotExist),
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
  Environment.Flow [Common.PersonAPIEntity]
getAccountFetchUnverifiedAccounts _merchantShortId _opCity _apiTokenInfo mbFromDate mbToDate mbMobileNumber mbStatus mbLimit mbOffset = do
  encryptPersonLs <- findAllByFromDateAndToDateAndMobileNumberAndStatusWithLimitOffset mbFromDate mbToDate mbMobileNumber mbStatus mbLimit mbOffset
  traverse convertPersonToPersonAPIEntity encryptPersonLs
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

postAccountVerifyAccount ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Common.VerifyAccountReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postAccountVerifyAccount merchantShortId opCity apiTokenInfo req = do
  let personId = Kernel.Types.Id.cast req.fleetOwnerId
  case req.status of
    Common.Rejected ->
      softDeletePerson personId req.reason
        >> pure Kernel.Types.APISuccess.Success
    Common.Approved -> do
      person <- findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      case person.verified of
        Just True -> throwError (InvalidRequest "FleetOwner already exist!")
        _ -> do
          updatePersonVerifiedStatus personId True
          merchant <-
            findByShortId merchantShortId
              >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
          fleetOwnerRegisterReq <- createFleetOwnerRegisterReq merchant.id person
          checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
          transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
          SharedLogic.Transaction.withTransactionStoring transaction $
            API.Client.ProviderPlatform.Management.callManagementAPI
              checkedMerchantId
              opCity
              (.accountDSL.postAccountVerifyAccount)
              (Just $ Kernel.Types.Id.cast personId)
              (merchant.requireAdminApprovalForFleetOnboarding)
              fleetOwnerRegisterReq
  where
    createFleetOwnerRegisterReq merchantId DP.Person {..} = do
      mobileNumber' <- decrypt mobileNumber
      email' <- traverse decrypt email
      pure $
        Common.FleetOwnerRegisterReq
          { mobileNumber = mobileNumber',
            merchantId = merchantId.getId,
            email = email',
            city = opCity,
            fleetType = Nothing,
            panNumber = Nothing,
            gstNumber = Nothing,
            panImageId1 = Nothing,
            panImageId2 = Nothing,
            gstCertificateImage = Nothing,
            ..
          }
