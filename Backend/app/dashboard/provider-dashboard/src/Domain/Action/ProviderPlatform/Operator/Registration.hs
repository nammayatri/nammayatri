module Domain.Action.ProviderPlatform.Operator.Registration (postOperatorRegister, postRegistrationDashboardRegister) where

import qualified API.Client.ProviderPlatform.Operator as Client
import qualified API.Types.ProviderPlatform.Operator.Registration as Common
import qualified Domain.Action.Dashboard.Person as DP
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Person as PT
import qualified Domain.Types.Person.Type as DP
import qualified Domain.Types.Role as DRole
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified SharedLogic.Transaction as ST
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified "lib-dashboard" Storage.Queries.Person as QP
import qualified Storage.Queries.Role as QRole
import Tools.Auth.Api
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error

postOperatorRegister ::
  ShortId DM.Merchant ->
  Context.City ->
  ApiTokenInfo ->
  Common.OperatorRegisterReq ->
  Flow APISuccess
postOperatorRegister merchantShortId opCity apiTokenInfo req = do
  runRequestValidation validateOperator req
  unlessM (null <$> QP.findByEmailOrMobile req.email req.mobileNumber req.mobileCountryCode) $ throwError (InvalidRequest "Phone or Email already registered")
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  void $ merchantServerAccessCheck merchant
  transaction <- ST.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing (Just req)
  res <- ST.withTransactionStoring transaction do
    Client.callOperatorAPI checkedMerchantId opCity (.registrationDSL.postOperatorRegister) req
  registerOperator opCity req.email req.mobileNumber req.mobileCountryCode req.firstName req.lastName Nothing res.personId merchant Nothing
  pure Success

registerOperator ::
  Context.City ->
  Maybe Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  Id Common.Operator ->
  DM.Merchant ->
  Maybe Text ->
  Flow ()
registerOperator opCity email mobileNumber mobileCountryCode firstName lastName password operatorId merchant mbRoleId = do
  operatorRole <-
    case mbRoleId of
      Just roleId -> QRole.findById (Id roleId) >>= fromMaybeM (RoleNotFound roleId)
      Nothing -> QRole.findByDashboardAccessType DRole.DASHBOARD_OPERATOR >>= fromMaybeM (RoleNotFound "OPERATOR")
  operator <- buildOperator email mobileNumber mobileCountryCode firstName lastName password operatorId operatorRole
  merchantAccess <- DP.buildMerchantAccess operator.id merchant.id merchant.shortId opCity
  QP.create operator
  QAccess.create merchantAccess

buildOperator :: (EncFlow m r) => Maybe Text -> Text -> Text -> Text -> Text -> Maybe Text -> Id Common.Operator -> DRole.Role -> m DP.Person
buildOperator emailUnencrypted mobileNumberUnencrypted mobileCountryCode firstName lastName password operatorId role = do
  now <- getCurrentTime
  mobileNumber <- encrypt mobileNumberUnencrypted
  email <- forM emailUnencrypted encrypt
  passwordHash <- getDbHash `mapM` password
  return
    DP.Person
      { id = cast @Common.Operator @DP.Person operatorId,
        firstName = firstName,
        lastName = lastName,
        roleId = role.id,
        email = email,
        mobileNumber,
        mobileCountryCode = mobileCountryCode,
        passwordHash,
        dashboardAccessType = Just role.dashboardAccessType,
        receiveNotification = Nothing,
        createdAt = now,
        updatedAt = now,
        verified = Just True,
        rejectionReason = Nothing,
        rejectedAt = Nothing,
        dashboardType = PT.DEFAULT_DASHBOARD,
        passwordUpdatedAt = Nothing,
        approvedBy = Nothing,
        rejectedBy = Nothing
      }

validateOperator :: Validate Common.OperatorRegisterReq
validateOperator Common.OperatorRegisterReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ MinLength 1 `And` MaxLength 50 `And` P.name,
      validateField "lastName" lastName $ (MaxLength 50 `And` P.name),
      validateField "mobileNumber" mobileNumber P.indianMobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileIndianCode,
      validateField "email" email $ InMaybe P.email
    ]

postRegistrationDashboardRegister :: (ShortId DM.Merchant -> Context.City -> ApiTokenInfo -> Common.CreateDashboardOperatorReq -> Flow APISuccess)
postRegistrationDashboardRegister merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  unlessM (null <$> QP.findByEmailOrMobile (Just req.email) req.mobileNumber req.mobileCountryCode) $ throwError (InvalidRequest "Phone or Email already registered")
  void $ merchantServerAccessCheck merchant
  res <- Client.callOperatorAPI checkedMerchantId opCity (.registrationDSL.postRegistrationDashboardRegister) req
  registerOperator opCity (Just req.email) req.mobileNumber req.mobileCountryCode req.firstName req.lastName (Just req.password) res.personId merchant (Just req.roleId)
  pure Success
