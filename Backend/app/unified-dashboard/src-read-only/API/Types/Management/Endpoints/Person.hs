{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Management.Endpoints.Person where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import qualified Domain.Types.AccessMatrix
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Role
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Verification.SafetyPortal.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data AccessMatrixItemAPIEntity = AccessMatrixItemAPIEntity
  { additionalUserActions :: Kernel.Prelude.Maybe Data.Text.Text,
    serverName :: Kernel.Prelude.Maybe Domain.Types.AccessMatrix.ServerName,
    userActionType :: Domain.Types.AccessMatrix.UserActionType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AccessMatrixRowAPIEntity = AccessMatrixRowAPIEntity {accessMatrixRow :: [AccessMatrixItemAPIEntity], role :: Domain.Types.Role.RoleAPIEntity}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChangeEmailByAdminReq = ChangeEmailByAdminReq {newEmail :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChangeEnabledStatusReq = ChangeEnabledStatusReq {enabled :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChangeMobileNumberByAdminReq = ChangeMobileNumberByAdminReq {newMobileNumber :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChangePasswordAfterExpiryReq = ChangePasswordAfterExpiryReq {email :: Data.Text.Text, newPassword :: Data.Text.Text, oldPassword :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChangePasswordByAdminReq = ChangePasswordByAdminReq {newPassword :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChangePasswordReq = ChangePasswordReq {newPassword :: Data.Text.Text, oldPassword :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ListPersonResp = ListPersonResp {list :: [PersonAPIEntity], summary :: Kernel.External.Verification.SafetyPortal.Types.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MerchantAccessResp = MerchantAccessResp {merchantId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant, operatingCity :: Kernel.Types.Beckn.Context.City}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MerchantCityAccessReq = MerchantCityAccessReq {merchantId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant, operatingCity :: Kernel.Types.Beckn.Context.City}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PersonAPIEntity = PersonAPIEntity
  { availableCitiesForMerchant :: Kernel.Prelude.Maybe [Domain.Types.Merchant.AvailableCitiesForMerchant],
    availableMerchants :: [Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant],
    email :: Kernel.Prelude.Maybe Data.Text.Text,
    firstName :: Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    lastName :: Data.Text.Text,
    mobileCountryCode :: Data.Text.Text,
    mobileNumber :: Data.Text.Text,
    registeredAt :: Kernel.Prelude.UTCTime,
    role :: Domain.Types.Role.RoleAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = (GetPersonList :<|> PostPersonAssignRole :<|> PostPersonAssignMerchantCityAccess :<|> PostPersonResetMerchantAccess :<|> PostPersonResetMerchantCityAccess :<|> DeletePerson :<|> PostPersonChangeEnabledStatus :<|> PostPersonChangeEmailByAdmin :<|> PostPersonChangePasswordByAdmin :<|> PostPersonChangeMobileByAdmin :<|> GetUserProfile :<|> GetUserCurrentMerchant :<|> PostUserChangePassword :<|> GetUserAccessMatrix :<|> PostUserChangePasswordAfterExpiry)

type GetPersonList =
  ( "person" :> "list" :> QueryParam "searchString" Data.Text.Text :> QueryParam "limit" Kernel.Prelude.Integer :> QueryParam "offset" Kernel.Prelude.Integer
      :> QueryParam
           "personId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> Get '[JSON] ListPersonResp
  )

type PostPersonAssignRole =
  ( "person" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "assignRole"
      :> Capture
           "roleId"
           (Kernel.Types.Id.Id Domain.Types.Role.Role)
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostPersonAssignMerchantCityAccess =
  ( "person" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "assignMerchantCityAccess"
      :> ReqBody
           '[JSON]
           MerchantCityAccessReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostPersonResetMerchantAccess =
  ( "person" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "resetMerchantAccess"
      :> ReqBody
           '[JSON]
           MerchantCityAccessReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostPersonResetMerchantCityAccess =
  ( "person" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "resetMerchantCityAccess"
      :> ReqBody
           '[JSON]
           MerchantCityAccessReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type DeletePerson = ("person" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostPersonChangeEnabledStatus =
  ( "person" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "changeEnabledStatus"
      :> ReqBody
           '[JSON]
           ChangeEnabledStatusReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostPersonChangeEmailByAdmin =
  ( "person" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "change" :> "email" :> ReqBody '[JSON] ChangeEmailByAdminReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostPersonChangePasswordByAdmin =
  ( "person" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "change" :> "password"
      :> ReqBody
           '[JSON]
           ChangePasswordByAdminReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostPersonChangeMobileByAdmin =
  ( "person" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "change" :> "mobile"
      :> ReqBody
           '[JSON]
           ChangeMobileNumberByAdminReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetUserProfile = ("user" :> "profile" :> Get '[JSON] PersonAPIEntity)

type GetUserCurrentMerchant = ("user" :> "getCurrentMerchant" :> Get '[JSON] MerchantAccessResp)

type PostUserChangePassword = ("user" :> "changePassword" :> ReqBody '[JSON] ChangePasswordReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetUserAccessMatrix = ("user" :> "getAccessMatrix" :> Get '[JSON] AccessMatrixRowAPIEntity)

type PostUserChangePasswordAfterExpiry = ("user" :> "changePasswordAfterExpiry" :> ReqBody '[JSON] ChangePasswordAfterExpiryReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data PersonAPIs = PersonAPIs
  { getPersonList :: Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> EulerHS.Types.EulerClient ListPersonResp,
    postPersonAssignRole :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Role.Role -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPersonAssignMerchantCityAccess :: Kernel.Types.Id.Id Domain.Types.Person.Person -> MerchantCityAccessReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPersonResetMerchantAccess :: Kernel.Types.Id.Id Domain.Types.Person.Person -> MerchantCityAccessReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPersonResetMerchantCityAccess :: Kernel.Types.Id.Id Domain.Types.Person.Person -> MerchantCityAccessReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deletePerson :: Kernel.Types.Id.Id Domain.Types.Person.Person -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPersonChangeEnabledStatus :: Kernel.Types.Id.Id Domain.Types.Person.Person -> ChangeEnabledStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPersonChangeEmailByAdmin :: Kernel.Types.Id.Id Domain.Types.Person.Person -> ChangeEmailByAdminReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPersonChangePasswordByAdmin :: Kernel.Types.Id.Id Domain.Types.Person.Person -> ChangePasswordByAdminReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPersonChangeMobileByAdmin :: Kernel.Types.Id.Id Domain.Types.Person.Person -> ChangeMobileNumberByAdminReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getUserProfile :: EulerHS.Types.EulerClient PersonAPIEntity,
    getUserCurrentMerchant :: EulerHS.Types.EulerClient MerchantAccessResp,
    postUserChangePassword :: ChangePasswordReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getUserAccessMatrix :: EulerHS.Types.EulerClient AccessMatrixRowAPIEntity,
    postUserChangePasswordAfterExpiry :: ChangePasswordAfterExpiryReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkPersonAPIs :: (Client EulerHS.Types.EulerClient API -> PersonAPIs)
mkPersonAPIs personClient = (PersonAPIs {..})
  where
    getPersonList :<|> postPersonAssignRole :<|> postPersonAssignMerchantCityAccess :<|> postPersonResetMerchantAccess :<|> postPersonResetMerchantCityAccess :<|> deletePerson :<|> postPersonChangeEnabledStatus :<|> postPersonChangeEmailByAdmin :<|> postPersonChangePasswordByAdmin :<|> postPersonChangeMobileByAdmin :<|> getUserProfile :<|> getUserCurrentMerchant :<|> postUserChangePassword :<|> getUserAccessMatrix :<|> postUserChangePasswordAfterExpiry = personClient

data PersonUserActionType
  = GET_PERSON_LIST
  | POST_PERSON_ASSIGN_ROLE
  | POST_PERSON_ASSIGN_MERCHANT_CITY_ACCESS
  | POST_PERSON_RESET_MERCHANT_ACCESS
  | POST_PERSON_RESET_MERCHANT_CITY_ACCESS
  | DELETE_PERSON
  | POST_PERSON_CHANGE_ENABLED_STATUS
  | POST_PERSON_CHANGE_EMAIL_BY_ADMIN
  | POST_PERSON_CHANGE_PASSWORD_BY_ADMIN
  | POST_PERSON_CHANGE_MOBILE_BY_ADMIN
  | GET_USER_PROFILE
  | GET_USER_CURRENT_MERCHANT
  | POST_USER_CHANGE_PASSWORD
  | GET_USER_ACCESS_MATRIX
  | POST_USER_CHANGE_PASSWORD_AFTER_EXPIRY
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PersonUserActionType])
