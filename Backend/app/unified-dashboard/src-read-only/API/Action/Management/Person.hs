{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Management.Person
  ( API,
    handler,
  )
where

import qualified "this" API.Types.Management
import qualified "this" API.Types.Management.Person
import qualified Data.Text
import qualified Domain.Action.Management.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Role
import qualified Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = (GetPersonList :<|> PostPersonAssignRole :<|> PostPersonAssignMerchantCityAccess :<|> PostPersonResetMerchantAccess :<|> PostPersonResetMerchantCityAccess :<|> DeletePerson :<|> PostPersonChangeEnabledStatus :<|> PostPersonChangeEmailByAdmin :<|> PostPersonChangePasswordByAdmin :<|> PostPersonChangeMobileByAdmin :<|> GetUserProfile :<|> GetUserCurrentMerchant :<|> PostUserChangePassword :<|> GetUserAccessMatrix :<|> PostUserChangePasswordAfterExpiry)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPersonList merchantId city :<|> postPersonAssignRole merchantId city :<|> postPersonAssignMerchantCityAccess merchantId city :<|> postPersonResetMerchantAccess merchantId city :<|> postPersonResetMerchantCityAccess merchantId city :<|> deletePerson merchantId city :<|> postPersonChangeEnabledStatus merchantId city :<|> postPersonChangeEmailByAdmin merchantId city :<|> postPersonChangePasswordByAdmin merchantId city :<|> postPersonChangeMobileByAdmin merchantId city :<|> getUserProfile merchantId city :<|> getUserCurrentMerchant merchantId city :<|> postUserChangePassword merchantId city :<|> getUserAccessMatrix merchantId city :<|> postUserChangePasswordAfterExpiry merchantId city

type GetPersonList = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('GET_MANAGEMENT_PERSON_LIST) :> API.Types.Management.Person.GetPersonList)

type PostPersonAssignRole = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('POST_MANAGEMENT_PERSON_ASSIGN_ROLE) :> API.Types.Management.Person.PostPersonAssignRole)

type PostPersonAssignMerchantCityAccess =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('POST_MANAGEMENT_PERSON_ASSIGN_MERCHANT_CITY_ACCESS)
      :> API.Types.Management.Person.PostPersonAssignMerchantCityAccess
  )

type PostPersonResetMerchantAccess = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('POST_MANAGEMENT_PERSON_RESET_MERCHANT_ACCESS) :> API.Types.Management.Person.PostPersonResetMerchantAccess)

type PostPersonResetMerchantCityAccess = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('POST_MANAGEMENT_PERSON_RESET_MERCHANT_CITY_ACCESS) :> API.Types.Management.Person.PostPersonResetMerchantCityAccess)

type DeletePerson = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('DELETE_MANAGEMENT_PERSON) :> API.Types.Management.Person.DeletePerson)

type PostPersonChangeEnabledStatus = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('POST_MANAGEMENT_PERSON_CHANGE_ENABLED_STATUS) :> API.Types.Management.Person.PostPersonChangeEnabledStatus)

type PostPersonChangeEmailByAdmin = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('POST_MANAGEMENT_PERSON_CHANGE_EMAIL_BY_ADMIN) :> API.Types.Management.Person.PostPersonChangeEmailByAdmin)

type PostPersonChangePasswordByAdmin = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('POST_MANAGEMENT_PERSON_CHANGE_PASSWORD_BY_ADMIN) :> API.Types.Management.Person.PostPersonChangePasswordByAdmin)

type PostPersonChangeMobileByAdmin = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('POST_MANAGEMENT_PERSON_CHANGE_MOBILE_BY_ADMIN) :> API.Types.Management.Person.PostPersonChangeMobileByAdmin)

type GetUserProfile = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('GET_MANAGEMENT_USER_PROFILE) :> API.Types.Management.Person.GetUserProfile)

type GetUserCurrentMerchant = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('GET_MANAGEMENT_USER_CURRENT_MERCHANT) :> API.Types.Management.Person.GetUserCurrentMerchant)

type PostUserChangePassword = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('POST_MANAGEMENT_USER_CHANGE_PASSWORD) :> API.Types.Management.Person.PostUserChangePassword)

type GetUserAccessMatrix = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('GET_MANAGEMENT_USER_ACCESS_MATRIX) :> API.Types.Management.Person.GetUserAccessMatrix)

type PostUserChangePasswordAfterExpiry = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('POST_MANAGEMENT_USER_CHANGE_PASSWORD_AFTER_EXPIRY) :> API.Types.Management.Person.PostUserChangePasswordAfterExpiry)

getPersonList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Environment.FlowHandler API.Types.Management.Person.ListPersonResp)
getPersonList merchantShortId opCity apiTokenInfo searchString limit offset personId = withFlowHandlerAPI' $ Domain.Action.Management.Person.getPersonList merchantShortId opCity apiTokenInfo searchString limit offset personId

postPersonAssignRole :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Role.Role -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPersonAssignRole merchantShortId opCity apiTokenInfo personId roleId = withFlowHandlerAPI' $ Domain.Action.Management.Person.postPersonAssignRole merchantShortId opCity apiTokenInfo personId roleId

postPersonAssignMerchantCityAccess :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Management.Person.MerchantCityAccessReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPersonAssignMerchantCityAccess merchantShortId opCity apiTokenInfo personId req = withFlowHandlerAPI' $ Domain.Action.Management.Person.postPersonAssignMerchantCityAccess merchantShortId opCity apiTokenInfo personId req

postPersonResetMerchantAccess :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Management.Person.MerchantCityAccessReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPersonResetMerchantAccess merchantShortId opCity apiTokenInfo personId req = withFlowHandlerAPI' $ Domain.Action.Management.Person.postPersonResetMerchantAccess merchantShortId opCity apiTokenInfo personId req

postPersonResetMerchantCityAccess :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Management.Person.MerchantCityAccessReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPersonResetMerchantCityAccess merchantShortId opCity apiTokenInfo personId req = withFlowHandlerAPI' $ Domain.Action.Management.Person.postPersonResetMerchantCityAccess merchantShortId opCity apiTokenInfo personId req

deletePerson :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deletePerson merchantShortId opCity apiTokenInfo personId = withFlowHandlerAPI' $ Domain.Action.Management.Person.deletePerson merchantShortId opCity apiTokenInfo personId

postPersonChangeEnabledStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Management.Person.ChangeEnabledStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPersonChangeEnabledStatus merchantShortId opCity apiTokenInfo personId req = withFlowHandlerAPI' $ Domain.Action.Management.Person.postPersonChangeEnabledStatus merchantShortId opCity apiTokenInfo personId req

postPersonChangeEmailByAdmin :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Management.Person.ChangeEmailByAdminReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPersonChangeEmailByAdmin merchantShortId opCity apiTokenInfo personId req = withFlowHandlerAPI' $ Domain.Action.Management.Person.postPersonChangeEmailByAdmin merchantShortId opCity apiTokenInfo personId req

postPersonChangePasswordByAdmin :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Management.Person.ChangePasswordByAdminReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPersonChangePasswordByAdmin merchantShortId opCity apiTokenInfo personId req = withFlowHandlerAPI' $ Domain.Action.Management.Person.postPersonChangePasswordByAdmin merchantShortId opCity apiTokenInfo personId req

postPersonChangeMobileByAdmin :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Management.Person.ChangeMobileNumberByAdminReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPersonChangeMobileByAdmin merchantShortId opCity apiTokenInfo personId req = withFlowHandlerAPI' $ Domain.Action.Management.Person.postPersonChangeMobileByAdmin merchantShortId opCity apiTokenInfo personId req

getUserProfile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler API.Types.Management.Person.PersonAPIEntity)
getUserProfile merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.Management.Person.getUserProfile merchantShortId opCity apiTokenInfo

getUserCurrentMerchant :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler API.Types.Management.Person.MerchantAccessResp)
getUserCurrentMerchant merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.Management.Person.getUserCurrentMerchant merchantShortId opCity apiTokenInfo

postUserChangePassword :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Management.Person.ChangePasswordReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postUserChangePassword merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.Management.Person.postUserChangePassword merchantShortId opCity apiTokenInfo req

getUserAccessMatrix :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler API.Types.Management.Person.AccessMatrixRowAPIEntity)
getUserAccessMatrix merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.Management.Person.getUserAccessMatrix merchantShortId opCity apiTokenInfo

postUserChangePasswordAfterExpiry :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Management.Person.ChangePasswordAfterExpiryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postUserChangePasswordAfterExpiry merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.Management.Person.postUserChangePasswordAfterExpiry merchantShortId opCity apiTokenInfo req
