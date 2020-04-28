module Beckn.App.Routes where

import qualified Beckn.Data.Accessor                 as Accessor
import qualified Beckn.Product.Customer              as Customer
import qualified Beckn.Product.HealthCheck           as HealthCheck
import qualified Beckn.Product.LocationBlacklist     as LocationBlacklist
import qualified Beckn.Product.Organization          as Organization
import qualified Beckn.Product.Pass                  as Pass
import qualified Beckn.Product.PassApplication       as PassApplication
import qualified Beckn.Product.Quota                 as Quota
import qualified Beckn.Product.Registration          as Registration
import qualified Beckn.Product.User                  as User
import           Beckn.Types.API.Customer
import qualified Beckn.Types.API.LocationBlacklist   as LocationBlacklist
import           Beckn.Types.API.Organization
import           Beckn.Types.API.Pass
import           Beckn.Types.API.PassApplication
import qualified Beckn.Types.API.Quota               as Quota
import           Beckn.Types.API.Registration
import qualified Beckn.Types.API.User                as User
import           Beckn.Types.App
import           Beckn.Types.Common
import           Data.Aeson
import qualified Data.Vault.Lazy                     as V
import           EulerHS.Prelude
import           Servant

import qualified Beckn.Types.Storage.Pass            as SP
import qualified Beckn.Types.Storage.PassApplication as PA

type EPassAPIs
   = "v1" :> (Get '[ JSON] Text :<|> RegistrationAPIs :<|> PassApplicationAPIs :<|> OrganizationAPIs :<|> CustomerAPIs :<|> PassAPIs :<|> UserAPIS :<|> LocationBlacklistAPIS)

epassAPIs :: Proxy EPassAPIs
epassAPIs = Proxy

epassServer' :: V.Key (HashMap Text Text) -> FlowServer EPassAPIs
epassServer' key =
  HealthCheck.healthCheckApp
  :<|> registrationFlow
  :<|> passApplicationFlow
  :<|> organizationFlow
  :<|> customerFlow
  :<|> passFlow
  :<|> userFlow
  :<|> locationBlacklistFlow

---- Registration Flow ------
type RegistrationAPIs
   = "token"
   :> ( ReqBody '[ JSON] InitiateLoginReq
        :> Post '[ JSON] InitiateLoginRes
      :<|> Capture "tokenId" Text
          :> ReqBody '[ JSON] LoginReq
          :> Post '[ JSON] LoginRes
      :<|> Capture "tokenId" Text
          :> ReqBody '[ JSON] ReInitiateLoginReq
          :> Post '[ JSON] InitiateLoginRes
      )

registrationFlow :: FlowServer RegistrationAPIs
registrationFlow =
  Registration.initiateLogin
  :<|> Registration.login
  :<|> Registration.reInitiateLogin
-------------------------------

---- Pass Application Flow ------
--
type PassApplicationAPIs
   = "passApplication"
   :> Header "registrationToken" Text
   :> ( ReqBody '[ JSON] CreatePassApplicationReq
        :> Post '[ JSON] PassApplicationRes
      :<|> "list"
        :> QueryParam "limit" Int
        :> QueryParam "offset" Int
        :> QueryParams "status" PA.Status
        :> QueryParams "type" PassType
        :> Get '[ JSON] ListPassApplicationRes
      :<|> Capture "passApplicationId" PassApplicationId :> Get '[ JSON] PassApplicationRes
      :<|> Capture "passApplicationId" PassApplicationId
           :> ReqBody '[ JSON] UpdatePassApplicationReq
           :> Post '[ JSON] PassApplicationRes
      )

passApplicationFlow registrationToken =
  PassApplication.createPassApplication registrationToken
  :<|> PassApplication.listPassApplication registrationToken
  :<|> PassApplication.getPassApplicationById registrationToken
  :<|> PassApplication.updatePassApplication registrationToken

----- Organization Flow -------
--
type OrganizationAPIs
   = "organization"
   :> Header "registrationToken" Text
   :> ( ReqBody '[ JSON] CreateOrganizationReq :> Post '[ JSON] OrganizationRes
       :<|> Capture "organizationId" Text :> Get '[ JSON] OrganizationRes
       :<|> QueryParam "limit" Int
            :> QueryParam "offset" Int
            :> QueryParam "type" Text
            :> Get '[ JSON] ListOrganizationRes
       :<|> Capture "organizationId" Text
            :> ReqBody '[ JSON] UpdateOrganizationReq
            :> Post '[ JSON] OrganizationRes
      )

organizationFlow registrationToken =
  Organization.createOrganization registrationToken
  :<|> Organization.getOrganization registrationToken
  :<|> Organization.listOrganization registrationToken
  :<|> Organization.updateOrganization registrationToken
---------------------------------

----- Customer Flow -------
type CustomerAPIs
  = "customer"
  :> Header "registrationToken" Text
  :> Capture "customerId" Text
  :> Get '[ JSON] GetCustomerRes

customerFlow registrationToken =
  Customer.getCustomerInfo registrationToken
---------------------------

------ Pass Flow ---------
type PassAPIs
  = "pass" :> Header "registrationToken" Text
  :> (Capture "passId" Text :> Get '[ JSON] PassRes
     :<|> Capture "passId" Text
          :> ReqBody '[ JSON] UpdatePassReq
          :> Post '[ JSON] PassRes
     :<|> "list"
          :> QueryParam "identifierType" PassIDType
          :> QueryParam "identifier" Text
          :> QueryParam "limit" Int
          :> QueryParam "offset" Int
          :> QueryParams "status" SP.Status
          :> QueryParams "type" PassType
          :> Post '[ JSON] ListPassRes
     )

passFlow registrationToken =
  Pass.getPassById registrationToken
  :<|> Pass.updatePass registrationToken
  :<|> Pass.listPass registrationToken

------ Quota Flow ----------
type QuotaAPIS
  = "quota"
  :> Header "registrationToken" RegistrationToken
  :> ( ReqBody '[JSON] Quota.CreateReq
        :> Post '[JSON] Quota.CreateRes
      :<|> Capture "quotaId" QuotaId
        :> ReqBody '[JSON] Quota.UpdateReq
        :> Post '[JSON] Quota.UpdateRes
      :<|> "list"
        :> QueryParam "type" Text
        :> QueryParam "limit" Int
        :> QueryParam "offset" Int
        :> Get '[JSON] Quota.ListRes
      )

quotaFlow registrationToken =
  Quota.create registrationToken
  :<|> Quota.update registrationToken
  :<|> Quota.list registrationToken

------ User Flow ----------
type UserAPIS
  = "user" :> Header "registrationToken" RegistrationToken
  :> (  ReqBody '[JSON] User.CreateReq
        :> Post '[JSON] User.CreateRes
      :<|> Capture "userId" UserId
        :> ReqBody '[JSON] User.UpdateReq
        :> Put '[JSON] User.UpdateRes
      :<|> "list"
        :> QueryParam "limit" Int
        :> QueryParam "offset" Int
        :> Get '[JSON] User.ListRes
      :<|> Capture ":id" UserId
        :> Get '[JSON] User.GetRes
      )

userFlow registrationToken =
  User.create registrationToken
  :<|> User.update registrationToken
  :<|> User.list registrationToken
  :<|> User.get registrationToken


------ Location Blacklist ----------
type LocationBlacklistAPIS
  = "location_blacklist" :> Header "registrationToken" RegistrationToken
  :> (  ReqBody '[JSON] LocationBlacklist.CreateReq
        :> Post '[JSON] LocationBlacklist.CreateRes
      :<|> Capture "location_blacklist_id" LocationBlacklistId
        :> ReqBody '[JSON] LocationBlacklist.UpdateReq
        :> Put '[JSON] LocationBlacklist.UpdateRes
      :<|> "list"
        :> QueryParam "ward" Text
        :> QueryParam "district" Text
        :> QueryParam "city" Text
        :> QueryParam "state" Text
        :> QueryParam "pincode" Int
        :> QueryParam "limit" Int
        :> QueryParam "offset" Int
        :> Get '[JSON] LocationBlacklist.ListRes
      :<|> Capture ":id" LocationBlacklistId
        :> Get '[JSON] LocationBlacklist.GetRes
      )

locationBlacklistFlow registrationToken =
  LocationBlacklist.create registrationToken
  :<|> LocationBlacklist.update registrationToken
  :<|> LocationBlacklist.list registrationToken
  :<|> LocationBlacklist.get registrationToken
