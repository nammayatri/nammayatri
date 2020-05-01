module Beckn.App.Routes where

import qualified Beckn.Data.Accessor                  as Accessor
import qualified Beckn.Product.Blacklist              as Blacklist
import qualified Beckn.Product.Customer               as Customer
import qualified Beckn.Product.Document               as Document
import qualified Beckn.Product.HealthCheck            as HealthCheck
import qualified Beckn.Product.Organization           as Organization
import qualified Beckn.Product.Pass                   as Pass
import qualified Beckn.Product.PassApplication.Create as PassApplication
import qualified Beckn.Product.PassApplication.Fetch  as PassApplication
import qualified Beckn.Product.PassApplication.Update as PassApplication
import qualified Beckn.Product.Quota                  as Quota
import qualified Beckn.Product.Registration           as Registration
import qualified Beckn.Product.User                   as User
import qualified Beckn.Types.API.Blacklist            as Blacklist
import           Beckn.Types.API.Customer
import           Beckn.Types.API.Document
import           Beckn.Types.API.Organization
import           Beckn.Types.API.Pass
import           Beckn.Types.API.PassApplication
import qualified Beckn.Types.API.Quota                as Quota
import           Beckn.Types.API.Registration
import qualified Beckn.Types.API.User                 as User
import           Beckn.Types.App
import           Beckn.Types.Common
import           Data.Aeson
import qualified Data.Vault.Lazy                      as V
import           EulerHS.Prelude
import           Network.Wai.Parse
import           Servant
import           Servant.Multipart

import qualified Beckn.Types.Storage.Pass             as SP
import qualified Beckn.Types.Storage.PassApplication  as PA

epassContext :: Context '[ MultipartOptions Mem]
epassContext = defaultMultipartOptio (Proxy :: Proxy Mem) :. EmptyContext

-- 5 MB size each and max of 3 files
defaultMultipartOptio ::
  MultipartBackend tag => Proxy tag -> MultipartOptions tag
defaultMultipartOptio pTag =
  MultipartOptions
    { generalOptions =
        setMaxRequestNumFiles 3 $
        setMaxRequestFileSize (5 * 1024) defaultParseRequestBodyOptions
    , backendOptions = defaultBackendOptions pTag
    }

type EPassAPIs
   = "v1" :> (Get '[ JSON] Text :<|> RegistrationAPIs :<|> PassApplicationAPIs :<|> OrganizationAPIs :<|> CustomerAPIs :<|> PassAPIs :<|> UserAPIS :<|> QuotaAPIS :<|> BlacklistAPIS :<|> DocumentAPIs)

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
  :<|> quotaFlow
  :<|> blacklistFlow
  :<|> documentFlow

---- Registration Flow ------
type RegistrationAPIs
   = "token"
   :> ( ReqBody '[ JSON] InitiateLoginReq
        :> Post '[ JSON] InitiateLoginRes
      :<|> Capture "tokenId" Text
          :> "verify"
          :> ReqBody '[ JSON] LoginReq
          :> Post '[ JSON] LoginRes
      :<|> Capture "tokenId" Text
          :> "resend"
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
   :> Header "registrationToken" RegistrationTokenText
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
   :> Header "registrationToken" RegistrationTokenText
   :> ( ReqBody '[ JSON] CreateOrganizationReq :> Post '[ JSON] OrganizationRes
       :<|> "list"
            :> QueryParam "limit" Int
            :> QueryParam "offset" Int
            :> QueryParam "lat" Double
            :> QueryParam "long" Double
            :> QueryParam "ward" Text
            :> QueryParam "locationType" LocationType
            :> QueryParam "city" Text
            :> QueryParam "district" Text
            :> QueryParam "state" Text
            :> QueryParam "country" Text
            :> QueryParam "pincode" Int
            :> Get '[ JSON] ListOrganizationRes
       :<|> Capture "organizationId" Text :> Get '[ JSON] OrganizationRes
       :<|> Capture "organizationId" Text
            :> ReqBody '[ JSON] UpdateOrganizationReq
            :> Post '[ JSON] OrganizationRes
      )

organizationFlow registrationToken =
  Organization.createOrganization registrationToken
  :<|> Organization.listOrganization registrationToken
  :<|> Organization.getOrganization registrationToken
  :<|> Organization.updateOrganization registrationToken
---------------------------------

----- Customer Flow -------
type CustomerAPIs
  = "customer"
  :> Header "registrationToken" RegistrationTokenText
  :> Capture "customerId" Text
  :> Get '[ JSON] GetCustomerRes

customerFlow registrationToken =
  Customer.getCustomerInfo registrationToken
---------------------------

------ Pass Flow ---------
type PassAPIs
  = "pass" :> Header "registrationToken" RegistrationTokenText
  :> (Capture "passId" Text :> Get '[ JSON] PassRes
     :<|> Capture "passId" Text
          :> ReqBody '[ JSON] UpdatePassReq
          :> Post '[ JSON] PassRes
     :<|> "list"
          :> MandatoryQueryParam "identifierType" PassIDType
          :> MandatoryQueryParam "identifier" Text
          :> QueryParam "limit" Int
          :> QueryParam "offset" Int
          :> MandatoryQueryParam "type" PassType
          :> Get '[ JSON] ListPassRes
     )

passFlow registrationToken =
  Pass.getPassById registrationToken
  :<|> Pass.updatePass registrationToken
  :<|> Pass.listPass registrationToken

------ Quota Flow ----------
type QuotaAPIS
  = "quota"
  :> Header "registrationToken" RegistrationTokenText
  :> ( ReqBody '[JSON] Quota.CreateReq
        :> Post '[JSON] Quota.CreateRes
      :<|> Capture "quotaId" QuotaId
        :> ReqBody '[JSON] Quota.UpdateReq
        :> Put '[JSON] Quota.UpdateRes
      :<|> "list"
        :> QueryParam "limit" Int
        :> QueryParam "offset" Int
        :> MandatoryQueryParam "entityType" EntityType
        :> MandatoryQueryParam "entityId" Text
        :> Get '[JSON] Quota.ListRes
      :<|> Capture ":id" QuotaId
        :> Get '[JSON] Quota.GetRes
      )

quotaFlow registrationToken =
  Quota.create registrationToken
  :<|> Quota.update registrationToken
  :<|> Quota.list registrationToken
  :<|> Quota.get registrationToken

------ User Flow ----------
type UserAPIS
  = "user" :> Header "registrationToken" RegistrationTokenText
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
      :<|> Capture ":id" UserId
        :> Delete '[JSON] Ack
      )

userFlow registrationToken =
  User.create registrationToken
  :<|> User.update registrationToken
  :<|> User.list registrationToken
  :<|> User.get registrationToken
  :<|> User.delete registrationToken

------ Location Blacklist ----------
type BlacklistAPIS
  = "blacklist" :> Header "registrationToken" RegistrationTokenText
  :> (  ReqBody '[JSON] Blacklist.CreateReq
        :> Post '[JSON] Blacklist.CreateRes
      :<|> Capture "blacklist_id" BlacklistId
        :> ReqBody '[JSON] Blacklist.UpdateReq
        :> Put '[JSON] Blacklist.UpdateRes
      :<|> "list"
        :> QueryParam "limit" Int
        :> QueryParam "offset" Int
        :> MandatoryQueryParam "entityType" EntityType
        :> MandatoryQueryParam "entityId" Text
        :> Get '[JSON] Blacklist.ListRes
      :<|> Capture ":id" BlacklistId
        :> Get '[JSON] Blacklist.GetRes
      :<|> Capture "id" BlacklistId
        :> Delete '[JSON] Ack
      )

blacklistFlow registrationToken =
  Blacklist.create registrationToken
  :<|> Blacklist.update registrationToken
  :<|> Blacklist.list registrationToken
  :<|> Blacklist.get registrationToken
  :<|> Blacklist.delete registrationToken


--------
---- Document Api
type DocumentAPIs
   = "document"
   :> Header "registrationToken" RegistrationTokenText
   :> (    Capture "entityType" DocumentEntity
        :> Capture "entityId" Text
        :> "upload"
        :> MultipartForm Mem (MultipartData Mem)
        :> Post '[ JSON] UpdateDocumentRes
      )

documentFlow registrationToken enType enId =
  Document.upload registrationToken enId enType
