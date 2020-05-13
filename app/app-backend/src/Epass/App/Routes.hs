module Epass.App.Routes where

import qualified Beckn.Types.Storage.Person as Person
import Data.Aeson
import qualified Data.Vault.Lazy as V
import qualified Epass.Data.Accessor as Accessor
import qualified Epass.Product.Blacklist as Blacklist
import qualified Epass.Product.Comment as Comment
import qualified Epass.Product.Customer as Customer
import qualified Epass.Product.Document as Document
import qualified Epass.Product.HealthCheck as HealthCheck
import qualified Epass.Product.Location.CRUD as Location
import qualified Epass.Product.Organization as Organization
import qualified Epass.Product.Pass as Pass
import qualified Epass.Product.PassApplication.Create as PassApplication
import qualified Epass.Product.PassApplication.Fetch as PassApplication
import qualified Epass.Product.PassApplication.Update as PassApplication
import qualified Epass.Product.Quota as Quota
import qualified Epass.Product.Registration as Registration
import qualified Epass.Product.Tag as Tag
import qualified Epass.Product.User.CRUD as User
import qualified Epass.Types.API.Blacklist as Blacklist
import qualified Epass.Types.API.Comment as Comment
import Epass.Types.API.Customer
import Epass.Types.API.Document
import Epass.Types.API.Location.CRUD
import Epass.Types.API.Organization
import Epass.Types.API.Pass
import Epass.Types.API.PassApplication
import qualified Epass.Types.API.Quota as Quota
import Epass.Types.API.Registration
import qualified Epass.Types.API.Tag as Tag
import qualified Epass.Types.API.User as User
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.Organization as SO
import qualified Epass.Types.Storage.Pass as SP
import qualified Epass.Types.Storage.Pass as SP
import qualified Epass.Types.Storage.PassApplication as PA
import qualified Epass.Types.Storage.PassApplication as PA
import EulerHS.Prelude
import Network.Wai.Parse
import Servant
import Servant.Multipart

epassContext :: Context '[MultipartOptions Mem]
epassContext = epassMultipartOptions (Proxy :: Proxy Mem) :. EmptyContext

-- 5 MB size each and max of 3 files
epassMultipartOptions ::
  MultipartBackend tag => Proxy tag -> MultipartOptions tag
epassMultipartOptions pTag =
  MultipartOptions
    { generalOptions =
        setMaxRequestNumFiles 3 $
          setMaxRequestFileSize (5 * 1024) defaultParseRequestBodyOptions,
      backendOptions = defaultBackendOptions pTag
    }

type EPassAPIs =
  "epass"
    :> ( Get '[JSON] Text
           :<|> RegistrationAPIs
           :<|> PassApplicationAPIs
           :<|> OrganizationAPIs
           :<|> CustomerAPIs
           :<|> PassAPIs
           :<|> UserAPIS
           :<|> QuotaAPIS
           :<|> BlacklistAPIS
           :<|> DocumentAPIs
           :<|> TagAPIs
           :<|> CommentAPIs
           :<|> LocationAPIs
       )

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
    :<|> tagFlow
    :<|> commentFlow
    :<|> locationFlow

---- Registration Flow ------
type RegistrationAPIs =
  "token"
    :> ( ReqBody '[JSON] InitiateLoginReq
           :> Post '[JSON] InitiateLoginRes
           :<|> Capture "tokenId" Text
             :> "verify"
             :> ReqBody '[JSON] LoginReq
             :> Post '[JSON] LoginRes
           :<|> Capture "tokenId" Text
             :> "resend"
             :> ReqBody '[JSON] ReInitiateLoginReq
             :> Post '[JSON] InitiateLoginRes
       )

registrationFlow :: FlowServer RegistrationAPIs
registrationFlow =
  Registration.initiateLogin
    :<|> Registration.login
    :<|> Registration.reInitiateLogin

---------------------------------
---- Pass Application Flow ------
type PassApplicationAPIs =
  "pass_application"
    :> Header "registrationToken" RegistrationTokenText
    :> ( ReqBody '[JSON] CreatePassApplicationReq
           :> Post '[JSON] PassApplicationRes'
           :<|> "list"
             :> QueryParam "limit" Int
             :> QueryParam "offset" Int
             :> QueryParams "from_pincode" Int
             :> QueryParams "from_city" Text
             :> QueryParams "from_district" Text
             :> QueryParams "from_ward" Text
             :> QueryParams "from_state" Text
             :> QueryParams "to_pincode" Int
             :> QueryParams "to_city" Text
             :> QueryParams "to_district" Text
             :> QueryParams "to_ward" Text
             :> QueryParams "to_state" Text
             :> QueryParams "status" PA.Status
             :> QueryParams "organization" OrganizationId
             :> QueryParams "type" PassType
             :> Get '[JSON] ListPassApplicationRes
           :<|> Capture "passApplicationId" PassApplicationId
             :> Get '[JSON] GetPassApplication
           :<|> Capture "caseId" CaseId
             :> ReqBody '[JSON] UpdatePassApplicationReq
             :> Post '[JSON] PassApplicationRes'
       )

passApplicationFlow registrationToken =
  PassApplication.createPassApplication registrationToken
    :<|> PassApplication.listPassApplication registrationToken
    :<|> PassApplication.getPassApplicationById registrationToken
    :<|> PassApplication.updatePassApplication registrationToken

----- Organization Flow -------
--
type OrganizationAPIs =
  "organization"
    :> Header "registrationToken" RegistrationTokenText
    :> ( ReqBody '[JSON] CreateOrganizationReq
           :> Post '[JSON] OrganizationRes
           :<|> "list"
             :> QueryParam "limit" Int
             :> QueryParam "offset" Int
             :> QueryParams "locationType" LocationType
             :> QueryParams "pincode" Int
             :> QueryParams "city" Text
             :> QueryParams "district" Text
             :> QueryParams "ward" Text
             :> QueryParams "state" Text
             :> QueryParams "status" SO.Status
             :> QueryParam "verified" Bool
             :> Get '[JSON] ListOrganizationRes
           :<|> Capture "organizationId" Text :> Get '[JSON] GetOrganizationRes
           :<|> Capture "organizationId" Text
             :> ReqBody '[JSON] UpdateOrganizationReq
             :> Post '[JSON] OrganizationRes
       )

organizationFlow registrationToken =
  Organization.createOrganization registrationToken
    :<|> Organization.listOrganization registrationToken
    :<|> Organization.getOrganization registrationToken
    :<|> Organization.updateOrganization registrationToken

---------------------------------
----- Customer Flow -------
type CustomerAPIs =
  "customer"
    :> Header "registrationToken" RegistrationTokenText
    :> Capture "customerId" Text
    :> Get '[JSON] GetCustomerRes

customerFlow registrationToken =
  Customer.getCustomerInfo registrationToken

---------------------------
------ Pass Flow ---------

-- | Please be cautious while changing the order of the routes
-- | The /pass/list?.. was getting overriden by /pass/:passId
type PassAPIs =
  "pass"
    :> Header "registrationToken" RegistrationTokenText
    :> ( "list"
           :> MandatoryQueryParam "identifierType" PassIDType
           :> MandatoryQueryParam "identifier" Text
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> MandatoryQueryParam "type" PassType
           :> Get '[JSON] ListPassRes
           :<|> Capture "passId" Text
           :> Get '[JSON] PassRes
           :<|> Capture "passId" Text
           :> ReqBody '[JSON] UpdatePassReq
           :> Post '[JSON] PassRes
       )

passFlow registrationToken =
  Pass.listPass registrationToken
    :<|> Pass.getPassById registrationToken
    :<|> Pass.updatePass registrationToken

------ Quota Flow ----------
type QuotaAPIS =
  "quota"
    :> Header "registrationToken" RegistrationTokenText
    :> ( ReqBody '[JSON] Quota.CreateReq
           :> Post '[JSON] Quota.CreateRes
           :<|> Capture "quotaId" QuotaId
             :> ReqBody '[JSON] Quota.UpdateReq
             :> Post '[JSON] Quota.UpdateRes
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
type UserAPIS =
  "user" :> Header "registrationToken" RegistrationTokenText
    :> ( ReqBody '[JSON] User.CreateReq
           :> Post '[JSON] User.CreateRes
           :<|> Capture "userId" PersonId
             :> ReqBody '[JSON] User.UpdateReq
             :> Post '[JSON] User.UpdateRes
           :<|> "list"
             :> QueryParam "limit" Int
             :> QueryParam "offset" Int
             :> QueryParam "filterBy" LocateBy
             :> QueryParams "location" Text
             :> QueryParams "roles" Person.Role
             :> Get '[JSON] User.ListRes
           :<|> Capture ":id" PersonId
             :> Get '[JSON] User.GetRes
           :<|> Capture ":id" PersonId
             :> Delete '[JSON] Ack
           :<|> "roles"
             :> Get '[JSON] [Person.Role]
       )

userFlow registrationToken =
  User.create registrationToken
    :<|> User.update registrationToken
    :<|> User.list registrationToken
    :<|> User.get registrationToken
    :<|> User.delete registrationToken
    :<|> User.listRoles registrationToken

------ Location Blacklist ----------
type BlacklistAPIS =
  "blacklist" :> Header "registrationToken" RegistrationTokenText
    :> ( ReqBody '[JSON] Blacklist.CreateReq
           :> Post '[JSON] Blacklist.CreateRes
           :<|> Capture "blacklist_id" BlacklistId
             :> ReqBody '[JSON] Blacklist.UpdateReq
             :> Post '[JSON] Blacklist.UpdateRes
           :<|> "list"
             :> QueryParam "limit" Int
             :> QueryParam "offset" Int
             :> MandatoryQueryParam "entityType" EntityType
             :> MandatoryQueryParam "entityId" Text
             :> Get '[JSON] Blacklist.ListRes
           :<|> Capture ":id" BlacklistId :> Get '[JSON] Blacklist.GetRes
           :<|> Capture "id" BlacklistId :> Delete '[JSON] Ack
       )

blacklistFlow registrationToken =
  Blacklist.create registrationToken
    :<|> Blacklist.update registrationToken
    :<|> Blacklist.list registrationToken
    :<|> Blacklist.get registrationToken
    :<|> Blacklist.delete registrationToken

--------
---- Document Api
type DocumentAPIs =
  "document"
    :> Header "registrationToken" RegistrationTokenText
    :> ( Capture "entityType" DocumentEntity
           :> Capture "entityId" Text
           :> "upload"
           :> MultipartForm Mem (MultipartData Mem)
           :> Post '[JSON] DocumentRes
           :<|> Capture "entityType" DocumentByType
           :> Capture "entityId" Text
           :> Get '[JSON] [ListDocumentRes]
       )

documentFlow registrationToken =
  Document.upload registrationToken
    :<|> Document.listDocuments registrationToken

--------
---- Tag Api
type TagAPIs =
  "tag"
    :> Header "registrationToken" RegistrationTokenText
    :> ( ReqBody '[JSON] Tag.CreateReq
           :> Post '[JSON] Tag.CreateRes
           :<|> "list"
             :> QueryParams "tagId" TagId
             :> QueryParam "entityType" Text
             :> QueryParam "entityId" Text
             :> Get '[JSON] Tag.ListRes
           :<|> "entity"
             :> ReqBody '[JSON] Tag.TagEntityReq
             :> Post '[JSON] Tag.TagEntityRes
           :<|> "types"
             :> Get '[JSON] Tag.ListVal
           :<|> Capture "tagType" Text
             :> Get '[JSON] Tag.ListVal
           :<|> Capture "tagType" Text
             :> Capture "tag" Text
             :> Get '[JSON] Tag.ListRes
       )

tagFlow registrationToken =
  Tag.create registrationToken
    :<|> Tag.list registrationToken
    :<|> Tag.tagEntity registrationToken
    :<|> Tag.listTypes registrationToken
    :<|> Tag.listTags registrationToken
    :<|> Tag.listByTag registrationToken

--------
---- Comment Api
type CommentAPIs =
  "comment"
    :> Header "registrationToken" RegistrationTokenText
    :> ( ReqBody '[JSON] Comment.CreateReq
           :> Post '[JSON] Comment.CreateRes
           :<|> Capture "primaryEntityType" Text
             :> Capture "primaryEntityId" Text
             :> "list"
             :> Get '[JSON] Comment.ListRes
       )

commentFlow registrationToken =
  Comment.create registrationToken
    :<|> Comment.list registrationToken

--------------------------------------------
----- Location API
type LocationAPIs =
  "location"
    :> Header "registrationToken" RegistrationTokenText
    :> ( "list"
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> MandatoryQueryParam "distinctBy" LocateBy -- can be null
           :> MandatoryQueryParam "filterBy" LocateBy
           :> MandatoryQueryParam "filter" Text
           :> Get '[JSON] ListLocationRes
       )

locationFlow registrationToken =
  Location.listLocation registrationToken
