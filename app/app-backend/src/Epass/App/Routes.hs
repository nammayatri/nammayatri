module Epass.App.Routes where

import qualified Beckn.Types.Storage.Case as Case
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
import qualified Epass.Product.Tag as Tag
import qualified Epass.Product.User.Get as User
import qualified Epass.Product.User.Update as User
import qualified Epass.Types.API.Blacklist as Blacklist
import qualified Epass.Types.API.Comment as Comment
import Epass.Types.API.Customer
import Epass.Types.API.Document
import Epass.Types.API.Location.CRUD
import Epass.Types.API.Organization
import Epass.Types.API.Pass
import Epass.Types.API.PassApplication
import qualified Epass.Types.API.Quota as Quota
import qualified Epass.Types.API.Tag as Tag
import qualified Epass.Types.API.User as User
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.Organization as SO
import qualified Epass.Types.Storage.Pass as SP
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

type EPassAPI =
  "epass"
    :> ( Get '[JSON] Text
           :<|> PassApplicationAPI
           :<|> OrganizationAPI
           :<|> CustomerAPI
           :<|> PassAPI
           :<|> UserAPIS
           :<|> QuotaAPIS
           :<|> BlacklistAPIS
           :<|> DocumentAPI
           :<|> TagAPI
           :<|> CommentAPI
           :<|> LocationAPI
       )

epassAPI :: Proxy EPassAPI
epassAPI = Proxy

epassServer :: V.Key (HashMap Text Text) -> FlowServer EPassAPI
epassServer key =
  HealthCheck.healthCheckApp
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

---------------------------------
---- Pass Application Flow ------
type PassApplicationAPI =
  "pass_application"
    :> AuthHeader
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
             :> QueryParams "status" Case.CaseStatus
             :> QueryParams "organization" OrganizationId
             :> QueryParams "type" Text
             :> Get '[JSON] ListPassApplicationRes
           :<|> Capture "caseId" CaseId
             :> Get '[JSON] CaseInfo
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
type OrganizationAPI =
  "organization"
    :> AuthHeader
    :> ( ReqBody '[JSON] CreateOrganizationReq
           :> Post '[JSON] OrganizationRes
           :<|> "list"
             :> ReqBody '[JSON] ListOrganizationReq
             :> Post '[JSON] ListOrganizationRes
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
type CustomerAPI =
  "customer"
    :> AuthHeader
    :> Capture "customerId" Text
    :> Get '[JSON] GetCustomerRes

customerFlow = Customer.getCustomerInfo

---------------------------
------ Pass Flow ---------

-- | Please be cautious while changing the order of the routes
-- | The /pass/list?.. was getting overriden by /pass/:passId
type PassAPI =
  "pass"
    :> AuthHeader
    :> ( "list"
           :> MandatoryQueryParam "identifierType" PassIDType
           :> MandatoryQueryParam "identifier" Text
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> MandatoryQueryParam "type" PassType
           :> Get '[JSON] ListPassRes
           :<|> Capture "passId" ProductInstanceId
           :> Get '[JSON] PassRes
           :<|> Capture "passId" ProductInstanceId
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
    :> AuthHeader
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
  "user" :> AuthHeader
    :> ( Capture "userId" PersonId
           :> ReqBody '[JSON] User.UpdateReq
           :> Post '[JSON] User.UpdateRes
           :<|> "list"
             :> QueryParam "limit" Int
             :> QueryParam "offset" Int
             :> QueryParam "filterBy" LocateBy
             :> QueryParams "location" Text
             :> QueryParams "roles" Person.Role
             :> Get '[JSON] User.ListRes
           :<|> "roles"
             :> Get '[JSON] [Person.Role]
           :<|> Capture ":id" PersonId
             :> Get '[JSON] User.GetRes
           :<|> Capture ":id" PersonId
             :> Delete '[JSON] Ack
       )

userFlow registrationToken =
  User.update registrationToken
    :<|> User.list registrationToken
    :<|> User.listRoles registrationToken
    :<|> User.get registrationToken
    :<|> User.delete registrationToken

------ Location Blacklist ----------
type BlacklistAPIS =
  "blacklist" :> AuthHeader
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
type DocumentAPI =
  "document"
    :> AuthHeader
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
type TagAPI =
  "tag"
    :> AuthHeader
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
type CommentAPI =
  "comment"
    :> AuthHeader
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
type LocationAPI =
  "location"
    :> AuthHeader
    :> ( "list"
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> MandatoryQueryParam "distinctBy" LocateBy -- can be null
           :> MandatoryQueryParam "filterBy" LocateBy
           :> MandatoryQueryParam "filter" Text
           :> Get '[JSON] ListLocationRes
       )

locationFlow = Location.listLocation
