module App.Routes where

-- import           Beckn.Types.API.Search
-- import           Beckn.Types.API.Confirm
-- import           Beckn.Types.Common

import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Storage.Case
import Data.Aeson
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Network.Wai.Parse
import Product.BecknProvider.BP as BP
import qualified Product.Case.CRUD as Case
import qualified Product.CaseProduct as CaseProduct
import qualified Product.Person as Person
import qualified Product.Products as Product
import qualified Product.Registration as Registration
import qualified Product.Transporter as Transporter
import Servant
import Servant.Multipart
import Types.API.Case
import Types.API.CaseProduct
import Types.API.Person
import Types.API.Products
import Types.API.Registration
import Types.API.Registration
import Types.API.Transporter

type TransporterAPIs =
  "v1"
    :> ( Get '[JSON] Text
           :<|> RegistrationAPIs
           :<|> UpdatePersonAPIs
           :<|> OrganizationAPIs --Transporter
           :<|> SearchAPIs
           :<|> ConfirmAPIs
           :<|> CaseAPIs
           :<|> CaseProductAPIs
       )

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

-- Following is Update person flow
type UpdatePersonAPIs =
  "person"
    :> ( Capture "personId" Text
           :> "update"
           :> Header "authorization" Text
           :> ReqBody '[JSON] UpdatePersonReq
           :> Post '[JSON] UpdatePersonRes
       )

updatePersonFlow :: FlowServer UpdatePersonAPIs
updatePersonFlow = Person.updatePerson

-- Following is organization creation
type OrganizationAPIs =
  "transporter"
      :> ( Header "authorization" Text
          :> ReqBody '[JSON] TransporterReq
          :> Post '[JSON] TransporterRes
          :<|> "gateway"
            :> Header "authorization" Text
            :> ReqBody '[JSON] TransporterReq
            :> Post '[JSON] GatewayRes
       )

organizationFlow :: FlowServer OrganizationAPIs
organizationFlow =
  Transporter.createTransporter
  :<|> Transporter.createGateway

-----------------------------
-------- Case Flow----------
type CaseAPIs =
     "case"
       :> (  Header "authorization" Text
              :> ReqBody '[ JSON] CaseReq
              :>  Post '[ JSON] CaseListRes
          :<|>  Header "authorization" Text
                 :> Capture "caseId" Text
                 :> ReqBody '[JSON] UpdateCaseReq
                 :> Post '[JSON] Case
          )

caseFlow =
    Case.list
    :<|> Case.update

-------- CaseProduct Flow----------
type CaseProductAPIs =
  "caseProduct"
    :> (  Header "authorization" Text
           :> ReqBody '[JSON] CaseProdReq
           :> Post '[JSON] CaseProductList
       )

caseProductFlow =
  CaseProduct.list

-------- Product Flow----------
type ProductAPIs =
  "update"
    :> (  Header "authorization" Text
           :> ReqBody '[JSON] ProdReq
           :> Post '[JSON] ProdInfoRes
       )

productFlow =
  Product.updateInfo

transporterAPIs :: Proxy TransporterAPIs
transporterAPIs = Proxy

transporterServer' :: V.Key (HashMap Text Text) -> FlowServer TransporterAPIs
transporterServer' key =
  pure "App is UP"
    :<|> registrationFlow
    :<|> updatePersonFlow
    :<|> organizationFlow
    :<|> searchApiFlow
    :<|> confirmApiFlow
    :<|> caseFlow
    :<|> caseProductFlow

type SearchAPIs =
  "search"
    :> "services"
    :> ( ReqBody '[JSON] SearchReq
           :> Post '[JSON] AckResponse
       )

searchApiFlow :: FlowServer SearchAPIs
searchApiFlow = BP.search

type ConfirmAPIs =
  "confirm"
    :> "services"
    :> ( ReqBody '[JSON] ConfirmReq
           :> Post '[JSON] AckResponse
       )

confirmApiFlow :: FlowServer ConfirmAPIs
confirmApiFlow = BP.confirm
