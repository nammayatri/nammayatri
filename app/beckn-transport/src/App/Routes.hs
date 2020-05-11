module App.Routes where

-- import           Beckn.Types.API.Search
-- import           Beckn.Types.API.Confirm
-- import           Beckn.Types.Common
import Data.Aeson
import qualified Data.Vault.Lazy as V
import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.Common
import EulerHS.Prelude
import Network.Wai.Parse
import Product.BecknProvider.BP as BP
import qualified Product.Registration as Registration
import qualified Product.Person as Person
import Types.API.Person
import qualified Product.CaseProduct as CaseProduct
import qualified Product.Case.CRUD as Case
import Servant
import Servant.Multipart
import Types.API.Registration
import Types.API.Case
import Types.API.CaseProduct
import Types.App

type TransporterAPIs =
  "v1"
    :> ( Get '[JSON] Text
           :<|> RegistrationAPIs
           :<|> UpdatePersonAPIs
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
    :> ( Capture "regToken" Text
          :> "update"
          :> ReqBody '[JSON] UpdatePersonReq
          :> Post '[JSON] UpdatePersonRes
       )

updatePersonFlow :: FlowServer UpdatePersonAPIs
updatePersonFlow = Person.updatePerson

-----------------------------
-------- Case Flow----------
type CaseAPIs =
     "case"
       :> (    ReqBody '[ JSON] CaseReq
           :>  Post '[ JSON] CaseListRes
          )

caseFlow =
    Case.list

-------- CaseProduct Flow----------
type CaseProductAPIs =
     "caseProduct"
       :> (    ReqBody '[ JSON] CaseProdReq
           :>  Post '[ JSON] CaseProductList
          )

caseProductFlow =
    CaseProduct.list


transporterAPIs :: Proxy TransporterAPIs
transporterAPIs = Proxy

transporterServer' :: V.Key (HashMap Text Text) -> FlowServer TransporterAPIs
transporterServer' key =
  pure "App is UP"
    :<|> registrationFlow
    :<|> updatePersonFlow
    :<|> searchApiFlow
    :<|> confirmApiFlow
    :<|> caseFlow
    :<|> caseProductFlow

type SearchAPIs =
      "search"
        :> "services"
        :> (    ReqBody '[ JSON] SearchReq
            :>  Post '[ JSON] AckResponse
            )

searchApiFlow :: FlowServer SearchAPIs
searchApiFlow = BP.search


type ConfirmAPIs =
      "confirm"
        :> "services"
        :> (    ReqBody '[ JSON] ConfirmReq
            :>  Post '[ JSON] AckResponse
            )

confirmApiFlow :: FlowServer ConfirmAPIs
confirmApiFlow = BP.confirm