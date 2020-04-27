module Beckn.App.Routes where

import qualified Beckn.Data.Accessor as Accessor
import Beckn.Types.API.Common
import Beckn.Types.API.Customer
import Beckn.Types.API.Pass
import Beckn.Types.API.PassApplication
import Beckn.Types.API.Registration
import Beckn.Types.API.Organization
import Beckn.Types.App
import Data.Aeson
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Servant

type EPassAPIs
   = "v1" :> (Get '[ JSON] Text :<|> RegistrationAPIs :<|> PassApplicationAPIs :<|> OrganizationAPIs :<|> CustomerAPIs :<|> PassAPIs)

epassAPIs :: Proxy EPassAPIs
epassAPIs = Proxy

---------- health Flow ------
healthCheckApp :: FlowHandler Text
healthCheckApp = pure "App is UP"
----------------------------

---- Registration Flow ------
type RegistrationAPIs
   = "token"
   :> ( ReqBody '[ JSON] InitiateLoginReq
        :> Post '[ JSON] InitiateLoginRes
      :<|> Capture "tokenId" Text
          :> ReqBody '[ JSON] LoginReq
          :> Post '[ JSON] Value
      )

initiateLogin :: InitiateLoginReq -> FlowHandler InitiateLoginRes
initiateLogin loginRes = undefined

login :: Text -> LoginReq -> FlowHandler Value
login tokenId req = do
  case req ^. Accessor.action of
    VERIFY -> undefined
    RESEND -> undefined
-------------------------------

---- Pass Application Flow ------
--
type PassApplicationAPIs
   = "passApplication"
   :> Header "registrationToken" Text
   :> ( ReqBody '[ JSON] CreatePassApplicationReq
        :> Post '[ JSON] PassApplicationRes
      :<|> ReqBody '[ JSON] ListPassApplicationReq
           :> Post '[ JSON] ListPassApplicationRes
      :<|> Capture "passApplicationId" Text :> Get '[ JSON] PassApplicationRes
      :<|> Capture "passApplicationId" Text
           :> ReqBody '[ JSON] UpdatePassApplicationReq
           :> Post '[ JSON] PassApplicationRes
      )

createPassApplication ::
  Maybe Text -> CreatePassApplicationReq -> FlowHandler PassApplicationRes
createPassApplication regToken req = undefined

listPassApplication ::
  Maybe Text -> ListPassApplicationReq -> FlowHandler ListPassApplicationRes
listPassApplication regToken req = undefined

getPassApplicationById :: Maybe Text -> Text -> FlowHandler PassApplicationRes
getPassApplicationById regToken applicationId = undefined

updatePassApplication ::
  Maybe Text ->
  Text ->
  UpdatePassApplicationReq ->
  FlowHandler PassApplicationRes
updatePassApplication regToken passApplicationId req = undefined

----- Organization Flow -------
--
type OrganizationAPIs
   = "organization"
   :> Header "registrationToken" Text
   :> ( ReqBody '[ JSON] CreateOrganizationReq :> Post '[ JSON] OrganizationRes
       :<|> Capture "organizationId" Text :> Get '[ JSON] OrganizationRes
       :<|> ReqBody '[ JSON] ListOrganizationReq
            :> Post '[ JSON] ListOrganizationRes
       :<|> Capture "organizationId" Text
            :> ReqBody '[ JSON] UpdateOrganizationReq
            :> Post '[ JSON] OrganizationRes
      )

createOrganization ::
  Maybe Text -> CreateOrganizationReq -> FlowHandler OrganizationRes
createOrganization regToken req = undefined

getOrganization :: Maybe Text -> Text -> FlowHandler OrganizationRes
getOrganization regToken orgId = undefined

listOrganization ::
  Maybe Text -> ListOrganizationReq -> FlowHandler ListOrganizationRes
listOrganization regToken req = undefined

updateOrganization ::
  Maybe Text -> Text -> UpdateOrganizationReq -> FlowHandler OrganizationRes
updateOrganization regToken orgId req = undefined
---------------------------------

----- Customer Flow -------
type CustomerAPIs
  = "customer"
  :> Header "registrationToken" Text
  :> Capture "customerId" Text
  :> Get '[ JSON] GetCustomerRes

getCustomerInfo ::
  Maybe Text -> Text -> FlowHandler GetCustomerRes
getCustomerInfo regToken customerId = undefined
---------------------------

------ Pass Flow ---------
type PassAPIs
  = "pass" :> Header "registrationToken" Text
  :> (Capture "passId" Text :> Get '[ JSON] PassRes
     :<|> Capture "passId" Text
          :> ReqBody '[ JSON] UpdatePassReq
          :> Post '[ JSON] PassRes
     :<|> "list"
          :> ReqBody '[ JSON] ListPassReq
          :> Post '[ JSON] ListPassRes
     )

getPassById :: Maybe Text -> Text -> FlowHandler PassRes
getPassById regToken passId = undefined

updatePass :: Maybe Text -> Text -> UpdatePassReq -> FlowHandler PassRes
updatePass regToken passId req = undefined

listPass :: Maybe Text -> ListPassReq -> FlowHandler ListPassRes
listPass regToken req = undefined
