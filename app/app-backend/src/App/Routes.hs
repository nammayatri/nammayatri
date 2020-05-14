{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Routes where

import qualified Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Search as Search
import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..), generateGUID)
import Beckn.Types.Core.Ack
import Data.Aeson
import Data.Aeson
import qualified Data.Vault.Lazy as V
import qualified Data.Vault.Lazy as V
import qualified Epass.App.Routes as Epass
import EulerHS.Prelude
import EulerHS.Prelude
import Network.Wai.Parse
import Network.Wai.Parse
import qualified Product.Case as Case
import qualified Product.Confirm as Confirm
import qualified Product.Registration as Registration
import qualified Product.Search as Search
import Servant
import qualified Types.API.Case as Case
import qualified Types.API.Confirm as ConfirmAPI
import Types.API.Registration
import Types.App

type AppAPIs =
  "v1"
    :> ( Get '[JSON] Text
           :<|> RegistrationAPIs
           :<|> SearchAPIs
           :<|> ConfirmAPIs
           :<|> CaseAPIs
       )
    :<|> Epass.EPassAPIs

appAPIs :: Proxy AppAPIs
appAPIs = Proxy

appServer' :: V.Key (HashMap Text Text) -> FlowServer AppAPIs
appServer' key = do
  ( pure "App is UP"
      :<|> registrationFlow
      :<|> searchFlow
      :<|> confirmFlow
      :<|> caseFlow
    )
    :<|> Epass.epassServer' key

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

-------- Search Flow --------
type SearchAPIs =
  "search" :> "services"
    :> Header "token" RegToken
    :> ReqBody '[JSON] Search.SearchReq
    :> Post '[JSON] Search.SearchRes
    -- on_search
    :<|> "on_search"
    :> "services"
    :> Header "token" RegToken
    :> ReqBody '[JSON] Search.OnSearchReq
    :> Post '[JSON] Search.OnSearchRes

searchFlow :: FlowServer SearchAPIs
searchFlow =
  Search.search
    :<|> Search.search_cb

-------- Confirm Flow --------
type ConfirmAPIs =
  ( "confirm"
      :> Header "token" RegToken
      :> ReqBody '[JSON] ConfirmAPI.ConfirmReq
      :> Post '[JSON] AckResponse
      :<|> "on_confirm"
      :> "services"
      :> Header "token" RegToken
      :> ReqBody '[JSON] Confirm.OnConfirmReq
      :> Post '[JSON] Confirm.OnConfirmRes
  )

confirmFlow :: FlowServer ConfirmAPIs
confirmFlow =
  Confirm.confirm
    :<|> Confirm.onConfirm

------- Case Flow -------
type CaseAPIs =
  "case"
    :> Header "token" RegToken
    :> Capture "caseId" CaseId
    :> Get '[JSON] Case.StatusRes

caseFlow :: FlowServer CaseAPIs
caseFlow =
  Case.status
