{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Routes where

import qualified Beckn.Types.API.Cancel as Cancel (OnCancelReq (..), OnCancelRes (..))
import qualified Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Search as Search
import Beckn.Types.API.Track
import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..), generateGUID)
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Storage.Case as Case
import Data.Aeson
import qualified Data.Vault.Lazy as V
import qualified Epass.App.Routes as Epass
import EulerHS.Prelude
import Network.Wai.Parse
import qualified Product.Cancel as Cancel
import qualified Product.Case as Case
import qualified Product.Confirm as Confirm
import qualified Product.Cron as Cron
import qualified Product.Info as Info
import qualified Product.Location as Location
import qualified Product.ProductInstance as ProductInstance
import qualified Product.Registration as Registration
import qualified Product.Search as Search
import qualified Product.TrackTrip as TrackTrip
import Servant
import qualified Types.API.Cancel as Cancel
import qualified Types.API.Case as Case
import qualified Types.API.Confirm as ConfirmAPI
import qualified Types.API.Cron as Cron
import qualified Types.API.Location as Location
import Types.API.Product
import qualified Types.API.ProductInstance as ProductInstance
import Types.API.Registration
import Types.App
import Utils.Common (TokenAuth)

type AppAPIs =
  "v1"
    :> ( Get '[JSON] Text
           :<|> RegistrationAPIs
           :<|> SearchAPIs
           :<|> ConfirmAPIs
           :<|> CaseAPIs
           :<|> InfoAPIs
           :<|> TrackTripAPIs
           :<|> ProductInstanceAPIs
           :<|> CancelAPIs
           :<|> CronAPIs
           :<|> RouteAPI
           :<|> Epass.EPassAPIs
       )

appAPIs :: Proxy AppAPIs
appAPIs = Proxy

appServer' :: V.Key (HashMap Text Text) -> FlowServer AppAPIs
appServer' key = do
  ( pure "App is UP"
      :<|> registrationFlow
      :<|> searchFlow
      :<|> confirmFlow
      :<|> caseFlow
      :<|> infoFlow
      :<|> trackTripFlow
      :<|> productInstanceFlow
      :<|> cancelFlow
      :<|> cronFlow
      :<|> routeApiFlow
      :<|> Epass.epassServer' key
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

-------- Search Flow --------
type SearchAPIs =
  "search" :> "services"
    :> TokenAuth
    :> ReqBody '[JSON] Search.SearchReq
    :> Post '[JSON] Search.SearchRes
    -- on_search
    :<|> "on_search"
    :> "services"
    :> ReqBody '[JSON] Search.OnSearchReq
    :> Post '[JSON] Search.OnSearchRes

searchFlow :: FlowServer SearchAPIs
searchFlow =
  Search.search
    :<|> Search.search_cb

-------- Confirm Flow --------
type ConfirmAPIs =
  ( "confirm"
      :> TokenAuth
      :> ReqBody '[JSON] ConfirmAPI.ConfirmReq
      :> Post '[JSON] AckResponse
      :<|> "on_confirm"
      :> "services"
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
    :> TokenAuth
    :> ( "list"
           :> MandatoryQueryParam "type" Case.CaseType
           :> QueryParams "status" Case.CaseStatus
           :> QueryParam "limit" Integer
           :> QueryParam "offset" Integer
           :> Get '[JSON] Case.ListRes
           :<|> Capture "caseId" CaseId
           :> Get '[JSON] Case.StatusRes
       )

caseFlow :: FlowServer CaseAPIs
caseFlow regToken =
  Case.list regToken
    :<|> Case.status regToken

-------- Info Flow ------
type InfoAPIs =
  TokenAuth
    :> ( "product"
           :> Capture "id" Text
           :> Get '[JSON] GetProductInfoRes
           :<|> "location"
           :> Capture "caseId" Text
           :> Get '[JSON] Location.GetLocationRes
       )

infoFlow :: FlowServer InfoAPIs
infoFlow regToken =
  Info.getProductInfo regToken
    :<|> Info.getLocation regToken

------- Track trip Flow -------
type TrackTripAPIs =
  "track"
    :> "trip"
    :> TokenAuth
    :> ReqBody '[JSON] TrackTripReq
    :> Post '[JSON] TrackTripRes
    :<|> "on_track"
    :> "trip"
    :> ReqBody '[JSON] OnTrackTripReq
    :> Post '[JSON] OnTrackTripRes

trackTripFlow :: FlowServer TrackTripAPIs
trackTripFlow =
  TrackTrip.track
    :<|> TrackTrip.track_cb

-------- ProductInstance Flow----------
type ProductInstanceAPIs =
  "productInstance"
    :> ( TokenAuth
           :> ReqBody '[JSON] ProductInstance.ProdInstReq
           :> Post '[JSON] ProductInstance.ProductInstanceList
       )

productInstanceFlow =
  ProductInstance.list

-------- Cancel Flow----------
type CancelAPIs =
  "cancel"
    :> "services"
    :> TokenAuth
    :> ReqBody '[JSON] Cancel.CancelReq
    :> Post '[JSON] Cancel.CancelRes
    -- on cancel
    :<|> "on_cancel"
    :> "services"
    :> ReqBody '[JSON] Cancel.OnCancelReq
    :> Post '[JSON] Cancel.OnCancelRes

cancelFlow =
  Cancel.cancel
    :<|> Cancel.onCancel

-------- Cron APIs --------
type CronAPIs =
  "cron"
    :> "expire_cases"
    :> Header "Authorization" CronAuthKey
    :> ReqBody '[JSON] Cron.ExpireCaseReq
    :> Post '[JSON] Cron.ExpireCaseRes

cronFlow :: FlowServer CronAPIs
cronFlow =
  Cron.updateCases

type RouteAPI =
  "route"
    :> AuthHeader
    :> ReqBody '[JSON] Location.Request
    :> Post '[JSON] Location.Response

routeApiFlow :: FlowServer RouteAPI
routeApiFlow = Location.getRoute
