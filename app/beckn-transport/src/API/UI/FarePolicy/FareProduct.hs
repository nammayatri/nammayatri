module API.UI.FarePolicy.FareProduct (module Reexport, API, handler) where

import App.Types
import Beckn.Types.APISuccess
import Domain.Action.UI.FarePolicy.FareProduct as Reexport
  ( ListFareProductsRes (..),
    UpdateFareProductReq (..),
  )
import qualified Domain.Action.UI.FarePolicy.FareProduct as DFareProduct
import qualified Domain.Types.Person as SP
import EulerHS.Prelude
import Servant
import Utils.Auth
import Utils.Common

type API =
  "fareProduct"
    :> ( AdminTokenAuth
           :> Get '[JSON] ListFareProductsRes
           :<|> AdminTokenAuth
             :> ReqBody '[JSON] UpdateFareProductReq
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  listFareProducts
    :<|> updateFareProduct

listFareProducts :: SP.Person -> FlowHandler ListFareProductsRes
listFareProducts = withFlowHandlerAPI . DFareProduct.listFareProducts

updateFareProduct :: SP.Person -> UpdateFareProductReq -> FlowHandler APISuccess
updateFareProduct person = withFlowHandlerAPI . DFareProduct.updateFareProduct person
