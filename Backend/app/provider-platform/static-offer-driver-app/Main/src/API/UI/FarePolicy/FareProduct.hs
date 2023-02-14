 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.FarePolicy.FareProduct (module Reexport, API, handler) where

import Domain.Action.UI.FarePolicy.FareProduct as Reexport
  ( ListFareProductsRes (..),
    UpdateFareProductReq (..),
  )
import qualified Domain.Action.UI.FarePolicy.FareProduct as DFareProduct
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Tools.Auth

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
