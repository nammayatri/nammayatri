{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.MetroBeckn.OnSearch (API, handler) where

import qualified Beckn.ACL.Metro.OnSearch as MetroACL
import Beckn.Types.Core.Metro.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Metro.API.OnSearch as Metro
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import qualified SharedLogic.MetroOffer as MetroOffers
import Storage.Beam.SystemConfigs ()

type API =
  SignatureAuth "X-Gateway-Authorization"
    :> Metro.OnSearchAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onSearch

onSearch ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  OnSearch.OnSearchReq ->
  FlowHandler AckResponse
onSearch _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  let searchReqId = Id req.context.message_id
  mbMetroOffers <- MetroACL.buildMetroOffers req
  whenJust mbMetroOffers $ MetroOffers.cacheMetroOffers searchReqId
  pure Ack
