{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Quote
  ( DQuote.GetQuotesRes (..),
    DQuote.OfferRes (..),
    API,
    getQuotes',
    handler,
  )
where

import qualified Domain.Action.UI.Quote as DQuote
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MultiModalSearchRequest as DMSR
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as SSR
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "rideSearch"
    :> Capture "searchId" (Id SSR.SearchRequest)
    :> TokenAuth
    :> "results"
    :> QueryParam "allowMultiple" Bool
    :> Get '[JSON] DQuote.GetQuotesRes
    -- MIGRATION CHANGES --
    :<|> "rideSearch"
      :> Capture "multimodalSearchId" (Id DMSR.MultiModalSearchRequest)
      :> TokenAuth
      :> "results"
      :> QueryParam "allowMultiple" Bool
      :> Get '[JSON] DQuote.GetQuotesRes

handler :: FlowServer API
handler =
  getQuotes :<|> getQuotesForMultimodal

getQuotes :: Id SSR.SearchRequest -> (Id Person.Person, Id Merchant.Merchant) -> Maybe Bool -> FlowHandler DQuote.GetQuotesRes
getQuotes searchRequestId token = withFlowHandlerAPI . getQuotes' searchRequestId token

getQuotes' :: Id SSR.SearchRequest -> (Id Person.Person, Id Merchant.Merchant) -> Maybe Bool -> Flow DQuote.GetQuotesRes
getQuotes' searchRequestId _ mbAllowMultiple = DQuote.getQuotes searchRequestId mbAllowMultiple

getQuotesForMultimodal :: Id DMSR.MultiModalSearchRequest -> (Id Person.Person, Id Merchant.Merchant) -> Maybe Bool -> FlowHandler DQuote.GetQuotesRes
getQuotesForMultimodal multimodalSearchRequestId token = withFlowHandlerAPI . getQuotesForMultimodal' multimodalSearchRequestId token

getQuotesForMultimodal' :: Id DMSR.MultiModalSearchRequest -> (Id Person.Person, Id Merchant.Merchant) -> Maybe Bool -> Flow DQuote.GetQuotesRes
getQuotesForMultimodal' multimodalSearchRequestId _ mbAllowMultiple = DQuote.getQuotesForMultimodal multimodalSearchRequestId mbAllowMultiple
