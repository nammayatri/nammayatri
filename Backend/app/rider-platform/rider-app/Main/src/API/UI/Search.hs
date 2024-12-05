{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Search
  ( DSearch.SearchReq (..),
    DSearch.OneWaySearchReq (..),
    DSearch.RentalSearchReq (..),
    API,
    search',
    search,
    handler,
  )
where

import API.Types.UI.Search
import qualified Beckn.ACL.Search as TaxiACL
import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Action.UI.Search as DSearch
import qualified Domain.Types.Client as DC
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.Search as SLS
import Storage.Beam.SystemConfigs ()
import Tools.Auth

-------- Search Flow --------

type API =
  "rideSearch"
    :> TokenAuth
    :> ReqBody '[JSON] DSearch.SearchReq
    :> Header "x-bundle-version" Version
    :> Header "x-client-version" Version
    :> Header "x-config-version" Version
    :> Header "client-id" (Id DC.Client)
    :> Header "x-device" Text
    :> Header "is-dashboard-request" Bool
    :> Post '[JSON] SearchResp

handler :: FlowServer API
handler = search

search :: (Id Person.Person, Id Merchant.Merchant) -> DSearch.SearchReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe (Id DC.Client) -> Maybe Text -> Maybe Bool -> FlowHandler SearchResp
search (personId, merchantId) req mbBundleVersion mbClientVersion mbClientConfigVersion mbClientId mbDevice = withFlowHandlerAPI . search' (personId, merchantId) req mbBundleVersion mbClientVersion mbClientConfigVersion mbClientId mbDevice

search' :: (Id Person.Person, Id Merchant.Merchant) -> DSearch.SearchReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe (Id DC.Client) -> Maybe Text -> Maybe Bool -> Flow SearchResp
search' (personId, merchantId) req mbBundleVersion mbClientVersion mbClientConfigVersion mbClientId mbDevice mbIsDashboardRequest = withPersonIdLogTag personId $ do
  checkSearchRateLimit personId
  fork "updating person versions" $ updateVersions personId mbBundleVersion mbClientVersion mbClientConfigVersion mbDevice
  dSearchRes <- DSearch.search personId req mbBundleVersion mbClientVersion mbClientConfigVersion mbClientId mbDevice (fromMaybe False mbIsDashboardRequest) Nothing
  fork "search cabs" . withShortRetry $ do
    becknTaxiReqV2 <- TaxiACL.buildSearchReqV2 dSearchRes
    let generatedJson = encode becknTaxiReqV2
    logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
    void $ CallBPP.searchV2 dSearchRes.gatewayUrl becknTaxiReqV2 merchantId
  return $ SearchResp dSearchRes.shortestRouteInfo dSearchRes.searchRequestExpiry dSearchRes.searchId
