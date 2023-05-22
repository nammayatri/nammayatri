{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.RideBooking.Maps where

import qualified "rider-app" API.Dashboard.RideBooking.Maps as BAP
import qualified Dashboard.RiderPlatform.Customer as Common
import qualified "rider-app" Domain.Action.UI.Maps as DMaps
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp as Client
import Servant
import qualified SharedLogic.Transaction as T
import qualified "rider-app" SharedLogic.Types.Person as DP
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "maps"
    :> ( ApiAuth 'APP_BACKEND 'CUSTOMERS 'AUTOCOMPLETE
           :> BAP.RideAutoCompleteAPI
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'PLACEDETAIL
             :> BAP.RideGetPlaceDetailsAPI
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'PLACENAME
             :> BAP.RideGetPlaceNameAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  callAutoComplete merchantId
    :<|> callGetPlaceDetails merchantId
    :<|> callGetPlaceName merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  BAP.MapEndPoints ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.MapsAPI endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

callAutoComplete :: ShortId DM.Merchant -> ApiTokenInfo -> Id DP.Person -> DMaps.AutoCompleteReq -> FlowHandler DMaps.AutoCompleteResp
callAutoComplete merchantShortId apiTokenInfo personId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction BAP.AutoCompleteEndPoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.rideBooking.maps.autoComplete) personId req

callGetPlaceDetails :: ShortId DM.Merchant -> ApiTokenInfo -> Id DP.Person -> DMaps.GetPlaceDetailsReq -> FlowHandler DMaps.GetPlaceDetailsResp
callGetPlaceDetails merchantShortId apiTokenInfo personId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction BAP.GetPlaceDetailsEndPoints apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.rideBooking.maps.getPlaceDetails) personId req

callGetPlaceName :: ShortId DM.Merchant -> ApiTokenInfo -> Id DP.Person -> DMaps.GetPlaceNameReq -> FlowHandler DMaps.GetPlaceNameResp
callGetPlaceName merchantShortId apiTokenInfo personId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction BAP.GetPlaceNameEndPoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.rideBooking.maps.getPlaceName) personId req
