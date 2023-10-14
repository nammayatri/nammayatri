{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.RideBooking.Confirm where

import qualified "rider-app" API.Dashboard.RideBooking.Confirm as BAP
import qualified "rider-app" API.UI.Confirm as UC
import qualified Dashboard.RiderPlatform.Customer as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified "rider-app" Domain.Types.Person as DP
import qualified "rider-app" Domain.Types.Quote as Quote
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp as Client
import Servant
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "confirm"
    :> ApiAuth 'APP_BACKEND 'CUSTOMERS 'CONFIRM
    :> BAP.CustomerConfirmAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler = callConfirm

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  BAP.RideConfirmEndPoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.ConfirmAPI endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

callConfirm :: ShortId DM.Merchant -> ApiTokenInfo -> Id DP.Person -> Id Quote.Quote -> Maybe (Id DMPM.MerchantPaymentMethod) -> Maybe UTCTime -> FlowHandler UC.ConfirmRes
callConfirm merchantShortId apiTokenInfo personId quoteId mbMerchantPaymentId startTime = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction BAP.ConfirmEndPoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.rideBooking.confirm.rconfirm) personId quoteId mbMerchantPaymentId startTime
