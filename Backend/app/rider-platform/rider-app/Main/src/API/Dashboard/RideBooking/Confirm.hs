{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.RideBooking.Confirm where

import qualified API.UI.Confirm as UC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Quote as Quote
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.Merchant
import Storage.Beam.SystemConfigs ()

data RideConfirmEndPoint = ConfirmEndPoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

derivePersistField "RideConfirmEndPoint"

type API =
  "confirm"
    :> CustomerConfirmAPI

type CustomerConfirmAPI =
  "rideSearch"
    :> Capture "customerId" (Id DP.Person)
    :> "quotes"
    :> Capture "quoteId" (Id Quote.Quote)
    :> "confirm"
    :> QueryParam "paymentMethodId" (Id DMPM.MerchantPaymentMethod)
    :> Post '[JSON] UC.ConfirmRes

handler :: ShortId DM.Merchant -> FlowServer API
handler =
  callConfirm

callConfirm :: ShortId DM.Merchant -> Id DP.Person -> Id Quote.Quote -> Maybe (Id DMPM.MerchantPaymentMethod) -> FlowHandler UC.ConfirmRes
callConfirm merchantId personId quote mbPaymentMethodId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  UC.confirm (personId, m.id) quote mbPaymentMethodId
