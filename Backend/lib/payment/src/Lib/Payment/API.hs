{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Payment.API
  ( API,
  )
where

import GHC.Base (Symbol)
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Types.App (MandatoryQueryParam)
import Kernel.Types.Id
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Servant

type API (entityId :: Symbol) (notificationEntityId :: Symbol) entity notificationEntity =
  "payment"
    :> ( Capture entityId (Id entity)
           :> "createOrder"
           :> Post '[JSON] Payment.CreateOrderResp
           :<|> Capture "orderId" (Id DOrder.PaymentOrder)
           :> "status"
           :> Get '[JSON] DPayment.PaymentStatusResp
           :<|> MandatoryQueryParam "orderId" (Id DOrder.PaymentOrder)
           :> Get '[JSON] DOrder.PaymentOrderAPIEntity
           :<|> Capture "notificationId" (Id notificationEntity)
           :> "notification"
           :> Get '[JSON] Payment.NotificationStatusResp
       )
