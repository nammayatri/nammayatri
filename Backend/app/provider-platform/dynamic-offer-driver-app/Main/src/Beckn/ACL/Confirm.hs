{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Confirm where

import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Booking.BookingLocation as DBL
import Kernel.Prelude
import Kernel.Product.Validation.Context
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Field
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing

buildConfirmReq ::
  (HasFlowEnv m r '["coreVersion" ::: Text]) =>
  Confirm.ConfirmReq ->
  m DConfirm.DConfirmReq
buildConfirmReq req = do
  validateContext Context.CONFIRM req.context
  let bookingId = Id req.message.order.id
      phone = req.message.order.customer.contact.phone
      customerMobileCountryCode = phone.country_code
      customerPhoneNumber = phone.number
      fulfillment = req.message.order.fulfillment
      fromAddress = castAddress fulfillment.start.location.address
      mbRiderName = req.message.order.customer.person <&> (.name)
  toAddress <- (castAddress . (.location.address) <$> fulfillment.end) & fromMaybeM (InvalidRequest "end location missing")

  return $
    DConfirm.DConfirmReq
      { ..
      }
  where
    castAddress Confirm.Address {..} = DBL.LocationAddress {areaCode = area_code, area = locality, ..}
