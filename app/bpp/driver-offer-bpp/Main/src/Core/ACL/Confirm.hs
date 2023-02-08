module Core.ACL.Confirm where

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
    castAddress Confirm.Address {..} = DBL.LocationAddress {areaCode = area_code, ..}
