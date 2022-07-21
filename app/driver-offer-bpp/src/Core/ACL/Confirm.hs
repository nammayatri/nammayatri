module Core.ACL.Confirm where

import Beckn.Prelude
import Beckn.Product.Validation.Context
import Beckn.Types.App
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import Beckn.Types.Error
import Beckn.Types.Field
import Beckn.Types.Id
import Beckn.Utils.Error.Throwing
import Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Booking.BookingLocation as DBL

buildConfirmReq ::
  (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) =>
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
  toAddress <- (castAddress . (.location.address) <$> fulfillment.end) & fromMaybeM (InvalidRequest "end location missing")

  return $
    DConfirm.DConfirmReq
      { ..
      }
  where
    castAddress Confirm.Address {..} = DBL.LocationAddress {areaCode = area_code, ..}
