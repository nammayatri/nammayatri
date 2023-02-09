module Beckn.ACL.Confirm (buildConfirmReq) where

import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import Beckn.Types.Core.Taxi.Confirm
import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Booking.BookingLocation as DBL
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error

buildConfirmReq ::
  (HasFlowEnv m r '["coreVersion" ::: Text]) =>
  Subscriber.Subscriber ->
  Confirm.ConfirmReq ->
  m DConfirm.DConfirmReq
buildConfirmReq subscriber req = do
  validateContext Context.CONFIRM req.context
  unless (subscriber.subscriber_id == req.context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  let bookingId = Id req.message.order.id
      phone = req.message.order.customer.contact.phone
      customerMobileCountryCode = phone.country_code
      customerPhoneNumber = phone.number
      fulfillment = req.message.order.fulfillment
      fromAddress = castAddress fulfillment.start.location.address
      mbRiderName = req.message.order.customer.person <&> (.name)
      toAddress = castAddress . (.location.address) <$> fulfillment.end
  return $
    DConfirm.DConfirmReq
      { ..
      }
  where
    castAddress Confirm.Address {..} = DBL.LocationAddress {areaCode = area_code, ..}
