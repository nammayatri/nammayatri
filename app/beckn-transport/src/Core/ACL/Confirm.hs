module Core.ACL.Confirm (buildConfirmReq) where

import Beckn.Prelude
import Beckn.Product.Validation.Context
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import Beckn.Types.Core.Taxi.Confirm
import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import Beckn.Types.Id
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.BookingLocation as DBL
import Utils.Common

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
      toAddress = castAddress . (.location.address) <$> fulfillment.end
  return $
    DConfirm.DConfirmReq
      { ..
      }
  where
    castAddress Confirm.Address {..} = DBL.LocationAddress {areaCode = area_code, ..}
