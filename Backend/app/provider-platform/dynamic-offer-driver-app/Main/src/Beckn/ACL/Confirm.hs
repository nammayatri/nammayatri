{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Confirm where

import Beckn.ACL.Common
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import qualified Data.Text as T
import Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Vehicle.Variant as VehVar
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
      fulfillment = req.message.order.fulfillment
      phone = fulfillment.customer.contact.phone
      customerMobileCountryCode = phone.phoneCountryCode
      customerPhoneNumber = phone.phoneNumber
      fromAddress = castAddress fulfillment.start.location.address
      mbRiderName = fulfillment.customer.person <&> (.name)
      vehicleVariant = castVehicleVariant fulfillment.vehicle.category
      driverId = req.message.order.provider <&> (.id)
      nightSafetyCheck = buildNightSafetyCheckTag $ (.tags) =<< fulfillment.customer.person
  toAddress <- (castAddress . (.location.address) <$> fulfillment.end) & fromMaybeM (InvalidRequest "end location missing")

  return $
    DConfirm.DConfirmReq
      { ..
      }
  where
    castAddress Confirm.Address {..} = DL.LocationAddress {areaCode = area_code, area = locality, fullAddress = Nothing, ..}
    castVehicleVariant = \case
      Confirm.SEDAN -> VehVar.SEDAN
      Confirm.SUV -> VehVar.SUV
      Confirm.HATCHBACK -> VehVar.HATCHBACK
      Confirm.AUTO_RICKSHAW -> VehVar.AUTO_RICKSHAW
      Confirm.TAXI -> VehVar.TAXI
      Confirm.TAXI_PLUS -> VehVar.TAXI_PLUS

buildNightSafetyCheckTag :: Maybe Confirm.TagGroups -> Bool
buildNightSafetyCheckTag tagGroups' = do
  maybe True getTagValue tagGroups'
  where
    getTagValue tagGroups = do
      let tagValue = getTag "customer_info" "night_safety_check" tagGroups
          res = maybe (Just True) ((\val -> readMaybe val :: Maybe Bool) . T.unpack) tagValue
      fromMaybe True res
