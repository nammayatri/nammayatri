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
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as Utils
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
  let toAddress = (castAddress . (.location.address) <$> fulfillment.end)

  return $
    DConfirm.DConfirmReq
      { ..
      }
  where
    castAddress Confirm.Address {..} = DL.LocationAddress {areaCode = area_code, area = locality, fullAddress = mkFullAddress Confirm.Address {..}, ..}
    castVehicleVariant = \case
      Confirm.SEDAN -> VehVar.SEDAN
      Confirm.SUV -> VehVar.SUV
      Confirm.HATCHBACK -> VehVar.HATCHBACK
      Confirm.AUTO_RICKSHAW -> VehVar.AUTO_RICKSHAW
      Confirm.TAXI -> VehVar.TAXI
      Confirm.TAXI_PLUS -> VehVar.TAXI_PLUS
    mkFullAddress Confirm.Address {..} = do
      let strictFields = catMaybes $ filter (not . isEmpty) [door, building, street, locality, city, state, area_code, country]
      if null strictFields
        then Nothing
        else Just $ T.intercalate ", " strictFields

isEmpty :: Maybe Text -> Bool
isEmpty = maybe True (T.null . T.replace " " "")

buildNightSafetyCheckTag :: Maybe Confirm.TagGroups -> Bool
buildNightSafetyCheckTag tagGroups' = do
  maybe True getTagValue tagGroups'
  where
    getTagValue tagGroups = do
      let tagValue = getTag "customer_info" "night_safety_check" tagGroups
          res = maybe (Just True) ((\val -> readMaybe val :: Maybe Bool) . T.unpack) tagValue
      fromMaybe True res

buildConfirmReqV2 ::
  (HasFlowEnv m r '["_version" ::: Text]) =>
  Spec.ConfirmReq ->
  m DConfirm.DConfirmReq
buildConfirmReqV2 req = do
  Utils.validateContext Context.CONFIRM req.confirmReqContext
  bookingId <- fmap Id req.confirmReqMessage.confirmReqMessageOrder.orderId & fromMaybeM (InvalidRequest "orderId not found")
  fulfillment <- req.confirmReqMessage.confirmReqMessageOrder.orderFulfillments >>= listToMaybe & fromMaybeM (InvalidRequest "Fulfillment not found")
  customerPhoneNumber <- fulfillment.fulfillmentCustomer >>= (.customerContact) >>= (.contactPhone) & fromMaybeM (InvalidRequest "Customer Phone not found")
  let customerMobileCountryCode = "+91" -- TODO: check how to get countrycode via ONDC
  fromAddress <- fulfillment.fulfillmentStops >>= Utils.getStartLocation >>= (.stopLocation) >>= Utils.parseAddress & fromMaybeM (InvalidRequest "Start location not found")
  let mbRiderName = fulfillment.fulfillmentCustomer >>= (.customerPerson) >>= (.personName)
  let vehCategory = fulfillment.fulfillmentVehicle >>= (.vehicleCategory)
      vehVariant = fulfillment.fulfillmentVehicle >>= (.vehicleVariant)
  vehicleVariant <- Utils.parseVehicleVariant vehCategory vehVariant & fromMaybeM (InvalidRequest $ "Unable to parse vehicle category:- " <> show vehCategory <> " and variant:- " <> show vehVariant)
  let driverId = req.confirmReqMessage.confirmReqMessageOrder.orderProvider >>= (.providerId)
  let nightSafetyCheck = fulfillment.fulfillmentCustomer >>= (.customerPerson) >>= (.personTags) & getNightSafetyCheckTag
  let toAddress = fulfillment.fulfillmentStops >>= Utils.getDropLocation >>= (.stopLocation) >>= Utils.parseAddress
  return $
    DConfirm.DConfirmReq
      { ..
      }

getNightSafetyCheckTag :: Maybe [Spec.TagGroup] -> Bool
getNightSafetyCheckTag tagGroups' = do
  maybe True getTagValue tagGroups'
  where
    getTagValue tagGroups = do
      let tagValue = Utils.getTagV2 "customer_info" "night_safety_check" tagGroups
          res = maybe (Just True) ((\val -> readMaybe val :: Maybe Bool) . T.unpack) tagValue
      fromMaybe True res
