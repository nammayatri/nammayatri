{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnInit where

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as UtilsV2
import BecknV2.OnDemand.Utils.Constructors
import BecknV2.OnDemand.Utils.Payment
import qualified Data.List as L
import Domain.Action.Beckn.Init as DInit
import Domain.Types
import Domain.Types.BecknConfig as DBC
import qualified Domain.Types.FarePolicy as FarePolicyD
import Kernel.Prelude
import Kernel.Utils.Common

mkOnInitMessageV2 :: DInit.InitRes -> DBC.BecknConfig -> Maybe FarePolicyD.FullFarePolicy -> Maybe Text -> Spec.ConfirmReqMessage
mkOnInitMessageV2 res becknConfig mbFarePolicy mbPolyline =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res becknConfig mbFarePolicy mbPolyline
    }

tfOrder :: DInit.InitRes -> DBC.BecknConfig -> Maybe FarePolicyD.FullFarePolicy -> Maybe Text -> Spec.Order
tfOrder res becknConfig mbFarePolicy mbPolyline = do
  let farePolicy = case mbFarePolicy of
        Nothing -> Nothing
        Just fullFarePolicy -> Just $ FarePolicyD.fullFarePolicyToFarePolicy fullFarePolicy
  Spec.Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms = Just $ Utils.tfCancellationTerms Nothing (Just Enums.RIDE_ASSIGNED),
      orderFulfillments = tfFulfillments res mbPolyline,
      orderId = Just res.booking.id.getId,
      orderItems = Utils.tfItems res.booking res.transporter.shortId.getShortId Nothing farePolicy (Just res.paymentId),
      orderPayments = tfPayments res becknConfig,
      orderProvider = Utils.tfProvider becknConfig,
      orderQuote = Utils.tfQuotation res.booking,
      orderStatus = Nothing,
      orderTags = Utils.mkBppTermsTagsWithAmount (Just res.booking.estimatedFare) becknConfig,
      orderCreatedAt = Just res.booking.createdAt,
      orderUpdatedAt = Just res.booking.updatedAt
    }

tfFulfillments :: DInit.InitRes -> Maybe Text -> Maybe [Spec.Fulfillment]
tfFulfillments res mbPolyline =
  Just
    [ emptyFulfillment
        { Spec.fulfillmentCustomer = tfCustomer res,
          Spec.fulfillmentId = Just $ Utils.getBookingFulfillmentId res.booking,
          Spec.fulfillmentStops = Utils.mkStops' res.booking.fromLocation res.booking.toLocation res.booking.stops Nothing,
          Spec.fulfillmentType = Just $ UtilsV2.tripCategoryToFulfillmentType res.booking.tripCategory,
          Spec.fulfillmentVehicle = tfVehicle res,
          Spec.fulfillmentTags = Utils.mkRouteInfoTagsFromPolyline mbPolyline
        }
    ]

-- TODO: Discuss payment info transmission with ONDC
tfPayments :: DInit.InitRes -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments res bppConfig = do
  let mPrice = Just $ mkPrice (Just res.booking.currency) res.booking.estimatedFare
  let mkParams :: (Maybe BknPaymentParams) = decodeFromText =<< bppConfig.paymentParamsJson
  Just . L.singleton $ mkPayment (show res.transporter.city) (show bppConfig.collectedBy) Enums.NOT_PAID mPrice (Just res.paymentId) mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee False Nothing Nothing

tfVehicle :: DInit.InitRes -> Maybe Spec.Vehicle
tfVehicle res = do
  let (category, variant) = Utils.castVariant res.vehicleVariant
  Just
    emptyVehicle
      { Spec.vehicleCategory = Just category,
        Spec.vehicleVariant = Just variant
      }


tfCustomer :: DInit.InitRes -> Maybe Spec.Customer
tfCustomer res =
  return $
    Spec.Customer
      { customerContact =
          Just Spec.Contact {contactPhone = Just res.riderPhoneNumber},
        customerPerson = do
          riderName <- res.riderName
          Just $ emptyPerson {Spec.personName = Just riderName}
      }
