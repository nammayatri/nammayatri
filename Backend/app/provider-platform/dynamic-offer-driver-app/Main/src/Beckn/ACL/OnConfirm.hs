{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnConfirm (buildOnConfirmMessageV2) where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import BecknV2.OnDemand.Enums
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.OnDemand.Utils.Payment
import qualified Data.List as L
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Confirm as DConfirm
import Domain.Types
import Domain.Types.BecknConfig as DBC
import Kernel.Prelude

bookingStatusCode :: DConfirm.ValidatedQuote -> Maybe Text
bookingStatusCode (DConfirm.DriverQuote _ _) = Nothing -- TODO: refactor it like so case match is not needed
bookingStatusCode (DConfirm.StaticQuote _) = Just "NEW"
bookingStatusCode (DConfirm.RideOtpQuote _) = Just "NEW"

buildOnConfirmMessageV2 :: DConfirm.DConfirmResp -> DBC.BecknConfig -> Spec.ConfirmReqMessage
buildOnConfirmMessageV2 res becknConfig = do
  let order = tfOrder res becknConfig
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = order
    }

tfOrder :: DConfirm.DConfirmResp -> DBC.BecknConfig -> Spec.Order
tfOrder res bppConfig = do
  let fulfillments = tfFulfillments res
      payments = tfPayments res bppConfig
  Spec.Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms = Just $ tfCancellationTerms bppConfig,
      orderFulfillments = fulfillments,
      orderId = Just res.booking.id.getId,
      orderItems = Utils.tfItems res.booking res.transporter.shortId.getShortId,
      orderPayments = payments,
      orderProvider = Nothing,
      orderQuote = Utils.tfQuotation res.booking,
      orderStatus = Just "ACTIVE"
    }

tfFulfillments :: DConfirm.DConfirmResp -> Maybe [Spec.Fulfillment]
tfFulfillments res = do
  let stops = Utils.mkStops' res.booking.fromLocation res.booking.toLocation res.booking.specialZoneOtpCode
  Just
    [ Spec.Fulfillment
        { fulfillmentAgent = Nothing,
          fulfillmentCustomer = tfCustomer res,
          fulfillmentId = Just res.booking.quoteId,
          fulfillmentState = mkFulfillmentState res.quoteType,
          fulfillmentStops = stops,
          fulfillmentTags = Nothing,
          fulfillmentType = Just $ Common.mkFulfillmentType res.booking.tripCategory,
          fulfillmentVehicle = tfVehicle res
        }
    ]
  where
    mkFulfillmentState quoteType = do
      fulfilState <- bookingStatusCode quoteType
      Just $
        Spec.FulfillmentState
          { fulfillmentStateDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just fulfilState,
                    descriptorShortDesc = Nothing,
                    descriptorName = Nothing
                  }
          }

-- TODO: Discuss payment info transmission with ONDC
tfPayments :: DConfirm.DConfirmResp -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments res bppConfig = do
  let amount = fromIntegral (res.booking.estimatedFare.getMoney)
  let mkParams :: (Maybe BknPaymentParams) = (readMaybe . T.unpack) =<< bppConfig.paymentParamsJson
  Just $ L.singleton $ mkPayment (show res.booking.bapCity) (show bppConfig.collectedBy) NOT_PAID (Just amount) Nothing mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee

tfVehicle :: DConfirm.DConfirmResp -> Maybe Spec.Vehicle
tfVehicle res = do
  let (category, variant) = Utils.castVariant res.booking.vehicleVariant
  Just
    Spec.Vehicle
      { vehicleCategory = Just category,
        vehicleVariant = Just variant,
        vehicleColor = Nothing,
        vehicleMake = Nothing,
        vehicleModel = Nothing,
        vehicleRegistration = Nothing
      }

tfCustomer :: DConfirm.DConfirmResp -> Maybe Spec.Customer
tfCustomer res =
  return $
    Spec.Customer
      { customerContact =
          Just
            Spec.Contact
              { contactPhone = Just res.riderPhoneNumber -- TODO: Check with ONDC how to pass country code
              },
        customerPerson = do
          riderName <- res.riderName
          Just $
            Spec.Person
              { personId = Nothing,
                personImage = Nothing,
                personName = Just riderName,
                personTags = Nothing
              }
      }

tfCancellationTerms :: DBC.BecknConfig -> [Spec.CancellationTerm]
tfCancellationTerms becknConfig =
  L.singleton
    Spec.CancellationTerm
      { cancellationTermCancellationFee = Utils.tfCancellationFee becknConfig.cancellationFeeAmount becknConfig.cancellationFeePercentage,
        cancellationTermFulfillmentState = Nothing,
        cancellationTermReasonRequired = Just False -- TODO : Make true if reason parsing is added
      }
