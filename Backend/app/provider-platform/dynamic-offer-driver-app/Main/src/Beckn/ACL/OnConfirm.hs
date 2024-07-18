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
import qualified BecknV2.OnDemand.Enums as Enum
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.OnDemand.Utils.Payment
import qualified Data.List as L
import qualified Domain.Action.Beckn.Confirm as DConfirm
import Domain.Types
import Domain.Types.BecknConfig as DBC
import qualified Domain.Types.FarePolicy as FarePolicyD
import Kernel.Prelude
import Kernel.Utils.Common

bookingStatusCode :: DConfirm.ValidatedQuote -> Maybe Enum.FulfillmentState
bookingStatusCode (DConfirm.DriverQuote _ _) = Just Enum.RIDE_ASSIGNED
bookingStatusCode _ = Just Enum.NEW

buildOnConfirmMessageV2 :: DConfirm.DConfirmResp -> Utils.Pricing -> DBC.BecknConfig -> Maybe FarePolicyD.FullFarePolicy -> Spec.ConfirmReqMessage
buildOnConfirmMessageV2 res pricing becknConfig mbFarePolicy = do
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res pricing becknConfig mbFarePolicy
    }

tfOrder :: DConfirm.DConfirmResp -> Utils.Pricing -> DBC.BecknConfig -> Maybe FarePolicyD.FullFarePolicy -> Spec.Order
tfOrder res pricing bppConfig mbFarePolicy = do
  let farePolicy = case mbFarePolicy of
        Nothing -> Nothing
        Just fullFarePolicy -> Just $ FarePolicyD.fullFarePolicyToFarePolicy fullFarePolicy
  Spec.Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms = Just $ tfCancellationTerms res,
      orderFulfillments = tfFulfillments res,
      orderId = Just res.booking.id.getId,
      orderItems = Utils.tfItems res.booking res.transporter.shortId.getShortId pricing.estimatedDistance farePolicy res.paymentId,
      orderPayments = tfPayments res bppConfig,
      orderProvider = Utils.tfProvider bppConfig,
      orderQuote = Utils.tfQuotation res.booking,
      orderStatus = Just "ACTIVE",
      orderCreatedAt = Just res.booking.createdAt,
      orderUpdatedAt = Just res.booking.updatedAt
    }

tfFulfillments :: DConfirm.DConfirmResp -> Maybe [Spec.Fulfillment]
tfFulfillments res =
  Just
    [ Spec.Fulfillment
        { fulfillmentAgent = tfAgent res,
          fulfillmentCustomer = tfCustomer res,
          fulfillmentId = Just res.booking.quoteId,
          fulfillmentState = Utils.mkFulfillmentState <$> bookingStatusCode res.quoteType,
          fulfillmentStops = Utils.mkStops' res.booking.fromLocation res.booking.toLocation res.booking.specialZoneOtpCode,
          fulfillmentTags = Nothing,
          fulfillmentType = Just $ Common.mkFulfillmentType res.booking.tripCategory,
          fulfillmentVehicle = tfVehicle res
        }
    ]

-- TODO: Discuss payment info transmission with ONDC
tfPayments :: DConfirm.DConfirmResp -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments res bppConfig = do
  let mPrice = Just $ mkPrice (Just res.booking.currency) res.booking.estimatedFare
  let mkParams :: Maybe BknPaymentParams = decodeFromText =<< bppConfig.paymentParamsJson
  Just . L.singleton $ mkPayment (show res.booking.bapCity) (show bppConfig.collectedBy) NOT_PAID mPrice res.paymentId mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee

tfVehicle :: DConfirm.DConfirmResp -> Maybe Spec.Vehicle
tfVehicle res = do
  let (category, variant) = Utils.castVariant res.vehicleVariant
  Just
    Spec.Vehicle
      { vehicleCategory = Just category,
        vehicleVariant = Just variant,
        vehicleColor = Nothing,
        vehicleMake = Nothing,
        vehicleModel = Nothing,
        vehicleRegistration = Nothing,
        vehicleCapacity = Nothing
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
                personGender = Nothing,
                personTags = Nothing
              }
      }

tfCancellationTerms :: DConfirm.DConfirmResp -> [Spec.CancellationTerm]
tfCancellationTerms res =
  L.singleton
    Spec.CancellationTerm
      { cancellationTermCancellationFee = Utils.tfCancellationFee res.cancellationFee,
        cancellationTermFulfillmentState = Utils.mkFulfillmentState <$> bookingStatusCode res.quoteType,
        cancellationTermReasonRequired = Just False -- TODO : Make true if reason parsing is added
      }

tfAgent :: DConfirm.DConfirmResp -> Maybe Spec.Agent
tfAgent res =
  case res.rideInfo of
    Just rideInfo -> do
      let driverName = maybe (Just rideInfo.driver.firstName) (\ln -> Just rideInfo.driver.firstName <> Just " " <> Just ln) rideInfo.driver.lastName
      return $
        Spec.Agent
          { agentContact = Nothing,
            agentPerson =
              Just
                Spec.Person
                  { personId = Nothing,
                    personImage = Nothing,
                    personName = driverName,
                    personGender = Nothing,
                    personTags = Nothing
                  }
          }
    Nothing -> Nothing
