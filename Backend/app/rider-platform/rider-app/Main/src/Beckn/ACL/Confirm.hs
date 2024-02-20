{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Confirm (buildConfirmReqV2) where

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Types.Booking as DRB
import EulerHS.Prelude hiding (id, state, (%~))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Utils.Common

buildConfirmReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DOnInit.OnInitRes ->
  m Spec.ConfirmReq
buildConfirmReqV2 res = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- ContextV2.buildContextV2 Context.CONFIRM Context.MOBILITY messageId (Just res.transactionId) res.merchant.bapId bapUrl (Just res.bppId) (Just res.bppUrl) res.city res.merchant.country

  let message = mkConfirmMessageV2 res
  pure $ Spec.ConfirmReq context message

mkConfirmMessageV2 :: DOnInit.OnInitRes -> Spec.ConfirmReqMessage
mkConfirmMessageV2 res = do
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res
    }

tfOrder :: DOnInit.OnInitRes -> Spec.Order
tfOrder res =
  Spec.Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms = Nothing,
      orderFulfillments = tfFulfillments res,
      orderId = Just res.bppBookingId.getId,
      orderItems = tfItems res,
      orderPayments = tfPayments res,
      orderProvider = Nothing,
      orderQuote = tfQuotation res,
      orderStatus = Nothing
    }

tfFulfillments :: DOnInit.OnInitRes -> Maybe [Spec.Fulfillment]
tfFulfillments res =
  Just
    [ Spec.Fulfillment
        { fulfillmentAgent = Nothing,
          fulfillmentCustomer = tfCustomer res,
          fulfillmentId = res.fulfillmentId,
          fulfillmentState = Nothing,
          fulfillmentStops = Utils.mkStops' res.fromLocation res.mbToLocation,
          fulfillmentTags = Nothing,
          fulfillmentType = Just $ mkFulfillmentType res.bookingDetails,
          fulfillmentVehicle = tfVehicle res
        }
    ]
  where
    mkFulfillmentType = \case
      DRB.OneWaySpecialZoneDetails _ -> show Enums.RIDE_OTP
      DRB.RentalDetails _ -> show Enums.RENTAL
      DRB.InterCityDetails _ -> show Enums.INTER_CITY
      _ -> show Enums.DELIVERY

tfItems :: DOnInit.OnInitRes -> Maybe [Spec.Item]
tfItems res =
  Just
    [ Spec.Item
        { itemDescriptor = Nothing,
          itemFulfillmentIds = Nothing,
          itemId = Just res.itemId,
          itemLocationIds = Nothing,
          itemPaymentIds = Nothing,
          itemPrice = Nothing,
          itemTags = Nothing
        }
    ]

-- TODO: Discuss payment info transmission with ONDC
tfPayments :: DOnInit.OnInitRes -> Maybe [Spec.Payment]
tfPayments res =
  Just
    [ Spec.Payment
        { paymentCollectedBy = Just $ show Enums.SELLER,
          paymentId = Nothing,
          paymentParams = mkParams,
          paymentStatus = Nothing,
          paymentTags = Nothing,
          paymentType = Just $ show Enums.ON_FULFILLMENT
        }
    ]
  where
    mkParams =
      Just $
        Spec.PaymentParams
          { paymentParamsAmount = Just $ encodeToText res.estimatedTotalFare,
            paymentParamsBankAccountNumber = Nothing,
            paymentParamsBankCode = Nothing,
            paymentParamsCurrency = Just "INR",
            paymentParamsVirtualPaymentAddress = Nothing
          }

tfQuotation :: DOnInit.OnInitRes -> Maybe Spec.Quotation
tfQuotation res =
  Just $
    Spec.Quotation
      { quotationBreakup = Nothing,
        quotationPrice = tfQuotationPrice res,
        quotationTtl = Nothing
      }

tfQuotationPrice :: DOnInit.OnInitRes -> Maybe Spec.Price
tfQuotationPrice res =
  Just $
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText res.estimatedTotalFare,
        priceValue = Just $ encodeToText res.estimatedFare
      }

tfCustomer :: DOnInit.OnInitRes -> Maybe Spec.Customer
tfCustomer res =
  Just $
    Spec.Customer
      { customerContact = mkContact,
        customerPerson = mkPerson
      }
  where
    mkContact =
      Just $
        Spec.Contact
          { contactPhone = Just res.riderPhoneNumber
          -- handling of passing virtual number at on_init domain handler.
          }

    mkPerson = do
      riderName <- res.mbRiderName
      return $
        Spec.Person
          { personId = Nothing,
            personImage = Nothing,
            personName = Just riderName,
            personTags = mkPersonTags
          }

    mkPersonTags
      | not res.isValueAddNP = Nothing
      | otherwise =
        Just
          [ Spec.TagGroup
              { tagGroupDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.CUSTOMER_INFO,
                        descriptorName = Just "Customer Information",
                        descriptorShortDesc = Nothing
                      },
                tagGroupDisplay = Just False,
                tagGroupList = mkPersonTag
              }
          ]

    mkPersonTag =
      Just
        [ Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.NIGHT_SAFETY_CHECK,
                      descriptorName = Just "Night Safety Check",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = Just $ show res.nightSafetyCheck
            },
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "enable_frequent_location_updates",
                      descriptorName = Just "Enable Frequent Location Updates",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = Just $ show res.enableFrequentLocationUpdates
            }
        ]

tfVehicle :: DOnInit.OnInitRes -> Maybe Spec.Vehicle
tfVehicle res = do
  let (category, variant) = Utils.castVehicleVariant res.vehicleVariant
  Just $
    Spec.Vehicle
      { vehicleCategory = Just category,
        vehicleVariant = Just variant,
        vehicleColor = Nothing,
        vehicleMake = Nothing,
        vehicleModel = Nothing,
        vehicleRegistration = Nothing
      }
