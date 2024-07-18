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
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601)
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified BecknV2.OnDemand.Utils.Payment as OUP
import BecknV2.Utils
import Control.Lens ((%~))
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnInit as DOnInit
import Domain.Types
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import EulerHS.Prelude hiding (id, state, (%~))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BecknConfig as QBC

buildConfirmReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], CacheFlow m r, EsqDBFlow m r) =>
  DOnInit.OnInitRes ->
  m Spec.ConfirmReq
buildConfirmReqV2 res = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle res.merchant.id "MOBILITY" (Utils.mapVariantToVehicle res.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
  ttl <- bapConfig.confirmTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  context <- ContextV2.buildContextV2 Context.CONFIRM Context.MOBILITY messageId (Just res.transactionId) res.merchant.bapId bapUrl (Just res.bppId) (Just res.bppUrl) res.city res.merchant.country (Just ttl)
  let message = mkConfirmMessageV2 res bapConfig
  pure $ Spec.ConfirmReq context message

mkConfirmMessageV2 :: DOnInit.OnInitRes -> DBC.BecknConfig -> Spec.ConfirmReqMessage
mkConfirmMessageV2 res bapConfig = do
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res bapConfig
    }

tfOrder :: DOnInit.OnInitRes -> DBC.BecknConfig -> Spec.Order
tfOrder res bapConfig = do
  Spec.Order
    { orderBilling = tfOrderBilling res,
      orderCancellation = Nothing,
      orderCancellationTerms = Nothing,
      orderFulfillments = tfFulfillments res,
      orderId = res.bppBookingId >>= Just . (.getId),
      orderItems = tfItems res,
      orderPayments = tfPayments res bapConfig,
      orderProvider = tfProvider res,
      orderQuote = tfQuotation res,
      orderStatus = Nothing,
      orderCreatedAt = Nothing,
      orderUpdatedAt = Nothing
    }

tfFulfillments :: DOnInit.OnInitRes -> Maybe [Spec.Fulfillment]
tfFulfillments res =
  Just
    [ Spec.Fulfillment
        { fulfillmentAgent = Nothing,
          fulfillmentCustomer = tfCustomer res,
          fulfillmentId = res.fulfillmentId,
          fulfillmentState = Nothing,
          fulfillmentStops = Utils.mkStops' (Just res.fromLocation) res.mbToLocation,
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
      DRB.AmbulanceDetails _ -> show Enums.AMBULANCE_FLOW
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
tfPayments :: DOnInit.OnInitRes -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments res bapConfig = do
  let mPrice = Just res.estimatedTotalFare
  let mkParams :: (Maybe BknPaymentParams) = decodeFromText =<< bapConfig.paymentParamsJson
  Just $ DL.singleton $ OUP.mkPayment (show res.city) (show bapConfig.collectedBy) Enums.NOT_PAID mPrice res.paymentId mkParams bapConfig.settlementType bapConfig.settlementWindow bapConfig.staticTermsUrl bapConfig.buyerFinderFee

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
        priceCurrency = Just $ show res.estimatedTotalFare.currency,
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText res.estimatedTotalFare.amount,
        priceValue = Just $ encodeToText res.estimatedFare.amount
      }

tfCustomer :: DOnInit.OnInitRes -> Maybe Spec.Customer
tfCustomer res =
  Just $
    Spec.Customer
      { customerContact = mkContact,
        customerPerson = mkPerson
      }
  where
    mkContact = do
      let trimCountryCode number = fromMaybe number (T.stripPrefix "+91" number)
      Just $
        Spec.Contact
          { contactPhone = Just $ trimCountryCode res.riderPhoneNumber
          -- handling of passing virtual number at on_init domain handler.
          }

    mkPerson = do
      return $
        Spec.Person
          { personId = Nothing,
            personImage = Nothing,
            personName = res.mbRiderName,
            personGender = Nothing,
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
                    { descriptorCode = Just $ show Tags.ENABLE_FREQUENT_LOCATION_UPDATES,
                      descriptorName = Just "Enable Frequent Location Updates",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = Just $ show res.enableFrequentLocationUpdates
            },
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.ENABLE_OTP_LESS_RIDE,
                      descriptorName = Just "Enable OTP Less Ride",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = Just $ show res.enableOtpLessRide
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
        vehicleRegistration = Nothing,
        vehicleCapacity = Nothing
      }

tfProvider :: DOnInit.OnInitRes -> Maybe Spec.Provider
tfProvider res =
  Just $
    Spec.Provider
      { providerId = Just res.bppId,
        providerItems = Nothing,
        providerLocations = Nothing,
        providerPayments = Nothing,
        providerDescriptor = Nothing,
        providerFulfillments = Nothing
      }

tfOrderBilling :: DOnInit.OnInitRes -> Maybe Spec.Billing
tfOrderBilling res =
  Just $
    Spec.Billing
      { billingPhone = Just $ maskNumber res.riderPhoneNumber,
        billingName = res.mbRiderName
      }
