{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnCancel
  ( buildOnCancelMessageV2,
    module Reexport,
  )
where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as BUtils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601)
import qualified BecknV2.OnDemand.Utils.Context as CU
import BecknV2.OnDemand.Utils.Payment
import qualified Data.List as L
import qualified Domain.Action.UI.Person as DP
import Domain.Types
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.Merchant as DM
import Domain.Types.OnCancel as Reexport
import qualified Domain.Types.OnCancel as OC
import Domain.Types.Ride
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (mkPrice)
import qualified Kernel.Utils.Common as Common (mkPrice)
import SharedLogic.FareCalculator
import qualified SharedLogic.FarePolicy as SFP
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error

buildOnCancelMessageV2 ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  DM.Merchant ->
  Maybe Context.City ->
  Maybe Context.Country ->
  Text ->
  OnCancelBuildReq ->
  Maybe Text ->
  m Spec.OnCancelReq
buildOnCancelMessageV2 merchant mbBapCity mbBapCountry cancelStatus (OC.BookingCancelledBuildReqV2 OC.DBookingCancelledReqV2 {..}) mbMsgId = do
  msgId <- maybe generateGUID return mbMsgId
  let bppId = getShortId $ merchant.subscriberId
      city = fromMaybe merchant.city mbBapCity
      country = fromMaybe merchant.country mbBapCountry
  bppUri <- BUtils.mkBppUri merchant.id.getId
  riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "rider_id")
  mbRide <- QRide.findOneByBookingId booking.id
  riderDetails <- runInReplica $ QRiderDetails.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  customerPhoneNo <- decrypt riderDetails.mobileNumber
  let vehicleCategory = BUtils.mapServiceTierToCategory booking.vehicleServiceTier
  mbVehicle <- maybe (pure Nothing) (runInReplica . QVeh.findById . (.driverId)) mbRide
  mbPerson <- maybe (pure Nothing) (runInReplica . QPerson.findById . (.driverId)) mbRide
  mbFarePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback booking.quoteId
  becknConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id (show Context.MOBILITY) vehicleCategory >>= fromMaybeM (InternalError "Beckn Config not found")
  let driverName = DP.getPersonFullName =<< mbPerson
  driverPhone <- maybe (pure Nothing) DP.getPersonNumber mbPerson
  buildOnCancelReq Context.ON_CANCEL Context.MOBILITY msgId bppId bppUri city country cancelStatus merchant driverName customerPhoneNo (OC.BookingCancelledBuildReqV2 OC.DBookingCancelledReqV2 {..}) (mbRide <&> (.status)) becknConfig mbVehicle mbFarePolicy driverPhone

buildOnCancelReq ::
  (MonadFlow m, EncFlow m r) =>
  Context.Action ->
  Context.Domain ->
  Text ->
  Text ->
  BaseUrl ->
  Context.City ->
  Context.Country ->
  Text ->
  DM.Merchant ->
  Maybe Text ->
  Text ->
  OC.OnCancelBuildReq ->
  Maybe RideStatus ->
  DBC.BecknConfig ->
  Maybe DVeh.Vehicle ->
  Maybe FarePolicyD.FullFarePolicy ->
  Maybe Text ->
  m Spec.OnCancelReq
buildOnCancelReq action domain messageId bppSubscriberId bppUri city country cancelStatus merchant driverName customerPhoneNo (OC.BookingCancelledBuildReqV2 OC.DBookingCancelledReqV2 {..}) rideStatus becknConfig mbVehicle mbFarePolicy driverPhone = do
  ttl <- becknConfig.onCancelTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country (Just ttl)
  pure $
    Spec.OnCancelReq
      { onCancelReqError = Nothing,
        onCancelReqContext = context,
        onCancelReqMessage = buildOnCancelMessageReqV2 booking cancelStatus cancellationSource cancellationFee merchant driverName customerPhoneNo becknConfig rideStatus mbVehicle mbFarePolicy driverPhone
      }

buildOnCancelMessageReqV2 :: DRB.Booking -> Text -> SBCR.CancellationSource -> Maybe PriceAPIEntity -> DM.Merchant -> Maybe Text -> Text -> DBC.BecknConfig -> Maybe RideStatus -> Maybe DVeh.Vehicle -> Maybe FarePolicyD.FullFarePolicy -> Maybe Text -> Maybe Spec.ConfirmReqMessage
buildOnCancelMessageReqV2 booking cancelStatus cancellationSource cancellationFee merchant driverName customerPhoneNo becknConfig rideStatus mbVehicle mbFarePolicy driverPhone = do
  Just $
    Spec.ConfirmReqMessage
      { confirmReqMessageOrder = tfOrder booking cancelStatus cancellationSource cancellationFee merchant driverName customerPhoneNo becknConfig rideStatus mbVehicle mbFarePolicy driverPhone
      }

tfOrder :: DRB.Booking -> Text -> SBCR.CancellationSource -> Maybe PriceAPIEntity -> DM.Merchant -> Maybe Text -> Text -> DBC.BecknConfig -> Maybe RideStatus -> Maybe DVeh.Vehicle -> Maybe FarePolicyD.FullFarePolicy -> Maybe Text -> Spec.Order
tfOrder booking cancelStatus cancellationSource cancellationFee merchant driverName customerPhoneNo becknConfig rideStatus mbVehicle mbFarePolicy driverPhone = do
  Spec.Order
    { orderId = Just booking.id.getId,
      orderStatus = Just cancelStatus,
      orderFulfillments = tfFulfillments booking driverName customerPhoneNo rideStatus mbVehicle driverPhone,
      orderCancellation = tfCancellation cancellationSource,
      orderBilling = Nothing,
      orderCancellationTerms = Just $ tfCancellationTerms cancellationFee,
      orderItems = tfItems booking merchant mbFarePolicy,
      orderPayments = tfPayments (Common.mkPrice (Just booking.currency) booking.estimatedFare) merchant becknConfig booking.paymentId,
      orderProvider = BUtils.tfProvider becknConfig,
      orderQuote = tfQuotation booking,
      orderCreatedAt = Just booking.createdAt,
      orderUpdatedAt = Just booking.updatedAt
    }

tfFulfillments :: DRB.Booking -> Maybe Text -> Text -> Maybe RideStatus -> Maybe DVeh.Vehicle -> Maybe Text -> Maybe [Spec.Fulfillment]
tfFulfillments booking driverName customerPhoneNo rideStatus mbVehicle driverPhone = do
  let stops = BUtils.mkStops' booking.fromLocation booking.toLocation booking.specialZoneOtpCode
  Just
    [ Spec.Fulfillment
        { fulfillmentId = Just booking.quoteId,
          fulfillmentState = mkFulfillmentState rideStatus,
          fulfillmentStops = stops,
          fulfillmentType = Just $ BUtils.mkFulfillmentType booking.tripCategory,
          fulfillmentAgent = tfAgent booking driverName driverPhone,
          fulfillmentCustomer = tfCustomer booking customerPhoneNo,
          fulfillmentTags = Nothing,
          fulfillmentVehicle = tfVehicle mbVehicle
        }
    ]
  where
    mkFulfillmentState rideStatus' =
      Just $
        Spec.FulfillmentState
          { fulfillmentStateDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = (Just . show . BUtils.mapRideStatus) rideStatus',
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  }
          }

tfQuotation :: DRB.Booking -> Maybe Spec.Quotation
tfQuotation booking =
  Just
    Spec.Quotation
      { quotationBreakup = mkQuotationBreakup booking,
        quotationPrice = tfQuotationPrice booking,
        quotationTtl = Nothing
      }

tfQuotationPrice :: DRB.Booking -> Maybe Spec.Price
tfQuotationPrice booking =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just $ show booking.currency,
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText booking.estimatedFare,
        priceValue = Just $ encodeToText booking.estimatedFare
      }

mkQuotationBreakup :: DRB.Booking -> Maybe [Spec.QuotationBreakupInner]
mkQuotationBreakup booking =
  -- TODO::Beckn, `quotationBreakupInnerTitle` may not be according to spec.
  Just $
    mkFareParamsBreakups mkPrice mkQuotationBreakupInner booking.fareParams
  where
    mkPrice money =
      Just
        Spec.Price
          { priceComputedValue = Nothing,
            priceCurrency = Just $ show booking.currency,
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Nothing,
            priceValue = Just $ encodeToText money
          }

    mkQuotationBreakupInner title price =
      Spec.QuotationBreakupInner
        { quotationBreakupInnerPrice = price,
          quotationBreakupInnerTitle = Just title
        }

tfPayments :: Price -> DM.Merchant -> DBC.BecknConfig -> Maybe Text -> Maybe [Spec.Payment]
tfPayments estimatedFare merchant bppConfig mbPaymentId = do
  let mPrice = Just estimatedFare
  let mkParams :: Maybe BknPaymentParams = decodeFromText =<< bppConfig.paymentParamsJson
  Just . L.singleton $ mkPayment (show merchant.city) (show bppConfig.collectedBy) Enums.NOT_PAID mPrice mbPaymentId mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee

tfItems :: DRB.Booking -> DM.Merchant -> Maybe FarePolicyD.FullFarePolicy -> Maybe [Spec.Item]
tfItems booking merchant mbFarePolicy =
  Just
    [ Spec.Item
        { itemDescriptor = BUtils.tfItemDescriptor booking,
          itemFulfillmentIds = Just [booking.quoteId],
          itemId = Just $ maybe (Common.mkItemId merchant.shortId.getShortId booking.vehicleServiceTier) getId (booking.estimateId),
          itemLocationIds = Nothing,
          itemPaymentIds = tfPaymentId booking.paymentId,
          itemPrice = tfItemPrice booking,
          itemTags = BUtils.mkRateCardTag Nothing Nothing . Just . FarePolicyD.fullFarePolicyToFarePolicy =<< mbFarePolicy
        }
    ]

tfPaymentId :: Maybe Text -> Maybe [Text]
tfPaymentId mbPaymentId = do
  paymentId <- mbPaymentId
  Just [paymentId]

tfItemPrice :: DRB.Booking -> Maybe Spec.Price
tfItemPrice booking =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just $ show booking.currency,
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Nothing,
        priceValue = Just $ encodeToText booking.estimatedFare
      }

tfVehicle :: Maybe DVeh.Vehicle -> Maybe Spec.Vehicle
tfVehicle mbVehicle =
  mbVehicle >>= \vehicle -> do
    let (category, variant) = BUtils.castVariant vehicle.variant
    Just $
      Spec.Vehicle
        { vehicleColor = Just vehicle.color,
          vehicleModel = Just vehicle.model,
          vehicleRegistration = Just vehicle.registrationNo,
          vehicleCategory = Just category,
          vehicleVariant = Just variant,
          vehicleMake = Nothing,
          vehicleCapacity = Nothing
        }

tfAgent :: DRB.Booking -> Maybe Text -> Maybe Text -> Maybe Spec.Agent
tfAgent _booking driverName driverPhone = do
  Just $
    Spec.Agent
      { agentContact = Common.tfContact driverPhone,
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

tfCustomer :: DRB.Booking -> Text -> Maybe Spec.Customer
tfCustomer booking customerPhoneNo = do
  Just $
    Spec.Customer
      { customerContact =
          Just
            Spec.Contact
              { contactPhone = Just customerPhoneNo
              },
        customerPerson =
          Just
            Spec.Person
              { personId = Nothing,
                personImage = Nothing,
                personName = booking.riderName,
                personGender = Nothing,
                personTags = Nothing
              }
      }

tfCancellation :: SBCR.CancellationSource -> Maybe Spec.Cancellation
tfCancellation cancellationSource =
  Just $
    Spec.Cancellation
      { cancellationCancelledBy = castCancellatonSource cancellationSource
      }
  where
    castCancellatonSource = \case
      SBCR.ByUser -> Just (show Enums.CONSUMER)
      SBCR.ByDriver -> Just (show Enums.PROVIDER)
      _ -> Just (show Enums.PROVIDER) -- if it is cancelled by any other source like by ByMerchant, ByAllocator or ByApplication then we are considering as ByProvider

tfCancellationTerms :: Maybe PriceAPIEntity -> [Spec.CancellationTerm]
tfCancellationTerms cancellationFee =
  L.singleton
    Spec.CancellationTerm
      { cancellationTermCancellationFee = BUtils.tfCancellationFee cancellationFee,
        cancellationTermFulfillmentState = BUtils.tfFulfillmentState Enums.RIDE_CANCELLED,
        cancellationTermReasonRequired = Just False -- TODO : Make true if reason parsing is added
      }
