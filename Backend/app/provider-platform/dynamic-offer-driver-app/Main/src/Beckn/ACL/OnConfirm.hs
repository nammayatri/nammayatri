{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnConfirm (buildOnConfirmMessageV2, BPPInvoiceInfo (..), emptyBPPInvoiceInfo, resolveBPPInvoiceInfo) where

import qualified Beckn.OnDemand.Utils.Common as Utils
import BecknV2.OnDemand.Enums
import qualified BecknV2.OnDemand.Enums as Enum
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as UtilsV2
import BecknV2.OnDemand.Utils.Constructors
import BecknV2.OnDemand.Utils.Payment
import Control.Applicative ((<|>))
import qualified Data.List as L
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Confirm as DConfirm
import Domain.Types
import Domain.Types.BecknConfig as DBC
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.Person as DP
import Environment (Flow)
import qualified Kernel.External.Payment.Stripe.Types as Stripe
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOwnerInformation as QFOI

data BPPInvoiceInfo = BPPInvoiceInfo
  { issuedById :: Maybe Text,
    issuedByName :: Maybe Text,
    issuedByAddress :: Maybe Text,
    supplierName :: Maybe Text,
    supplierAddress :: Maybe Text,
    supplierGSTIN :: Maybe Text,
    supplierTaxNo :: Maybe Text,
    supplierId :: Maybe Text
  }

emptyBPPInvoiceInfo :: BPPInvoiceInfo
emptyBPPInvoiceInfo = BPPInvoiceInfo Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

formatStripeAddress :: Stripe.Address -> Text
formatStripeAddress addr =
  T.intercalate ", " $ catMaybes [addr.line1, addr.line2, addr.city, addr.state, addr.postal_code, addr.country]

-- | Resolve the BPP invoice/provider details propagated to BAP at on_confirm.
--   If the booking has an assigned driver associated with a fleet, supplier
--   fields come from FleetOwnerInformation; otherwise they fall back to the
--   transporter merchant.  issuedBy* always reflects the merchant.
resolveBPPInvoiceInfo :: DConfirm.DConfirmResp -> Flow BPPInvoiceInfo
resolveBPPInvoiceInfo res = do
  let merchant = res.transporter
      mid = merchant.id.getId
      mName = merchant.name
  mbMerchantOpCity <- CQMOC.findById res.booking.merchantOperatingCityId
  let merchantAddress =
        mbMerchantOpCity <&> \city ->
          show city.city <> ", " <> show city.state <> ", " <> show city.country
  mbFleetInfo <- case res.rideInfo of
    Just rideInfo -> do
      mFleetAssoc <- QFDA.findByDriverId rideInfo.driver.id True
      case mFleetAssoc of
        Just assoc -> QFOI.findByPrimaryKey (Id assoc.fleetOwnerId :: Id DP.Person)
        Nothing -> pure Nothing
    Nothing -> pure Nothing
  pure $ case mbFleetInfo of
    Just fleetInfo ->
      BPPInvoiceInfo
        { issuedById = Just mid,
          issuedByName = Just mName,
          issuedByAddress = merchantAddress,
          supplierName = fleetInfo.fleetName,
          supplierAddress = fleetInfo.stripeAddress <&> formatStripeAddress,
          supplierGSTIN = fleetInfo.gstNumberDec,
          supplierTaxNo = fleetInfo.vatNumber <|> fleetInfo.gstNumberDec,
          supplierId = Just fleetInfo.fleetOwnerPersonId.getId
        }
    Nothing ->
      BPPInvoiceInfo
        { issuedById = Just mid,
          issuedByName = Just mName,
          issuedByAddress = merchantAddress,
          supplierName = Just mName,
          supplierAddress = merchantAddress,
          supplierGSTIN = merchant.gstin,
          supplierTaxNo = merchant.vatNumber <|> merchant.gstin,
          supplierId = Just mid
        }

mkBPPInvoiceInfoTagGroup :: BPPInvoiceInfo -> Maybe [Spec.TagGroup]
mkBPPInvoiceInfoTagGroup info =
  Tags.convertToTagGroup
    [ (Tags.ISSUED_BY_ID, info.issuedById),
      (Tags.ISSUED_BY_NAME, info.issuedByName),
      (Tags.ISSUED_BY_ADDRESS, info.issuedByAddress),
      (Tags.SUPPLIER_NAME, info.supplierName),
      (Tags.SUPPLIER_ADDRESS, info.supplierAddress),
      (Tags.SUPPLIER_GSTIN, info.supplierGSTIN),
      (Tags.SUPPLIER_TAX_NO, info.supplierTaxNo),
      (Tags.SUPPLIER_ID, info.supplierId)
    ]

bookingStatusCode :: DConfirm.ValidatedQuote -> Maybe Enum.FulfillmentState
bookingStatusCode (DConfirm.DriverQuote _ _) = Just Enum.RIDE_ASSIGNED -- ONDC v2.1.0: phased confirmation, driver details sent via on_update RIDE_ASSIGNED
bookingStatusCode _ = Just Enum.NEW

buildOnConfirmMessageV2 :: DConfirm.DConfirmResp -> Utils.Pricing -> DBC.BecknConfig -> Maybe FarePolicyD.FullFarePolicy -> BPPInvoiceInfo -> Spec.ConfirmReqMessage
buildOnConfirmMessageV2 res pricing becknConfig mbFarePolicy bppInvoiceInfo =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res pricing becknConfig mbFarePolicy bppInvoiceInfo
    }

tfOrder :: DConfirm.DConfirmResp -> Utils.Pricing -> DBC.BecknConfig -> Maybe FarePolicyD.FullFarePolicy -> BPPInvoiceInfo -> Spec.Order
tfOrder res pricing bppConfig mbFarePolicy bppInvoiceInfo = do
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
      orderTags = mkBPPInvoiceInfoTagGroup bppInvoiceInfo,
      orderStatus = Just "ACTIVE",
      orderCreatedAt = Just res.booking.createdAt,
      orderUpdatedAt = Just res.booking.updatedAt
    }

tfFulfillments :: DConfirm.DConfirmResp -> Maybe [Spec.Fulfillment]
tfFulfillments res =
  Just
    [ emptyFulfillment
        { Spec.fulfillmentCustomer = tfCustomer res,
          Spec.fulfillmentId = Just res.booking.quoteId,
          Spec.fulfillmentState = Utils.mkFulfillmentState <$> bookingStatusCode res.quoteType,
          Spec.fulfillmentStops = Utils.mkStops' res.booking.fromLocation res.booking.toLocation res.booking.stops res.booking.specialZoneOtpCode Nothing Nothing Nothing,
          Spec.fulfillmentType = Just $ UtilsV2.tripCategoryToFulfillmentType res.booking.tripCategory,
          Spec.fulfillmentVehicle = tfVehicle res
        }
    ]

-- TODO: Discuss payment info transmission with ONDC
tfPayments :: DConfirm.DConfirmResp -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments res bppConfig = do
  let mPrice = Just $ mkPrice (Just res.booking.currency) res.booking.estimatedFare
  let mkParams :: Maybe BknPaymentParams = decodeFromText =<< bppConfig.paymentParamsJson
  Just . L.singleton $ mkPayment (show res.booking.bapCity) (show bppConfig.collectedBy) NOT_PAID mPrice res.paymentId mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee False Nothing Nothing

tfVehicle :: DConfirm.DConfirmResp -> Maybe Spec.Vehicle
tfVehicle res = do
  let (category, variant) = Utils.castVariant res.vehicleVariant
  Just
    emptyVehicle
      { Spec.vehicleCategory = Just category,
        Spec.vehicleVariant = Just variant
      }

tfCustomer :: DConfirm.DConfirmResp -> Maybe Spec.Customer
tfCustomer res =
  return $
    Spec.Customer
      { customerContact =
          Just Spec.Contact {contactPhone = Just res.riderPhoneNumber},
        customerPerson = do
          riderName <- res.riderName
          Just $ emptyPerson {Spec.personName = Just riderName}
      }

tfCancellationTerms :: DConfirm.DConfirmResp -> [Spec.CancellationTerm]
tfCancellationTerms res =
  L.singleton
    Spec.CancellationTerm
      { cancellationTermCancellationFee = Utils.tfCancellationFee res.cancellationFee,
        cancellationTermFulfillmentState = Utils.mkFulfillmentState <$> bookingStatusCode res.quoteType,
        cancellationTermReasonRequired = Just False -- TODO : Make true if reason parsing is added
      }
