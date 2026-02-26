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
import BecknV2.OnDemand.Utils.Constructors
import qualified BecknV2.OnDemand.Utils.Common as Utils
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
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

buildConfirmReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], CacheFlow m r, EsqDBFlow m r) =>
  DOnInit.OnInitRes ->
  m Spec.ConfirmReq
buildConfirmReqV2 res = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  moc <- CQMOC.findByMerchantIdAndCity res.merchant.id res.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> res.merchant.id.getId <> "-city-" <> show res.city)
  bapConfigs <- QBC.findByMerchantIdDomainandMerchantOperatingCityId res.merchant.id "MOBILITY" moc.id
  bapConfig <- listToMaybe bapConfigs & fromMaybeM (InvalidRequest $ "BecknConfig not found for merchantId " <> show res.merchant.id.getId <> " merchantOperatingCityId " <> show moc.id.getId) -- Using findAll for backward compatibility, TODO : Remove findAll and use findOne
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
  emptyOrder
    { Spec.orderBilling = tfOrderBilling res,
      Spec.orderCancellationTerms = Nothing,
      Spec.orderFulfillments = tfFulfillments res,
      Spec.orderId = res.bppBookingId >>= Just . (.getId),
      Spec.orderItems = tfItems res,
      Spec.orderPayments = tfPayments res bapConfig,
      Spec.orderProvider = tfProvider res,
      Spec.orderQuote = tfQuotation res
    }

tfFulfillments :: DOnInit.OnInitRes -> Maybe [Spec.Fulfillment]
tfFulfillments res =
  Just
    [ emptyFulfillment
        { Spec.fulfillmentCustomer = tfCustomer res,
          Spec.fulfillmentId = res.fulfillmentId,
          Spec.fulfillmentStops = Utils.mkStops' (Just res.fromLocation) stops res.mbToLocation,
          Spec.fulfillmentType = Utils.tripCategoryToFulfillmentType <$> res.tripCategory,
          Spec.fulfillmentVehicle = tfVehicle res
        }
    ]
  where
    stops = case res.bookingDetails of
      DRB.OneWayDetails details -> details.stops
      DRB.OneWaySpecialZoneDetails details -> details.stops
      _ -> []

tfItems :: DOnInit.OnInitRes -> Maybe [Spec.Item]
tfItems res =
  Just
    [ emptyItem { Spec.itemId = Just res.itemId } ]

-- TODO: Discuss payment info transmission with ONDC
tfPayments :: DOnInit.OnInitRes -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments res bapConfig = do
  let mPrice = Just res.estimatedTotalFare
  let mkParams :: (Maybe BknPaymentParams) = decodeFromText =<< bapConfig.paymentParamsJson
  let paymentInstrument = show <$> res.paymentInstrument
  Just $ DL.singleton $ OUP.mkPayment (show res.city) (show bapConfig.collectedBy) Enums.NOT_PAID mPrice res.paymentId mkParams bapConfig.settlementType bapConfig.settlementWindow bapConfig.staticTermsUrl bapConfig.buyerFinderFee False res.paymentMode paymentInstrument

tfQuotation :: DOnInit.OnInitRes -> Maybe Spec.Quotation
tfQuotation res =
  Just $ emptyQuotation { Spec.quotationPrice = tfQuotationPrice res }

tfQuotationPrice :: DOnInit.OnInitRes -> Maybe Spec.Price
tfQuotationPrice res =
  Just $
    emptyPrice
      { Spec.priceCurrency = Just $ show res.estimatedTotalFare.currency,
        Spec.priceOfferedValue = Just $ encodeToText res.estimatedTotalFare.amount,
        Spec.priceValue = Just $ encodeToText res.estimatedFare.amount
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
        emptyPerson
          { Spec.personName = res.mbRiderName,
            Spec.personTags = mkPersonTags
          }

    mkPersonTags
      | not res.isValueAddNP = Nothing
      | otherwise =
        Tags.buildTagGroups
          [ Tags.NIGHT_SAFETY_CHECK Tags.~= show res.nightSafetyCheck,
            Tags.ENABLE_FREQUENT_LOCATION_UPDATES Tags.~= show res.enableFrequentLocationUpdates,
            Tags.ENABLE_OTP_LESS_RIDE Tags.~= show res.enableOtpLessRide
          ]

tfVehicle :: DOnInit.OnInitRes -> Maybe Spec.Vehicle
tfVehicle res = do
  let (category, variant) = Utils.castVehicleVariant res.vehicleVariant
  Just $
    emptyVehicle
      { Spec.vehicleCategory = Just category,
        Spec.vehicleVariant = Just variant
      }

tfProvider :: DOnInit.OnInitRes -> Maybe Spec.Provider
tfProvider res =
  Just $ emptyProvider { Spec.providerId = Just res.bppId }

tfOrderBilling :: DOnInit.OnInitRes -> Maybe Spec.Billing
tfOrderBilling res =
  Just $
    Spec.Billing
      { billingPhone = Just $ maskNumber res.riderPhoneNumber,
        billingName = res.mbRiderName
      }
