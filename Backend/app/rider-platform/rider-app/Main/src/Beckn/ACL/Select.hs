{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Select (buildSelectReqV2) where

import qualified Beckn.OnDemand.Utils.Common as UCommon
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as UCommonV2
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601)
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import BecknV2.OnDemand.Utils.Payment
import Control.Lens ((%~))
import qualified Data.List as L
import qualified Data.Text as T
import qualified Domain.Action.UI.Select as DSelect
import Domain.Types
import Domain.Types.BecknConfig
import qualified Domain.Types.Location as Location
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BecknConfig as QBC
import Tools.Error

buildSelectReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], CacheFlow m r, EsqDBFlow m r) =>
  DSelect.DSelectRes ->
  m Spec.SelectReq
buildSelectReqV2 dSelectRes = do
  endLoc <- dSelectRes.searchRequest.toLocation & fromMaybeM (InternalError "To location address not found")
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle dSelectRes.merchant.id "MOBILITY" (UCommon.mapVariantToVehicle dSelectRes.variant) >>= fromMaybeM (InternalError "Beckn Config not found")
  messageId <- generateGUID
  let message = buildSelectReqMessage dSelectRes endLoc dSelectRes.isValueAddNP bapConfig
      transactionId = dSelectRes.searchRequest.id.getId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack dSelectRes.merchant.id.getId)
  ttl <- bapConfig.selectTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  context <- ContextV2.buildContextV2 Context.SELECT Context.MOBILITY messageId (Just transactionId) dSelectRes.merchant.bapId bapUrl (Just dSelectRes.providerId) (Just dSelectRes.providerUrl) dSelectRes.city dSelectRes.merchant.country (Just ttl)
  pure $ Spec.SelectReq {selectReqContext = context, selectReqMessage = message}

buildSelectReqMessage :: DSelect.DSelectRes -> Location.Location -> Bool -> BecknConfig -> Spec.ConfirmReqMessage
buildSelectReqMessage res endLoc isValueAddNP bapConfig =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res endLoc isValueAddNP bapConfig
    }

tfOrder :: DSelect.DSelectRes -> Location.Location -> Bool -> BecknConfig -> Spec.Order
tfOrder res endLoc isValueAddNP bapConfig =
  let orderBilling = Nothing
      orderCancellation = Nothing
      orderCancellationTerms = Nothing
      orderId = Nothing
      orderPayments = tfPayments res bapConfig
      orderProvider = Just $ tfProvider res
      orderQuote = Nothing
      orderStatus = Nothing
      startLoc = res.searchRequest.fromLocation
      orderItem = tfOrderItem res
      orderFulfillment = tfFulfillment res startLoc endLoc isValueAddNP
      orderCreatedAt = Nothing
      orderUpdatedAt = Nothing
   in Spec.Order
        { orderFulfillments = Just [orderFulfillment],
          orderItems = Just [orderItem],
          ..
        }

tfFulfillment :: DSelect.DSelectRes -> Location.Location -> Location.Location -> Bool -> Spec.Fulfillment
tfFulfillment res startLoc endLoc isValueAddNP =
  let fulfillmentAgent = Nothing
      fulfillmentTags = Nothing
      fulfillmentState = Nothing
      fulfillmentCustomer = if isValueAddNP then tfCustomer res.phoneNumber else Nothing
      fulfillmentId = Just res.estimate.bppEstimateId.getId
      fulfillmentType = Just $ show Enums.DELIVERY
      fulfillmentStops = UCommon.mkStops' (Just startLoc) (Just endLoc)
      fulfillmentVehicle = tfVehicle res
   in Spec.Fulfillment
        { fulfillmentStops = fulfillmentStops,
          fulfillmentVehicle = Just fulfillmentVehicle,
          ..
        }

tfCustomer :: Maybe T.Text -> Maybe Spec.Customer
tfCustomer mbPhoneNumber = do
  let customerContact = Just $ Spec.Contact {contactPhone = mbPhoneNumber}
      customerPerson = Nothing
      returnData = Spec.Customer {customerContact = customerContact, customerPerson = customerPerson}
      allNothing = UCommonV2.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfVehicle :: DSelect.DSelectRes -> Spec.Vehicle
tfVehicle res =
  let (category, variant) = UCommon.castVehicleVariant res.variant
      vehicleColor = Nothing
      vehicleMake = Nothing
      vehicleModel = Nothing
      vehicleRegistration = Nothing
      vehicleVariant = Just variant
      vehicleCategory = Just category
      vehicleCapacity = Nothing
   in Spec.Vehicle {..}

tfOrderItem :: DSelect.DSelectRes -> Spec.Item
tfOrderItem res =
  let itemDescriptor = Nothing
      itemFulfillmentIds = Nothing
      itemLocationIds = Nothing
      itemPaymentIds = Nothing
      itemId = Just res.estimate.itemId
      itemTags = Tags.convertToTagGroup . (.itemTags) =<< res.taggings
      itemPrice = tfPrice res
   in Spec.Item
        { itemPrice = Just itemPrice,
          ..
        }

tfPrice :: DSelect.DSelectRes -> Spec.Price
tfPrice res =
  let priceCurrency = Just $ show res.estimate.estimatedFare.currency
      priceValue = Just $ show res.estimate.estimatedFare.amount
      priceComputedValue = Nothing
      priceMaximumValue = Nothing
      priceMinimumValue = Nothing
      priceOfferedValue = Nothing
   in Spec.Price {..}

tfPayments :: DSelect.DSelectRes -> BecknConfig -> Maybe [Spec.Payment]
tfPayments res bapConfig = do
  let mPrice = Just res.estimate.estimatedFare
  let mkParams :: (Maybe BknPaymentParams) = decodeFromText =<< bapConfig.paymentParamsJson
  Just $ L.singleton $ mkPayment (show res.city) (show bapConfig.collectedBy) Enums.NOT_PAID mPrice Nothing mkParams bapConfig.settlementType bapConfig.settlementWindow bapConfig.staticTermsUrl bapConfig.buyerFinderFee

tfProvider :: DSelect.DSelectRes -> Spec.Provider
tfProvider res =
  let providerDescriptor = Nothing
      providerFulfillments = Nothing
      providerItems = Nothing
      providerLocations = Nothing
      providerPayments = Nothing
      providerId = Just res.providerId
   in Spec.Provider {..}
