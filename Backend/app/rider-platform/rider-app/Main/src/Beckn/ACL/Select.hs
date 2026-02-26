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
import BecknV2.OnDemand.Utils.Constructors
import qualified BecknV2.OnDemand.Utils.Common as UCommonV2
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601)
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import BecknV2.OnDemand.Utils.Payment
import Control.Lens ((%~))
import qualified Data.List as L
import qualified Data.Text as T
import qualified Domain.Action.UI.Select as DSelect
import Domain.Types.BecknConfig
import qualified Domain.Types.Location as Location
import Domain.Types.RiderConfig
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.MerchantPaymentMethod as SLMPM
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import Tools.Error

buildSelectReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], CacheFlow m r, EsqDBFlow m r) =>
  DSelect.DSelectRes ->
  m Spec.SelectReq
buildSelectReqV2 dSelectRes = do
  endLoc <- dSelectRes.searchRequest.toLocation & fromMaybeM (InternalError "To location address not found")
  moc <- CQMOC.findByMerchantIdAndCity dSelectRes.merchant.id dSelectRes.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> dSelectRes.merchant.id.getId <> "-city-" <> show dSelectRes.city)
  riderConfig <- QRC.findByMerchantOperatingCityId moc.id Nothing >>= fromMaybeM (RiderConfigDoesNotExist moc.id.getId)
  bapConfigs <- QBC.findByMerchantIdDomainandMerchantOperatingCityId dSelectRes.merchant.id "MOBILITY" moc.id
  bapConfig <- listToMaybe bapConfigs & fromMaybeM (InvalidRequest $ "BecknConfig not found for merchantId " <> show dSelectRes.merchant.id.getId <> " merchantOperatingCityId " <> show moc.id.getId) -- Using findAll for backward compatibility, TODO : Remove findAll and use findOne
  -- stops <- dSelectRes.searchRequest.stops
  messageId <- generateGUID
  let message = buildSelectReqMessage dSelectRes endLoc dSelectRes.isValueAddNP bapConfig riderConfig
      transactionId = dSelectRes.searchRequest.id.getId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack dSelectRes.merchant.id.getId)
  ttl <- bapConfig.selectTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  context <- ContextV2.buildContextV2 Context.SELECT Context.MOBILITY messageId (Just transactionId) dSelectRes.merchant.bapId bapUrl (Just dSelectRes.providerId) (Just dSelectRes.providerUrl) dSelectRes.city dSelectRes.merchant.country (Just ttl)
  pure $ Spec.SelectReq {selectReqContext = context, selectReqMessage = message}

buildSelectReqMessage :: DSelect.DSelectRes -> Location.Location -> Bool -> BecknConfig -> RiderConfig -> Spec.ConfirmReqMessage
buildSelectReqMessage res endLoc isValueAddNP bapConfig riderConfig =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res endLoc isValueAddNP bapConfig riderConfig
    }

tfOrder :: DSelect.DSelectRes -> Location.Location -> Bool -> BecknConfig -> RiderConfig -> Spec.Order
tfOrder res endLoc isValueAddNP bapConfig riderConfig =
  let orderPayments = tfPayments res bapConfig riderConfig
      startLoc = res.searchRequest.fromLocation
      orderItem = tfOrderItem res isValueAddNP
      orderFulfillment = tfFulfillment res startLoc endLoc isValueAddNP res.searchRequest.stops
   in emptyOrder
        { Spec.orderFulfillments = Just [orderFulfillment],
          Spec.orderItems = Just [orderItem],
          Spec.orderPayments = orderPayments,
          Spec.orderProvider = Just $ tfProvider res
        }

tfFulfillment :: DSelect.DSelectRes -> Location.Location -> Location.Location -> Bool -> [Location.Location] -> Spec.Fulfillment
tfFulfillment res startLoc endLoc isValueAddNP stops =
  let fulfillmentCustomer = if isValueAddNP then tfCustomer res.phoneNumber else Nothing
      fulfillmentStops = UCommon.mkStops' (Just startLoc) stops (Just endLoc)
   in emptyFulfillment
        { Spec.fulfillmentId = Just res.estimate.bppEstimateId.getId,
          Spec.fulfillmentType = UCommonV2.tripCategoryToFulfillmentType <$> res.tripCategory,
          Spec.fulfillmentStops = fulfillmentStops,
          Spec.fulfillmentVehicle = Just $ tfVehicle res,
          Spec.fulfillmentCustomer = fulfillmentCustomer
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
   in emptyVehicle
        { Spec.vehicleVariant = Just variant,
          Spec.vehicleCategory = Just category
        }

tfOrderItem :: DSelect.DSelectRes -> Bool -> Spec.Item
tfOrderItem res isValueAddNP =
  let itemTags =
        if isValueAddNP
          then Just $ mkItemTags res
          else Nothing
   in emptyItem
        { Spec.itemId = Just res.estimate.itemId,
          Spec.itemTags = itemTags,
          Spec.itemPrice = Just $ tfPrice res
        }

mkItemTags :: DSelect.DSelectRes -> [Spec.TagGroup]
mkItemTags res =
  let baseTags =
        Tags.buildTagGroups $
          [ Tags.IS_AUTO_ASSIGN_ENABLED Tags.~= show res.autoAssignEnabled,
            Tags.IS_FORWARD_BATCH_ENABLED Tags.~= show res.isAdvancedBookingEnabled,
            Tags.DEVICE_ID_FLAG Tags.~=? (show <$> res.isMultipleOrNoDeviceIdExist),
            Tags.TO_UPDATE_DEVICE_ID Tags.~= show res.toUpdateDeviceIdInfo,
            Tags.CUSTOMER_DISABILITY_DISABLE Tags.~=? ((T.pack . show) <$> res.disabilityDisable),
            Tags.PREFER_SAFETY_PLUS Tags.~= show res.preferSafetyPlus,
            Tags.IS_PET_RIDE Tags.~=? ((T.pack . show) <$> res.isPetRide),
            Tags.BILLING_CATEGORY Tags.~= show res.billingCategory,
            Tags.EMAIL_DOMAIN Tags.~=? res.emailDomain,
            Tags.CUSTOMER_TIP Tags.~=? ((\charges -> show charges.getMoney) <$> res.customerExtraFee),
            Tags.OTHER_SELECT_ESTIMATES Tags.~=| (not (null res.remainingEstimateBppIds), show (getId <$> res.remainingEstimateBppIds))
          ]
      deliveryTags = mkSelectResDetailsTagGroup res
   in fromMaybe [] baseTags <> deliveryTags

mkSelectResDetailsTagGroup :: DSelect.DSelectRes -> [Spec.TagGroup]
mkSelectResDetailsTagGroup res =
  maybe
    []
    ( \case
        (DSelect.DSelectResDelivery details) ->
          fromMaybe [] $
            Tags.buildTagGroups
              [ Tags.PARCEL_TYPE Tags.~= show details.parcelType,
                Tags.PARCEL_QUANTITY Tags.~=? (show <$> details.quantity)
              ]
    )
    res.selectResDetails

tfPrice :: DSelect.DSelectRes -> Spec.Price
tfPrice res =
  emptyPrice
    { Spec.priceCurrency = Just $ show res.estimate.estimatedFare.currency,
      Spec.priceValue = Just $ show res.estimate.estimatedFare.amount
    }

tfPayments :: DSelect.DSelectRes -> BecknConfig -> RiderConfig -> Maybe [Spec.Payment]
tfPayments res bapConfig riderConfig = do
  let mPrice = Just res.estimate.estimatedFare
  let mkParams = SLMPM.mkBknPaymentParams res.paymentMethodInfo bapConfig riderConfig
  Just $ L.singleton $ mkPayment (show res.city) (show bapConfig.collectedBy) Enums.NOT_PAID mPrice Nothing mkParams bapConfig.settlementType bapConfig.settlementWindow bapConfig.staticTermsUrl bapConfig.buyerFinderFee False res.paymentMode Nothing

tfProvider :: DSelect.DSelectRes -> Spec.Provider
tfProvider res = emptyProvider { Spec.providerId = Just res.providerId }
