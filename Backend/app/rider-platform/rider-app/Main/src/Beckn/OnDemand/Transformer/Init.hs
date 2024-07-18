{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.Init where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.OnDemand.Utils.Init
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
import qualified BecknV2.OnDemand.Utils.Context
import BecknV2.Utils (maskNumber)
import qualified Data.List
import qualified Data.Text
import qualified Domain.Types.BecknConfig as DBC
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Text
import qualified SharedLogic.Confirm
import qualified Storage.CachedQueries.ValueAddNP as VNP

buildInitReq :: (Kernel.Types.App.MonadFlow m) => SharedLogic.Confirm.DConfirmRes -> Kernel.Prelude.BaseUrl -> Kernel.Types.Beckn.Context.Action -> Kernel.Types.Beckn.Context.Domain -> Data.Text.Text -> Maybe Data.Text.Text -> Bool -> DBC.BecknConfig -> Text -> m BecknV2.OnDemand.Types.InitReq
buildInitReq uiConfirm bapUrl action domain fulfillmentType mbBppFullfillmentId isValueAddNP bapConfig initTtl = do
  initReqContext_ <- BecknV2.OnDemand.Utils.Context.buildContextV2 action domain uiConfirm.booking.id.getId (Just uiConfirm.searchRequestId.getId) uiConfirm.merchant.bapId bapUrl (Just uiConfirm.providerId) (Just uiConfirm.providerUrl) uiConfirm.city uiConfirm.merchant.country (Just initTtl)
  let initReqMessage_ = buildInitReqMessage uiConfirm fulfillmentType mbBppFullfillmentId isValueAddNP bapConfig
  pure $ BecknV2.OnDemand.Types.InitReq {initReqContext = initReqContext_, initReqMessage = initReqMessage_}

buildInitReqMessage :: SharedLogic.Confirm.DConfirmRes -> Data.Text.Text -> Maybe Data.Text.Text -> Bool -> DBC.BecknConfig -> BecknV2.OnDemand.Types.ConfirmReqMessage
buildInitReqMessage uiConfirm fulfillmentType mbBppFullfillmentId isValueAddNP bapConfig = do
  let confirmReqMessageOrder_ = tfOrder uiConfirm fulfillmentType mbBppFullfillmentId isValueAddNP bapConfig
  BecknV2.OnDemand.Types.ConfirmReqMessage {confirmReqMessageOrder = confirmReqMessageOrder_}

tfFulfillmentVehicle :: SharedLogic.Confirm.DConfirmRes -> BecknV2.OnDemand.Types.Vehicle
tfFulfillmentVehicle uiConfirm = do
  let (category, variant) = Beckn.OnDemand.Utils.Common.castVehicleVariant uiConfirm.vehicleVariant
  let vehicleCategory_ = Just category
  let vehicleColor_ = Nothing
  let vehicleMake_ = Nothing
  let vehicleModel_ = Nothing
  let vehicleRegistration_ = Nothing
  let vehicleVariant_ = Just variant
  let vehicleCapacity_ = Nothing
  BecknV2.OnDemand.Types.Vehicle {vehicleCategory = vehicleCategory_, vehicleColor = vehicleColor_, vehicleMake = vehicleMake_, vehicleModel = vehicleModel_, vehicleRegistration = vehicleRegistration_, vehicleVariant = vehicleVariant_, vehicleCapacity = vehicleCapacity_}

tfOrder :: SharedLogic.Confirm.DConfirmRes -> Data.Text.Text -> Maybe Data.Text.Text -> Bool -> DBC.BecknConfig -> BecknV2.OnDemand.Types.Order
tfOrder uiConfirm fulfillmentType mbBppFullfillmentId isValueAddNP bapConfig = do
  let orderCancellation_ = Nothing
  let orderCancellationTerms_ = Nothing
  let orderId_ = Nothing
  let orderProvider_ = Just $ tfProvider uiConfirm
  let orderPayments_ = Beckn.OnDemand.Utils.Init.mkPayment uiConfirm.paymentMethodInfo bapConfig uiConfirm.city & Just
  let orderStatus_ = Nothing
  let orderQuote_ = Nothing
  let orderBilling_ = tfOrderBilling uiConfirm.riderPhone uiConfirm.riderName & Just
  let orderFulfillments_ = Just $ Data.List.singleton $ tfOrderFulfillments uiConfirm fulfillmentType mbBppFullfillmentId isValueAddNP
  let orderItems_ = Just $ Data.List.singleton $ tfOrderItems uiConfirm mbBppFullfillmentId
  BecknV2.OnDemand.Types.Order {orderBilling = orderBilling_, orderCancellation = orderCancellation_, orderCancellationTerms = orderCancellationTerms_, orderFulfillments = orderFulfillments_, orderId = orderId_, orderItems = orderItems_, orderPayments = orderPayments_, orderProvider = orderProvider_, orderQuote = orderQuote_, orderStatus = orderStatus_, orderCreatedAt = Just uiConfirm.booking.createdAt, orderUpdatedAt = Just uiConfirm.booking.updatedAt}

tfOrderBilling :: Maybe Data.Text.Text -> Maybe Data.Text.Text -> BecknV2.OnDemand.Types.Billing
tfOrderBilling mbPhoneNumber mbRiderName = do
  let billingPhone_ = mbPhoneNumber <&> maskNumber
  BecknV2.OnDemand.Types.Billing {billingPhone = billingPhone_, billingName = mbRiderName}

tfOrderFulfillments :: SharedLogic.Confirm.DConfirmRes -> Data.Text.Text -> Maybe Data.Text.Text -> Bool -> BecknV2.OnDemand.Types.Fulfillment
tfOrderFulfillments uiConfirm fulfillmentType mbBppFullfillmentId isValueAddNP = do
  let fulfillmentAgent_ = Nothing
  let fulfillmentCustomer_ = tfCustomer uiConfirm
  let fulfillmentId_ = mbBppFullfillmentId
  let fulfillmentState_ = Nothing
  let fulfillmentStops_ = Beckn.OnDemand.Utils.Init.mkStops uiConfirm.fromLoc uiConfirm.toLoc Nothing
  let fulfillmentTags_ = if isValueAddNP then Beckn.OnDemand.Utils.Init.mkFulfillmentTags uiConfirm.maxEstimatedDistance else Nothing
  let fulfillmentType_ = Just fulfillmentType
  let fulfillmentVehicle_ = tfFulfillmentVehicle uiConfirm & Just
  BecknV2.OnDemand.Types.Fulfillment {fulfillmentAgent = fulfillmentAgent_, fulfillmentCustomer = fulfillmentCustomer_, fulfillmentId = fulfillmentId_, fulfillmentState = fulfillmentState_, fulfillmentStops = fulfillmentStops_, fulfillmentTags = fulfillmentTags_, fulfillmentType = fulfillmentType_, fulfillmentVehicle = fulfillmentVehicle_}

tfOrderItems :: SharedLogic.Confirm.DConfirmRes -> Maybe Data.Text.Text -> BecknV2.OnDemand.Types.Item
tfOrderItems uiConfirm mbBppFullfillmentId = do
  let itemDescriptor_ = Nothing
  let itemFulfillmentIds_ = Data.List.singleton <$> mbBppFullfillmentId
  let itemId_ = Just uiConfirm.itemId
  let itemLocationIds_ = Nothing
  let itemPaymentIds_ = Nothing
  let itemPrice_ = Nothing
  let itemTags_ = Nothing
  BecknV2.OnDemand.Types.Item {itemDescriptor = itemDescriptor_, itemFulfillmentIds = itemFulfillmentIds_, itemId = itemId_, itemLocationIds = itemLocationIds_, itemPaymentIds = itemPaymentIds_, itemPrice = itemPrice_, itemTags = itemTags_}

tfPrice :: SharedLogic.Confirm.DConfirmRes -> BecknV2.OnDemand.Types.Price
tfPrice uiConfirm = do
  let priceComputedValue_ = Nothing
  let priceCurrency_ = Nothing
  let priceMaximumValue_ = Nothing
  let priceMinimumValue_ = Nothing
  let priceOfferedValue_ = Kernel.Utils.Text.encodeToText uiConfirm.booking.estimatedTotalFare.amount & Just
  let priceValue_ = Kernel.Utils.Text.encodeToText uiConfirm.booking.estimatedTotalFare.amount & Just
  BecknV2.OnDemand.Types.Price {priceComputedValue = priceComputedValue_, priceCurrency = priceCurrency_, priceMaximumValue = priceMaximumValue_, priceMinimumValue = priceMinimumValue_, priceOfferedValue = priceOfferedValue_, priceValue = priceValue_}

tfProvider :: SharedLogic.Confirm.DConfirmRes -> BecknV2.OnDemand.Types.Provider
tfProvider uiConfirm = do
  let providerId = Just uiConfirm.providerId
      providerItems = Nothing
      providerLocations = Nothing
      providerPayments = Nothing
      providerDescriptor = Nothing
      providerFulfillments = Nothing

  BecknV2.OnDemand.Types.Provider {..}

tfCustomer :: SharedLogic.Confirm.DConfirmRes -> Maybe BecknV2.OnDemand.Types.Customer
tfCustomer res =
  Just $
    BecknV2.OnDemand.Types.Customer
      { customerContact = mkContact,
        customerPerson = mkPerson
      }
  where
    mkContact = do
      let trimCountryCode mbNumber = mbNumber <&> \number -> fromMaybe number (Data.Text.stripPrefix "+91" number)
      Just $
        BecknV2.OnDemand.Types.Contact
          { contactPhone = trimCountryCode res.riderPhone
          -- handling of passing virtual number at UIconfirm domain handler.
          }

    mkPerson = do
      riderName <- res.riderName
      return $
        BecknV2.OnDemand.Types.Person
          { personId = Nothing,
            personImage = Nothing,
            personName = Just riderName,
            personGender = Nothing,
            personTags = Nothing
          }
