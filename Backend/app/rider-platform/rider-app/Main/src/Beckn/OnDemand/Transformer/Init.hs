{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.Init where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.Init
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
import qualified BecknV2.OnDemand.Utils.Context
import qualified Data.List
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common (type (:::))
import qualified Kernel.Utils.Text
import qualified SharedLogic.Confirm

buildInitReq :: (Kernel.Types.App.MonadFlow m) => SharedLogic.Confirm.DConfirmRes -> Kernel.Prelude.BaseUrl -> Kernel.Types.Beckn.Context.Action -> Kernel.Types.Beckn.Context.Domain -> Data.Text.Text -> Maybe Data.Text.Text -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.InitReq
buildInitReq uiConfirm bapUrl action domain fulfillmentType mbBppFullfillmentId mbDriverId = do
  initReqContext_ <- BecknV2.OnDemand.Utils.Context.buildContextV2 action domain uiConfirm.booking.id.getId (Just uiConfirm.searchRequestId.getId) uiConfirm.merchant.bapId bapUrl (Just uiConfirm.providerId) (Just uiConfirm.providerUrl) uiConfirm.merchant.defaultCity uiConfirm.merchant.country
  initReqMessage_ <- buildInitReqMessage uiConfirm fulfillmentType mbBppFullfillmentId mbDriverId
  pure $ BecknV2.OnDemand.Types.InitReq {initReqContext = initReqContext_, initReqMessage = initReqMessage_}

buildInitReqMessage :: (Kernel.Types.App.MonadFlow m) => SharedLogic.Confirm.DConfirmRes -> Data.Text.Text -> Maybe Data.Text.Text -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.ConfirmReqMessage
buildInitReqMessage uiConfirm fulfillmentType mbBppFullfillmentId mbDriverId = do
  confirmReqMessageOrder_ <- tfOrder uiConfirm fulfillmentType mbBppFullfillmentId mbDriverId
  pure $ BecknV2.OnDemand.Types.ConfirmReqMessage {confirmReqMessageOrder = confirmReqMessageOrder_}

tfFulfillmentVehicle :: (Kernel.Types.App.MonadFlow m) => SharedLogic.Confirm.DConfirmRes -> m BecknV2.OnDemand.Types.Vehicle
tfFulfillmentVehicle uiConfirm = do
  let (category, variant) = Beckn.OnDemand.Utils.Common.castVehicleVariant uiConfirm.vehicleVariant
  let vehicleCategory_ = Just category
  let vehicleColor_ = Nothing
  let vehicleMake_ = Nothing
  let vehicleModel_ = Nothing
  let vehicleRegistration_ = Nothing
  let vehicleVariant_ = Just variant
  pure $ BecknV2.OnDemand.Types.Vehicle {vehicleCategory = vehicleCategory_, vehicleColor = vehicleColor_, vehicleMake = vehicleMake_, vehicleModel = vehicleModel_, vehicleRegistration = vehicleRegistration_, vehicleVariant = vehicleVariant_}

tfOrder :: (Kernel.Types.App.MonadFlow m) => SharedLogic.Confirm.DConfirmRes -> Data.Text.Text -> Maybe Data.Text.Text -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.Order
tfOrder uiConfirm fulfillmentType mbBppFullfillmentId mbDriverId = do
  let orderCancellation_ = Nothing
  let orderCancellationTerms_ = Nothing
  let orderId_ = Nothing
  let orderPayments_ = Beckn.OnDemand.Utils.Init.mkPayment uiConfirm.paymentMethodInfo & Just
  let orderStatus_ = Nothing
  orderBilling_ <- tfOrderBilling uiConfirm <&> Just
  orderFulfillments_ <- Data.List.singleton <$> tfOrderFulfillments uiConfirm fulfillmentType mbBppFullfillmentId <&> Just
  orderItems_ <- Data.List.singleton <$> tfOrderItems uiConfirm mbBppFullfillmentId <&> Just
  orderProvider_ <- tfOrderProvider mbDriverId <&> Just
  orderQuote_ <- tfOrderQuote uiConfirm <&> Just
  pure $ BecknV2.OnDemand.Types.Order {orderBilling = orderBilling_, orderCancellation = orderCancellation_, orderCancellationTerms = orderCancellationTerms_, orderFulfillments = orderFulfillments_, orderId = orderId_, orderItems = orderItems_, orderPayments = orderPayments_, orderProvider = orderProvider_, orderQuote = orderQuote_, orderStatus = orderStatus_}

tfOrderBilling :: (Kernel.Types.App.MonadFlow m) => SharedLogic.Confirm.DConfirmRes -> m BecknV2.OnDemand.Types.Billing
tfOrderBilling uiConfirm = do
  let billingPhone_ = uiConfirm.riderPhone
  pure $ BecknV2.OnDemand.Types.Billing {billingPhone = billingPhone_}

tfOrderFulfillments :: (Kernel.Types.App.MonadFlow m) => SharedLogic.Confirm.DConfirmRes -> Data.Text.Text -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.Fulfillment
tfOrderFulfillments uiConfirm fulfillmentType mbBppFullfillmentId = do
  let fulfillmentAgent_ = Nothing
  let fulfillmentCustomer_ = Nothing
  let fulfillmentId_ = mbBppFullfillmentId
  let fulfillmentState_ = Nothing
  let fulfillmentStops_ = Beckn.OnDemand.Utils.Init.mkStops uiConfirm.fromLoc uiConfirm.toLoc Nothing
  let fulfillmentTags_ = Beckn.OnDemand.Utils.Init.mkFulfillmentTags uiConfirm.maxEstimatedDistance
  let fulfillmentType_ = Just fulfillmentType
  fulfillmentVehicle_ <- tfFulfillmentVehicle uiConfirm <&> Just
  pure $ BecknV2.OnDemand.Types.Fulfillment {fulfillmentAgent = fulfillmentAgent_, fulfillmentCustomer = fulfillmentCustomer_, fulfillmentId = fulfillmentId_, fulfillmentState = fulfillmentState_, fulfillmentStops = fulfillmentStops_, fulfillmentTags = fulfillmentTags_, fulfillmentType = fulfillmentType_, fulfillmentVehicle = fulfillmentVehicle_}

tfOrderItems :: (Kernel.Types.App.MonadFlow m) => SharedLogic.Confirm.DConfirmRes -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.Item
tfOrderItems uiConfirm mbBppFullfillmentId = do
  let itemDescriptor_ = Nothing
  let itemFulfillmentIds_ = Data.List.singleton <$> mbBppFullfillmentId
  let itemId_ = Just uiConfirm.itemId
  let itemLocationIds_ = Nothing
  let itemPaymentIds_ = Nothing
  let itemPrice_ = Nothing
  let itemTags_ = Nothing
  pure $ BecknV2.OnDemand.Types.Item {itemDescriptor = itemDescriptor_, itemFulfillmentIds = itemFulfillmentIds_, itemId = itemId_, itemLocationIds = itemLocationIds_, itemPaymentIds = itemPaymentIds_, itemPrice = itemPrice_, itemTags = itemTags_}

tfOrderProvider :: (Kernel.Types.App.MonadFlow m) => Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.Provider
tfOrderProvider mbDriverId = do
  let providerDescriptor_ = Nothing
  let providerFulfillments_ = Nothing
  let providerId_ = mbDriverId
  let providerItems_ = Nothing
  let providerLocations_ = Nothing
  let providerPayments_ = Nothing
  pure $ BecknV2.OnDemand.Types.Provider {providerDescriptor = providerDescriptor_, providerFulfillments = providerFulfillments_, providerId = providerId_, providerItems = providerItems_, providerLocations = providerLocations_, providerPayments = providerPayments_}

tfOrderQuote :: (Kernel.Types.App.MonadFlow m) => SharedLogic.Confirm.DConfirmRes -> m BecknV2.OnDemand.Types.Quotation
tfOrderQuote uiConfirm = do
  let quotationBreakup_ = Nothing
  let quotationTtl_ = Nothing
  quotationPrice_ <- tfPrice uiConfirm <&> Just
  pure $ BecknV2.OnDemand.Types.Quotation {quotationBreakup = quotationBreakup_, quotationPrice = quotationPrice_, quotationTtl = quotationTtl_}

tfPrice :: (Kernel.Types.App.MonadFlow m) => SharedLogic.Confirm.DConfirmRes -> m BecknV2.OnDemand.Types.Price
tfPrice uiConfirm = do
  let priceComputedValue_ = Nothing
  let priceCurrency_ = Nothing
  let priceMaximumValue_ = Nothing
  let priceMinimumValue_ = Nothing
  let priceOfferedValue_ = Kernel.Utils.Text.encodeToText uiConfirm.booking.estimatedTotalFare.getMoney & Just
  let priceValue_ = Kernel.Utils.Text.encodeToText uiConfirm.booking.estimatedTotalFare.getMoney & Just
  pure $ BecknV2.OnDemand.Types.Price {priceComputedValue = priceComputedValue_, priceCurrency = priceCurrency_, priceMaximumValue = priceMaximumValue_, priceMinimumValue = priceMinimumValue_, priceOfferedValue = priceOfferedValue_, priceValue = priceValue_}
