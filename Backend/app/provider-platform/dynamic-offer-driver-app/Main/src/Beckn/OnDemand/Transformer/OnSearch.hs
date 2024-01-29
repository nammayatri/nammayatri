{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.OnSearch where

import qualified Beckn.ACL.Common
import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.OnSearch
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
import qualified BecknV2.OnDemand.Utils.Context
import qualified Data.List
import qualified Data.Text
import qualified Domain.Action.Beckn.Search
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common (type (:::))

buildOnSearchMessage :: (Monad m, Kernel.Types.App.MonadFlow m) => [Domain.Action.Beckn.Search.EstimateInfo] -> Domain.Action.Beckn.Search.DSearchRes -> m (Maybe BecknV2.OnDemand.Types.OnSearchReqMessage)
buildOnSearchMessage estimates res = do
  onSearchReqMessageCatalog_ <- tfCatalog estimates res
  let returnData = BecknV2.OnDemand.Types.OnSearchReqMessage {onSearchReqMessageCatalog = onSearchReqMessageCatalog_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then pure Nothing
    else pure $ Just returnData

buildOnSearchRideReq :: (Monad m, Kernel.Types.App.MonadFlow m) => [Domain.Action.Beckn.Search.EstimateInfo] -> Domain.Action.Beckn.Search.DSearchRes -> Kernel.Types.Beckn.Context.Action -> Kernel.Types.Beckn.Context.Domain -> Data.Text.Text -> Maybe Data.Text.Text -> Data.Text.Text -> Kernel.Prelude.BaseUrl -> Maybe Data.Text.Text -> Maybe Kernel.Prelude.BaseUrl -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Beckn.Context.Country -> m BecknV2.OnDemand.Types.OnSearchReq
buildOnSearchRideReq estimates res action domain messageId transactionId bapId bapUri bppId bppUri city country = do
  let onSearchReqError_ = Nothing
  onSearchReqContext_ <- BecknV2.OnDemand.Utils.Context.buildContextV2 action domain messageId transactionId bapId bapUri bppId bppUri city country
  onSearchReqMessage_ <- buildOnSearchMessage estimates res
  pure $ BecknV2.OnDemand.Types.OnSearchReq {onSearchReqContext = onSearchReqContext_, onSearchReqError = onSearchReqError_, onSearchReqMessage = onSearchReqMessage_}

tfCatalog :: (Monad m, Kernel.Types.App.MonadFlow m) => [Domain.Action.Beckn.Search.EstimateInfo] -> Domain.Action.Beckn.Search.DSearchRes -> m BecknV2.OnDemand.Types.Catalog
tfCatalog estimates res = do
  catalogDescriptor_ <- tfCatalogDescriptor res
  catalogProviders_ <- tfCatalogProviders estimates res <&> Just . Data.List.singleton
  pure $ BecknV2.OnDemand.Types.Catalog {catalogDescriptor = catalogDescriptor_, catalogProviders = catalogProviders_}

tfCatalogDescriptor :: (Monad m, Kernel.Types.App.MonadFlow m) => Domain.Action.Beckn.Search.DSearchRes -> m (Maybe BecknV2.OnDemand.Types.Descriptor)
tfCatalogDescriptor res = do
  let descriptorCode_ = Nothing
  let descriptorName_ = Just res.provider.name
  let descriptorShortDesc_ = Nothing
  let returnData = BecknV2.OnDemand.Types.Descriptor {descriptorCode = descriptorCode_, descriptorName = descriptorName_, descriptorShortDesc = descriptorShortDesc_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then pure Nothing
    else pure $ Just returnData

tfCatalogProviders :: (Monad m, Kernel.Types.App.MonadFlow m) => [Domain.Action.Beckn.Search.EstimateInfo] -> Domain.Action.Beckn.Search.DSearchRes -> m BecknV2.OnDemand.Types.Provider
tfCatalogProviders estimates res = do
  let providerId_ = Just res.provider.subscriberId.getShortId
  let providerLocations_ = Beckn.OnDemand.Utils.OnSearch.mkProviderLocations estimates & Just
  let providerPayments_ = Nothing
  providerDescriptor_ <- tfCatalogDescriptor res
  providerFulfillments_ <- mapM (tfProviderFulfillments res) estimates <&> Just
  providerItems_ <- mapM (tfProviderItems res) estimates <&> Just
  pure $ BecknV2.OnDemand.Types.Provider {providerDescriptor = providerDescriptor_, providerFulfillments = providerFulfillments_, providerId = providerId_, providerItems = providerItems_, providerLocations = providerLocations_, providerPayments = providerPayments_}

tfItemPrice :: (Monad m, Kernel.Types.App.MonadFlow m) => Domain.Action.Beckn.Search.EstimateInfo -> m (Maybe BecknV2.OnDemand.Types.Price)
tfItemPrice estInfo = do
  let priceComputedValue_ = Nothing
  let priceCurrency_ = Just "INR"
  let priceMaximumValue_ = Beckn.OnDemand.Utils.Common.rationaliseMoney estInfo.estimate.maxFare & Just
  let priceMinimumValue_ = Beckn.OnDemand.Utils.Common.rationaliseMoney estInfo.estimate.minFare & Just
  let priceOfferedValue_ = Beckn.OnDemand.Utils.Common.rationaliseMoney estInfo.estimate.minFare & Just
  let priceValue_ = Beckn.OnDemand.Utils.Common.rationaliseMoney estInfo.estimate.minFare & Just
  let returnData = BecknV2.OnDemand.Types.Price {priceComputedValue = priceComputedValue_, priceCurrency = priceCurrency_, priceMaximumValue = priceMaximumValue_, priceMinimumValue = priceMinimumValue_, priceOfferedValue = priceOfferedValue_, priceValue = priceValue_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then pure Nothing
    else pure $ Just returnData

tfProviderFulfillments :: (Monad m, Kernel.Types.App.MonadFlow m) => Domain.Action.Beckn.Search.DSearchRes -> Domain.Action.Beckn.Search.EstimateInfo -> m BecknV2.OnDemand.Types.Fulfillment
tfProviderFulfillments res estInfo = do
  let fulfillmentAgent_ = Nothing
  let fulfillmentCustomer_ = Nothing
  let fulfillmentId_ = Just estInfo.estimate.id.getId
  let fulfillmentState_ = Nothing
  let fulfillmentStops_ = Beckn.OnDemand.Utils.Common.mkStops res.fromLocation res.toLocation
  let fulfillmentTags_ = Nothing
  let fulfillmentType_ = Just "RIDE"
  fulfillmentVehicle_ <- tfVehicle estInfo
  pure $ BecknV2.OnDemand.Types.Fulfillment {fulfillmentAgent = fulfillmentAgent_, fulfillmentCustomer = fulfillmentCustomer_, fulfillmentId = fulfillmentId_, fulfillmentState = fulfillmentState_, fulfillmentStops = fulfillmentStops_, fulfillmentTags = fulfillmentTags_, fulfillmentType = fulfillmentType_, fulfillmentVehicle = fulfillmentVehicle_}

tfProviderItems :: (Monad m, Kernel.Types.App.MonadFlow m) => Domain.Action.Beckn.Search.DSearchRes -> Domain.Action.Beckn.Search.EstimateInfo -> m BecknV2.OnDemand.Types.Item
tfProviderItems res estInfo = do
  let itemDescriptor_ = Nothing
  let itemFulfillmentIds_ = Just [estInfo.estimate.id.getId]
  let itemId_ = Beckn.ACL.Common.mkItemId res.provider.shortId.getShortId estInfo.estimate.vehicleVariant & Just
  let itemLocationIds_ = Nothing
  let itemPaymentIds_ = Nothing
  let itemTags_ = Beckn.OnDemand.Utils.OnSearch.mkItemTags estInfo & Just
  itemPrice_ <- tfItemPrice estInfo
  pure $ BecknV2.OnDemand.Types.Item {itemDescriptor = itemDescriptor_, itemFulfillmentIds = itemFulfillmentIds_, itemId = itemId_, itemLocationIds = itemLocationIds_, itemPaymentIds = itemPaymentIds_, itemPrice = itemPrice_, itemTags = itemTags_}

tfVehicle :: (Monad m, Kernel.Types.App.MonadFlow m) => Domain.Action.Beckn.Search.EstimateInfo -> m (Maybe BecknV2.OnDemand.Types.Vehicle)
tfVehicle estInfo = do
  let (category, variant) = Beckn.OnDemand.Utils.Common.castVariant estInfo.estimate.vehicleVariant
  let vehicleCategory_ = Just category
  let vehicleColor_ = Nothing
  let vehicleMake_ = Nothing
  let vehicleModel_ = Nothing
  let vehicleRegistration_ = Nothing
  let vehicleVariant_ = Just variant
  let returnData = BecknV2.OnDemand.Types.Vehicle {vehicleCategory = vehicleCategory_, vehicleColor = vehicleColor_, vehicleMake = vehicleMake_, vehicleModel = vehicleModel_, vehicleRegistration = vehicleRegistration_, vehicleVariant = vehicleVariant_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then pure Nothing
    else pure $ Just returnData
