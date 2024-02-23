{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.OnSearch where

import qualified Beckn.ACL.Common
import qualified Beckn.OnDemand.Utils.Common
import Beckn.OnDemand.Utils.OnSearch
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
import qualified BecknV2.OnDemand.Utils.Context
import qualified Data.List
import qualified Data.Text
import qualified Domain.Action.Beckn.Search
import Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Quote as DQuote
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common (type (:::))

buildOnSearchMessage :: Domain.Action.Beckn.Search.DSearchRes -> DBC.BecknConfig -> Maybe BecknV2.OnDemand.Types.OnSearchReqMessage
buildOnSearchMessage res bppConfig = do
  let onSearchReqMessageCatalog_ = tfCatalog res bppConfig
      returnData = BecknV2.OnDemand.Types.OnSearchReqMessage {onSearchReqMessageCatalog = onSearchReqMessageCatalog_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

buildOnSearchRideReq :: (Monad m, Kernel.Types.App.MonadFlow m) => Kernel.Prelude.Text -> DBC.BecknConfig -> Domain.Action.Beckn.Search.DSearchRes -> Kernel.Types.Beckn.Context.Action -> Kernel.Types.Beckn.Context.Domain -> Data.Text.Text -> Maybe Data.Text.Text -> Data.Text.Text -> Kernel.Prelude.BaseUrl -> Maybe Data.Text.Text -> Maybe Kernel.Prelude.BaseUrl -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Beckn.Context.Country -> m BecknV2.OnDemand.Types.OnSearchReq
buildOnSearchRideReq onSearchTtl bppConfig res action domain messageId transactionId bapId bapUri bppId bppUri city country = do
  let onSearchReqError_ = Nothing
  onSearchReqContext_ <- BecknV2.OnDemand.Utils.Context.buildContextV2 action domain messageId transactionId bapId bapUri bppId bppUri city country (Just onSearchTtl)
  let onSearchReqMessage_ = buildOnSearchMessage res bppConfig
  pure $ BecknV2.OnDemand.Types.OnSearchReq {onSearchReqContext = onSearchReqContext_, onSearchReqError = onSearchReqError_, onSearchReqMessage = onSearchReqMessage_}

tfCatalog :: Domain.Action.Beckn.Search.DSearchRes -> DBC.BecknConfig -> BecknV2.OnDemand.Types.Catalog
tfCatalog res bppConfig = do
  let catalogDescriptor_ = tfCatalogDescriptor res
      catalogProviders_ = tfCatalogProviders res bppConfig & Just . Data.List.singleton
  BecknV2.OnDemand.Types.Catalog {catalogDescriptor = catalogDescriptor_, catalogProviders = catalogProviders_}

tfCatalogDescriptor :: Domain.Action.Beckn.Search.DSearchRes -> Maybe BecknV2.OnDemand.Types.Descriptor
tfCatalogDescriptor res = do
  let descriptorCode_ = Nothing
      descriptorName_ = Just res.provider.name
      descriptorShortDesc_ = Nothing
      returnData = BecknV2.OnDemand.Types.Descriptor {descriptorCode = descriptorCode_, descriptorName = descriptorName_, descriptorShortDesc = descriptorShortDesc_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfCatalogProviders :: Domain.Action.Beckn.Search.DSearchRes -> DBC.BecknConfig -> BecknV2.OnDemand.Types.Provider
tfCatalogProviders res bppConfig = do
  let providerId_ = Just res.provider.subscriberId.getShortId
      providerLocations_ = Just $ Beckn.OnDemand.Utils.OnSearch.mkProviderLocations ((map snd res.estimates) <> (map snd res.quotes))
      providerPayments_ = Just $ mkPayment res.provider bppConfig
      providerDescriptor_ = tfCatalogDescriptor res
      pricings = (map Beckn.OnDemand.Utils.Common.convertEstimateToPricing res.estimates) <> (map Beckn.OnDemand.Utils.Common.convertQuoteToPricing res.quotes)
      providerFulfillments_ = map (tfProviderFulfillments res) pricings & Just
      providerItems_ = Just $ map (tfProviderItems res) pricings
  BecknV2.OnDemand.Types.Provider {providerDescriptor = providerDescriptor_, providerFulfillments = providerFulfillments_, providerId = providerId_, providerItems = providerItems_, providerLocations = providerLocations_, providerPayments = providerPayments_}

tfItemPrice :: Beckn.OnDemand.Utils.Common.Pricing -> Maybe BecknV2.OnDemand.Types.Price
tfItemPrice pricing = do
  let priceComputedValue_ = Nothing
      priceCurrency_ = Just "INR"
      priceMaximumValue_ = Beckn.OnDemand.Utils.Common.rationaliseMoney pricing.pricingMaxFare & Just
      priceMinimumValue_ = Beckn.OnDemand.Utils.Common.rationaliseMoney pricing.pricingMinFare & Just
      priceOfferedValue_ = Beckn.OnDemand.Utils.Common.rationaliseMoney pricing.pricingMinFare & Just
      priceValue_ = Beckn.OnDemand.Utils.Common.rationaliseMoney pricing.pricingMinFare & Just
      returnData = BecknV2.OnDemand.Types.Price {priceComputedValue = priceComputedValue_, priceCurrency = priceCurrency_, priceMaximumValue = priceMaximumValue_, priceMinimumValue = priceMinimumValue_, priceOfferedValue = priceOfferedValue_, priceValue = priceValue_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfProviderFulfillments :: Domain.Action.Beckn.Search.DSearchRes -> Beckn.OnDemand.Utils.Common.Pricing -> BecknV2.OnDemand.Types.Fulfillment
tfProviderFulfillments res pricing = do
  let fulfillmentAgent_ = Nothing
      fulfillmentCustomer_ = Nothing
      fulfillmentId_ = Just pricing.pricingId
      fulfillmentState_ = Nothing
      fulfillmentStops_ = Beckn.OnDemand.Utils.Common.mkStops res.fromLocation res.toLocation
      fulfillmentTags_ = Nothing
      fulfillmentType_ = Just pricing.fulfillmentType
      fulfillmentVehicle_ = tfVehicle pricing
  BecknV2.OnDemand.Types.Fulfillment {fulfillmentAgent = fulfillmentAgent_, fulfillmentCustomer = fulfillmentCustomer_, fulfillmentId = fulfillmentId_, fulfillmentState = fulfillmentState_, fulfillmentStops = fulfillmentStops_, fulfillmentTags = fulfillmentTags_, fulfillmentType = fulfillmentType_, fulfillmentVehicle = fulfillmentVehicle_}

tfProviderItems :: Domain.Action.Beckn.Search.DSearchRes -> Beckn.OnDemand.Utils.Common.Pricing -> BecknV2.OnDemand.Types.Item
tfProviderItems res pricing = do
  let itemDescriptor_ = Nothing
      itemFulfillmentIds_ = Just [pricing.pricingId]
      itemId_ = Beckn.ACL.Common.mkItemId res.provider.shortId.getShortId pricing.vehicleVariant & Just
      itemLocationIds_ = Nothing
      itemPaymentIds_ = Nothing
      itemTags_ = Just $ Beckn.OnDemand.Utils.OnSearch.mkItemTags pricing
      itemPrice_ = tfItemPrice pricing
  BecknV2.OnDemand.Types.Item {itemDescriptor = itemDescriptor_, itemFulfillmentIds = itemFulfillmentIds_, itemId = itemId_, itemLocationIds = itemLocationIds_, itemPaymentIds = itemPaymentIds_, itemPrice = itemPrice_, itemTags = itemTags_}

tfVehicle :: Beckn.OnDemand.Utils.Common.Pricing -> Maybe BecknV2.OnDemand.Types.Vehicle
tfVehicle pricing = do
  let (category, variant) = Beckn.OnDemand.Utils.Common.castVariant pricing.vehicleVariant
      vehicleCategory_ = Just category
      vehicleColor_ = Nothing
      vehicleMake_ = Nothing
      vehicleModel_ = Nothing
      vehicleRegistration_ = Nothing
      vehicleVariant_ = Just variant
      returnData = BecknV2.OnDemand.Types.Vehicle {vehicleCategory = vehicleCategory_, vehicleColor = vehicleColor_, vehicleMake = vehicleMake_, vehicleModel = vehicleModel_, vehicleRegistration = vehicleRegistration_, vehicleVariant = vehicleVariant_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData
