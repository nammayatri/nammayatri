{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.OnSearch where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.OnSearch
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
import qualified Data.Maybe
import qualified Data.Text
import qualified Domain.Action.Beckn.OnSearch
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Id
import Kernel.Utils.Common (type (:::))
import qualified Kernel.Utils.Error
import qualified Tools.Error

buildOnSearchReq :: (Monad m, Kernel.Types.App.MonadFlow m) => BecknV2.OnDemand.Types.OnSearchReq -> BecknV2.OnDemand.Types.Provider -> [BecknV2.OnDemand.Types.Item] -> [BecknV2.OnDemand.Types.Fulfillment] -> m Domain.Action.Beckn.OnSearch.DOnSearchReq
buildOnSearchReq req provider items fulfillments = do
  let paymentMethodsInfo_ = []
  estimatesInfo_ <- traverse (tfEstimatesInfo provider) items
  providerInfo_ <- tfProviderInfo req
  quotesInfo_ <- traverse (tfQuotesInfo provider fulfillments) items
  requestId_ <- Beckn.OnDemand.Utils.Common.getMessageId req.onSearchReqContext
  pure $ Domain.Action.Beckn.OnSearch.DOnSearchReq {estimatesInfo = estimatesInfo_, paymentMethodsInfo = paymentMethodsInfo_, providerInfo = providerInfo_, quotesInfo = quotesInfo_, requestId = requestId_}

tfEstimatesInfo :: (Monad m, Kernel.Types.App.MonadFlow m) => BecknV2.OnDemand.Types.Provider -> BecknV2.OnDemand.Types.Item -> m Domain.Action.Beckn.OnSearch.EstimateInfo
tfEstimatesInfo provider item = do
  bppEstimateId_ <- Beckn.OnDemand.Utils.OnSearch.getFulfillmentId item
  let descriptions_ = []
  let discount_ = Nothing
  driversLocation_ <- Beckn.OnDemand.Utils.OnSearch.getProviderLocation provider
  estimatedFare_ <- Beckn.OnDemand.Utils.OnSearch.getEstimatedFare item
  estimatedTotalFare_ <- Beckn.OnDemand.Utils.OnSearch.getEstimatedFare item
  itemId_ <- Beckn.OnDemand.Utils.OnSearch.getItemId item
  let nightShiftInfo_ = Beckn.OnDemand.Utils.OnSearch.buildNightShiftInfo item
  specialLocationTag_ <- Beckn.OnDemand.Utils.OnSearch.buildSpecialLocationTag item
  totalFareRange_ <- Beckn.OnDemand.Utils.OnSearch.getTotalFareRange item
  vehicleVariant_ <- Beckn.OnDemand.Utils.OnSearch.getVehicleVariant provider item
  waitingCharges_ <- Beckn.OnDemand.Utils.OnSearch.buildWaitingChargeInfo item
  estimateBreakupList_ <- Beckn.OnDemand.Utils.OnSearch.buildEstimateBreakupList item
  pure $ Domain.Action.Beckn.OnSearch.EstimateInfo {bppEstimateId = bppEstimateId_, descriptions = descriptions_, discount = discount_, driversLocation = driversLocation_, estimateBreakupList = estimateBreakupList_, estimatedFare = estimatedFare_, estimatedTotalFare = estimatedTotalFare_, itemId = itemId_, nightShiftInfo = nightShiftInfo_, specialLocationTag = specialLocationTag_, totalFareRange = totalFareRange_, vehicleVariant = vehicleVariant_, waitingCharges = waitingCharges_}

tfProviderInfo :: (Monad m, Kernel.Types.App.MonadFlow m) => BecknV2.OnDemand.Types.OnSearchReq -> m Domain.Action.Beckn.OnSearch.ProviderInfo
tfProviderInfo req = do
  let mobileNumber_ = ""
  name_ <- Beckn.OnDemand.Utils.OnSearch.getProviderName req
  let ridesCompleted_ = 0
  providerId_ <- req.onSearchReqContext.contextBppId & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing bpp_id")
  url_ <- Beckn.OnDemand.Utils.Common.getContextBppUri req.onSearchReqContext >>= Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing bpp_uri")
  pure $ Domain.Action.Beckn.OnSearch.ProviderInfo {mobileNumber = mobileNumber_, name = name_, providerId = providerId_, ridesCompleted = ridesCompleted_, url = url_}

tfQuoteDetails :: (Monad m, Kernel.Types.App.MonadFlow m) => [BecknV2.OnDemand.Types.Fulfillment] -> BecknV2.OnDemand.Types.Item -> m Domain.Action.Beckn.OnSearch.QuoteDetails
tfQuoteDetails fulfillments item = do
  quoteId_ <- Beckn.OnDemand.Utils.OnSearch.getQuoteFulfillmentId item
  fulfillment <- filter (\f -> f.fulfillmentId == Just quoteId_) fulfillments & Data.Maybe.listToMaybe & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing fulfillment for item")
  fulfillmentType <- fulfillment.fulfillmentType & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing fulfillment type")
  case fulfillmentType of
    "RENTAL" -> do
      quoteInfo <- builRentalQuoteInfo item quoteId_ & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing rental quote details")
      pure $ Domain.Action.Beckn.OnSearch.RentalDetails quoteInfo
    _ -> pure $ Domain.Action.Beckn.OnSearch.OneWaySpecialZoneDetails (Domain.Action.Beckn.OnSearch.OneWaySpecialZoneQuoteDetails {quoteId = quoteId_})

builRentalQuoteInfo :: BecknV2.OnDemand.Types.Item -> Text -> Maybe Domain.Action.Beckn.OnSearch.RentalQuoteDetails
builRentalQuoteInfo item quoteId_ = do
  itemTags <- item.itemTags
  let id = quoteId_
  baseFare <- Beckn.OnDemand.Utils.OnSearch.getRentalBaseFare itemTags
  perHourCharge <- Beckn.OnDemand.Utils.OnSearch.getRentalPerHourCharge itemTags
  perExtraMinRate <- Beckn.OnDemand.Utils.OnSearch.getRentalPerExtraMinRate itemTags
  perExtraKmRate <- Beckn.OnDemand.Utils.OnSearch.getRentalPerExtraKmRate itemTags
  includedKmPerHr <- Beckn.OnDemand.Utils.OnSearch.getRentalIncludedKmPerHr itemTags
  plannedPerKmRate <- Beckn.OnDemand.Utils.OnSearch.getRentalPlannedPerKmRate itemTags
  let nightShiftInfo = Beckn.OnDemand.Utils.OnSearch.buildNightShiftInfo item
  Just $ Domain.Action.Beckn.OnSearch.RentalQuoteDetails {..}

tfQuotesInfo :: (Monad m, Kernel.Types.App.MonadFlow m) => BecknV2.OnDemand.Types.Provider -> [BecknV2.OnDemand.Types.Fulfillment] -> BecknV2.OnDemand.Types.Item -> m Domain.Action.Beckn.OnSearch.QuoteInfo
tfQuotesInfo provider fulfillments item = do
  let descriptions_ = []
  let discount_ = Nothing
  estimatedFare_ <- Beckn.OnDemand.Utils.OnSearch.getEstimatedFare item
  estimatedTotalFare_ <- Beckn.OnDemand.Utils.OnSearch.getEstimatedFare item
  itemId_ <- Beckn.OnDemand.Utils.OnSearch.getItemId item
  specialLocationTag_ <- Beckn.OnDemand.Utils.OnSearch.buildSpecialLocationTag item
  vehicleVariant_ <- Beckn.OnDemand.Utils.OnSearch.getVehicleVariant provider item
  quoteDetails_ <- tfQuoteDetails fulfillments item
  pure $ Domain.Action.Beckn.OnSearch.QuoteInfo {descriptions = descriptions_, discount = discount_, estimatedFare = estimatedFare_, estimatedTotalFare = estimatedTotalFare_, itemId = itemId_, quoteDetails = quoteDetails_, specialLocationTag = specialLocationTag_, vehicleVariant = vehicleVariant_}
