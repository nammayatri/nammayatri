{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.OnSearch where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.OnSearch
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnSearch
import Domain.Types
import qualified Domain.Types.VehicleVariant as DVeh
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common (logInfo, type (:::))
import Kernel.Utils.Error
import Tools.Error

buildOnSearchReq :: (Monad m, Kernel.Types.App.MonadFlow m) => BecknV2.OnDemand.Types.OnSearchReq -> BecknV2.OnDemand.Types.Provider -> [BecknV2.OnDemand.Types.Item] -> [BecknV2.OnDemand.Types.Fulfillment] -> Kernel.Prelude.UTCTime -> m Domain.Action.Beckn.OnSearch.DOnSearchReq
buildOnSearchReq req provider items fulfillments validTill = do
  let paymentMethodsInfo_ = []
  providerInfo_ <- tfProviderInfo req
  (estimatesInfo_, quotesInfo_) <- partitionEithers <$> traverse (tfQuotesInfo provider fulfillments validTill) items
  requestId_ <- BecknV2.OnDemand.Utils.Common.getTransactionId req.onSearchReqContext
  pure $ Domain.Action.Beckn.OnSearch.DOnSearchReq {estimatesInfo = estimatesInfo_, paymentMethodsInfo = paymentMethodsInfo_, providerInfo = providerInfo_, quotesInfo = quotesInfo_, requestId = Id requestId_}

tfProviderInfo :: (Monad m, Kernel.Types.App.MonadFlow m) => BecknV2.OnDemand.Types.OnSearchReq -> m Domain.Action.Beckn.OnSearch.ProviderInfo
tfProviderInfo req = do
  let mobileNumber_ = ""
  name_ <- Beckn.OnDemand.Utils.OnSearch.getProviderName req
  let ridesCompleted_ = 0
  providerId_ <- req.onSearchReqMessage >>= (.onSearchReqMessageCatalog.catalogProviders) >>= Kernel.Prelude.listToMaybe >>= (.providerId) & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing provider_id")
  url_ <- Beckn.OnDemand.Utils.Common.getContextBppUri req.onSearchReqContext >>= Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing bpp_uri")
  pure $ Domain.Action.Beckn.OnSearch.ProviderInfo {mobileNumber = mobileNumber_, name = name_, providerId = providerId_, ridesCompleted = ridesCompleted_, url = url_}

buildRentalQuoteInfo :: BecknV2.OnDemand.Types.Item -> Text -> Currency -> Maybe Domain.Action.Beckn.OnSearch.RentalQuoteDetails
buildRentalQuoteInfo item quoteId_ currency = do
  let itemTags = item.itemTags
  let id = quoteId_
  baseFare <- Beckn.OnDemand.Utils.OnSearch.getBaseFare itemTags currency
  perHourCharge <- Beckn.OnDemand.Utils.OnSearch.getPerHourCharge itemTags currency
  perExtraMinRate <- Beckn.OnDemand.Utils.OnSearch.getPerExtraMinRate itemTags currency
  perExtraKmRate <- Beckn.OnDemand.Utils.OnSearch.getPerExtraKmRate itemTags currency
  includedDistancePerHr <- Beckn.OnDemand.Utils.OnSearch.getIncludedKmPerHr itemTags
  plannedPerKmRate <- Beckn.OnDemand.Utils.OnSearch.getPlannedPerKmRate itemTags currency
  deadKmFare <- Beckn.OnDemand.Utils.OnSearch.getDeadKilometerFare itemTags currency
  let nightShiftInfo = Beckn.OnDemand.Utils.OnSearch.buildNightShiftInfo item currency
  Just $ Domain.Action.Beckn.OnSearch.RentalQuoteDetails {..}

buildInterCityQuoteInfo :: BecknV2.OnDemand.Types.Item -> Text -> Currency -> Maybe Domain.Action.Beckn.OnSearch.InterCityQuoteDetails
buildInterCityQuoteInfo item quoteId_ currency = do
  let itemTags = item.itemTags
  let quoteId = quoteId_
  let defaultPrice =
        -- for backward compatibility (fix properly later)
        Price
          { amountInt = Money 0,
            amount = HighPrecMoney 0.0,
            currency = INR
          }
  let baseFare = fromMaybe defaultPrice (Beckn.OnDemand.Utils.OnSearch.getBaseFare itemTags currency)
  let perHourCharge = fromMaybe defaultPrice (Beckn.OnDemand.Utils.OnSearch.getPerHourCharge itemTags currency)
  let perExtraMinRate = fromMaybe defaultPrice (Beckn.OnDemand.Utils.OnSearch.getPerExtraMinRate itemTags currency)
  let perExtraKmRate = fromMaybe defaultPrice (Beckn.OnDemand.Utils.OnSearch.getPerExtraKmRate itemTags currency)
  let kmPerPlannedExtraHour = fromMaybe 0 (Beckn.OnDemand.Utils.OnSearch.getIncludedKmPerHr itemTags)
  let plannedPerKmRateOneWay = fromMaybe defaultPrice (Beckn.OnDemand.Utils.OnSearch.getPlannedPerKmRate itemTags currency)
  let plannedPerKmRateRoundTrip = fromMaybe defaultPrice (Beckn.OnDemand.Utils.OnSearch.getPlannedPerKmRateRoundTrip itemTags currency)
  let perDayMaxHourAllowance = fromMaybe (Hours 0) (Beckn.OnDemand.Utils.OnSearch.getPerDayMaxHourAllowance itemTags)
  let perDayMaxAllowanceInMins' = fromMaybe (Minutes 0) (Beckn.OnDemand.Utils.OnSearch.getPerDayMaxAllowanceInMins itemTags)
  let deadKmFare = fromMaybe defaultPrice (Beckn.OnDemand.Utils.OnSearch.getDeadKilometerFare itemTags currency)
  let nightShiftInfo = Beckn.OnDemand.Utils.OnSearch.buildNightShiftInfo item currency
  Just $ Domain.Action.Beckn.OnSearch.InterCityQuoteDetails {perDayMaxAllowanceInMins = Just perDayMaxAllowanceInMins', ..}

tfQuotesInfo :: (Monad m, Kernel.Types.App.MonadFlow m) => BecknV2.OnDemand.Types.Provider -> [BecknV2.OnDemand.Types.Fulfillment] -> Kernel.Prelude.UTCTime -> BecknV2.OnDemand.Types.Item -> m (Either Domain.Action.Beckn.OnSearch.EstimateInfo Domain.Action.Beckn.OnSearch.QuoteInfo)
tfQuotesInfo provider fulfillments validTill item = do
  let descriptions_ = []
  let discount_ = Nothing
  currency <- getCurrency item
  estimatedFare_ <- Beckn.OnDemand.Utils.OnSearch.getEstimatedFare item currency
  estimatedTotalFare_ <- Beckn.OnDemand.Utils.OnSearch.getEstimatedFare item currency
  itemId_ <- Beckn.OnDemand.Utils.OnSearch.getItemId item
  specialLocationTag_ <- Beckn.OnDemand.Utils.OnSearch.buildSpecialLocationTag item
  let specialLocationName_ = Beckn.OnDemand.Utils.OnSearch.getSpecialLocationName item
  (vehicleVariant_, vehicleCapacity_) <- Beckn.OnDemand.Utils.OnSearch.getVehicleVariant provider item
  vehicleServiceTierAirConditioned_ <- Beckn.OnDemand.Utils.OnSearch.getVehicleServiceTierAirConditioned provider item
  isAirConditioned_ <- Beckn.OnDemand.Utils.OnSearch.getIsAirConditioned provider item
  let mbServiceTierType = Beckn.OnDemand.Utils.OnSearch.getServiceTierType item
      mbServiceTierName = Beckn.OnDemand.Utils.OnSearch.getServiceTierName item
      vehicleCategory = BecknV2.OnDemand.Utils.Common.mapVariantToVehicle vehicleVariant_
      mbServiceTierShortDesc = Beckn.OnDemand.Utils.OnSearch.getServiceTierShortDesc item
      isCustomerPrefferedSearchRoute_ = Beckn.OnDemand.Utils.OnSearch.getIsCustomerPrefferedSearchRoute item
      isBlockedRoute_ = Beckn.OnDemand.Utils.OnSearch.getIsBlockedRoute item
      tollChargesInfo_ = Beckn.OnDemand.Utils.OnSearch.buildTollChargesInfo item currency
      estimatedPickupDuration = Beckn.OnDemand.Utils.OnSearch.getestimatedPickupDuration item
  quoteOrEstId_ <- Beckn.OnDemand.Utils.OnSearch.getQuoteFulfillmentId item
  fulfillment <- find (\f -> f.fulfillmentId == Just quoteOrEstId_) fulfillments & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing fulfillment for item")
  tripCategory <- (fulfillment.fulfillmentType >>= (Just . BecknV2.OnDemand.Utils.Common.fulfillmentTypeToTripCategory)) & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing fulfillment type")
  case tripCategoryToPricingPolicy tripCategory of
    EstimateBased _ -> do
      let bppEstimateId_ = Id itemId_
      driversLocation_ <- Beckn.OnDemand.Utils.OnSearch.getProviderLocation provider vehicleVariant_
      let nightShiftInfo_ = Beckn.OnDemand.Utils.OnSearch.buildNightShiftInfo item currency
      totalFareRange_ <- Beckn.OnDemand.Utils.OnSearch.getTotalFareRange item currency
      waitingCharges_ <- Beckn.OnDemand.Utils.OnSearch.buildWaitingChargeInfo item currency
      estimateBreakupList_ <- Beckn.OnDemand.Utils.OnSearch.buildEstimateBreakupList item currency
      pure $
        Left $
          Domain.Action.Beckn.OnSearch.EstimateInfo
            { bppEstimateId = bppEstimateId_,
              descriptions = descriptions_,
              discount = discount_,
              driversLocation = driversLocation_,
              estimateBreakupList = estimateBreakupList_,
              estimatedFare = estimatedFare_,
              estimatedTotalFare = estimatedTotalFare_,
              itemId = itemId_,
              nightShiftInfo = nightShiftInfo_,
              specialLocationTag = specialLocationTag_,
              specialLocationName = specialLocationName_,
              totalFareRange = totalFareRange_,
              vehicleVariant = vehicleVariant_,
              waitingCharges = waitingCharges_,
              validTill,
              serviceTierName = mbServiceTierName,
              serviceTierType = mbServiceTierType,
              serviceTierShortDesc = mbServiceTierShortDesc,
              isAirConditioned = isAirConditioned_,
              isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute_,
              isBlockedRoute = isBlockedRoute_,
              tollChargesInfo = tollChargesInfo_,
              estimatedPickupDuration = estimatedPickupDuration,
              vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned_,
              vehicleServiceTierSeatingCapacity = vehicleCapacity_,
              tripCategory = tripCategory,
              vehicleCategory
            }
    QuoteBased _ -> do
      quoteBreakupList_ <- Beckn.OnDemand.Utils.OnSearch.buildQuoteBreakupList item currency
      quoteDetails_ <-
        case tripCategory of
          Rental _ -> do
            quoteInfo <- buildRentalQuoteInfo item quoteOrEstId_ currency & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing rental quote details")
            pure $ Domain.Action.Beckn.OnSearch.RentalDetails quoteInfo
          OneWay OneWayRideOtp -> pure $ Domain.Action.Beckn.OnSearch.OneWaySpecialZoneDetails (Domain.Action.Beckn.OnSearch.OneWaySpecialZoneQuoteDetails {quoteId = quoteOrEstId_})
          OneWay OneWayOnDemandStaticOffer -> pure $ Domain.Action.Beckn.OnSearch.OneWayScheduledDetails (Domain.Action.Beckn.OnSearch.OneWayScheduledQuoteDetails {quoteId = quoteOrEstId_})
          InterCity OneWayRideOtp _ -> pure $ Domain.Action.Beckn.OnSearch.OneWaySpecialZoneDetails (Domain.Action.Beckn.OnSearch.OneWaySpecialZoneQuoteDetails {quoteId = quoteOrEstId_})
          InterCity _ _ -> do
            interCityQuoteInfo <- buildInterCityQuoteInfo item quoteOrEstId_ currency & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing intercity quote details")
            pure $ Domain.Action.Beckn.OnSearch.InterCityDetails interCityQuoteInfo
          ft -> throwError (InternalError $ "tfQuotesInfo not implemented for fulfillmentType: " <> show ft)
      pure $
        Right $
          Domain.Action.Beckn.OnSearch.QuoteInfo
            { descriptions = descriptions_,
              discount = discount_,
              estimatedFare = estimatedFare_,
              estimatedTotalFare = estimatedTotalFare_,
              itemId = itemId_,
              quoteDetails = quoteDetails_,
              specialLocationTag = specialLocationTag_,
              specialLocationName = specialLocationName_,
              vehicleVariant = vehicleVariant_,
              validTill,
              serviceTierName = mbServiceTierName,
              serviceTierType = mbServiceTierType,
              serviceTierShortDesc = mbServiceTierShortDesc,
              isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute_,
              isBlockedRoute = isBlockedRoute_,
              tollChargesInfo = tollChargesInfo_,
              estimatedPickupDuration = estimatedPickupDuration,
              isAirConditioned = isAirConditioned_,
              vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned_,
              vehicleServiceTierSeatingCapacity = vehicleCapacity_,
              quoteBreakupList = quoteBreakupList_,
              tripCategory = tripCategory,
              vehicleCategory
            }

getCurrency :: Kernel.Types.App.MonadFlow m => BecknV2.OnDemand.Types.Item -> m Currency
getCurrency item =
  item.itemPrice
    >>= (.priceCurrency)
    >>= readMaybe @Currency
    & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing Currency")
