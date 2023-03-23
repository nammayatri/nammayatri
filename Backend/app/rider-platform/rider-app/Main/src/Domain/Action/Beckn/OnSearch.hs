{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnSearch
  ( DOnSearchReq (..),
    ProviderInfo (..),
    EstimateInfo (..),
    QuoteInfo (..),
    QuoteDetails (..),
    OneWayQuoteDetails (..),
    OneWaySpecialZoneQuoteDetails (..),
    RecurringQuoteDetails (..),
    RentalQuoteDetails (..),
    EstimateBreakupInfo (..),
    BreakupPriceInfo (..),
    NightShiftInfo (..),
    WaitingChargesInfo (..),
    onSearch,
  )
where

import qualified Data.Either as E
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RecurringQuote as DRecurringQuote
import qualified Domain.Types.RentalSlab as DRentalSlab
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SpecialZoneQuote as DSpecialZoneQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant
import Environment
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RecurringQuote as QRecurringQuote
import qualified Storage.Queries.SearchRequest as QSearchReq
import Tools.Error
import qualified Tools.Metrics as Metrics

data DOnSearchReq = DOnSearchReq
  { requestId :: Id DSearchReq.SearchRequest,
    providerInfo :: ProviderInfo,
    estimatesInfo :: [EstimateInfo],
    quotesInfo :: [QuoteInfo]
  }

data ProviderInfo = ProviderInfo
  { providerId :: Text,
    name :: Text,
    url :: BaseUrl,
    mobileNumber :: Text,
    ridesCompleted :: Int
  }

data EstimateInfo = EstimateInfo
  { vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    totalFareRange :: DEstimate.FareRange,
    descriptions :: [Text],
    estimateBreakupList :: [EstimateBreakupInfo],
    nightShiftRate :: Maybe NightShiftInfo,
    waitingCharges :: Maybe WaitingChargesInfo,
    driversLocation :: [LatLong]
  }

data NightShiftInfo = NightShiftInfo
  { nightShiftMultiplier :: Maybe Centesimal,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay
  }

data WaitingChargesInfo = WaitingChargesInfo
  { waitingTimeEstimatedThreshold :: Maybe Seconds,
    waitingChargePerMin :: Maybe Money
  }

data EstimateBreakupInfo = EstimateBreakupInfo
  { title :: Text,
    price :: BreakupPriceInfo
  }

data BreakupPriceInfo = BreakupPriceInfo
  { currency :: Text,
    value :: Money
  }

data QuoteInfo = QuoteInfo
  { vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    quoteDetails :: QuoteDetails,
    descriptions :: [Text]
  }

data QuoteDetails
  = OneWayDetails OneWayQuoteDetails
  | RentalDetails RentalQuoteDetails
  | OneWaySpecialZoneDetails OneWaySpecialZoneQuoteDetails
  | RecurringDetails RecurringQuoteDetails

newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: HighPrecMeters
  }

newtype OneWaySpecialZoneQuoteDetails = OneWaySpecialZoneQuoteDetails
  { quoteId :: Text
  }

data RentalQuoteDetails = RentalQuoteDetails
  { baseDistance :: Kilometers,
    baseDuration :: Hours
  }

newtype RecurringQuoteDetails = RecurringQuoteDetails
  { distanceToNearestDriver :: HighPrecMeters
  }

onSearch ::
  Text ->
  Maybe DOnSearchReq ->
  Flow ()
onSearch transactionId mbReq = do
  whenJust mbReq (onSearchService transactionId)

onSearchService ::
  Text ->
  DOnSearchReq ->
  Flow ()
onSearchService transactionId DOnSearchReq {..} = do
  searchRequest <- QSearchReq.findById requestId >>= fromMaybeM (SearchRequestDoesNotExist requestId.getId)
  merchant <- QMerch.findById searchRequest.merchantId >>= fromMaybeM (MerchantNotFound searchRequest.merchantId.getId)
  Metrics.finishSearchMetrics merchant.name transactionId
  now <- getCurrentTime
  estimates <- traverse (buildEstimate requestId providerInfo now) estimatesInfo
  (quotes, recurringQuotes) <-
    E.partitionEithers <$> traverse (buildQuote requestId providerInfo now searchRequest.merchantId) quotesInfo
  DB.runTransaction do
    QEstimate.createMany estimates
    QQuote.createMany quotes
    QRecurringQuote.createMany recurringQuotes
    QPFS.updateStatus searchRequest.riderId DPFS.GOT_ESTIMATE {requestId = searchRequest.id, validTill = searchRequest.validTill}

buildEstimate ::
  MonadFlow m =>
  Id DSearchReq.SearchRequest ->
  ProviderInfo ->
  UTCTime ->
  EstimateInfo ->
  m DEstimate.Estimate
buildEstimate requestId providerInfo now EstimateInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  estimateBreakupList' <- buildEstimateBreakUp estimateBreakupList uid
  pure
    DEstimate.Estimate
      { id = uid,
        autoAssignEnabled = False,
        autoAssignQuoteId = Nothing,
        autoAssignEnabledV2 = False,
        providerMobileNumber = providerInfo.mobileNumber,
        providerName = providerInfo.name,
        providerCompletedRidesCount = providerInfo.ridesCompleted,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        createdAt = now,
        updatedAt = now,
        status = Just DEstimate.NEW,
        estimateBreakupList = estimateBreakupList',
        driversLocation = driversLocation,
        nightShiftRate =
          Just $
            DEstimate.NightShiftRate
              { nightShiftMultiplier = nightShiftRate >>= (.nightShiftMultiplier),
                nightShiftStart = nightShiftRate >>= (.nightShiftStart),
                nightShiftEnd = nightShiftRate >>= (.nightShiftEnd)
              },
        waitingCharges =
          DEstimate.WaitingCharges
            { waitingChargePerMin = waitingCharges >>= (.waitingChargePerMin),
              waitingTimeEstimatedThreshold = waitingCharges >>= (.waitingTimeEstimatedThreshold)
            },
        ..
      }

buildQuote ::
  MonadFlow m =>
  Id DSearchReq.SearchRequest ->
  ProviderInfo ->
  UTCTime ->
  Id DMerchant.Merchant ->
  QuoteInfo ->
  m (Either DQuote.Quote DRecurringQuote.RecurringQuote)
buildQuote requestId providerInfo now merchantId QuoteInfo {..} = do
  uid <- generateGUID
  uid' <- generateGUID
  tripTerms <- buildTripTerms descriptions
  eQuoteDetails <- case quoteDetails of
    OneWayDetails oneWayDetails ->
      pure . Left $ DQuote.OneWayDetails $ mkOneWayQuoteDetails oneWayDetails
    RentalDetails rentalSlab -> do
      Left . DQuote.RentalDetails <$> buildRentalSlab rentalSlab
    OneWaySpecialZoneDetails details -> do
      Left . DQuote.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneQuoteDetails details
    RecurringDetails recurringDetails ->
      pure . Right $ mkRecurringQuoteDetails recurringDetails
  pure $
    case eQuoteDetails of
      Left nonRecurringQuoteDetails ->
        Left $
          DQuote.Quote
            { id = uid,
              providerMobileNumber = providerInfo.mobileNumber,
              providerName = providerInfo.name,
              providerCompletedRidesCount = providerInfo.ridesCompleted,
              providerId = providerInfo.providerId,
              providerUrl = providerInfo.url,
              createdAt = now,
              quoteDetails = nonRecurringQuoteDetails,
              ..
            }
      Right recurringQuoteDetails ->
        Right $
          DRecurringQuote.RecurringQuote
            { id = uid',
              providerId = providerInfo.providerId,
              providerUrl = providerInfo.url,
              quoteDetails = recurringQuoteDetails,
              createdAt = now,
              ..
            }

mkOneWayQuoteDetails :: OneWayQuoteDetails -> DQuote.OneWayQuoteDetails
mkOneWayQuoteDetails OneWayQuoteDetails {..} = DQuote.OneWayQuoteDetails {..}

buildOneWaySpecialZoneQuoteDetails :: MonadFlow m => OneWaySpecialZoneQuoteDetails -> m DSpecialZoneQuote.SpecialZoneQuote
buildOneWaySpecialZoneQuoteDetails OneWaySpecialZoneQuoteDetails {..} = do
  id <- generateGUID
  pure DSpecialZoneQuote.SpecialZoneQuote {..}

mkRecurringQuoteDetails :: RecurringQuoteDetails -> DRecurringQuote.RecurringQuoteDetails
mkRecurringQuoteDetails RecurringQuoteDetails {..} = DRecurringQuote.RecurringQuoteDetails {..}

buildRentalSlab :: MonadFlow m => RentalQuoteDetails -> m DRentalSlab.RentalSlab
buildRentalSlab RentalQuoteDetails {..} = do
  id <- generateGUID
  pure DRentalSlab.RentalSlab {..}

buildTripTerms ::
  MonadFlow m =>
  [Text] ->
  m (Maybe DTripTerms.TripTerms)
buildTripTerms [] = pure Nothing
buildTripTerms descriptions = do
  id <- generateGUID
  pure . Just $ DTripTerms.TripTerms {..}

buildEstimateBreakUp ::
  MonadFlow m =>
  [EstimateBreakupInfo] ->
  Id DEstimate.Estimate ->
  m [DEstimate.EstimateBreakup]
buildEstimateBreakUp estimatesItems estId =
  estimatesItems
    `for` \estimateItem -> do
      id <- generateGUID
      price' <- mkEstimatePrice estimateItem.price
      pure
        DEstimate.EstimateBreakup
          { title = estimateItem.title,
            price = price',
            estimateId = estId,
            ..
          }

mkEstimatePrice ::
  MonadFlow m =>
  BreakupPriceInfo ->
  m DEstimate.EstimateBreakupPrice
mkEstimatePrice BreakupPriceInfo {..} = pure DEstimate.EstimateBreakupPrice {..}
