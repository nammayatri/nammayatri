{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FullEntityBuilders where

import Domain.Types.Booking as Booking
import Domain.Types.Estimate
import Domain.Types.FarePolicy.FareProductType
import qualified Domain.Types.LocationMapping as Domain
import Domain.Types.Quote as Quote
import Domain.Types.SearchRequest as SR
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Storage.Queries.EstimateBreakup as QEB
import Storage.Queries.LocationMapping as QLM
import qualified Storage.Tabular.Booking as Booking
import Storage.Tabular.DriverOffer
import Storage.Tabular.Estimate
import Storage.Tabular.Estimate.Instances
import Storage.Tabular.Location
import Storage.Tabular.LocationMapping
import Storage.Tabular.Quote as Quote
import Storage.Tabular.Quote.Instances as Quote
import Storage.Tabular.RentalSlab
import Storage.Tabular.SearchRequest (FullSearchRequestT, SearchRequestT)
import qualified Storage.Tabular.SearchRequest as SR
import Storage.Tabular.SpecialZoneQuote
import Storage.Tabular.TripTerms

buildFullQuote ::
  Transactionable m =>
  (QuoteT, Maybe TripTermsT, Maybe RentalSlabT, Maybe DriverOfferT, Maybe SpecialZoneQuoteT) ->
  DTypeBuilder m (Maybe (SolidType FullQuoteT))
buildFullQuote (quoteT@QuoteT {..}, mbTripTermsT, mbRentalSlab, mbDriverOffer, mbspecialZoneQuote) = runMaybeT $ do
  quoteDetailsT <- case fareProductType of
    ONE_WAY -> pure Quote.OneWayDetailsT
    RENTAL -> hoistMaybe (Quote.RentalDetailsT <$> mbRentalSlab)
    DRIVER_OFFER -> hoistMaybe (Quote.DriverOfferDetailsT <$> mbDriverOffer)
    ONE_WAY_SPECIAL_ZONE -> hoistMaybe (Quote.OneWaySpecialZoneDetailsT <$> mbspecialZoneQuote)
  return $ extractSolidType @Quote (quoteT, mbTripTermsT, quoteDetailsT)

getLocationMappingOrder :: LocationMappingT -> Int
getLocationMappingOrder LocationMappingT {..} = order

buildFullSearchRequest ::
  Transactionable m =>
  SearchRequestT ->
  DTypeBuilder m (Maybe (SolidType FullSearchRequestT))
buildFullSearchRequest searchRequestT@SR.SearchRequestT {..} = do
  mappings <- Esq.findAll' $ do
    mapping <- from $ table @LocationMappingT
    where_ $ mapping ^. LocationMappingTagId ==. val id
    orderBy [asc $ mapping ^. LocationMappingOrder]
    return mapping
  let allIds :: [LocationTId] = map (\(LocationMappingT _ locationId _ _ _ _) -> locationId) mappings
  mAllLocations <- mapM (Esq.findById' @LocationT) allIds
  let allLocations = sequence mAllLocations
  case allLocations of
    Just locations -> do
      searchReq <- runMaybeT $ do
        return (searchRequestT, head locations, drop 1 locations)
      case searchReq of
        Just sReq -> return $ Just (extractSolidType @SearchRequest sReq)
        Nothing -> return Nothing
    Nothing -> return Nothing

buildFullBooking ::
  Transactionable m =>
  Booking.BookingT ->
  DTypeBuilder m (Maybe (SolidType Booking.FullBookingT))
buildFullBooking bookingT@Booking.BookingT {..} = do
  mappings <- Esq.findAll' $ do
    mapping <- from $ table @LocationMappingT
    where_ $ mapping ^. LocationMappingTagId ==. val id
    orderBy [asc $ mapping ^. LocationMappingOrder]
    return mapping
  let allIds :: [LocationTId] = map (\(LocationMappingT _ locationId _ _ _ _) -> locationId) mappings
  mAllLocations <- mapM (Esq.findById' @LocationT) allIds
  tripTerms <- case tripTermsId of
    Just _ -> return Nothing
    Nothing -> return Nothing
  let allLocations = sequence mAllLocations

  case allLocations of
    Just locations -> do
      bookin <- runMaybeT $ do
        let mbToLocT = drop 1 locations
        bookingDetails <- case fareProductType of
          ONE_WAY -> hoistMaybe (Booking.OneWayDetailsT <$> Just mbToLocT)
          RENTAL -> hoistMaybe (Booking.RentalDetailsT <$> Nothing)
          DRIVER_OFFER -> hoistMaybe (Booking.DriverOfferDetailsT <$> Just mbToLocT)
          ONE_WAY_SPECIAL_ZONE -> hoistMaybe (Booking.OneWaySpecialZoneDetailsT <$> Just mbToLocT)
        return (bookingT, head locations, tripTerms, bookingDetails)
      case bookin of
        Just book -> return $ Just (extractSolidType @Booking book)
        Nothing -> return Nothing
    Nothing -> return Nothing

selectByTagIdAndOrder :: Transactionable m => Text -> m [Domain.LocationMapping]
selectByTagIdAndOrder tagId = Esq.buildDType $ do
  mbFullLocationMappingT <- Esq.findAll' $ do
    (locationMapping :& location) <- from fullLocationMappingTable
    where_ $ locationMapping ^. LocationMappingTagId ==. val tagId
    orderBy [desc $ locationMapping ^. LocationMappingOrder]
    pure (locationMapping, location)
  mapM buildFullLocationMapping mbFullLocationMappingT

buildFullEstimate ::
  Transactionable m =>
  (EstimateT, Maybe TripTermsT) ->
  DTypeBuilder m (SolidType FullEstimateT)
buildFullEstimate (estimateT@EstimateT {..}, tripTermsT) = do
  estimateBreakupT <- QEB.findAllByEstimateId' (EstimateTKey id)
  return $ extractSolidType @Estimate (estimateT, estimateBreakupT, tripTermsT)
