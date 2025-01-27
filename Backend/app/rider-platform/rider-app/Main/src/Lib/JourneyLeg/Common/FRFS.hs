module Lib.JourneyLeg.Common.FRFS where

import qualified API.Types.UI.FRFSTicketService as API
import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import Control.Applicative
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import Domain.Types.FRFSQuote
import Domain.Types.FRFSSearch
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DPerson
import Domain.Types.Station
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JPT
import Lib.JourneyModule.Location
import qualified Lib.JourneyModule.Types as JT
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.Station as QStation

getState :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Id FRFSSearch -> [APITypes.RiderLocationReq] -> Bool -> m JT.JourneyLegState
getState searchId riderLastPoints isLastJustCompleted = do
  mbBooking <- QTBooking.findBySearchId searchId
  case mbBooking of
    Just booking -> do
      (statusChanged, newStatus) <- processOldStatus booking.journeyLegStatus booking.toStationId
      when statusChanged $ QTBooking.updateJourneyLegStatus (Just newStatus) booking.id
      journeyLegOrder <- booking.journeyLegOrder & fromMaybeM (BookingFieldNotPresent "journeyLegOrder")
      return $
        JT.JourneyLegState
          { status = if newStatus == JPT.InPlan then JT.getFRFSLegStatusFromBooking booking else newStatus,
            currentPosition = (.latLong) <$> listToMaybe riderLastPoints,
            legOrder = journeyLegOrder,
            statusChanged
          }
    Nothing -> do
      searchReq <- QFRFSSearch.findById searchId >>= fromMaybeM (SearchRequestNotFound searchId.getId)
      (statusChanged, newStatus) <- processOldStatus searchReq.journeyLegStatus searchReq.toStationId
      when statusChanged $ QFRFSSearch.updateJourneyLegStatus (Just newStatus) searchReq.id
      journeyLegInfo <- searchReq.journeyLegInfo & fromMaybeM (InvalidRequest "JourneySearchData not found")
      return $
        JT.JourneyLegState
          { status = newStatus,
            currentPosition = (.latLong) <$> listToMaybe riderLastPoints,
            legOrder = journeyLegInfo.journeyLegOrder,
            statusChanged
          }
  where
    processOldStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe JPT.JourneyLegStatus -> Id Station -> m (Bool, JPT.JourneyLegStatus)
    processOldStatus mbOldStatus toStationId = do
      mbToStation <- QStation.findById toStationId
      let mbToLatLong = LatLong <$> (mbToStation >>= (.lat)) <*> (mbToStation >>= (.lon))
      let oldStatus = fromMaybe (if isLastJustCompleted then JPT.Ongoing else JPT.InPlan) mbOldStatus
      return $ maybe (False, oldStatus) (\latLong -> updateJourneyLegStatus riderLastPoints latLong oldStatus isLastJustCompleted) mbToLatLong

getInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Id FRFSSearch -> Maybe HighPrecMoney -> m JT.LegInfo
getInfo searchId fallbackFare = do
  mbBooking <- QTBooking.findBySearchId searchId
  case mbBooking of
    Just booking -> do
      JT.mkLegInfoFromFrfsBooking booking
    Nothing -> do
      searchReq <- QFRFSSearch.findById searchId >>= fromMaybeM (SearchRequestNotFound searchId.getId)
      JT.mkLegInfoFromFrfsSearchRequest searchReq fallbackFare

search :: JT.SearchRequestFlow m r c => Spec.VehicleCategory -> Id DPerson.Person -> Id DMerchant.Merchant -> Int -> Context.City -> DJourneyLeg.JourneyLeg -> m JT.SearchResponse
search vehicleCategory personId merchantId quantity city journeyLeg = do
  let journeySearchData =
        JPT.JourneySearchData
          { journeyId = journeyLeg.journeyId.getId,
            journeyLegOrder = journeyLeg.sequenceNumber,
            agency = journeyLeg.agency <&> (.name),
            skipBooking = False,
            convenienceCost = 0,
            pricingId = Nothing
          }
  frfsSearchReq <- buildFRFSSearchReq (Just journeySearchData)
  let colorName = journeyLeg.routeDetails >>= (.shortName)
  let routeColorCode = journeyLeg.routeDetails >>= (.color)
  let frequency = Just 300
  res <- FRFSTicketService.postFrfsSearchHandler (Just personId, merchantId) (Just city) vehicleCategory frfsSearchReq Nothing Nothing colorName routeColorCode frequency
  return $ JT.SearchResponse {id = res.searchId.getId}
  where
    buildFRFSSearchReq journeySearchData = do
      fromStationCode <- ((journeyLeg.fromStopDetails >>= (.stopCode)) <|> (journeyLeg.fromStopDetails >>= (.gtfsId))) & fromMaybeM (InvalidRequest "From station code and gtfsId not found")
      toStationCode <- ((journeyLeg.toStopDetails >>= (.stopCode)) <|> (journeyLeg.toStopDetails >>= (.gtfsId))) & fromMaybeM (InvalidRequest "To station code and gtfsId not found")
      createStationIfRequired (journeyLeg.fromStopDetails >>= (.name)) fromStationCode journeyLeg.startLocation.latitude journeyLeg.startLocation.longitude
      createStationIfRequired (journeyLeg.toStopDetails >>= (.name)) toStationCode journeyLeg.endLocation.latitude journeyLeg.endLocation.longitude
      let routeCode = Nothing
      return $ API.FRFSSearchAPIReq {..}

    createStationIfRequired name code lat lon = do
      merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> "-city-" <> show city)
      mbStation <- QStation.findByStationCodeAndMerchantOperatingCityId code merchantOpCity.id
      when (isNothing mbStation) $ do
        mbNewStation <- createStation name code lat lon merchantOpCity.id
        whenJust mbNewStation $ \station -> QStation.create station

    createStation :: JT.SearchRequestFlow m r c => Maybe Text -> Text -> Double -> Double -> Id MerchantOperatingCity -> m (Maybe Station)
    createStation Nothing _ _ _ _ = return Nothing
    createStation (Just name) code lat lon merchantOpCityId = do
      newId <- generateGUID
      now <- getCurrentTime
      return $
        Just $
          Station
            { id = newId,
              vehicleType = vehicleCategory,
              name = name,
              possibleTypes = Nothing,
              code = code,
              lat = Just lat,
              lon = Just lon,
              address = Nothing,
              merchantId = merchantId,
              timeBounds = Kernel.Types.TimeBound.Unbounded,
              merchantOperatingCityId = merchantOpCityId,
              createdAt = now,
              updatedAt = now
            }

confirm :: JT.ConfirmFlow m r c => Id DPerson.Person -> Id DMerchant.Merchant -> Maybe (Id FRFSQuote) -> Bool -> Bool -> m ()
confirm personId merchantId mbQuoteId skipBooking bookingAllowed = do
  when (not skipBooking && bookingAllowed) $ do
    quoteId <- mbQuoteId & fromMaybeM (InvalidRequest "You can't confirm bus before getting the fare")
    void $ FRFSTicketService.postFrfsQuoteConfirm (Just personId, merchantId) quoteId
