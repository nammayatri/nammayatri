module Lib.JourneyLeg.Common.FRFS where

import qualified API.Types.UI.FRFSTicketService as API
import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Control.Applicative
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import Domain.Types.FRFSQuote
import Domain.Types.FRFSSearch
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DPerson
import Domain.Types.Station
import Domain.Types.Trip as DTrip
import ExternalBPP.CallAPI as CallExternalBPP
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JPT
import Lib.JourneyModule.Location
import qualified Lib.JourneyModule.Types as JT
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.BecknConfig as QBC
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Station as QStation

getState :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DTrip.MultimodalTravelMode -> Id FRFSSearch -> [APITypes.RiderLocationReq] -> Bool -> m JT.JourneyLegState
getState mode searchId riderLastPoints isLastCompleted = do
  mbBooking <- QTBooking.findBySearchId searchId
  case mbBooking of
    Just booking -> do
      (statusChanged, newStatus) <- processOldStatus booking.journeyLegStatus booking.toStationId
      when statusChanged $ QTBooking.updateJourneyLegStatus (Just newStatus) booking.id
      journeyLegOrder <- booking.journeyLegOrder & fromMaybeM (BookingFieldNotPresent "journeyLegOrder")
      return $
        JT.JourneyLegState
          { status = if newStatus == JPT.InPlan then JT.getFRFSLegStatusFromBooking booking else newStatus,
            userPosition = (.latLong) <$> listToMaybe riderLastPoints,
            vehiclePosition = Nothing,
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
            userPosition = (.latLong) <$> listToMaybe riderLastPoints,
            vehiclePosition = Nothing,
            legOrder = journeyLegInfo.journeyLegOrder,
            statusChanged
          }
  where
    processOldStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe JPT.JourneyLegStatus -> Id Station -> m (Bool, JPT.JourneyLegStatus)
    processOldStatus mbOldStatus toStationId = do
      mbToStation <- QStation.findById toStationId
      let mbToLatLong = LatLong <$> (mbToStation >>= (.lat)) <*> (mbToStation >>= (.lon))
      let oldStatus = fromMaybe (if isLastCompleted then JPT.Ongoing else JPT.InPlan) mbOldStatus
      return $ maybe (False, oldStatus) (\latLong -> updateJourneyLegStatus mode riderLastPoints latLong oldStatus isLastCompleted) mbToLatLong

getFare :: (CoreMetrics m, CacheFlow m r, EncFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => DMerchant.Merchant -> MerchantOperatingCity -> Spec.VehicleCategory -> Text -> Text -> Text -> m (Maybe JT.GetFareResponse)
getFare merchant merchantOperatingCity vehicleCategory routeCode startStationCode endStationCode = do
  QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory)
    >>= \case
      Just bapConfig -> do
        try @_ @SomeException (CallExternalBPP.getFares Nothing merchant merchantOperatingCity bapConfig (Just routeCode) startStationCode endStationCode vehicleCategory)
          >>= \case
            Right [] -> do
              logError $ "Getting Empty Fares for Vehicle Category : " <> show vehicleCategory
              return Nothing
            Right (fare : _) -> return (Just $ JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = fare.price.amount.getHighPrecMoney}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = fare.price.amount.getHighPrecMoney}})
            Left err -> do
              logError $ "Exception Occured in Get Fare for Vehicle Category : " <> show vehicleCategory <> ", Route : " <> routeCode <> ", Start Stop : " <> startStationCode <> ", End Stop : " <> endStationCode <> ", Error : " <> show err
              return Nothing
      Nothing -> do
        logError $ "Did not get Beckn Config for Vehicle Category : " <> show vehicleCategory
        return Nothing

getInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Id FRFSSearch -> Maybe HighPrecMoney -> Maybe Distance -> Maybe Seconds -> m JT.LegInfo
getInfo searchId fallbackFare distance duration = do
  mbBooking <- QTBooking.findBySearchId searchId
  case mbBooking of
    Just booking -> do
      JT.mkLegInfoFromFrfsBooking booking distance duration
    Nothing -> do
      searchReq <- QFRFSSearch.findById searchId >>= fromMaybeM (SearchRequestNotFound searchId.getId)
      JT.mkLegInfoFromFrfsSearchRequest searchReq fallbackFare distance duration

search :: JT.SearchRequestFlow m r c => Spec.VehicleCategory -> Id DPerson.Person -> Id DMerchant.Merchant -> Int -> Context.City -> DJourneyLeg.JourneyLeg -> m JT.SearchResponse
search vehicleCategory personId merchantId quantity city journeyLeg = do
  let journeySearchData =
        JPT.JourneySearchData
          { journeyId = journeyLeg.journeyId.getId,
            journeyLegOrder = journeyLeg.sequenceNumber,
            agency = journeyLeg.agency <&> (.name),
            skipBooking = False,
            convenienceCost = 0,
            pricingId = Nothing,
            isDeleted = Just False
          }
  frfsSearchReq <- buildFRFSSearchReq (Just journeySearchData)
  let colorName = journeyLeg.routeDetails >>= (.shortName)
  let routeColorCode = journeyLeg.routeDetails >>= (.color)
  let platformNumber = journeyLeg.fromStopDetails >>= (.platformCode)
  let frequency = Just 300
  res <- FRFSTicketService.postFrfsSearchHandler (Just personId, merchantId) (Just city) vehicleCategory frfsSearchReq Nothing Nothing colorName routeColorCode frequency platformNumber
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

confirm :: JT.ConfirmFlow m r c => Id DPerson.Person -> Id DMerchant.Merchant -> Id FRFSSearch -> Maybe (Id FRFSQuote) -> Bool -> Bool -> m ()
confirm personId merchantId searchId mbQuoteId skipBooking bookingAllowed = do
  mbBooking <- QTBooking.findBySearchId searchId -- if booking already there no need to confirm again
  when (not skipBooking && bookingAllowed && isNothing mbBooking) $ do
    quoteId <- mbQuoteId & fromMaybeM (InvalidRequest "You can't confirm bus before getting the fare")
    void $ FRFSTicketService.postFrfsQuoteConfirm (Just personId, merchantId) quoteId

cancel :: JT.CancelFlow m r c => Id FRFSSearch -> Spec.CancellationType -> Bool -> m ()
cancel searchId cancellationType isSkipped = do
  mbMetroBooking <- QTBooking.findBySearchId searchId
  case mbMetroBooking of
    Just metroBooking -> do
      merchant <- CQM.findById metroBooking.merchantId >>= fromMaybeM (MerchantDoesNotExist metroBooking.merchantId.getId)
      merchantOperatingCity <- CQMOC.findById metroBooking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound metroBooking.merchantOperatingCityId.getId)
      bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchant.id) (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory metroBooking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
      CallExternalBPP.cancel merchant merchantOperatingCity bapConfig cancellationType metroBooking
      if isSkipped then QTBooking.updateIsSkipped metroBooking.id (Just True) else QTBooking.updateIsCancelled metroBooking.id (Just True)
    Nothing -> do
      if isSkipped then QFRFSSearch.updateSkipBooking searchId (Just True) else QFRFSSearch.updateIsCancelled searchId (Just True)
  if isSkipped then QJourneyLeg.updateIsSkipped (Just True) (Just searchId.getId) else QJourneyLeg.updateIsDeleted (Just True) (Just searchId.getId)

isCancellable :: JT.CancelFlow m r c => Id FRFSSearch -> m JT.IsCancellableResponse
isCancellable searchId = do
  mbMetroBooking <- QTBooking.findBySearchId searchId
  case mbMetroBooking of
    Just metroBooking -> do
      frfsConfig <- CQFRFSConfig.findByMerchantOperatingCityId metroBooking.merchantOperatingCityId >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show metroBooking.merchantOperatingCityId)
      case metroBooking.journeyLegStatus of
        Just journeyLegStatus -> do
          let isBookingCancellable = journeyLegStatus `elem` JT.cannotCancelStatus
          case isBookingCancellable of
            True -> return $ JT.IsCancellableResponse {canCancel = False}
            False -> return $ JT.IsCancellableResponse {canCancel = frfsConfig.isCancellationAllowed}
        Nothing -> do
          return $ JT.IsCancellableResponse {canCancel = frfsConfig.isCancellationAllowed}
    Nothing -> do
      return $ JT.IsCancellableResponse {canCancel = True}
