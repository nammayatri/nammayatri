module Lib.JourneyLeg.Common.FRFS where

import qualified API.Types.UI.MultimodalConfirm as APITypes
import Domain.Types.FRFSSearch
import Domain.Types.Station
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (isNothing)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JPT
import Lib.JourneyModule.Location
import qualified Lib.JourneyModule.Types as JT
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
