{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.OnSearch where

import qualified API.Types.UI.FRFSTicketService as API
import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Types.FRFSQuote as Quote
import qualified Domain.Types.FRFSSearch as Search
import Domain.Types.Merchant
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSQuote as QQuote
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.PersonStats as QPStats
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.Station as QStation
import Tools.Error

data DOnSearch = DOnSearch
  { bppSubscriberId :: Text,
    bppSubscriberUrl :: Text,
    providerDescription :: Maybe Text,
    providerId :: Text,
    providerName :: Text,
    quotes :: [DQuote],
    validTill :: Maybe UTCTime,
    transactionId :: Text,
    messageId :: Text
  }

data DQuote = DQuote
  { bppItemId :: Text,
    price :: Price,
    vehicleType :: Spec.VehicleCategory,
    vehicleVariant :: Spec.VehicleVariant,
    stations :: [DStation],
    routeCode :: Maybe Text,
    _type :: Quote.FRFSQuoteType
  }

data DStation = DStation
  { stationCode :: Text,
    stationName :: Text,
    stationLat :: Maybe Double,
    stationLon :: Maybe Double,
    stationType :: API.StationType,
    stopSequence :: Maybe Int
  }

data ValidatedDOnSearch = ValidatedDOnSearch
  { merchant :: Merchant,
    search :: Search.FRFSSearch,
    ticketsBookedInEvent :: Int,
    isEventOngoing :: Bool,
    mbFreeTicketInterval :: Maybe Int,
    mbMaxFreeTicketCashback :: Maybe Int
  }

validateRequest :: DOnSearch -> Flow ValidatedDOnSearch
validateRequest DOnSearch {..} = do
  search <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  let merchantId = search.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  frfsConfig <- CQFRFSConfig.findByMerchantOperatingCityId search.merchantOperatingCityId >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show search.merchantOperatingCityId)
  if frfsConfig.isEventOngoing == Just True
    then do
      stats <- QPStats.findByPersonId search.riderId >>= fromMaybeM (InternalError "Person stats not found")
      return ValidatedDOnSearch {merchant, search, ticketsBookedInEvent = fromMaybe 0 stats.ticketsBookedInEvent, isEventOngoing = True, mbFreeTicketInterval = frfsConfig.freeTicketInterval, mbMaxFreeTicketCashback = frfsConfig.maxFreeTicketCashback}
    else return ValidatedDOnSearch {merchant, search, ticketsBookedInEvent = 0, isEventOngoing = False, mbFreeTicketInterval = Nothing, mbMaxFreeTicketCashback = Nothing}

onSearch ::
  DOnSearch ->
  ValidatedDOnSearch ->
  Flow ()
onSearch onSearchReq validatedReq = do
  quotes <- traverse (mkQuotes onSearchReq validatedReq) (onSearchReq.quotes)
  QQuote.createMany quotes
  return ()

mkQuotes :: DOnSearch -> ValidatedDOnSearch -> DQuote -> Flow Quote.FRFSQuote
mkQuotes dOnSearch ValidatedDOnSearch {..} DQuote {..} = do
  dStartStation <- getStartStation stations & fromMaybeM (InternalError "Start station not found")
  dEndStation <- getEndStation stations & fromMaybeM (InternalError "End station not found")

  startStation <- QStation.findByStationCode dStartStation.stationCode >>= fromMaybeM (InternalError $ "Station not found: " <> dStartStation.stationCode)
  endStation <- QStation.findByStationCode dEndStation.stationCode >>= fromMaybeM (InternalError $ "Station not found: " <> dEndStation.stationCode)
  routeId <-
    maybe
      (pure Nothing)
      ( \rCode -> do
          route <- QRoute.findByRouteCode rCode >>= fromMaybeM (RouteNotFound rCode)
          return $ Just route.id
      )
      routeCode
  let freeTicketInterval = fromMaybe (maxBound :: Int) mbFreeTicketInterval
  let stationsJSON = stations & map castStationToAPI & encodeToText
  let maxFreeTicketCashback = fromMaybe 0 mbMaxFreeTicketCashback
  uid <- generateGUID
  now <- getCurrentTime
  (discountedTickets, eventDiscountAmount) <-
    if isEventOngoing
      then do
        let perTicketCashback = min maxFreeTicketCashback price.amountInt.getMoney
            discountedTickets = ((ticketsBookedInEvent + search.quantity) `div` freeTicketInterval) - (ticketsBookedInEvent `div` freeTicketInterval)
            eventDiscountAmount = toHighPrecMoney $ discountedTickets * perTicketCashback
        return (Just discountedTickets, Just eventDiscountAmount)
      else return (Nothing, Nothing)
  let validTill = fromMaybe (addUTCTime (intToNominalDiffTime 900) now) dOnSearch.validTill -- If validTill is not present, set it to 15 minutes from now
  return
    Quote.FRFSQuote
      { Quote._type = _type,
        Quote.bppItemId,
        Quote.bppSubscriberId = dOnSearch.bppSubscriberId,
        Quote.bppSubscriberUrl = dOnSearch.bppSubscriberUrl,
        Quote.fromStationId = startStation.id,
        Quote.id = uid,
        Quote.price,
        Quote.providerDescription = dOnSearch.providerDescription,
        Quote.providerId = dOnSearch.providerId,
        Quote.providerName = dOnSearch.providerName,
        Quote.quantity = search.quantity,
        Quote.riderId = search.riderId,
        Quote.searchId = search.id,
        Quote.stationsJson = stationsJSON,
        Quote.toStationId = endStation.id,
        Quote.routeId = routeId,
        Quote.validTill,
        Quote.vehicleType,
        Quote.merchantId = search.merchantId,
        Quote.merchantOperatingCityId = search.merchantOperatingCityId,
        Quote.partnerOrgId = search.partnerOrgId,
        Quote.partnerOrgTransactionId = search.partnerOrgTransactionId,
        Quote.createdAt = now,
        Quote.updatedAt = now,
        ..
      }

getStartStation :: [DStation] -> Maybe DStation
getStartStation = find (\station -> station.stationType == API.START)

getEndStation :: [DStation] -> Maybe DStation
getEndStation = find (\station -> station.stationType == API.END)

castStationToAPI :: DStation -> API.FRFSStationAPI
castStationToAPI DStation {..} =
  API.FRFSStationAPI
    { API.address = Nothing,
      API.code = stationCode,
      API.color = Nothing,
      API.lat = stationLat,
      API.lon = stationLon,
      API.name = stationName,
      API.stationType = Just stationType,
      API.sequenceNum = stopSequence,
      API.distance = Nothing
    }
