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
import qualified Domain.Types.FRFSQuote as Quote
import qualified Domain.Types.FRFSSearch as Search
import Domain.Types.Merchant
import qualified Domain.Types.Station as DStation
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSQuote as QQuote
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.Station as QStation

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
    vehicleType :: DStation.FRFSVehicleType,
    stations :: [DStation],
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

validateRequest :: DOnSearch -> Flow (Merchant, Search.FRFSSearch)
validateRequest DOnSearch {..} = do
  search <- runInReplica $ QSearch.findById (Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
  let merchantId = search.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return (merchant, search)

onSearch ::
  DOnSearch ->
  Search.FRFSSearch ->
  Flow ()
onSearch onSearchReq search = do
  quotes <- traverse (mkQuotes onSearchReq search) (onSearchReq.quotes)
  QQuote.createMany quotes
  return ()

mkQuotes :: DOnSearch -> Search.FRFSSearch -> DQuote -> Flow Quote.FRFSQuote
mkQuotes dOnSearch search DQuote {..} = do
  dStartStation <- getStartStation stations & fromMaybeM (InternalError "Start station not found")
  dEndStation <- getEndStation stations & fromMaybeM (InternalError "End station not found")

  startStation <- QStation.findByStationCode dStartStation.stationCode >>= fromMaybeM (InternalError $ "Station not found: " <> dStartStation.stationCode)
  endStation <- QStation.findByStationCode dEndStation.stationCode >>= fromMaybeM (InternalError $ "Station not found: " <> dEndStation.stationCode)

  let stationsJSON = stations & map castStationToAPI & encodeToText
  uid <- generateGUID
  now <- getCurrentTime

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
        Quote.validTill,
        Quote.vehicleType,
        Quote.merchantId = search.merchantId,
        Quote.merchantOperatingCityId = search.merchantOperatingCityId,
        Quote.partnerOrgId = search.partnerOrgId,
        Quote.partnerOrgTransactionId = search.partnerOrgTransactionId,
        Quote.createdAt = now,
        Quote.updatedAt = now
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
      API.sequenceNum = stopSequence
    }
