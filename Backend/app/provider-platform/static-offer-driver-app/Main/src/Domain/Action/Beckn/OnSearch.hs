{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnSearch where

import qualified Data.List as List
import qualified Domain.Action.Beckn.OnSearch.OneWay as OneWay
import qualified Domain.Action.Beckn.OnSearch.Rental as Rental
import qualified Domain.Types.FarePolicy.FareProduct as DFareProduct
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Environment
import EulerHS.Prelude hiding (id, state)
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Storage.CachedQueries.FarePolicy.FareProduct as QFareProduct
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Metrics as Metrics

data DOnSearchReq = DOnSearchReq
  { searchRequest :: DSR.SearchRequest,
    transporter :: DM.Merchant,
    fromLocation :: DLoc.SearchReqLocation,
    mbToLocation :: Maybe DLoc.SearchReqLocation,
    searchMetricsMVar :: Metrics.SearchMetricsMVar,
    transactionId :: Text
  }

data DOnSearchRes = DOnSearchRes
  { transporterInfo :: TransporterInfo,
    fareProductType :: DFareProduct.FareProductType,
    quoteInfos :: QuoteInfos
  }

data TransporterInfo = TransporterInfo
  { subscriberId :: ShortId DM.Subscriber,
    name :: Text,
    contacts :: Text,
    ridesInProgress :: Int,
    ridesCompleted :: Int,
    ridesConfirmed :: Int
  }

data QuoteInfos = OneWayQuoteInfo [OneWay.QuoteInfo] | RentalQuoteInfo [Rental.QuoteInfo]

onSearch :: DOnSearchReq -> Flow DOnSearchRes
onSearch DOnSearchReq {..} = do
  now <- getCurrentTime
  fareProducts <- QFareProduct.findEnabledByMerchantId transporter.id
  let isRentalProduct = any (\fareProduct -> fareProduct._type == DFareProduct.RENTAL) fareProducts
  let isOneWayProduct = any (\fareProduct -> fareProduct._type == DFareProduct.ONE_WAY) fareProducts
  onSearchReq <-
    case mbToLocation of
      Nothing -> do
        quotesInfos <-
          if isRentalProduct
            then RentalQuoteInfo <$> Rental.onSearchCallback searchRequest transporter.id fromLocation now
            else pure (RentalQuoteInfo [])
        buildDOnSearchRes transporter quotesInfos DFareProduct.RENTAL
      Just toLocation -> do
        quotesInfos <-
          if isOneWayProduct
            then OneWayQuoteInfo <$> OneWay.onSearchCallback searchRequest transporter.id now fromLocation toLocation transactionId
            else pure (OneWayQuoteInfo [])
        buildDOnSearchRes transporter quotesInfos DFareProduct.ONE_WAY
  Metrics.finishSearchMetrics transporter.name searchMetricsMVar
  pure onSearchReq

buildDOnSearchRes ::
  forall m r.
  EsqDBFlow m r =>
  DM.Merchant ->
  QuoteInfos ->
  DFareProduct.FareProductType ->
  m DOnSearchRes
buildDOnSearchRes org quoteInfos fareProductType = do
  count <- QRide.getCountByStatus org.id (Proxy @m)
  let transporterInfo =
        TransporterInfo
          { subscriberId = org.subscriberId,
            name = org.name,
            contacts = fromMaybe "" org.mobileNumber,
            ridesInProgress = fromMaybe 0 $ List.lookup DRide.INPROGRESS count,
            ridesCompleted = fromMaybe 0 $ List.lookup DRide.COMPLETED count,
            ridesConfirmed = fromMaybe 0 $ List.lookup DRide.NEW count
          }
  pure $ DOnSearchRes {transporterInfo, fareProductType, quoteInfos}
