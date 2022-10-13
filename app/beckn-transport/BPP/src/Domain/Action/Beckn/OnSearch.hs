module Domain.Action.Beckn.OnSearch where

import Beckn.Types.Common
import Beckn.Types.Id
import qualified Data.List as List
import qualified Domain.Action.Beckn.OnSearch.OneWay as OneWay
import qualified Domain.Action.Beckn.OnSearch.Rental as Rental
import qualified Domain.Types.FarePolicy.FareProduct as DFareProduct
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Environment
import EulerHS.Prelude hiding (id, state)
import qualified Storage.CachedQueries.FarePolicy.FareProduct as QFareProduct
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Metrics as Metrics

data DOnSearchReq = DOnSearchReq
  { searchRequest :: DSR.SearchRequest,
    transporter :: DOrg.Organization,
    fromLocation :: DLoc.SearchReqLocation,
    mbToLocation :: Maybe DLoc.SearchReqLocation,
    searchMetricsMVar :: Metrics.SearchMetricsMVar
  }

data DOnSearchRes = DOnSearchRes
  { transporterInfo :: TransporterInfo,
    fareProductType :: DFareProduct.FareProductType,
    quoteInfos :: QuoteInfos
  }

data TransporterInfo = TransporterInfo
  { shortId :: ShortId DOrg.Organization,
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
  fareProducts <- QFareProduct.findEnabledByOrgId transporter.id
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
            then OneWayQuoteInfo <$> OneWay.onSearchCallback searchRequest transporter.id now fromLocation toLocation
            else pure (OneWayQuoteInfo [])
        buildDOnSearchRes transporter quotesInfos DFareProduct.ONE_WAY
  Metrics.finishSearchMetrics transporter.id searchMetricsMVar
  pure onSearchReq

buildDOnSearchRes ::
  EsqDBFlow m r =>
  DOrg.Organization ->
  QuoteInfos ->
  DFareProduct.FareProductType ->
  m DOnSearchRes
buildDOnSearchRes org quoteInfos fareProductType = do
  count <- QRide.getCountByStatus org.id
  let transporterInfo =
        TransporterInfo
          { shortId = org.shortId,
            name = org.name,
            contacts = fromMaybe "" org.mobileNumber,
            ridesInProgress = fromMaybe 0 $ List.lookup DRide.INPROGRESS count,
            ridesCompleted = fromMaybe 0 $ List.lookup DRide.COMPLETED count,
            ridesConfirmed = fromMaybe 0 $ List.lookup DRide.NEW count
          }
  pure $ DOnSearchRes {transporterInfo, fareProductType, quoteInfos}
