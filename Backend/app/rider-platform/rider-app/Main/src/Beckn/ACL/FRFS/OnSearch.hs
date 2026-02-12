{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.FRFS.OnSearch where

import qualified BecknV2.FRFS.Enums as Enums
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Data.List (groupBy, sortBy)
import qualified Data.Text as T
import qualified Domain.Action.Beckn.FRFS.OnSearch as Domain
import qualified Domain.Types.Extra.IntegratedBPPConfig as DIBCExtra
import qualified Domain.Types.FRFSQuote as DQuote
import Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.StationType as Station
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.IntegratedBPPConfig as QIBC

buildOnSearchReq ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Spec.OnSearchReq ->
  m Domain.DOnSearch
buildOnSearchReq onSearchReq = do
  let context = onSearchReq.onSearchReqContext
  Utils.validateContext Spec.ON_SEARCH context
  transactionId <- context.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  messageId <- context.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  bppSubscriberId <- context.contextBppId & fromMaybeM (InvalidRequest "BppSubscriberId not found")
  bppSubscriberUrl <- context.contextBppUri & fromMaybeM (InvalidRequest "BppSubscriberUrl not found")
  frfsSearch <- QSearch.findById (Id transactionId) >>= fromMaybeM (InvalidRequest $ "FrfsSearch not found for transactionId" <> show transactionId)
  timeStamp <- context.contextTimestamp & fromMaybeM (InvalidRequest "Timestamp not found")

  let ttl = context.contextTtl >>= Utils.getQuoteValidTill (convertRFC3339ToUTC timeStamp)

  message <- onSearchReq.onSearchReqMessage & fromMaybeM (InvalidRequest "Message not found")
  provider <- message.onSearchReqMessageCatalog.catalogProviders >>= listToMaybe & fromMaybeM (InvalidRequest "Provider not found")

  let providerDescription = Nothing -- TODO: Fix this in types
  providerId <- provider.providerId & fromMaybeM (InvalidRequest "ProviderId not found")
  providerName <- provider.providerDescriptor >>= (.descriptorName) & fromMaybeM (InvalidRequest "ProviderName not found")

  items <- provider.providerItems & fromMaybeM (InvalidRequest "Items not found")
  fulfillments <- provider.providerFulfillments & fromMaybeM (InvalidRequest "Fulfillments not found")
  payments <- provider.providerPayments & fromMaybeM (InvalidRequest "Payments not found")
  interestTags <-
    catMaybes
      <$> mapM
        ( \p ->
            case p.paymentTags of
              Nothing -> pure Nothing
              Just paymentTags -> pure $ Utils.getTag "SETTLEMENT_TERMS" "DELAY_INTEREST" paymentTags
        )
        payments

  let bppDelayedInterest = listToMaybe interestTags

  -- Get IntegratedBPPConfig to check mergeQuoteCriteria
  integratedBPPConfig <- QIBC.findById frfsSearch.integratedBppConfigId >>= fromMaybeM (InvalidRequest "IntegratedBPPConfig not found")

  quotes <- mkQuotes items fulfillments integratedBPPConfig
  return
    Domain.DOnSearch
      { providerDescription,
        providerId,
        providerName,
        quotes,
        bppSubscriberId,
        bppSubscriberUrl,
        validTill = ttl,
        transactionId,
        messageId,
        bppDelayedInterest
      }

mkQuotes :: (MonadFlow m) => [Spec.Item] -> [Spec.Fulfillment] -> DIBC.IntegratedBPPConfig -> m [Domain.DQuote]
mkQuotes items fulfillments integratedBPPConfig = do
  allQuotes <- traverse (parseItems fulfillments) items <&> concat
  return $ mergeQuotes integratedBPPConfig allQuotes

mergeQuotes :: DIBC.IntegratedBPPConfig -> [Domain.DQuote] -> [Domain.DQuote]
mergeQuotes config quotes = case config.providerConfig of
  DIBC.ONDC ondcConfig ->
    case ondcConfig.mergeQuoteCriteria of
      Just DIBCExtra.FULFILLMENT -> mergeQuotesByFulfillment quotes
      Just DIBCExtra.QUOTE_TYPE -> mergeQuotesByQuoteType quotes
      Nothing -> quotes
  _ -> quotes

mergeQuotesByFulfillment :: [Domain.DQuote] -> [Domain.DQuote]
mergeQuotesByFulfillment quotes =
  mapMaybe
    mergeQuotesForFulfillment
    ( quotes & sortBy (\q1 q2 -> compare q1.routeCode q2.routeCode)
        & groupBy (\q1 q2 -> q1.routeCode == q2.routeCode)
    )

mergeQuotesForFulfillment :: [Domain.DQuote] -> Maybe Domain.DQuote
mergeQuotesForFulfillment quotes = do
  quote <- find (\q -> q._type == DQuote.SingleJourney) quotes
  let allCategories = concatMap (.categories) quotes
  return $ quote {Domain.categories = allCategories}

mergeQuotesByQuoteType :: [Domain.DQuote] -> [Domain.DQuote]
mergeQuotesByQuoteType quotes =
  let sjtQuotes = filter (\q -> q._type == DQuote.SingleJourney || q._type == DQuote.SpecialFareSingleJourney) quotes
      rjtQuotes = filter (\q -> q._type == DQuote.ReturnJourney) quotes
      mergedRjtQuotes = mergeReturnJourneyQuotes sjtQuotes rjtQuotes
   in sjtQuotes ++ mergedRjtQuotes

mergeReturnJourneyQuotes :: [Domain.DQuote] -> [Domain.DQuote] -> [Domain.DQuote]
mergeReturnJourneyQuotes sjtQuotes rjtQuotes =
  case (listToMaybe sjtQuotes, rjtQuotes) of
    (Just sjtQuote, rjts@(_ : _)) ->
      let sjtStartStation = listToMaybe $ filter (\s -> s.stationType == Station.START) sjtQuote.stations
       in case sjtStartStation of
            Just refStart ->
              let forwardRjt = find (\rjt -> any (\s -> s.stationType == Station.START && s.stationCode == refStart.stationCode) rjt.stations) rjts
               in case forwardRjt of
                    Just fwdQuote -> [fwdQuote]
                    Nothing -> rjts
            Nothing -> rjts
    _ -> rjtQuotes

parseItems :: (MonadFlow m) => [Spec.Fulfillment] -> Spec.Item -> m [Domain.DQuote]
parseItems fulfillments item = do
  fulfillmentIds <- item.itemFulfillmentIds & fromMaybeM (InvalidRequest "FulfillmentIds not found")
  traverse (parseFulfillments item fulfillments) fulfillmentIds

parseFulfillments :: (MonadFlow m) => Spec.Item -> [Spec.Fulfillment] -> Text -> m Domain.DQuote
parseFulfillments item fulfillments fulfillmentId = do
  itemId <- item.itemId & fromMaybeM (InvalidRequest "ItemId not found")
  itemCode <- item.itemDescriptor >>= (.descriptorCode) & fromMaybeM (InvalidRequest "ItemCode not found")
  quoteType <- castQuoteType itemCode

  fulfillment <- fulfillments & find (\fulfillment -> fulfillment.fulfillmentId == Just fulfillmentId) & fromMaybeM (InvalidRequest "Fulfillment not found")
  fulfillmentStops <- fulfillment.fulfillmentStops & fromMaybeM (InvalidRequest "FulfillmentStops not found")

  stations <-
    if isParentIdAvailable fulfillmentStops
      then fulfillmentStops & sequenceStops & mapWithIndex (\idx stop -> mkDStation stop (Just $ idx + 1))
      else traverse (\s -> mkDStation s Nothing) fulfillmentStops
  price <- item.itemPrice >>= Utils.parsePrice & fromMaybeM (InvalidRequest "Price not found")
  let offerPrice = item.itemPrice >>= Utils.parseOfferPrice
  vehicleCategory <- fulfillment.fulfillmentVehicle >>= (.vehicleCategory) & fromMaybeM (InvalidRequest "VehicleType not found")
  vehicleType <- vehicleCategory & castVehicleVariant & fromMaybeM (InvalidRequest "VehicleType not found")

  -- Check if vehicle variant is present to build routeStations
  let vehicleVariant = fulfillment.fulfillmentVehicle >>= (.vehicleVariant)
  routeStations <-
    case vehicleVariant of
      Just _ -> pure $ fromMaybe [] (mkDRouteStations fulfillment stations price fulfillmentId)
      Nothing -> return []

  let category = createCategory price offerPrice itemCode itemId
      categories = [category]
  logDebug $ "Categories from OnSearch: " <> show categories
  return $
    Domain.DQuote
      { bppItemId = itemId,
        routeCode = fulfillmentId,
        vehicleType,
        routeStations,
        stations,
        fareDetails = Nothing,
        categories = categories,
        _type = quoteType
      }

mkDStation :: (MonadFlow m) => Spec.Stop -> Maybe Int -> m Domain.DStation
mkDStation stop seqNumber = do
  stationCode <- stop.stopLocation >>= (.locationDescriptor) >>= (.descriptorCode) & fromMaybeM (InvalidRequest "Stop Location code not found")
  stationName <- stop.stopLocation >>= (.locationDescriptor) >>= (.descriptorName) & fromMaybeM (InvalidRequest "Stop Location name not found")
  let mLatLon = stop.stopLocation >>= (.locationGps) >>= Utils.parseGPS
  stopType <- stop.stopType & fromMaybeM (InvalidRequest "Stop Location type not found")
  stationType <- stopType & castStationType & fromMaybeM (InvalidRequest "Stop Location type not found")
  return
    Domain.DStation
      { stationCode,
        stationName,
        stationType,
        stopSequence = seqNumber,
        stationLat = fst <$> mLatLon,
        stationLon = snd <$> mLatLon,
        towards = Nothing
      }

sequenceStops :: [Spec.Stop] -> [Spec.Stop]
sequenceStops stops = go Nothing []
  where
    go _ [] =
      case findFirstStop of
        Just firstStop -> go (Just firstStop) [firstStop]
        Nothing -> []
    go mPrevStop acc =
      case mPrevStop of
        Nothing -> acc -- shouldn't happen
        Just prevStop ->
          case findNextStop prevStop of
            Just nextStop -> go (Just nextStop) (acc ++ [nextStop])
            Nothing -> acc

    findFirstStop :: Maybe Spec.Stop
    findFirstStop = stops & find (\stop -> stop.stopParentStopId == Nothing)

    findNextStop :: Spec.Stop -> Maybe Spec.Stop
    findNextStop prevStop = stops & find (\stop -> stop.stopParentStopId == prevStop.stopId)

-- TODO: Remove this when ONDC makes parentStopId mandatory
isParentIdAvailable :: [Spec.Stop] -> Bool
isParentIdAvailable stops =
  let lenMissingParentId = length $ filter (\stop -> stop.stopParentStopId == Nothing) stops
   in lenMissingParentId == 1

mapWithIndex :: (MonadFlow m) => (Int -> a -> m b) -> [a] -> m [b]
mapWithIndex f xs = go 0 xs
  where
    go _ [] = return []
    go idx (x : xs') = do
      y <- f idx x
      ys <- go (idx + 1) xs'
      return (y : ys)

castVehicleVariant :: Text -> Maybe Enums.VehicleCategory
castVehicleVariant = \case
  "METRO" -> Just Enums.METRO
  "BUS" -> Just Enums.BUS
  "SUBWAY" -> Just Enums.SUBWAY
  _ -> Nothing

castStationType :: Text -> Maybe StationType
castStationType = \case
  "START" -> Just START
  "END" -> Just END
  "TRANSIT_STOP" -> Just TRANSIT
  "INTERMEDIATE_STOP" -> Just INTERMEDIATE
  _ -> Nothing

castQuoteType :: MonadFlow m => Text -> m DQuote.FRFSQuoteType
castQuoteType "SJT" = return DQuote.SingleJourney
castQuoteType "SFSJT" = return DQuote.SpecialFareSingleJourney
castQuoteType "RJT" = return DQuote.ReturnJourney
castQuoteType "PASS" = return DQuote.Pass
castQuoteType _ = throwError $ InvalidRequest "Invalid quote type"

createCategory :: Price -> Maybe Price -> Text -> Text -> Domain.DCategory
createCategory price offerPrice itemCode itemId = do
  let op = fromMaybe price offerPrice
  let category = case itemCode of
        "SJT" -> ADULT
        "SFSJT" -> FEMALE
        _ -> ADULT
   in Domain.DCategory
        { category = category,
          price = price,
          offeredPrice = op,
          bppItemId = itemId,
          eligibility = True
        }

mkDRouteStations :: Spec.Fulfillment -> [Domain.DStation] -> Price -> Text -> Maybe [Domain.DRouteStation]
mkDRouteStations fulfillment stops price fulfillmentId = do
  -- Extract route information from fulfillment tags
  routeInfo <-
    fulfillment.fulfillmentTags
      >>= find
        ( \tagGroup ->
            maybe False (\desc -> desc.descriptorCode == Just "ROUTE_INFO") tagGroup.tagGroupDescriptor
        )

  routeId <-
    routeInfo.tagGroupList
      >>= find
        ( \tag ->
            maybe False (\desc -> desc.descriptorCode == Just "ROUTE_ID") tag.tagDescriptor
        )
      >>= (.tagValue)

  startStop <- find (\stop -> stop.stationType == START) stops
  endStop <- find (\stop -> stop.stationType == END) stops
  startLat <- startStop.stationLat
  startLon <- startStop.stationLon
  endLat <- endStop.stationLat
  endLon <- endStop.stationLon
  let routeStartPoint = LatLong startLat startLon
      routeEndPoint = LatLong endLat endLon
      routeLongName = startStop.stationName <> " - " <> endStop.stationName
      -- Create vehicle service tier from vehicle variant
      routeServiceTier = createVehicleServiceTier fulfillment

  return
    [ Domain.DRouteStation
        { routeCode = fulfillmentId,
          routeLongName,
          routeShortName = routeId,
          routeStartPoint,
          routeEndPoint,
          routeStations = stops,
          routeTravelTime = Nothing,
          routeSequenceNum = Nothing,
          routeServiceTier,
          routePrice = price,
          routeColor = Just routeId,
          routeFarePolicyId = Nothing
        }
    ]

createVehicleServiceTier :: Spec.Fulfillment -> Maybe Domain.DVehicleServiceTier
createVehicleServiceTier fulfillment = do
  vehicle <- fulfillment.fulfillmentVehicle
  variant <- vehicle.vehicleVariant
  category <- vehicle.vehicleCategory

  let serviceTierType = castVehicleVariantToServiceTierType variant
      serviceTierProviderCode = variant
      serviceTierShortName = variant
      serviceTierDescription = category <> " " <> variant
      serviceTierLongName = category <> " " <> variant
      isAirConditioned = Just False

  Just $
    Domain.DVehicleServiceTier
      { serviceTierType,
        serviceTierProviderCode,
        serviceTierShortName,
        serviceTierDescription,
        serviceTierLongName,
        isAirConditioned
      }

castVehicleVariantToServiceTierType :: Text -> Spec.ServiceTierType
castVehicleVariantToServiceTierType = \case
  "AC" -> Spec.AC
  "NON_AC" -> Spec.NON_AC
  "EXPRESS" -> Spec.EXPRESS
  "SPECIAL" -> Spec.SPECIAL
  "EXECUTIVE" -> Spec.EXECUTIVE
  "FIRST_CLASS" -> Spec.FIRST_CLASS
  "SECOND_CLASS" -> Spec.SECOND_CLASS
  "THIRD_CLASS" -> Spec.THIRD_CLASS
  "AC_EMU_FIRST_CLASS" -> Spec.AC_EMU_FIRST_CLASS
  _ -> Spec.ORDINARY -- Default fallback

buildDiscoveryOnSearchReq :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Spec.OnSearchReq -> Domain.DiscoveryCounter -> m Domain.DiscoveryOnSearchReq
buildDiscoveryOnSearchReq req discoveryCounter = do
  msg <- req.onSearchReqMessage & fromMaybeM (InvalidRequest "Message not found")
  transactionId <- req.onSearchReqContext.contextTransactionId & fromMaybeM (InvalidRequest "Missing transactionId")
  messageId <- req.onSearchReqContext.contextMessageId & fromMaybeM (InvalidRequest "Missing messageId")
  let getPageInfo tag = (Utils.getTag "PAGINATION" "PAGINATION_ID" tag, Utils.getTag "PAGINATION" "MAX_PAGE_NUMBER" tag)
      (currPageNoTxt, totalPagesTxt) = maybe (Nothing, Nothing) getPageInfo msg.onSearchReqMessageCatalog.catalogTags
      mbCurrPageNo :: Maybe Int = currPageNoTxt >>= (readMaybe . T.unpack)
      mbMaxPageCount :: Maybe Int = totalPagesTxt >>= (readMaybe . T.unpack)
  case (mbCurrPageNo, mbMaxPageCount) of
    (Just currPageNo, Just maxPageCount) -> do
      when (currPageNo /= discoveryCounter.pageNo + 1 || maxPageCount /= discoveryCounter.maxPageNo) $ throwError (InvalidRequest "Invalid pagination. Page progression mismatch")
    _ -> pure ()
  bppSubscriberId <- req.onSearchReqContext.contextBppId & fromMaybeM (InvalidRequest "BppSubscriberId not found")
  bppSubscriberUrl <- req.onSearchReqContext.contextBppUri & fromMaybeM (InvalidRequest "BppSubscriberUrl not found")

  let providers = fromMaybe [] msg.onSearchReqMessageCatalog.catalogProviders
      fulfillments = providers >>= (fromMaybe [] . (.providerFulfillments))
      stopsWithIdx =
        fulfillments >>= \f ->
          let fs = fromMaybe [] f.fulfillmentStops
              len = length fs
           in zipWith (\stop idx -> (stop, idx, len)) fs [0 ..]
      stations = mapMaybe castStation stopsWithIdx
  return
    Domain.DiscoveryOnSearchReq
      { transactionId = transactionId,
        messageId = messageId,
        pageNumber = discoveryCounter.pageNo,
        totalPages = discoveryCounter.maxPageNo,
        bppSubscriberId = bppSubscriberId,
        bppSubscriberUrl = bppSubscriberUrl,
        stationList = stations,
        merchantId = discoveryCounter.merchantId
      }
  where
    castStation :: (Spec.Stop, Int, Int) -> Maybe Domain.DStation
    castStation (stop, idx, len) = do
      stopCode <- stop.stopLocation >>= (.locationDescriptor) >>= (.descriptorCode)
      stationName <- stop.stopLocation >>= (.locationDescriptor) >>= (.descriptorName)
      let stationGps = stop.stopLocation >>= (.locationGps) & parseLatLong
          stationLat = stationGps <&> (.lat)
          stationLon = stationGps <&> (.lon)
          stationType = getStationType idx len
      pure $
        Domain.DStation
          { stationCode = stopCode,
            stationName = stationName,
            stationLat = stationLat,
            stationLon = stationLon,
            stationType = stationType,
            stopSequence = Just idx,
            towards = Nothing
          }

    parseLatLong :: Maybe Text -> Maybe Maps.LatLong
    parseLatLong Nothing = Nothing
    parseLatLong (Just a) =
      case T.splitOn "," a of
        [latStr, lonStr] ->
          case (readMaybe (T.unpack latStr), readMaybe (T.unpack lonStr)) of
            (Just lat, Just lon) -> Just (Maps.LatLong lat lon)
            _ -> Nothing
        _ -> Nothing

    getStationType idx len =
      let lastIdx = len - 1
       in case idx of
            0 -> Station.START
            _ | idx == lastIdx -> Station.END
            _ -> Station.INTERMEDIATE

-- validateAndUpdateCounter :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Maybe Int -> Maybe Int -> m (Maybe Domain.DiscoveryCounter)
-- validateAndUpdateCounter transactionId mbCurrPageNo mbMaxPageCount = do
--   case (mbCurrPageNo, mbMaxPageCount) of
--     (Nothing, _)    -> pure Nothing
--     (_, Nothing)    -> pure Nothing
--     (Just currPageNo, Just maxPageCount) -> do
--       redisValue <- Redis.safeGet (Utils.discoverySearchCounterKey (show transactionId))
--       case redisValue of
--         Nothing -> pure Nothing
--         Just Domain.DiscoveryCounter {merchantId, pageNo, maxPageNo} -> do
--           when (currPageNo /= pageNo + 1 || maxPageCount /= maxPageNo) $ throwError (InvalidRequest "Invalid pagination. Page progression mismatch")
--           pure $
--             Just Domain.DiscoveryCounter
--               { merchantId = merchantId
--               , pageNo = currPageNo
--               , maxPageNo = maxPageNo
--               }
