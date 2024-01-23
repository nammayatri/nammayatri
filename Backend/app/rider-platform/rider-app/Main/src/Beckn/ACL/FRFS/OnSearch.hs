{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.FRFS.OnSearch where

import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Domain.Action.Beckn.FRFS.OnSearch as Domain
import qualified Domain.Types.FRFSQuote as DQuote
import Domain.Types.FRFSTrip as DTrip
import qualified Domain.Types.Station as Domain.DStation
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildOnSearchReq ::
  (MonadFlow m) =>
  Spec.OnSearchReq ->
  m Domain.DOnSearch
buildOnSearchReq onSearchReq = do
  -- validate context
  let context = onSearchReq.onSearchReqContext
  transactionId <- context.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  messageId <- context.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  bppSubscriberId <- context.contextBppId & fromMaybeM (InvalidRequest "BppSubscriberId not found")
  bppSubscriberUrl <- context.contextBppUri & fromMaybeM (InvalidRequest "BppSubscriberUrl not found")

  timeStamp <- context.contextTimestamp & fromMaybeM (InvalidRequest "Timestamp not found")

  let ttl = context.contextTtl >>= Utils.getQuoteValidTill timeStamp

  message <- onSearchReq.onSearchReqMessage & fromMaybeM (InvalidRequest "Message not found")
  provider <- message.onSearchReqMessageCatalog.catalogProviders >>= listToMaybe & fromMaybeM (InvalidRequest "Provider not found")

  let providerDescription = Nothing -- TODO: Fix this in types
  providerId <- provider.providerId & fromMaybeM (InvalidRequest "ProviderId not found")
  providerName <- provider.providerDescriptor >>= (.descriptorName) & fromMaybeM (InvalidRequest "ProviderName not found")

  items <- provider.providerItems & fromMaybeM (InvalidRequest "Items not found")
  fulfillments <- provider.providerFulfillments & fromMaybeM (InvalidRequest "Fulfillments not found")

  quotes <- mkQuotes items fulfillments

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
        messageId
      }

mkQuotes :: (MonadFlow m) => [Spec.Item] -> [Spec.Fulfillment] -> m [Domain.DQuote]
mkQuotes items fulfillments =
  traverse (parseItems fulfillments) items <&> concat

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
  price <- item.itemPrice >>= Utils.parseMoney & fromMaybeM (InvalidRequest "Price not found")
  vehicleCategory <- fulfillment.fulfillmentVehicle >>= (.vehicleCategory) & fromMaybeM (InvalidRequest "VehicleType not found")
  vehicleType <- vehicleCategory & castVehicleVariant & fromMaybeM (InvalidRequest "VehicleType not found")

  return $
    Domain.DQuote
      { bppItemId = itemId,
        price,
        vehicleType,
        stations,
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
        stationLon = snd <$> mLatLon
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

castVehicleVariant :: Text -> Maybe Domain.DStation.FRFSVehicleType
castVehicleVariant = \case
  "METRO" -> Just Domain.DStation.METRO
  "BUS" -> Just Domain.DStation.BUS
  _ -> Nothing

castStationType :: Text -> Maybe DTrip.StationType
castStationType = \case
  "START" -> Just DTrip.START
  "END" -> Just DTrip.END
  "TRANSIT_STOP" -> Just DTrip.TRANSIT
  "INTERMEDIATE_STOP" -> Just DTrip.INTERMEDIATE
  _ -> Nothing

castQuoteType :: MonadFlow m => Text -> m DQuote.FRFSQuoteType
castQuoteType "SJT" = return DQuote.SingleJourney
castQuoteType "SFSJT" = return DQuote.SpecialFareSingleJourney
castQuoteType "RJT" = return DQuote.ReturnJourney
castQuoteType "PASS" = return DQuote.Pass
castQuoteType _ = throwError $ InvalidRequest "Invalid quote type"
