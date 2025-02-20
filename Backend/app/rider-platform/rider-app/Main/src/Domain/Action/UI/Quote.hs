{-# OPTIONS_GHC -Wno-orphans #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Quote
  ( GetQuotesRes (..),
    OfferRes (..),
    getQuotes,
    estimateBuildLockKey,
    processActiveBooking,
    mkQAPIEntityList,
    mkQuoteBreakupAPIEntity,
    QuoteAPIEntity (..),
    QuoteBreakupAPIEntity (..),
  )
where

import qualified Beckn.ACL.Cancel as CancelACL
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HM
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.DriverOffer as UDriverOffer
import qualified Domain.Action.UI.Estimate as UEstimate
import qualified Domain.Action.UI.InterCityDetails as DInterCityDetails
import qualified Domain.Action.UI.Location as DL
import qualified Domain.Action.UI.MerchantPaymentMethod as DMPM
import qualified Domain.Action.UI.RentalDetails as DRentalDetails
import qualified Domain.Action.UI.SpecialZoneQuote as USpecialZoneQuote
import Domain.Types.Booking
import Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BppDetails as DBppDetails
import Domain.Types.CancellationReason
import qualified Domain.Types.DriverOffer as DDriverOffer
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import qualified Domain.Types.Location as DL
import Domain.Types.Quote as DQuote
import qualified Domain.Types.Quote as SQuote
import Domain.Types.QuoteBreakup
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as SSR
import Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.SpecialZoneQuote as DSpecialZoneQuote
import qualified Domain.Types.Trip as DTrip
import EulerHS.Prelude hiding (id, map, sum)
import Kernel.Beam.Functions
import Kernel.External.Maps.Types
import Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude hiding (whenJust)
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Streaming.Kafka.Topic.PublicTransportQuoteList
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JSON (objectWithSingleFieldParsing)
import qualified Kernel.Utils.Schema as S
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.MetroOffer (MetroOffer)
import qualified SharedLogic.MetroOffer as Metro
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import qualified Tools.JSON as J
import qualified Tools.Schema as S
import TransactionLogs.Types

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    vehicleVariant :: DVST.ServiceTierType,
    serviceTierName :: Maybe Text,
    serviceTierShortDesc :: Maybe Text,
    estimatedFare :: Money,
    estimatedTotalFare :: Money,
    estimatedPickupDuration :: Maybe Seconds,
    discount :: Maybe Money,
    estimatedFareWithCurrency :: PriceAPIEntity,
    estimatedTotalFareWithCurrency :: PriceAPIEntity,
    discountWithCurrency :: Maybe PriceAPIEntity,
    agencyName :: Text,
    agencyNumber :: Maybe Text,
    tripTerms :: [Text],
    quoteDetails :: QuoteAPIDetails,
    specialLocationTag :: Maybe Text,
    quoteFareBreakup :: [QuoteBreakupAPIEntity],
    agencyCompletedRidesCount :: Maybe Int,
    vehicleServiceTierAirConditioned :: Maybe Double,
    isAirConditioned :: Maybe Bool,
    vehicleServiceTierSeatingCapacity :: Maybe Int,
    createdAt :: UTCTime,
    isValueAddNP :: Bool,
    validTill :: UTCTime,
    vehicleIconUrl :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data QuoteBreakupAPIEntity = QuoteBreakupAPIEntity
  { title :: Text,
    priceWithCurrency :: PriceAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeQuoteAPIEntity :: Quote -> DBppDetails.BppDetails -> Bool -> QuoteAPIEntity
makeQuoteAPIEntity (Quote {..}) bppDetails isValueAddNP =
  let agencyCompletedRidesCount = Just 0
      providerNum = fromMaybe "+91" bppDetails.supportNumber
   in QuoteAPIEntity
        { agencyName = bppDetails.name,
          agencyNumber = Just providerNum,
          tripTerms = maybe [] (.descriptions) tripTerms,
          quoteDetails = mkQuoteAPIDetails (tollChargesInfo <&> (mkPriceAPIEntity . (.tollCharges))) quoteDetails,
          estimatedFare = estimatedFare.amountInt,
          estimatedTotalFare = estimatedTotalFare.amountInt,
          discount = discount <&> (.amountInt),
          estimatedFareWithCurrency = mkPriceAPIEntity estimatedFare,
          estimatedTotalFareWithCurrency = mkPriceAPIEntity estimatedTotalFare,
          discountWithCurrency = mkPriceAPIEntity <$> discount,
          vehicleVariant = vehicleServiceTierType,
          quoteFareBreakup = mkQuoteBreakupAPIEntity <$> quoteBreakupList,
          vehicleIconUrl = showBaseUrl <$> vehicleIconUrl,
          ..
        }

mkQuoteBreakupAPIEntity :: QuoteBreakup -> QuoteBreakupAPIEntity
mkQuoteBreakupAPIEntity QuoteBreakup {..} = do
  QuoteBreakupAPIEntity
    { title = title,
      priceWithCurrency = mkPriceAPIEntity price
    }

instance ToJSON QuoteAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON QuoteAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema QuoteAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

mkQuoteAPIDetails :: Maybe PriceAPIEntity -> QuoteDetails -> QuoteAPIDetails
mkQuoteAPIDetails tollCharges = \case
  DQuote.RentalDetails details -> DQuote.RentalAPIDetails $ DRentalDetails.mkRentalDetailsAPIEntity details tollCharges
  DQuote.OneWayDetails OneWayQuoteDetails {..} ->
    DQuote.OneWayAPIDetails
      OneWayQuoteAPIDetails
        { distanceToNearestDriver = distanceToHighPrecMeters distanceToNearestDriver,
          distanceToNearestDriverWithUnit = distanceToNearestDriver,
          ..
        }
  DQuote.AmbulanceDetails DDriverOffer.DriverOffer {..} ->
    let distanceToPickup' = distanceToHighPrecMeters <$> distanceToPickup
     in DQuote.DriverOfferAPIDetails UDriverOffer.DriverOfferAPIEntity {distanceToPickup = distanceToPickup', distanceToPickupWithUnit = distanceToPickup, durationToPickup = durationToPickup, rating = rating, isUpgradedToCab = fromMaybe False isUpgradedToCab, ..}
  DQuote.DeliveryDetails DDriverOffer.DriverOffer {..} ->
    -- TODO::is delivery entity required
    let distanceToPickup' = distanceToHighPrecMeters <$> distanceToPickup
     in DQuote.DriverOfferAPIDetails UDriverOffer.DriverOfferAPIEntity {distanceToPickup = distanceToPickup', distanceToPickupWithUnit = distanceToPickup, durationToPickup = durationToPickup, rating = rating, isUpgradedToCab = fromMaybe False isUpgradedToCab, ..}
  DQuote.DriverOfferDetails DDriverOffer.DriverOffer {..} ->
    let distanceToPickup' = (distanceToHighPrecMeters <$> distanceToPickup) <|> (Just . HighPrecMeters $ toCentesimal 0) -- TODO::remove this default value
        distanceToPickupWithUnit' = distanceToPickup <|> Just (Distance 0 Meter) -- TODO::remove this default value
        durationToPickup' = durationToPickup <|> Just 0 -- TODO::remove this default value
        rating' = rating <|> Just (toCentesimal 500) -- TODO::remove this default value
     in DQuote.DriverOfferAPIDetails UDriverOffer.DriverOfferAPIEntity {distanceToPickup = distanceToPickup', distanceToPickupWithUnit = distanceToPickupWithUnit', durationToPickup = durationToPickup', rating = rating', isUpgradedToCab = fromMaybe False isUpgradedToCab, ..}
  DQuote.OneWaySpecialZoneDetails DSpecialZoneQuote.SpecialZoneQuote {..} -> DQuote.OneWaySpecialZoneAPIDetails USpecialZoneQuote.SpecialZoneQuoteAPIEntity {..}
  DQuote.InterCityDetails details -> DQuote.InterCityAPIDetails $ DInterCityDetails.mkInterCityDetailsAPIEntity details tollCharges

mkQAPIEntityList :: [Quote] -> [DBppDetails.BppDetails] -> [Bool] -> [QuoteAPIEntity]
mkQAPIEntityList (q : qRemaining) (bpp : bppRemaining) (isValueAddNP : remVNP) =
  makeQuoteAPIEntity q bpp isValueAddNP : mkQAPIEntityList qRemaining bppRemaining remVNP
mkQAPIEntityList [] [] [] = []
mkQAPIEntityList _ _ _ = [] -- This should never happen as all the list are of same length

data GetQuotesRes = GetQuotesRes
  { fromLocation :: DL.LocationAPIEntity,
    toLocation :: Maybe DL.LocationAPIEntity,
    stops :: [DL.LocationAPIEntity],
    quotes :: [OfferRes],
    estimates :: [UEstimate.EstimateAPIEntity],
    paymentMethods :: [DMPM.PaymentMethodAPIEntity],
    journey :: Maybe [JourneyData]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data JourneyData = JourneyData
  { totalMinFare :: HighPrecMoney,
    totalMaxFare :: HighPrecMoney,
    duration :: Maybe Seconds,
    distance :: Distance,
    modes :: [DTrip.MultimodalTravelMode],
    startTime :: Maybe UTCTime,
    endTime :: Maybe UTCTime,
    journeyId :: Id DJ.Journey,
    journeyLegs :: [JourneyLeg]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data JourneyLeg = JourneyLeg
  { journeyLegOrder :: Int,
    journeyMode :: DTrip.MultimodalTravelMode,
    journeyLegId :: Id DJL.JourneyLeg,
    fromLatLong :: LatLong,
    toLatLong :: LatLong,
    fromStationCode :: Maybe Text,
    toStationCode :: Maybe Text,
    routeDetails :: [RouteDetail],
    color :: Maybe Text, -- TODO :: Deprecated, Moved to RouteDetail
    colorCode :: Maybe Text, -- TODO :: Deprecated, Moved to RouteDetail
    duration :: Maybe Seconds,
    distance :: Maybe Distance
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RouteDetail = RouteDetail
  { routeCode :: Maybe Text,
    fromStationCode :: Maybe Text,
    toStationCode :: Maybe Text,
    color :: Maybe Text,
    colorCode :: Maybe Text,
    fromStationLatLong :: LatLong,
    toStationLatLong :: LatLong
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- TODO: Needs to be fixed as quotes could be of both rentals and one way
data OfferRes
  = OnDemandCab QuoteAPIEntity
  | OnRentalCab QuoteAPIEntity
  | Metro MetroOffer
  | PublicTransport PublicTransportQuote
  deriving (Show, Generic)

instance ToJSON OfferRes where
  toJSON = genericToJSON $ objectWithSingleFieldParsing safeToLower

instance FromJSON OfferRes where
  parseJSON = genericParseJSON $ objectWithSingleFieldParsing safeToLower

instance ToSchema OfferRes where
  declareNamedSchema = genericDeclareNamedSchema $ S.objectWithSingleFieldParsing safeToLower

safeToLower :: String -> String
safeToLower (f : rest) = toLower f : rest
safeToLower [] = []

estimateBuildLockKey :: Text -> Text
estimateBuildLockKey searchReqid = "Customer:Estimate:Build:" <> searchReqid

getQuotes :: (CacheFlow m r, HasField "shortDurationRetryCfg" r RetryCfg, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], HasFlowEnv m r '["nwAddress" ::: BaseUrl], EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]) => Id SSR.SearchRequest -> Maybe Bool -> m GetQuotesRes
getQuotes searchRequestId mbAllowMultiple = do
  searchRequest <- runInReplica $ QSR.findById searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist searchRequestId.getId)
  unless (mbAllowMultiple == Just True) $ do
    activeBooking <- runInReplica $ QBooking.findLatestSelfAndPartyBookingByRiderId searchRequest.riderId
    whenJust activeBooking $ \booking -> processActiveBooking booking OnSearch
  logDebug $ "search Request is : " <> show searchRequest
  journeyData <- getJourneys searchRequest
  let lockKey = estimateBuildLockKey searchRequestId.getId
  Redis.withLockRedisAndReturnValue lockKey 5 $ do
    offers <- getOffers searchRequest
    estimates <- getEstimates searchRequestId (isJust searchRequest.driverIdentifier) -- TODO(MultiModal): only check for estimates which are done
    return $
      GetQuotesRes
        { fromLocation = DL.makeLocationAPIEntity searchRequest.fromLocation,
          toLocation = DL.makeLocationAPIEntity <$> searchRequest.toLocation,
          stops = DL.makeLocationAPIEntity <$> searchRequest.stops,
          quotes = offers,
          estimates,
          paymentMethods = [],
          journey = journeyData
        }

processActiveBooking :: (CacheFlow m r, HasField "shortDurationRetryCfg" r RetryCfg, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], HasFlowEnv m r '["nwAddress" ::: BaseUrl], EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]) => Booking -> CancellationStage -> m ()
processActiveBooking booking cancellationStage = do
  mbRide <- QRide.findActiveByRBId booking.id
  case mbRide of
    Just ride -> do
      unless (ride.status == DRide.UPCOMING) $ throwError (InvalidRequest "ACTIVE_BOOKING_ALREADY_PRESENT")
    Nothing -> do
      now <- getCurrentTime
      if addUTCTime 900 booking.startTime < now || not (isRentalOrInterCity booking.bookingDetails) || (addUTCTime 120 booking.startTime < now && isHighPriorityBooking booking.bookingDetails)
        then do
          let cancelReq =
                DCancel.CancelReq
                  { reasonCode = CancellationReasonCode "Active booking",
                    reasonStage = cancellationStage,
                    additionalInfo = Nothing,
                    reallocate = Nothing,
                    blockOnCancellationRate = Nothing
                  }
          fork "active booking processing" $ do
            dCancelRes <- DCancel.cancel booking Nothing cancelReq SBCR.ByApplication
            void . withShortRetry $ CallBPP.cancelV2 booking.merchantId dCancelRes.bppUrl =<< CancelACL.buildCancelReqV2 dCancelRes Nothing
        else throwError (InvalidRequest "ACTIVE_BOOKING_ALREADY_PRESENT")

isRentalOrInterCity :: DBooking.BookingDetails -> Bool
isRentalOrInterCity bookingDetails = case bookingDetails of
  DBooking.RentalDetails _ -> True
  DBooking.InterCityDetails _ -> True
  _ -> False

isHighPriorityBooking :: DBooking.BookingDetails -> Bool
isHighPriorityBooking bookingDetails = case bookingDetails of
  DBooking.AmbulanceDetails _ -> True
  _ -> False

getOffers :: (HedisFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => SSR.SearchRequest -> m [OfferRes]
getOffers searchRequest = do
  logDebug $ "search Request is : " <> show searchRequest
  case searchRequest.toLocation of
    Just _ -> do
      quoteList <- sortByNearestDriverDistance <$> runInReplica (QQuote.findAllBySRId searchRequest.id)
      logDebug $ "quotes are :-" <> show quoteList
      bppDetailList <- forM ((.providerId) <$> quoteList) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
      isValueAddNPList <- forM bppDetailList $ \bpp -> CQVAN.isValueAddNP bpp.subscriberId
      let quotes = case searchRequest.riderPreferredOption of
            SSR.Rental -> OnRentalCab <$> mkQAPIEntityList quoteList bppDetailList isValueAddNPList
            _ -> OnDemandCab <$> mkQAPIEntityList quoteList bppDetailList isValueAddNPList
      return . sortBy (compare `on` creationTime) $ quotes
    Nothing -> do
      quoteList <- sortByEstimatedFare <$> runInReplica (QQuote.findAllBySRId searchRequest.id)
      logDebug $ "quotes are :-" <> show quoteList
      bppDetailList <- forM ((.providerId) <$> quoteList) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
      isValueAddNPList <- forM bppDetailList $ \bpp -> CQVAN.isValueAddNP bpp.subscriberId
      let quotes = OnRentalCab <$> mkQAPIEntityList quoteList bppDetailList isValueAddNPList
      return . sortBy (compare `on` creationTime) $ quotes
  where
    sortByNearestDriverDistance quoteList = do
      let sortFunc = compare `on` getMbDistanceToNearestDriver
      sortBy sortFunc quoteList
    getMbDistanceToNearestDriver quote =
      case quote.quoteDetails of
        SQuote.OneWayDetails details -> Just details.distanceToNearestDriver
        SQuote.AmbulanceDetails details -> details.distanceToPickup
        SQuote.DeliveryDetails details -> details.distanceToPickup
        SQuote.RentalDetails _ -> Nothing
        SQuote.DriverOfferDetails details -> details.distanceToPickup
        SQuote.OneWaySpecialZoneDetails _ -> Just $ Distance 0 Meter
        SQuote.InterCityDetails _ -> Just $ Distance 0 Meter
    creationTime :: OfferRes -> UTCTime
    creationTime (OnDemandCab QuoteAPIEntity {createdAt}) = createdAt
    creationTime (Metro Metro.MetroOffer {createdAt}) = createdAt
    creationTime (OnRentalCab QuoteAPIEntity {createdAt}) = createdAt
    creationTime (PublicTransport PublicTransportQuote {createdAt}) = createdAt

getEstimates :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SSR.SearchRequest -> Bool -> m [UEstimate.EstimateAPIEntity]
getEstimates searchRequestId isReferredRide = do
  estimateList <- runInReplica $ QEstimate.findAllBySRId searchRequestId
  estimates <- mapM (UEstimate.mkEstimateAPIEntity isReferredRide) (sortByEstimatedFare estimateList)
  return . sortBy (compare `on` (.createdAt)) $ estimates

sortByEstimatedFare :: (HasField "estimatedFare" r Price) => [r] -> [r]
sortByEstimatedFare resultList = do
  let sortFunc = compare `on` (.estimatedFare.amount)
  sortBy sortFunc resultList

getJourneys :: (HedisFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => SSR.SearchRequest -> m (Maybe [JourneyData])
getJourneys searchRequest = do
  case searchRequest.hasMultimodalSearch of
    Just True -> do
      allJourneys :: [DJ.Journey] <- QJourney.findBySearchId searchRequest.id
      journeyData <-
        forM allJourneys \journey -> do
          journeyLegsFromOtp <- QJourneyLeg.findAllByJourneyId journey.id
          journeyLegs <- do
            forM journeyLegsFromOtp \journeyLeg -> do
              return $
                JourneyLeg
                  { journeyLegOrder = journeyLeg.sequenceNumber,
                    journeyMode = journeyLeg.mode,
                    journeyLegId = journeyLeg.id,
                    fromLatLong = LatLong {lat = journeyLeg.startLocation.latitude, lon = journeyLeg.startLocation.longitude},
                    toLatLong = LatLong {lat = journeyLeg.endLocation.latitude, lon = journeyLeg.endLocation.longitude},
                    fromStationCode = journeyLeg.fromStopDetails >>= (.stopCode),
                    toStationCode = journeyLeg.toStopDetails >>= (.stopCode),
                    color = listToMaybe $ catMaybes $ map (.shortName) journeyLeg.routeDetails,
                    colorCode = listToMaybe $ catMaybes $ map (.color) journeyLeg.routeDetails,
                    routeDetails = map mkRouteDetail journeyLeg.routeDetails,
                    duration = journeyLeg.duration,
                    distance = journeyLeg.distance
                  }
          let estimatedMinFare = sum $ mapMaybe (.estimatedMinFare) journeyLegsFromOtp
          let estimatedMaxFare = sum $ mapMaybe (.estimatedMaxFare) journeyLegsFromOtp
          return $
            JourneyData
              { totalMinFare = estimatedMinFare,
                totalMaxFare = estimatedMaxFare,
                modes = journey.modes,
                journeyLegs = sortBy (comparing (.journeyLegOrder)) journeyLegs,
                startTime = journey.startTime,
                endTime = journey.endTime,
                journeyId = journey.id,
                duration = journey.estimatedDuration,
                distance = journey.estimatedDistance
              }
      return $ Just journeyData
    _ -> return Nothing
  where
    mkRouteDetail :: MultiModalRouteDetails -> RouteDetail
    mkRouteDetail routeDetail =
      RouteDetail
        { routeCode = gtfsIdtoDomainCode <$> routeDetail.gtfsId,
          fromStationCode = gtfsIdtoDomainCode <$> (routeDetail.fromStopDetails >>= (.gtfsId)),
          toStationCode = gtfsIdtoDomainCode <$> (routeDetail.toStopDetails >>= (.gtfsId)),
          color = routeDetail.shortName,
          colorCode = routeDetail.shortName,
          fromStationLatLong =
            LatLong
              { lat = routeDetail.startLocation.latLng.latitude,
                lon = routeDetail.startLocation.latLng.longitude
              },
          toStationLatLong =
            LatLong
              { lat = routeDetail.endLocation.latLng.latitude,
                lon = routeDetail.endLocation.latLng.longitude
              }
        }
