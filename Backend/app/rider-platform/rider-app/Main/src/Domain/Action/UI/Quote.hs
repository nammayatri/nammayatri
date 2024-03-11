{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Domain.Action.UI.Quote
  ( GetQuotesRes (..),
    OfferRes (..),
    getQuotes,
    estimateBuildLockKey,
    processActiveBooking,
  )
where

import qualified Beckn.ACL.Cancel as CancelACL
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HM
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import qualified Domain.Action.UI.Cancel as DCancel
import Domain.Types.Booking
import Domain.Types.CancellationReason
import Domain.Types.Estimate (EstimateAPIEntity)
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import Domain.Types.Quote (QuoteAPIEntity)
import qualified Domain.Types.Quote as SQuote
import qualified Domain.Types.SearchRequest as SSR
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Topic.PublicTransportQuoteList
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JSON (objectWithSingleFieldParsing)
import qualified Kernel.Utils.Schema as S
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.MetroOffer (MetroOffer)
import qualified SharedLogic.MetroOffer as Metro
import qualified SharedLogic.PublicTransport as PublicTransport
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error

data GetQuotesRes = GetQuotesRes
  { fromLocation :: DL.LocationAPIEntity,
    toLocation :: Maybe DL.LocationAPIEntity,
    quotes :: [OfferRes],
    estimates :: [EstimateAPIEntity],
    paymentMethods :: [DMPM.PaymentMethodAPIEntity]
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
  toJSON = genericToJSON $ objectWithSingleFieldParsing \(f : rest) -> toLower f : rest

instance FromJSON OfferRes where
  parseJSON = genericParseJSON $ objectWithSingleFieldParsing \(f : rest) -> toLower f : rest

instance ToSchema OfferRes where
  declareNamedSchema = genericDeclareNamedSchema $ S.objectWithSingleFieldParsing \(f : rest) -> toLower f : rest

estimateBuildLockKey :: Text -> Text
estimateBuildLockKey searchReqid = "Customer:Estimate:Build:" <> searchReqid

getQuotes :: (CacheFlow m r, HasField "shortDurationRetryCfg" r RetryCfg, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], HasFlowEnv m r '["nwAddress" ::: BaseUrl], EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r) => Id SSR.SearchRequest -> m GetQuotesRes
getQuotes searchRequestId = do
  searchRequest <- runInReplica $ QSR.findById searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist searchRequestId.getId)
  activeBooking <- runInReplica $ QBooking.findLatestByRiderId searchRequest.riderId
  whenJust activeBooking $ \booking -> processActiveBooking booking OnSearch
  logDebug $ "search Request is : " <> show searchRequest
  let lockKey = estimateBuildLockKey (show searchRequestId)
  Redis.withLockRedisAndReturnValue lockKey 5 $ do
    offers <- getOffers searchRequest
    estimates <- getEstimates searchRequestId
    paymentMethods <- getPaymentMethods searchRequest
    return $
      GetQuotesRes
        { fromLocation = DL.makeLocationAPIEntity searchRequest.fromLocation,
          toLocation = DL.makeLocationAPIEntity <$> searchRequest.toLocation,
          quotes = offers,
          estimates,
          paymentMethods
        }

processActiveBooking :: (CacheFlow m r, HasField "shortDurationRetryCfg" r RetryCfg, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], HasFlowEnv m r '["nwAddress" ::: BaseUrl], EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r) => Booking -> CancellationStage -> m ()
processActiveBooking booking cancellationStage = do
  mbRide <- QRide.findActiveByRBId booking.id
  case mbRide of
    Just _ -> throwError (InvalidRequest "ACTIVE_BOOKING_ALREADY_PRESENT")
    Nothing -> do
      now <- getCurrentTime
      if addUTCTime 900 booking.startTime >= now
        then throwError (InvalidRequest "ACTIVE_BOOKING_ALREADY_PRESENT") -- 15 mins buffer
        else do
          let cancelReq =
                DCancel.CancelReq
                  { reasonCode = CancellationReasonCode "Active booking",
                    reasonStage = cancellationStage,
                    additionalInfo = Nothing
                  }
          fork "active booking processing" $ do
            dCancelRes <- DCancel.cancel booking.id (booking.riderId, booking.merchantId) cancelReq
            void . withShortRetry $ CallBPP.cancelV2 dCancelRes.bppUrl =<< CancelACL.buildCancelReqV2 dCancelRes

getOffers :: (HedisFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => SSR.SearchRequest -> m [OfferRes]
getOffers searchRequest = do
  logDebug $ "search Request is : " <> show searchRequest
  case searchRequest.toLocation of
    Just _ -> do
      quoteList <- sortByNearestDriverDistance <$> runInReplica (QQuote.findAllBySRId searchRequest.id)
      logDebug $ "quotes are :-" <> show quoteList
      bppDetailList <- forM ((.providerId) <$> quoteList) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
      isValueAddNPList <- forM bppDetailList $ \bpp -> CQVAN.isValueAddNP bpp.id.getId
      let quotes = OnDemandCab <$> SQuote.mkQAPIEntityList quoteList bppDetailList isValueAddNPList
      metroOffers <- map Metro <$> Metro.getMetroOffers searchRequest.id
      publicTransportOffers <- map PublicTransport <$> PublicTransport.getPublicTransportOffers searchRequest.id
      return . sortBy (compare `on` creationTime) $ quotes <> metroOffers <> publicTransportOffers
    Nothing -> do
      quoteList <- sortByEstimatedFare <$> runInReplica (QQuote.findAllBySRId searchRequest.id)
      logDebug $ "quotes are :-" <> show quoteList
      bppDetailList <- forM ((.providerId) <$> quoteList) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
      isValueAddNPList <- forM bppDetailList $ \bpp -> CQVAN.isValueAddNP bpp.id.getId
      let quotes = OnRentalCab <$> SQuote.mkQAPIEntityList quoteList bppDetailList isValueAddNPList
      return . sortBy (compare `on` creationTime) $ quotes
  where
    sortByNearestDriverDistance quoteList = do
      let sortFunc = compare `on` getMbDistanceToNearestDriver
      sortBy sortFunc quoteList
    getMbDistanceToNearestDriver quote =
      case quote.quoteDetails of
        SQuote.OneWayDetails details -> Just details.distanceToNearestDriver
        SQuote.RentalDetails _ -> Nothing
        SQuote.DriverOfferDetails details -> details.distanceToPickup
        SQuote.OneWaySpecialZoneDetails _ -> Just $ metersToHighPrecMeters $ Meters 0
        SQuote.InterCityDetails _ -> Just $ metersToHighPrecMeters $ Meters 0
    creationTime :: OfferRes -> UTCTime
    creationTime (OnDemandCab SQuote.QuoteAPIEntity {createdAt}) = createdAt
    creationTime (Metro Metro.MetroOffer {createdAt}) = createdAt
    creationTime (OnRentalCab SQuote.QuoteAPIEntity {createdAt}) = createdAt
    creationTime (PublicTransport PublicTransportQuote {createdAt}) = createdAt

getEstimates :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SSR.SearchRequest -> m [DEstimate.EstimateAPIEntity]
getEstimates searchRequestId = do
  estimateList <- runInReplica $ QEstimate.findAllBySRId searchRequestId
  estimates <- mapM DEstimate.mkEstimateAPIEntity (sortByEstimatedFare estimateList)
  return . sortBy (compare `on` (.createdAt)) $ estimates

sortByEstimatedFare :: (HasField "estimatedFare" r Money) => [r] -> [r]
sortByEstimatedFare resultList = do
  let sortFunc = compare `on` (.estimatedFare)
  sortBy sortFunc resultList

getPaymentMethods :: (CacheFlow m r, EsqDBFlow m r) => SSR.SearchRequest -> m [DMPM.PaymentMethodAPIEntity]
getPaymentMethods searchRequest = do
  let merchantOperatingCityId = searchRequest.merchantOperatingCityId
  allMerchantPaymentMethods <- CQMPM.findAllByMerchantOperatingCityId merchantOperatingCityId
  let availablePaymentMethods = filter (\mpm -> mpm.id `elem` searchRequest.availablePaymentMethods) allMerchantPaymentMethods
  pure $ DMPM.mkPaymentMethodAPIEntity <$> availablePaymentMethods
