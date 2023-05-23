{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Select
  ( DEstimateSelectReq (..),
    DSelectRes (..),
    SelectListRes (..),
    QuotesResultResponse (..),
    CancelAPIResponse (..),
    select,
    selectList,
    selectResult,
  )
where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (parseFail, typeMismatch)
import qualified Data.List.NonEmpty as NE
import qualified Domain.Action.UI.SimulatedFlow.Maps as SM
import Domain.Types.Booking.Type
import qualified Domain.Types.DriverOffer as DDriverOffer
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Domain.Types.Quote (QuoteAPIEntity (..))
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSearchReq
import Domain.Types.VehicleVariant (VehicleVariant)
import Environment
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Randomizer
import Kernel.Storage.Esqueleto (runInReplica)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.Validation
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.SimulatedFlow.Driver as CSDQ
import qualified Storage.CachedQueries.SimulatedFlow.SearchRequest as CSimulated
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import qualified Tools.Maps as Maps
import Tools.Metrics

data DEstimateSelectReq = DEstimateSelectReq
  { customerExtraFee :: Maybe Money,
    autoAssignEnabled :: Bool,
    autoAssignEnabledV2 :: Maybe Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

validateDSelectReq :: Validate DEstimateSelectReq
validateDSelectReq DEstimateSelectReq {..} =
  sequenceA_
    [ validateField "customerExtraFee" customerExtraFee $ InMaybe $ InRange @Money 1 100
    ]

data DSelectRes = DSelectRes
  { searchRequest :: DSearchReq.SearchRequest,
    estimate :: DEstimate.Estimate,
    providerId :: Text,
    providerUrl :: BaseUrl,
    variant :: VehicleVariant,
    city :: Text,
    customerLanguage :: Maybe Maps.Language,
    customerExtraFee :: Maybe Money,
    autoAssignEnabled :: Bool
  }

data QuotesResultResponse = QuotesResultResponse
  { selectedQuotes :: Maybe SelectListRes,
    bookingId :: Maybe (Id Booking)
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype SelectListRes = SelectListRes
  { selectedQuotes :: [QuoteAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CancelAPIResponse = BookingAlreadyCreated | FailedToCancel | Success
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

instance ToJSON CancelAPIResponse where
  toJSON Success = A.object ["result" .= ("Success" :: Text)]
  toJSON BookingAlreadyCreated = A.object ["result" .= ("BookingAlreadyCreated" :: Text)]
  toJSON FailedToCancel = A.object ["result" .= ("FailedToCancel" :: Text)]

instance FromJSON CancelAPIResponse where
  parseJSON (A.Object obj) = do
    result :: String <- obj .: "result"
    case result of
      "FailedToCancel" -> pure FailedToCancel
      "BookingAlreadyCreated" -> pure BookingAlreadyCreated
      "Success" -> pure Success
      _ -> parseFail "Expected \"Success\" in \"result\" field."
  parseJSON err = typeMismatch "Object APISuccess" err

select :: Id DPerson.Person -> Id DEstimate.Estimate -> DEstimateSelectReq -> Flow DSelectRes
select personId estimateId req@DEstimateSelectReq {..} = do
  runRequestValidation validateDSelectReq req
  now <- getCurrentTime
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  unless (estimate.status /= DEstimate.DRIVER_QUOTE_REQUESTED) $ throwError (InvalidRequest "Estimate already offered")
  when (DEstimate.isCancelled estimate.status) $ throwError $ EstimateCancelled estimate.id.getId
  let searchRequestId = estimate.requestId
  searchRequest <- QSearchRequest.findByPersonId personId searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist personId.getId)
  merchant <- CQM.findById searchRequest.merchantId >>= fromMaybeM (MerchantNotFound searchRequest.merchantId.getId)
  when ((searchRequest.validTill) < now) $
    throwError SearchRequestExpired
  Esq.runTransaction $ do
    QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = estimateId, validTill = searchRequest.validTill}
    QEstimate.updateStatus estimateId DEstimate.DRIVER_QUOTE_REQUESTED
    QEstimate.updateAutoAssign estimateId autoAssignEnabled (fromMaybe False autoAssignEnabledV2)
    when (isJust req.customerExtraFee) $ do
      QSearchRequest.updateCustomerExtraFee searchRequest.id req.customerExtraFee
  QPFS.clearCache searchRequest.riderId
  pure
    DSelectRes
      { providerId = estimate.providerId,
        providerUrl = estimate.providerUrl,
        variant = estimate.vehicleVariant,
        customerLanguage = searchRequest.language,
        city = merchant.city,
        ..
      }

selectList :: (EsqDBReplicaFlow m r, EsqDBFlow m r, CoreMetrics m, SimluatedCacheFlow m r, CacheFlow m r, EncFlow m r, HasField "simulatedQuoteExpirationSeconds" r NominalDiffTime) => Id DEstimate.Estimate -> m SelectListRes
selectList estimateId = do
  mEstimate <- runInReplica $ QEstimate.findById estimateId
  case mEstimate of
    Just estimate -> do
      when (DEstimate.isCancelled estimate.status) $ throwError $ EstimateCancelled estimate.id.getId
      selectedQuotes <- runInReplica $ QQuote.findAllByEstimateId estimateId
      pure $ SelectListRes $ map DQuote.makeQuoteAPIEntity selectedQuotes
    Nothing -> do
      personId <- CSimulated.getRiderIdByEstimateId estimateId >>= fromMaybeM (PersonNotFound $ "(Simulated) by estimateId: " <> estimateId.getId)
      person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      if person.isSimulated
        then do
          DEstimate.EstimateAPIEntity {..} <- CSimulated.getEstimateById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
          now <- getCurrentTime
          allDrivers <- CSDQ.findAllDrivers
          selectedDriver <- getRandomElement allDrivers
          simulatedQuoteExpirationSeconds <- asks (.simulatedQuoteExpirationSeconds)
          distanceToPickup <- getRandomInRange (100, 2000)
          searchReqId <- CSimulated.getSearchRequestIdByEstimateId estimateId >>= fromMaybeM (InvalidRequest $ "SimulatedFlow: searchReq not found by estiateId: " <> estimateId.getId)
          searchReq <- CSimulated.getSearchRequestById searchReqId >>= fromMaybeM (InvalidRequest $ "SimulatedFlow: searchReq not found by reqId: " <> estimateId.getId)
          let driverPosition = SM.addMeters (fromIntegral distanceToPickup) $ Maps.LatLong searchReq.fromLocation.lat searchReq.fromLocation.lat
          let destinationLocation = Maps.LatLong searchReq.fromLocation.lat searchReq.fromLocation.lat
          let request =
                Maps.GetRoutesReq
                  { waypoints = NE.fromList [driverPosition, destinationLocation],
                    calcPoints = True,
                    mode = Just Maps.CAR
                  }
          routeResponse <- Maps.getSimulatedRoutes person.merchantId request
          let defaultAverageSpeed = 8 -- m/s
          let durationToPickup = div distanceToPickup defaultAverageSpeed
              defaultRoute =
                Maps.RouteInfo
                  { duration = Nothing,
                    distance = Nothing,
                    boundingBox = Nothing,
                    snappedWaypoints = [],
                    points = [driverPosition, destinationLocation]
                  }
          CSDQ.cacheSelectedDriver estimateId selectedDriver driverPosition (fromMaybe defaultRoute $ listToMaybe routeResponse)
          let validTill = addUTCTime simulatedQuoteExpirationSeconds now
              quoteDetails =
                DQuote.DriverOfferAPIDetails $
                  DDriverOffer.DriverOfferAPIEntity
                    { driverName = selectedDriver.driverName,
                      durationToPickup = durationToPickup,
                      distanceToPickup = HighPrecMeters (fromIntegral distanceToPickup),
                      validTill,
                      rating = selectedDriver.driverRating
                    }
          guid <- generateGUID
          CSimulated.linkEstimateIdWithQuoteId estimateId guid
          pure $ SelectListRes [DQuote.QuoteAPIEntity {id = guid, ..}]
        else throwError $ EstimateDoesNotExist estimateId.getId

selectResult :: (EsqDBReplicaFlow m r) => Id DEstimate.Estimate -> m QuotesResultResponse
selectResult estimateId = do
  res <- runMaybeT $ do
    estimate <- MaybeT . runInReplica $ QEstimate.findById estimateId
    quoteId <- MaybeT $ pure estimate.autoAssignQuoteId
    when (DEstimate.isCancelled estimate.status) $ MaybeT $ throwError $ EstimateCancelled estimate.id.getId
    booking <- MaybeT . runInReplica $ QBooking.findAssignedByQuoteId (Id quoteId)
    return $ QuotesResultResponse {bookingId = Just booking.id, selectedQuotes = Nothing}
  case res of
    Just r -> pure r
    Nothing -> do
      selectedQuotes <- runInReplica $ QQuote.findAllByEstimateId estimateId
      return $ QuotesResultResponse {bookingId = Nothing, selectedQuotes = Just $ SelectListRes $ map DQuote.makeQuoteAPIEntity selectedQuotes}
