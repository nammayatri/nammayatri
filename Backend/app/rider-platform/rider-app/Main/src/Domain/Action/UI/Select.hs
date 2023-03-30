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
  ( DSelectRes (..),
    SelectListRes (..),
    select,
    selectList,
    selectResult,
    QuotesResultResponse (..),
    DEstimateSelectReq (..),
    CancelAPIResponse (..),
  )
where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (parseFail, typeMismatch)
import Domain.Types.Booking.Type
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
import Kernel.Storage.Esqueleto (runInReplica)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Estimate (checkIfEstimateCancelled)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error

data DSelectRes = DSelectRes
  { searchRequest :: DSearchReq.SearchRequest,
    estimateId :: Id DEstimate.Estimate,
    providerId :: Text,
    providerUrl :: BaseUrl,
    variant :: VehicleVariant,
    customerLanguage :: Maybe Maps.Language,
    city :: Text
  }

data DEstimateSelectReq = DEstimateSelectReq
  { autoAssignEnabled :: Bool,
    autoAssignEnabledV2 :: Maybe Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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

select :: Id DPerson.Person -> Id DEstimate.Estimate -> Bool -> Bool -> Flow DSelectRes
select personId estimateId autoAssignEnabled autoAssignEnabledV2 = do
  now <- getCurrentTime
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  checkIfEstimateCancelled estimate.id estimate.status
  let searchRequestId = estimate.requestId
  searchRequest <- QSearchRequest.findByPersonId personId searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist personId.getId)
  merchant <- CQM.findById searchRequest.merchantId >>= fromMaybeM (MerchantNotFound searchRequest.merchantId.getId)
  when ((searchRequest.validTill) < now) $
    throwError SearchRequestExpired
  Esq.runTransaction $ do
    QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = estimateId, validTill = searchRequest.validTill}
    QEstimate.updateStatus estimateId $ Just DEstimate.DRIVER_QUOTE_REQUESTED
    QEstimate.updateAutoAssign estimateId autoAssignEnabled autoAssignEnabledV2
  pure
    DSelectRes
      { providerId = estimate.providerId,
        providerUrl = estimate.providerUrl,
        variant = estimate.vehicleVariant,
        customerLanguage = searchRequest.language,
        city = merchant.city,
        ..
      }

selectList :: (EsqDBReplicaFlow m r) => Id DEstimate.Estimate -> m SelectListRes
selectList estimateId = do
  estimate <- runInReplica $ QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  checkIfEstimateCancelled estimate.id estimate.status
  selectedQuotes <- runInReplica $ QQuote.findAllByEstimateId estimateId
  pure $ SelectListRes $ map DQuote.makeQuoteAPIEntity selectedQuotes

selectResult :: (EsqDBReplicaFlow m r) => Id DEstimate.Estimate -> m QuotesResultResponse
selectResult estimateId = do
  res <- runMaybeT $ do
    estimate <- MaybeT . runInReplica $ QEstimate.findById estimateId
    quoteId <- MaybeT $ pure estimate.autoAssignQuoteId
    _ <- MaybeT (Just <$> checkIfEstimateCancelled estimate.id estimate.status)
    booking <- MaybeT . runInReplica $ QBooking.findAssignedByQuoteId (Id quoteId)
    return $ QuotesResultResponse {bookingId = Just booking.id, selectedQuotes = Nothing}
  case res of
    Just r -> pure r
    Nothing -> do
      selectedQuotes <- runInReplica $ QQuote.findAllByEstimateId estimateId
      return $ QuotesResultResponse {bookingId = Nothing, selectedQuotes = Just $ SelectListRes $ map DQuote.makeQuoteAPIEntity selectedQuotes}
