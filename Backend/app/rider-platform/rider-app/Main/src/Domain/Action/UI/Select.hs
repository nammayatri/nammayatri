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
  ( DSelectReq (..),
    DSelectRes (..),
    DSelectResultRes (..),
    SelectListRes (..),
    QuotesResultResponse (..),
    CancelAPIResponse (..),
    select,
    select2,
    selectList,
    selectResult,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (parseFail, typeMismatch)
import qualified Domain.Action.UI.Estimate as UEstimate
import Domain.Action.UI.Quote
import qualified Domain.Action.UI.Quote as UQuote
import Domain.Types.Booking
import qualified Domain.Types.DriverOffer as DDO
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.VehicleServiceTier as DVST
import Domain.Types.VehicleVariant (VehicleVariant)
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.Validation
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.CachedQueries.ValueAddNP as CQVNP
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverOffer as QDOffer
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error

data DSelectReq = DSelectReq
  { customerExtraFee :: Maybe Money,
    customerExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    autoAssignEnabled :: Bool,
    autoAssignEnabledV2 :: Maybe Bool,
    paymentMethodId :: Maybe (Id DMPM.MerchantPaymentMethod),
    otherSelectedEstimates :: Maybe [Id DEstimate.Estimate]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

validateDSelectReq :: Validate DSelectReq
validateDSelectReq DSelectReq {..} =
  sequenceA_
    [ validateField "customerExtraFee" customerExtraFee $ InMaybe $ InRange @Money 1 100000,
      whenJust customerExtraFeeWithCurrency $ \obj ->
        validateObject "customerExtraFeeWithCurrency" obj $ \obj' ->
          validateField "amount" obj'.amount $ InRange @HighPrecMoney 1.0 100000.0
    ]

data DSelectRes = DSelectRes
  { searchRequest :: DSearchReq.SearchRequest,
    estimate :: DEstimate.Estimate,
    remainingEstimateBppIds :: [Id DEstimate.BPPEstimate],
    providerId :: Text,
    providerUrl :: BaseUrl,
    variant :: VehicleVariant,
    customerExtraFee :: Maybe Money,
    customerExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    merchant :: DM.Merchant,
    city :: Context.City,
    autoAssignEnabled :: Bool,
    phoneNumber :: Maybe Text,
    isValueAddNP :: Bool
  }

newtype DSelectResultRes = DSelectResultRes
  { selectTtl :: Int
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

select :: Id DPerson.Person -> Id DEstimate.Estimate -> DSelectReq -> Flow DSelectRes
select personId estimateId req = do
  now <- getCurrentTime
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  when (estimate.validTill < now) $ throwError (InvalidRequest $ "Estimate expired " <> show estimate.id) -- select validation check
  select2 personId estimateId req

select2 :: Id DPerson.Person -> Id DEstimate.Estimate -> DSelectReq -> Flow DSelectRes
select2 personId estimateId req@DSelectReq {..} = do
  runRequestValidation validateDSelectReq req
  now <- getCurrentTime
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  let searchRequestId = estimate.requestId
  remainingEstimates <- catMaybes <$> (QEstimate.findById `mapM` filter ((/=) estimate.id) (fromMaybe [] otherSelectedEstimates))
  unless (all (\e -> e.requestId == searchRequestId) remainingEstimates) $ throwError (InvalidRequest "All selected estimate should belong to same search request")
  let remainingEstimateBppIds = remainingEstimates <&> (.bppEstimateId)
  isValueAddNP <- CQVNP.isValueAddNP estimate.providerId
  phoneNumber <- bool (pure Nothing) getPhoneNo isValueAddNP
  searchRequest <- QSearchRequest.findByPersonId personId searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist personId.getId)
  merchant <- QM.findById searchRequest.merchantId >>= fromMaybeM (MerchantNotFound searchRequest.merchantId.getId)
  when ((searchRequest.validTill) < now) $
    throwError SearchRequestExpired
  _ <- QSearchRequest.updateAutoAssign searchRequestId autoAssignEnabled (fromMaybe False autoAssignEnabledV2)
  _ <- QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = estimateId, otherSelectedEstimates, validTill = searchRequest.validTill}
  _ <- QEstimate.updateStatus DEstimate.DRIVER_QUOTE_REQUESTED estimateId
  _ <- QDOffer.updateStatus DDO.INACTIVE estimateId
  let mbCustomerExtraFee = (mkPriceFromAPIEntity <$> req.customerExtraFeeWithCurrency) <|> (mkPriceFromMoney Nothing <$> req.customerExtraFee)
  Kernel.Prelude.whenJust req.customerExtraFeeWithCurrency $ \reqWithCurrency -> do
    unless (estimate.estimatedFare.currency == reqWithCurrency.currency) $
      throwError $ InvalidRequest "Invalid currency"

  when (isJust mbCustomerExtraFee || isJust req.paymentMethodId) $ do
    void $ QSearchRequest.updateCustomerExtraFeeAndPaymentMethod searchRequest.id mbCustomerExtraFee req.paymentMethodId
  QPFS.clearCache searchRequest.riderId
  let merchantOperatingCityId = searchRequest.merchantOperatingCityId
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  pure
    DSelectRes
      { providerId = estimate.providerId,
        providerUrl = estimate.providerUrl,
        variant = DVST.castServiceTierToVariant estimate.vehicleServiceTierType, -- TODO: fix later
        ..
      }
  where
    getPhoneNo = do
      person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      mapM decrypt person.mobileNumber

--DEPRECATED
selectList :: (KvDbFlow m r, EsqDBReplicaFlow m r) => Id DEstimate.Estimate -> m SelectListRes
selectList estimateId = do
  estimate <- runInReplica $ QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  when (UEstimate.isCancelled estimate.status) $ throwError $ EstimateCancelled estimate.id.getId
  selectedQuotes <- runInReplica $ QQuote.findAllByEstimateId estimateId DDO.ACTIVE
  bppDetailList <- forM ((.providerId) <$> selectedQuotes) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
  isValueAddNPList <- forM bppDetailList $ \bpp -> CQVAN.isValueAddNP bpp.id.getId
  pure $ SelectListRes $ UQuote.mkQAPIEntityList selectedQuotes bppDetailList isValueAddNPList

selectResult :: (KvDbFlow m r, EsqDBReplicaFlow m r) => Id DEstimate.Estimate -> m QuotesResultResponse
selectResult estimateId = do
  res <- runMaybeT $ do
    estimate <- MaybeT . runInReplica $ QEstimate.findById estimateId
    when (UEstimate.isCancelled estimate.status) $ MaybeT $ throwError $ EstimateCancelled estimate.id.getId
    bookingId <- MaybeT . runInReplica $ QBooking.findBookingIdAssignedByEstimateId estimate.id [TRIP_ASSIGNED]
    return $ QuotesResultResponse {bookingId = Just bookingId, selectedQuotes = Nothing}
  case res of
    Just r -> pure r
    Nothing -> do
      selectedQuotes <- runInReplica $ QQuote.findAllByEstimateId estimateId DDO.ACTIVE
      bppDetailList <- forM ((.providerId) <$> selectedQuotes) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
      isValueAddNPList <- forM bppDetailList $ \bpp -> CQVAN.isValueAddNP bpp.id.getId
      return $ QuotesResultResponse {bookingId = Nothing, selectedQuotes = Just $ SelectListRes $ UQuote.mkQAPIEntityList selectedQuotes bppDetailList isValueAddNPList}
