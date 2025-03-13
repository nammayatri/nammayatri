{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Select
  ( DSelectReq (..),
    DSelectRes (..),
    DSelectResultRes (..),
    SelectListRes (..),
    QuotesResultResponse (..),
    CancelAPIResponse (..),
    DSelectResDetails (..),
    SelectFlow,
    select,
    select2,
    selectList,
    selectResult,
  )
where

import Control.Applicative ((<|>))
import qualified Control.Lens as L
import Control.Monad.Extra (anyM)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (parseFail, typeMismatch)
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashMap.Strict.InsOrd as HMSIO
import Data.OpenApi hiding (name)
import qualified Data.Text as T
import qualified Domain.Action.UI.Estimate as UEstimate
import Domain.Action.UI.Quote
import qualified Domain.Action.UI.Quote as UQuote
import qualified Domain.Action.UI.Registration as Reg
import Domain.Types.Booking (Booking, BookingStatus (..))
import Domain.Types.Common
import qualified Domain.Types.DeliveryDetails as DTDD
import qualified Domain.Types.DriverOffer as DDO
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.ParcelDetails as DParcel
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequestPartiesLink as DSRPL
import qualified Domain.Types.Trip as Trip
import qualified Domain.Types.VehicleVariant as DV
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.AppMetrics as Metrics
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import Lib.SessionizerMetrics.Types.Event
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.CachedQueries.ValueAddNP as CQVNP
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverOffer as QDOffer
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Location as QLoc
import qualified Storage.Queries.ParcelDetails as QParcel
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Storage.Queries.SearchRequestPartiesLink as QSRPL
import Tools.Error
import qualified Tools.SharedRedisKeys as SharedRedisKeys
import TransactionLogs.Types

type SelectFlow m r c =
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EventStreamFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasCoreMetrics r,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["version" ::: DeploymentVersion],
    Redis.HedisFlow m r
  )

data DSelectReq = DSelectReq
  { customerExtraFee :: Maybe Money,
    customerExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    autoAssignEnabled :: Bool,
    autoAssignEnabledV2 :: Maybe Bool,
    paymentMethodId :: Maybe Payment.PaymentMethodId,
    otherSelectedEstimates :: Maybe [Id DEstimate.Estimate],
    isAdvancedBookingEnabled :: Maybe Bool,
    deliveryDetails :: Maybe DTDD.DeliveryDetails,
    disabilityDisable :: Maybe Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

validateDSelectReq :: Validate DSelectReq
validateDSelectReq DSelectReq {..} =
  sequenceA_
    [ validateField "customerExtraFee" customerExtraFee $ InMaybe $ InRange @Money 1 100000,
      whenJust customerExtraFeeWithCurrency $ \obj ->
        validateObject "customerExtraFeeWithCurrency" obj $ \obj' ->
          validateField "amount" obj'.amount $ InRange @HighPrecMoney 1.0 100000.0,
      whenJust deliveryDetails $ \(DTDD.DeliveryDetails {..}) ->
        sequenceA_
          [ validateObject "senderDetails" senderDetails validatePersonDetails,
            validateObject "receiverDetails" receiverDetails validatePersonDetails,
            validateField "parcelQuantity" parcelQuantity $ InMaybe $ InRange @Int 1 100000
          ]
    ]

validatePersonDetails :: Validate DTDD.PersonDetails
validatePersonDetails DTDD.PersonDetails {..} =
  sequenceA_
    [ validateField "phoneNumber" phoneNumber P.mobileNumber,
      whenJust countryCode $ \cc -> validateField "countryCode" cc P.mobileCountryCode
    ]

data DSelectRes = DSelectRes
  { searchRequest :: DSearchReq.SearchRequest,
    estimate :: DEstimate.Estimate,
    remainingEstimateBppIds :: [Id DEstimate.BPPEstimate],
    providerId :: Text,
    providerUrl :: BaseUrl,
    variant :: DV.VehicleVariant,
    customerExtraFee :: Maybe Money,
    customerExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    merchant :: DM.Merchant,
    city :: Context.City,
    autoAssignEnabled :: Bool,
    phoneNumber :: Maybe Text,
    isValueAddNP :: Bool,
    isAdvancedBookingEnabled :: Bool,
    isMultipleOrNoDeviceIdExist :: Maybe Bool,
    toUpdateDeviceIdInfo :: Bool,
    tripCategory :: Maybe TripCategory,
    disabilityDisable :: Maybe Bool,
    selectResDetails :: Maybe DSelectResDetails
  }

data DSelectResDetails = DSelectResDelivery DParcel.ParcelDetails

newtype DSelectResultRes = DSelectResultRes
  { selectTtl :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data QuotesResultResponse = QuotesResultResponse
  { selectedQuotes :: Maybe SelectListRes,
    bookingId :: Maybe (Id Booking), -- DEPRECATED
    batchConfig :: Maybe SharedRedisKeys.BatchConfig,
    bookingIdV2 :: Maybe (Id Booking)
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype SelectListRes = SelectListRes
  { selectedQuotes :: [QuoteAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CancelAPIResponse = BookingAlreadyCreated | FailedToCancel | Success
  deriving stock (Generic, Show, Enum, Bounded)

allCancelAPIResponse :: [CancelAPIResponse]
allCancelAPIResponse = [minBound .. maxBound]

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

instance ToSchema CancelAPIResponse where
  declareNamedSchema _ = do
    return $
      NamedSchema (Just "CancelAPIResponse") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ HMSIO.singleton "result" enumsSchema
          & required L..~ ["result"]
    where
      enumsSchema =
        (mempty :: Schema)
          & type_ L.?~ OpenApiString
          & enum_ L.?~ map (A.String . T.pack . show) allCancelAPIResponse
          & Inline

select :: SelectFlow m r c => Id DPerson.Person -> Id DEstimate.Estimate -> DSelectReq -> m DSelectRes
select personId estimateId req = do
  now <- getCurrentTime
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  when (estimate.validTill < now) $ throwError (InvalidRequest $ "Estimate expired " <> show estimate.id) -- select validation check
  select2 personId estimateId req

select2 :: SelectFlow m r c => Id DPerson.Person -> Id DEstimate.Estimate -> DSelectReq -> m DSelectRes
select2 personId estimateId req@DSelectReq {..} = do
  runRequestValidation validateDSelectReq req
  now <- getCurrentTime
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  Metrics.startGenericLatencyMetrics Metrics.SELECT_TO_SEND_REQUEST estimate.requestId.getId
  let searchRequestId = estimate.requestId
  remainingEstimates <- catMaybes <$> (QEstimate.findById `mapM` filter ((/=) estimate.id) (fromMaybe [] otherSelectedEstimates))
  unless (all (\e -> e.requestId == searchRequestId) remainingEstimates) $ throwError (InvalidRequest "All selected estimate should belong to same search request")
  let remainingEstimateBppIds = remainingEstimates <&> (.bppEstimateId)
  isValueAddNP <- CQVNP.isValueAddNP estimate.providerId
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  phoneNumber <- bool (pure Nothing) (getPhoneNo person) isValueAddNP
  searchRequest <- QSearchRequest.findByPersonId personId searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist personId.getId)
  when (disabilityDisable == Just True) $ QSearchRequest.updateDisability searchRequest.id Nothing
  merchant <- QM.findById searchRequest.merchantId >>= fromMaybeM (MerchantNotFound searchRequest.merchantId.getId)
  when merchant.onlinePayment $ do
    when (isNothing paymentMethodId) $ throwError PaymentMethodRequired
    QP.updateDefaultPaymentMethodId paymentMethodId personId -- Make payment method as default payment method for customer
  when ((searchRequest.validTill) < now) $
    throwError SearchRequestExpired
  when (maybe False Trip.isDeliveryTrip (DEstimate.tripCategory estimate)) $ do
    validDeliveryDetails <- deliveryDetails & fromMaybeM (InvalidRequest "Delivery details not found for trip category Delivery")
    updateRequiredDeliveryDetails searchRequestId searchRequest.merchantId searchRequest.merchantOperatingCityId validDeliveryDetails
    let senderLocationId = searchRequest.fromLocation.id
    receiverLocationId <- (searchRequest.toLocation <&> (.id)) & fromMaybeM (InvalidRequest "Receiver location not found for trip category Delivery")
    let senderLocationAddress = validDeliveryDetails.senderDetails.address
        receiverLocationAddress = validDeliveryDetails.receiverDetails.address
    QLoc.updateInstructionsAndExtrasById senderLocationAddress.instructions senderLocationAddress.extras senderLocationId
    QLoc.updateInstructionsAndExtrasById receiverLocationAddress.instructions receiverLocationAddress.extras receiverLocationId
    QSearchRequest.updateInitiatedBy (Just $ Trip.DeliveryParty validDeliveryDetails.initiatedAs) searchRequestId

  QSearchRequest.updateMultipleByRequestId searchRequestId autoAssignEnabled (fromMaybe False autoAssignEnabledV2) isAdvancedBookingEnabled
  QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = estimateId, otherSelectedEstimates, validTill = searchRequest.validTill, providerId = Just estimate.providerId, tripCategory = estimate.tripCategory}
  QEstimate.updateStatus DEstimate.DRIVER_QUOTE_REQUESTED estimateId
  QDOffer.updateStatus DDO.INACTIVE estimateId
  let mbCustomerExtraFee = (mkPriceFromAPIEntity <$> req.customerExtraFeeWithCurrency) <|> (mkPriceFromMoney Nothing <$> req.customerExtraFee)
  Kernel.Prelude.whenJust req.customerExtraFeeWithCurrency $ \reqWithCurrency -> do
    unless (estimate.estimatedFare.currency == reqWithCurrency.currency) $
      throwError $ InvalidRequest "Invalid currency"
  dselectResDetails <-
    DEstimate.tripCategory estimate & \case
      Just (Trip.Delivery _) -> do
        parcelDetails <- QParcel.findBySearchRequestId searchRequest.id
        return $ DSelectResDelivery <$> parcelDetails
      _ -> pure Nothing
  when (isJust mbCustomerExtraFee || isJust req.paymentMethodId) $ do
    void $ QSearchRequest.updateCustomerExtraFeeAndPaymentMethod searchRequest.id mbCustomerExtraFee req.paymentMethodId
  let merchantOperatingCityId = searchRequest.merchantOperatingCityId
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  let toUpdateDeviceIdInfo = (fromMaybe 0 person.totalRidesCount) == 0
  isMultipleOrNoDeviceIdExist <-
    maybe
      (return Nothing)
      ( \deviceId -> do
          if toUpdateDeviceIdInfo
            then do
              personsWithSameDeviceId <- QP.findAllByDeviceId (Just deviceId)
              return $ Just (length personsWithSameDeviceId > 1)
            else return Nothing
      )
      person.deviceId
  pure
    DSelectRes
      { providerId = estimate.providerId,
        providerUrl = estimate.providerUrl,
        variant = DV.castServiceTierToVariant estimate.vehicleServiceTierType, -- TODO: fix later
        isAdvancedBookingEnabled = fromMaybe False isAdvancedBookingEnabled,
        tripCategory = estimate.tripCategory,
        selectResDetails = dselectResDetails,
        ..
      }
  where
    getPhoneNo :: EncFlow m r => DPerson.Person -> m (Maybe (UnencryptedItem (EncryptedHashed Text)))
    getPhoneNo person = do
      mapM decrypt person.mobileNumber

--DEPRECATED
selectList :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DEstimate.Estimate -> m SelectListRes
selectList estimateId = do
  estimate <- runInReplica $ QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  when (UEstimate.isCancelled estimate.status) $ throwError $ EstimateCancelled estimate.id.getId
  selectedQuotes <- runInReplica $ QQuote.findAllByEstimateId estimateId DDO.ACTIVE
  bppDetailList <- forM ((.providerId) <$> selectedQuotes) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
  isValueAddNPList <- forM bppDetailList $ \bpp -> CQVAN.isValueAddNP bpp.subscriberId
  pure $ SelectListRes $ UQuote.mkQAPIEntityList selectedQuotes bppDetailList isValueAddNPList

selectResult :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DEstimate.Estimate -> m QuotesResultResponse
selectResult estimateId = do
  estimate <- runInReplica $ QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  batchConfig <- SharedRedisKeys.getBatchConfig estimate.requestId.getId
  res <- runMaybeT $ do
    when (UEstimate.isCancelled estimate.status) $ MaybeT $ throwError $ EstimateCancelled estimate.id.getId
    booking <- MaybeT . runInReplica $ QBooking.findByTransactionIdAndStatus estimate.requestId.getId [NEW, CONFIRMED, TRIP_ASSIGNED, AWAITING_REASSIGNMENT, CANCELLED]
    let bookingId = if booking.status == TRIP_ASSIGNED then Just booking.id else Nothing
    let bookingIdV2 = Just booking.id
    return $ QuotesResultResponse {selectedQuotes = Nothing, ..}
  case res of
    Just r -> pure r
    Nothing -> do
      selectedQuotes <- runInReplica $ QQuote.findAllQuotesBySRId estimate.requestId DDO.ACTIVE
      bppDetailList <- forM ((.providerId) <$> selectedQuotes) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
      isValueAddNPList <- forM bppDetailList $ \bpp -> CQVAN.isValueAddNP bpp.subscriberId
      return $ QuotesResultResponse {bookingId = Nothing, bookingIdV2 = Nothing, selectedQuotes = Just $ SelectListRes $ UQuote.mkQAPIEntityList selectedQuotes bppDetailList isValueAddNPList, ..}

updateRequiredDeliveryDetails :: SelectFlow m r c => Id DSearchReq.SearchRequest -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTDD.DeliveryDetails -> m ()
updateRequiredDeliveryDetails searchRequestId merchantId merchantOperatingCityId deliveryDetails = do
  senderPartyId <- Reg.createPersonWithPhoneNumber merchantId (deliveryDetails.senderDetails.phoneNumber) (deliveryDetails.senderDetails.countryCode)
  receiverPartyId <- Reg.createPersonWithPhoneNumber merchantId (deliveryDetails.receiverDetails.phoneNumber) (deliveryDetails.receiverDetails.countryCode)
  let parcelType = fromMaybe (DParcel.Others "Unknown") deliveryDetails.parcelType
      quantity = deliveryDetails.parcelQuantity
  -- restrict here only as why send request to driver and restrict later during booking
  isActiveBookingPresentForAnyParty <- anyM (\partyId -> isJust <$> QBooking.findLatestSelfAndPartyBookingByRiderId partyId) [senderPartyId, receiverPartyId]
  when isActiveBookingPresentForAnyParty $ throwError $ InvalidRequest "ACTIVE_BOOKING_PRESENT_FOR_OTHER_INVOLVED_PARTIES"
  senderSpId <- Id <$> generateGUID
  receiverSpId <- Id <$> generateGUID
  now <- getCurrentTime
  let senderParty =
        DSRPL.SearchRequestPartiesLink
          { id = senderSpId,
            partyId = senderPartyId,
            partyName = deliveryDetails.senderDetails.name,
            partyType = Trip.DeliveryParty Trip.Sender,
            searchRequestId = searchRequestId,
            createdAt = now,
            updatedAt = now
          }
  let receiverParty =
        DSRPL.SearchRequestPartiesLink
          { id = receiverSpId,
            partyId = receiverPartyId,
            partyName = deliveryDetails.receiverDetails.name,
            partyType = Trip.DeliveryParty Trip.Receiver,
            searchRequestId = searchRequestId,
            createdAt = now,
            updatedAt = now
          }
  QSRPL.createMany [senderParty, receiverParty]
  QParcel.create $ DParcel.ParcelDetails {createdAt = now, updatedAt = now, ..}
