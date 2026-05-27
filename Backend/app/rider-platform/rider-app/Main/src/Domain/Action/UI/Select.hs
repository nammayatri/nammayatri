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
    DSelectResDetails (..),
    SelectFlow,
    select,
    select2,
    selectList,
    selectResult,
    MultimodalSelectRes (..),
  )
where

import qualified BecknV2.OnDemand.Enums as DVCT
import Control.Applicative ((<|>))
import Control.Monad.Extra (anyM)
import qualified Data.HashMap.Strict as HMS
import Data.OpenApi hiding (name)
import qualified Data.Text as T
import qualified Domain.Action.UI.Estimate as UEstimate
import qualified Domain.Action.UI.Registration as Reg
import Domain.Types.Booking
import Domain.Types.BookingStatus
import Domain.Types.Common
import qualified Domain.Types.DeliveryDetails as DTDD
import qualified Domain.Types.DriverOffer as DDO
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.EstimateStatus as DEstimate
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.ParcelDetails as DParcel
import qualified Domain.Types.ParcelType as DParcel
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequestPartiesLink as DSRPL
import qualified Domain.Types.Trip as Trip
import qualified Domain.Types.VehicleVariant as DV
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.AppMetrics as Metrics
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Version (CloudType)
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import Lib.ConfigPilot.Interface.Types (getConfig)
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.MerchantPaymentMethod
import qualified SharedLogic.Payment as SPayment
import SharedLogic.Quote
import SharedLogic.Type
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as QMPM
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.CachedQueries.ValueAddNP as CQVNP
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverOffer as QDOffer
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
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
    HasRequestId r,
    ServiceFlow m r,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["version" ::: DeploymentVersion, "cloudType" ::: Maybe CloudType],
    Redis.HedisFlow m r,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r
  )

data DSelectReq = DSelectReq
  { customerExtraFee :: Maybe Money,
    customerExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    autoAssignEnabled :: Bool,
    autoAssignEnabledV2 :: Maybe Bool,
    isPetRide :: Maybe Bool,
    paymentMethodId :: Maybe Payment.PaymentMethodId,
    paymentInstrument :: Maybe DMPM.PaymentInstrument,
    otherSelectedEstimates :: Maybe [Id DEstimate.Estimate],
    isAdvancedBookingEnabled :: Maybe Bool,
    deliveryDetails :: Maybe DTDD.DeliveryDetails,
    disabilityDisable :: Maybe Bool,
    billingCategory :: Maybe BillingCategory,
    preferSafetyPlus :: Maybe Bool,
    driverPreference :: Maybe [Text],
    selectedOfferId :: Maybe Text
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
    billingCategory :: BillingCategory,
    autoAssignEnabled :: Bool,
    isPetRide :: Maybe Bool,
    phoneNumber :: Maybe Text,
    isValueAddNP :: Bool,
    isAdvancedBookingEnabled :: Bool,
    isMultipleOrNoDeviceIdExist :: Maybe Bool,
    toUpdateDeviceIdInfo :: Bool,
    tripCategory :: Maybe TripCategory,
    disabilityDisable :: Maybe Bool,
    selectResDetails :: Maybe DSelectResDetails,
    preferSafetyPlus :: Bool,
    driverPreference :: Maybe [Text],
    mbJourneyId :: Maybe (Id DJ.Journey),
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    paymentMode :: Maybe DMPM.PaymentMode,
    emailDomain :: Maybe Text
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

select :: SelectFlow m r c => Id DPerson.Person -> Id DEstimate.Estimate -> DSelectReq -> m DSelectRes
select personId estimateId req = do
  now <- getCurrentTime
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  when (estimate.validTill < now) $ throwError (InvalidRequest $ "Estimate expired " <> show estimate.id) -- select validation check
  select2 personId estimateId req Nothing

select2 :: SelectFlow m r c => Id DPerson.Person -> Id DEstimate.Estimate -> DSelectReq -> Maybe (DJ.Journey, DJL.JourneyLeg) -> m DSelectRes
select2 personId estimateId req@DSelectReq {..} mbJourneyLegData = do
  runRequestValidation validateDSelectReq req
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  when (not (fromMaybe False person.businessProfileVerified) && billingCategory == Just BUSINESS) $ throwError (InvalidRequest "Business profile not verified for business billing category")
  merchant <- QM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  SPayment.validatePaymentInstrument merchant paymentInstrument paymentMethodId
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  Metrics.startGenericLatencyMetrics Metrics.SELECT_TO_SEND_REQUEST estimate.requestId.getId
  let searchRequestId = estimate.requestId
  remainingEstimates <- catMaybes <$> (QEstimate.findById `mapM` filter ((/=) estimate.id) (fromMaybe [] otherSelectedEstimates))
  unless (all (\e -> e.requestId == searchRequestId) remainingEstimates) $ throwError (InvalidRequest "All selected estimate should belong to same search request")
  let remainingEstimateBppIds = remainingEstimates <&> (.bppEstimateId)
  isValueAddNP <- CQVNP.isValueAddNP estimate.providerId
  phoneNumber <- bool (pure Nothing) (getPhoneNo person) isValueAddNP
  searchRequest <- QSearchRequest.findById searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist searchRequestId.getId)
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = searchRequest.merchantOperatingCityId.getId})
  when (disabilityDisable == Just True) $ QSearchRequest.updateDisability searchRequest.id Nothing
  let merchantOperatingCityId = searchRequest.merchantOperatingCityId

  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
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
  let lastUsedVehicleServiceTiers = insertVehicleServiceTierAndCategory (maybe 5 (.noOfRideRequestsConfig) riderConfig) estimate.vehicleServiceTierType person.lastUsedVehicleServiceTiers
  let lastUsedVehicleCategories = insertVehicleServiceTierAndCategory (maybe 5 (.noOfRideRequestsConfig) riderConfig) (fromMaybe DVCT.AUTO_RICKSHAW estimate.vehicleCategory) person.lastUsedVehicleCategories
  let toUpdateDeviceIdInfo =
        if fromMaybe False (riderConfig >>= (.isDeviceIdCheckDisabled))
          then False
          else (fromMaybe 0 person.totalRidesCount) == 0
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

  let mbUpdatedJourneyData =
        mbJourneyLegData <&> \(journey, journeyLeg) ->
          ( journey {DJ.status = DJ.INPROGRESS},
            journeyLeg {DJL.legPricingId = Just estimate.id.getId, DJL.legSearchId = Just searchRequest.id.getId}
          )

  -- Select Transaction
  -- Check and capture pending payments BEFORE driver allocation
  when merchant.onlinePayment $ do
    whenJust paymentMethodId $ \pmId ->
      SPayment.updateDefaultPersonPaymentMethodId person pmId -- Make payment method as default payment method for customer
    SPayment.capturePendingPaymentIfExists person merchantOperatingCityId

  merchantPaymentMethod <- maybe (return Nothing) (QMPM.findById . Id) req.paymentMethodId
  let paymentMethodInfo = mkPaymentMethodInfo <$> merchantPaymentMethod
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
  QP.updateLastUsedVehicleServiceTiersAndCategories lastUsedVehicleServiceTiers lastUsedVehicleCategories personId
  QSearchRequest.updateMultipleByRequestId searchRequestId autoAssignEnabled (fromMaybe False autoAssignEnabledV2) isAdvancedBookingEnabled
  QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = estimateId, otherSelectedEstimates, validTill = searchRequest.validTill, providerId = Just estimate.providerId, tripCategory = estimate.tripCategory}
  QEstimate.updateStatus DEstimate.DRIVER_QUOTE_REQUESTED estimateId
  QEstimate.updateSelectedOfferId selectedOfferId estimateId
  QDOffer.updateStatus DDO.INACTIVE estimateId
  when (isJust mbCustomerExtraFee || isJust req.paymentMethodId || isJust req.paymentInstrument) $ do
    void $ QSearchRequest.updateCustomerExtraFeeAndPaymentMethod searchRequest.id mbCustomerExtraFee req.paymentMethodId req.paymentInstrument
  when (isJust req.isPetRide) $ do
    QSearchRequest.updatePetRide req.isPetRide searchRequest.id
  whenJust mbUpdatedJourneyData $ \(journey, journeyLeg) -> do
    QJourney.updateByPrimaryKey journey
    QJourneyLeg.updateByPrimaryKey journeyLeg
  let emailToUse = if billingCategory == Just BUSINESS then person.businessEmail else person.email
  decryptedEmail <- mapM decrypt emailToUse
  let emailDomain = T.strip . snd <$> (decryptedEmail >>= (\e -> if T.isInfixOf "@" e then Just (T.breakOn "@" e) else Nothing))
  let emailDomain' = T.drop 1 <$> emailDomain -- drop the leading '@'
  pure
    DSelectRes
      { providerId = estimate.providerId,
        providerUrl = estimate.providerUrl,
        variant = DV.castServiceTierToVariant estimate.vehicleServiceTierType, -- TODO: fix later
        isAdvancedBookingEnabled = fromMaybe False isAdvancedBookingEnabled,
        tripCategory = estimate.tripCategory,
        selectResDetails = dselectResDetails,
        preferSafetyPlus = fromMaybe False preferSafetyPlus,
        driverPreference = driverPreference,
        mbJourneyId = (\(journey, _) -> journey.id) <$> mbUpdatedJourneyData,
        paymentMethodInfo = paymentMethodInfo,
        billingCategory = fromMaybe PERSONAL billingCategory,
        paymentMode = person.paymentMode,
        emailDomain = emailDomain',
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
  pure $ SelectListRes $ mkQAPIEntityList selectedQuotes bppDetailList isValueAddNPList

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
      return $ QuotesResultResponse {bookingId = Nothing, bookingIdV2 = Nothing, selectedQuotes = Just $ SelectListRes $ mkQAPIEntityList selectedQuotes bppDetailList isValueAddNPList, ..}

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

insertVehicleServiceTierAndCategory :: (Eq a) => Int -> a -> [a] -> [a]
insertVehicleServiceTierAndCategory n newVehicle currentList
  | length currentList < n = currentList ++ [newVehicle]
  | otherwise = tail currentList ++ [newVehicle]

data MultimodalSelectRes = MultimodalSelectRes
  { journeyId :: Maybe (Id DJ.Journey),
    result :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
