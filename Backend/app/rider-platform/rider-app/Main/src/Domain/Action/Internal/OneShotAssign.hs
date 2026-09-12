{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | One-shot assignment (dev/docs/one-shot-assign-plan.md): single internal callback
-- from a value-add-NP BPP replacing the on_select/init/on_init/confirm/on_confirm
-- relay. Creates quote + booking + ride in one go; the booking row is born
-- TRIP_ASSIGNED (no intermediate states), then the shared rideAssignedReqHandler
-- takes over (customer notification, callTrack, reminders). Designed resumable: the
-- BPP retries on failure, so every step is guarded by an existence check.
module Domain.Action.Internal.OneShotAssign where

import qualified Domain.Action.Beckn.Common as DCommon
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import Domain.Types
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.VehicleVariant as DVeh
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Confirm as SConfirm
import SharedLogic.Type (BillingCategory)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.QueriesExtra.SearchRequestLite as QSRLite
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Event

-- NOTE: field names (and JSON encoding) must stay in sync with the BPP client type
-- in dynamic-offer-driver-app SharedLogic.CallBAPInternal.OneShotAssignReq.
data OneShotAssignReq = OneShotAssignReq
  { transactionId :: Text,
    bppEstimateId :: Text,
    bppQuoteId :: Text,
    bppBookingId :: Text,
    bppRideId :: Text,
    currency :: Currency,
    estimatedFare :: HighPrecMoney,
    commission :: Maybe HighPrecMoney,
    paymentCharge :: Maybe HighPrecMoney,
    paymentChargeBearer :: Maybe Text,
    fareBreakups :: [OneShotFareBreakupItem],
    quoteValidTill :: UTCTime,
    otp :: Text,
    driverDetails :: OneShotDriverDetails,
    vehicleDetails :: OneShotVehicleDetails,
    distanceToPickup :: Maybe Meters,
    durationToPickup :: Maybe Seconds,
    previousRideEndPos :: Maybe LatLong,
    isAlreadyFav :: Bool,
    favCount :: Maybe Int,
    isSafetyPlus :: Bool,
    isFreeRide :: Bool,
    specialLocationTag :: Maybe Text,
    isTierUpgrade :: Bool,
    assignedServiceTierName :: Maybe Text,
    billingCategory :: BillingCategory
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data OneShotDriverDetails = OneShotDriverDetails
  { name :: Text,
    mobileCountryCode :: Maybe Text,
    mobileNumber :: Text,
    rating :: Maybe Centesimal,
    registeredAt :: Maybe UTCTime,
    image :: Maybe Text,
    isDriverBirthDay :: Bool,
    accountId :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data OneShotVehicleDetails = OneShotVehicleDetails
  { number :: Text,
    color :: Maybe Text,
    model :: Maybe Text,
    variant :: DVeh.VehicleVariant,
    serviceTierType :: DVST.ServiceTierType,
    serviceTierName :: Maybe Text,
    vehicleAge :: Maybe Months
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data OneShotFareBreakupItem = OneShotFareBreakupItem
  { title :: Text,
    amount :: HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

oneShotAssign :: Maybe Text -> OneShotAssignReq -> Flow APISuccess
oneShotAssign apiKey req = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  -- Waiting lock, not a skip-if-held one: a racing duplicate delivery (BPP timeout
  -- retry while the first attempt is still processing) must block until the first
  -- attempt finishes and then land on the idempotency ladder below — a skipped
  -- no-op answering Success would make the BPP stop retrying a callback that did
  -- nothing, and an error answer would make it cancel a ride the first attempt may
  -- be successfully creating (its retry wrapper only retries connection errors).
  -- args: lock TTL 60s (must outlive processing), wait budget 30s (a duplicate
  -- waits this long before giving up; the final check below then answers an error).
  Redis.withWaitOnLockRedisWithExpiry (oneShotAssignLockKey req.transactionId) 60 30 $ do
    -- Master reads throughout: these are read-your-own-write checks on a row this
    -- same flow just created; a stale replica answer here would fail a successful
    -- assignment and trigger the BPP's cancel path.
    mbExistingRide <- B.runInMasterDbAndRedis $ QRide.findByBPPRideId (Id req.bppRideId)
    case mbExistingRide of
      Just _ -> logInfo $ "One-shot assign: ride already exists for bppRideId " <> req.bppRideId <> ", idempotent replay ignored"
      Nothing -> do
        result <- withTryCatch "oneShotProcessAssignment" $ processAssignment req
        case result of
          Right _ -> pure ()
          Left err -> do
            -- A late failure after ride creation (e.g. a post-create side effect in
            -- assignRideUpdate threw) must not surface as a callback failure — the
            -- assignment is real, and an error answer would make the BPP cancel it.
            mbRideAfter <- B.runInMasterDbAndRedis $ QRide.findByBPPRideId (Id req.bppRideId)
            case mbRideAfter of
              Just _ -> logError $ "One-shot assign: late failure after ride creation for bppRideId " <> req.bppRideId <> ", treating as success: " <> show err
              Nothing -> throwM err
  -- Success must mean the ride actually exists (e.g. the waiting lock timed out
  -- without processing); otherwise answer an error so the BPP falls into its
  -- cancel-and-expire path instead of believing the callback was delivered.
  finalRide <- B.runInMasterDbAndRedis $ QRide.findByBPPRideId (Id req.bppRideId)
  when (isNothing finalRide) $
    throwError $ InternalError $ "One-shot assign not completed for bppRideId " <> req.bppRideId
  pure Success

oneShotAssignLockKey :: Text -> Text
oneShotAssignLockKey transactionId = "Customer:OneShotAssign:TxnId-" <> transactionId

processAssignment :: OneShotAssignReq -> Flow ()
processAssignment req = do
  now <- getCurrentTime
  estimate <- QEstimate.findByBPPEstimateId (Id req.bppEstimateId) >>= fromMaybeM (EstimateDoesNotExist $ "bppEstimateId-" <> req.bppEstimateId)
  searchRequest <- QSRLite.findByIdLite estimate.requestId >>= fromMaybeM (SearchRequestDoesNotExist estimate.requestId.getId)
  person <- QPerson.findById searchRequest.riderId >>= fromMaybeM (PersonNotFound searchRequest.riderId.getId)
  merchant <- CQM.findById searchRequest.merchantId >>= fromMaybeM (MerchantNotFound searchRequest.merchantId.getId)
  -- One-shot skips init/on_confirm, where online (Stripe) payment parameters are
  -- exchanged, so it must not be used for online-payment merchants — failing here makes
  -- the BPP cancel and the customer's search expire instead of silently breaking payment.
  when merchant.onlinePayment $
    throwError $ InvalidRequest "One-shot assignment is not supported for online-payment merchants"
  -- Resume-safe: a previous attempt may have crashed after booking creation; the BPP
  -- retry must continue from the ride step instead of failing on the duplicate guard.
  mbExistingBooking <- QRideB.findByBPPBookingId (Id req.bppBookingId)
  booking <- case mbExistingBooking of
    Just existingBooking -> pure existingBooking
    Nothing -> do
      -- Same mutual exclusion the legacy auto-assign/UI-confirm pair uses.
      isLockAcquired <- SConfirm.tryInitTriggerLock searchRequest.id
      unless isLockAcquired $
        throwError $ InvalidRequest $ "Booking creation lock already held for searchRequestId " <> searchRequest.id.getId
      quote' <- DOnSelect.buildSelectedQuote estimate (mkProviderInfo estimate) now searchRequest (mkQuoteInfo estimate)
      -- The BPP priced this ride without any BAP-side offer, so applying one here would
      -- make the customer's fare diverge from the driver's. Offers stay off for one-shot
      -- until the discount is carried in the round trip.
      let quote = quote' {DQuote.selectedOfferId = Nothing}
      triggerQuoteEvent QuoteEventData {quote = quote, person = person, merchantId = searchRequest.merchantId}
      QQuote.createMany [quote]
      dConfirmRes <-
        SConfirm.confirm
          SConfirm.DConfirmReq
            { personId = person.id,
              quote = quote,
              dashboardAgentId = Nothing,
              paymentMethodId = searchRequest.selectedPaymentMethodId,
              paymentInstrument = searchRequest.selectedPaymentInstrument,
              merchant = merchant,
              requiresPaymentBeforeConfirm = False,
              mbOneShotDetails =
                Just
                  SConfirm.OneShotConfirmDetails
                    { bppBookingId = Id req.bppBookingId,
                      commission = req.commission,
                      paymentCharge = req.paymentCharge,
                      paymentChargeBearer = req.paymentChargeBearer
                    }
            }
      pure dConfirmRes.booking
  -- Fare breakup rows are on_init's job in the legacy relay. Runs on the resume path
  -- too; the underlying FareBreakupInfo upsert replaces rather than appends, so this
  -- is idempotent.
  DOnInit.createFareBreakup booking dFareBreakups
  DCommon.rideAssignedReqHandler (mkValidatedRideAssignedReq booking)
  logInfo $ "One-shot assign completed for booking " <> booking.id.getId <> ", bppRideId " <> req.bppRideId
  where
    dFareBreakups =
      req.fareBreakups <&> \item ->
        DCommon.DFareBreakup
          { amount = mkPrice (Just req.currency) item.amount,
            description = item.title
          }
    mkProviderInfo estimate =
      DOnSelect.ProviderInfo
        { providerId = estimate.providerId,
          name = Nothing,
          url = estimate.providerUrl,
          mobileNumber = Nothing
        }
    mkQuoteInfo estimate =
      DOnSelect.QuoteInfo
        { vehicleVariant = req.vehicleDetails.variant,
          estimatedFare = mkPrice (Just req.currency) req.estimatedFare,
          discount = Nothing,
          quoteDetails =
            DOnSelect.DriverOfferQuoteDetails
              { driverName = req.driverDetails.name,
                durationToPickup = (.getSeconds) <$> req.durationToPickup,
                distanceToPickup = metersToHighPrecMeters <$> req.distanceToPickup,
                validTill = req.quoteValidTill,
                rating = req.driverDetails.rating,
                isUpgradedToCab = Just False, -- upgrades are excluded from one-shot on the BPP
                bppDriverQuoteId = req.bppQuoteId,
                isSafetyPlus = req.isSafetyPlus,
                driverSelectedFare = Nothing
              },
          specialLocationTag = req.specialLocationTag,
          serviceTierName = req.vehicleDetails.serviceTierName,
          serviceTierType = Just req.vehicleDetails.serviceTierType,
          serviceTierShortDesc = Nothing,
          isCustomerPrefferedSearchRoute = Nothing,
          isBlockedRoute = Nothing,
          quoteValidTill = req.quoteValidTill,
          billingCategory = req.billingCategory,
          tripCategory = fromMaybe (OneWay OneWayOnDemandDynamicOffer) estimate.tripCategory,
          quoteBreakupList = mkQuoteBreakupInfo <$> req.fareBreakups
        }
    mkQuoteBreakupInfo item =
      DOnSearch.QuoteBreakupInfo
        { title = item.title,
          price = DOnSearch.BreakupPriceInfo {value = mkPrice (Just req.currency) item.amount}
        }
    mkValidatedRideAssignedReq booking =
      DCommon.ValidatedRideAssignedReq
        { bookingDetails =
            DCommon.BookingDetails
              { bppBookingId = Id req.bppBookingId,
                bppRideId = Id req.bppRideId,
                driverName = req.driverDetails.name,
                driverImage = req.driverDetails.image,
                driverMobileNumber = req.driverDetails.mobileNumber,
                driverAlternatePhoneNumber = Nothing,
                driverMobileCountryCode = req.driverDetails.mobileCountryCode,
                driverRating = req.driverDetails.rating,
                driverRegisteredAt = req.driverDetails.registeredAt,
                vehicleNumber = req.vehicleDetails.number,
                vehicleColor = req.vehicleDetails.color,
                vehicleModel = fromMaybe "" req.vehicleDetails.model,
                otp = req.otp,
                isInitiatedByCronJob = False,
                isTierUpgrade = req.isTierUpgrade,
                assignedServiceTierName = req.assignedServiceTierName
              },
          isDriverBirthDay = req.driverDetails.isDriverBirthDay,
          isFreeRide = req.isFreeRide,
          vehicleAge = req.vehicleDetails.vehicleAge,
          onlinePaymentParameters = Nothing, -- online payment is out of one-shot scope (phase 1)
          driverAccountId = Nothing,
          previousRideEndPos = req.previousRideEndPos,
          booking = booking,
          bppUri = Nothing, -- booking.providerUrl was set from the quote at build time
          fareBreakups = Just dFareBreakups,
          driverTrackingUrl = Nothing,
          isAlreadyFav = req.isAlreadyFav,
          favCount = req.favCount,
          isSafetyPlus = req.isSafetyPlus,
          isSynchronousOnUpdateProcessing = True,
          bppInvoiceProviderFields = QRideB.BPPInvoiceProviderFields Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing,
          bookingPrePersisted = True
        }
