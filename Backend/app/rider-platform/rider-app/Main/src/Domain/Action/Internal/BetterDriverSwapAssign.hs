{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | "Find a better driver" swap: single internal callback from the BPP, sent once a
-- stand-by search (started on an already-active booking) finds a replacement driver.
-- Mirrors Domain.Action.Internal.OneShotAssign - same single-callback shape, same
-- resumable/idempotent design - but reuses the OLD booking's existing SearchRequest +
-- Estimate instead of requiring a fresh one (there is no BAP-initiated search for this
-- flow at all), and explicitly tells SharedLogic.Confirm.confirm that the rider already
-- having an active booking is expected, not an error.
module Domain.Action.Internal.BetterDriverSwapAssign where

import qualified Domain.Action.Beckn.Common as DCommon
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import qualified Domain.Action.Internal.OneShotAssign as OneShot
import Domain.Types
import Environment
import qualified Kernel.Beam.Functions as B
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
-- in dynamic-offer-driver-app SharedLogic.CallBAPInternal.BetterDriverSwapAssignReq.
data BetterDriverSwapAssignReq = BetterDriverSwapAssignReq
  { -- | The BPP-side booking id of the booking this swap is replacing - used to find
    -- that booking on this (BAP) side so it can be marked superseded.
    oldBppBookingId :: Text,
    bppEstimateId :: Text,
    bppQuoteId :: Text,
    bppBookingId :: Text,
    bppRideId :: Text,
    currency :: Currency,
    estimatedFare :: HighPrecMoney,
    commission :: Maybe HighPrecMoney,
    paymentCharge :: Maybe HighPrecMoney,
    paymentChargeBearer :: Maybe Text,
    fareBreakups :: [OneShot.OneShotFareBreakupItem],
    quoteValidTill :: UTCTime,
    otp :: Text,
    trackingUrl :: BaseUrl,
    driverDetails :: OneShot.OneShotDriverDetails,
    vehicleDetails :: OneShot.OneShotVehicleDetails,
    distanceToPickup :: Maybe Meters,
    durationToPickup :: Maybe Seconds,
    isAlreadyFav :: Bool,
    favCount :: Maybe Int,
    isSafetyPlus :: Bool,
    isFreeRide :: Bool,
    specialLocationTag :: Maybe Text,
    assignedServiceTierName :: Maybe Text,
    billingCategory :: BillingCategory
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

betterDriverSwapAssign :: Maybe Text -> BetterDriverSwapAssignReq -> Flow APISuccess
betterDriverSwapAssign apiKey req = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  -- Same waiting-lock reasoning as OneShotAssign.oneShotAssign: a racing duplicate
  -- delivery must block until the first attempt finishes, not race it.
  Redis.withWaitOnLockRedisWithExpiry (betterDriverSwapAssignLockKey req.bppBookingId) 60 30 $ do
    mbExistingRide <- B.runInMasterDbAndRedis $ QRide.findByBPPRideId (Id req.bppRideId)
    case mbExistingRide of
      Just _ -> logInfo $ "Better-driver-swap assign: ride already exists for bppRideId " <> req.bppRideId <> ", idempotent replay ignored"
      Nothing -> do
        result <- withTryCatch "betterDriverSwapProcessAssignment" $ processAssignment req
        case result of
          Right _ -> pure ()
          Left err -> do
            mbRideAfter <- B.runInMasterDbAndRedis $ QRide.findByBPPRideId (Id req.bppRideId)
            case mbRideAfter of
              Just _ -> logError $ "Better-driver-swap assign: late failure after ride creation for bppRideId " <> req.bppRideId <> ", treating as success: " <> show err
              Nothing -> throwM err
  finalRide <- B.runInMasterDbAndRedis $ QRide.findByBPPRideId (Id req.bppRideId)
  when (isNothing finalRide) $
    throwError $ InternalError $ "Better-driver-swap assign not completed for bppRideId " <> req.bppRideId
  pure Success

betterDriverSwapAssignLockKey :: Text -> Text
betterDriverSwapAssignLockKey bppBookingId = "Customer:BetterDriverSwapAssign:BppBookingId-" <> bppBookingId

processAssignment :: BetterDriverSwapAssignReq -> Flow ()
processAssignment req = do
  now <- getCurrentTime
  oldBooking <- QRideB.findByBPPBookingId (Id req.oldBppBookingId) >>= fromMaybeM (BookingDoesNotExist $ "oldBppBookingId-" <> req.oldBppBookingId)
  estimate <- QEstimate.findByBPPEstimateId (Id req.bppEstimateId) >>= fromMaybeM (EstimateDoesNotExist $ "bppEstimateId-" <> req.bppEstimateId)
  searchRequest <- QSRLite.findByIdLite estimate.requestId >>= fromMaybeM (SearchRequestDoesNotExist estimate.requestId.getId)
  person <- QPerson.findById searchRequest.riderId >>= fromMaybeM (PersonNotFound searchRequest.riderId.getId)
  merchant <- CQM.findById searchRequest.merchantId >>= fromMaybeM (MerchantNotFound searchRequest.merchantId.getId)
  when merchant.onlinePayment $
    throwError $ InvalidRequest "Better-driver-swap assignment is not supported for online-payment merchants"
  mbExistingBooking <- QRideB.findByBPPBookingId (Id req.bppBookingId)
  booking <- case mbExistingBooking of
    Just existingBooking -> pure existingBooking
    Nothing -> do
      quote <- DOnSelect.buildSelectedQuote estimate (mkProviderInfo estimate) now searchRequest (mkQuoteInfo estimate)
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
              supportsBookingDeposit = Nothing,
              mbBetterDriverSwapDetails = Just SConfirm.BetterDriverSwapDetails {oldBookingId = oldBooking.id},
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
  DOnInit.createFareBreakup booking dFareBreakups
  DCommon.rideAssignedReqHandler (mkValidatedRideAssignedReq booking)
  logInfo $ "Better-driver-swap assign completed for booking " <> booking.id.getId <> ", bppRideId " <> req.bppRideId <> ", superseding old booking " <> oldBooking.id.getId
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
                isUpgradedToCab = Just False,
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
                assignedVehicleVariant = Just req.vehicleDetails.variant,
                otp = req.otp,
                isInitiatedByCronJob = False,
                isBetterDriverSwap = True,
                isTierUpgrade = False,
                assignedServiceTierName = req.assignedServiceTierName
              },
          isDriverBirthDay = req.driverDetails.isDriverBirthDay,
          isFreeRide = req.isFreeRide,
          vehicleAge = req.vehicleDetails.vehicleAge,
          onlinePaymentParameters = Nothing,
          driverAccountId = Nothing,
          previousRideEndPos = Nothing,
          booking = booking,
          bppUri = Nothing,
          fareBreakups = Just dFareBreakups,
          driverTrackingUrl = Just req.trackingUrl,
          isAlreadyFav = req.isAlreadyFav,
          favCount = req.favCount,
          isSafetyPlus = req.isSafetyPlus,
          isSynchronousOnUpdateProcessing = True,
          bppInvoiceProviderFields = QRideB.BPPInvoiceProviderFields Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing,
          bookingPrePersisted = True
        }
