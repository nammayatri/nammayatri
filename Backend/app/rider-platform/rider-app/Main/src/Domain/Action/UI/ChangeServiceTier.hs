module Domain.Action.UI.ChangeServiceTier
  ( getChangeServiceTierQuotes,
    postChangeServiceTierConfirm,
    ChangeServiceTierConfirmReq (..),
  )
where

import qualified Beckn.ACL.Update as ACL
import qualified Domain.Action.UI.Quote as QuoteAction
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingStatus as DBS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Quote as DQuote
import Domain.Types.Trip (isRideOtpTrip)
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Quote as QQuote
import Tools.Error

newtype ChangeServiceTierConfirmReq = ChangeServiceTierConfirmReq
  { quoteId :: Id DQuote.Quote
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | Returns quotes from the original search that have higher fare than current booking,
-- in the same format as search/{searchId}/results — with estimates and journeys empty.
getChangeServiceTierQuotes ::
  (Id DP.Person, Id DM.Merchant) ->
  Id DBooking.Booking ->
  Flow QuoteAction.GetQuotesRes
getChangeServiceTierQuotes (_personId, _merchantId) bookingId = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.status == DBS.NEW || booking.status == DBS.CONFIRMED) $
    throwError $ ChangeServiceTierInvalidStatus (show booking.status)
  unless (maybe False isRideOtpTrip booking.tripCategory) $
    throwError ChangeServiceTierNotSupported

  -- Get the original search request ID via the booking's quote
  quoteId <- booking.quoteId & fromMaybeM (ChangeServiceTierOriginalQuoteNotFound bookingId.getId)
  currentQuote <- QQuote.findById quoteId >>= fromMaybeM (ChangeServiceTierOriginalQuoteNotFound bookingId.getId)
  let searchRequestId = currentQuote.requestId

  -- Reuse existing search results logic, then filter
  searchResults <- QuoteAction.getQuotes searchRequestId (Just True)

  -- Filter quotes: different tier
  let notSameTierQuote = \case
        QuoteAction.OnDemandCab q -> q.vehicleVariant /= booking.vehicleServiceTierType
        QuoteAction.OnRentalCab q -> q.vehicleVariant /= booking.vehicleServiceTierType
        QuoteAction.OnMeterRide q -> q.vehicleVariant /= booking.vehicleServiceTierType
        _ -> False

  pure
    searchResults
      { QuoteAction.quotes = filter notSameTierQuote searchResults.quotes,
        QuoteAction.estimates = [],
        QuoteAction.journey = Nothing
      }

-- | Confirms the tier change by sending a BECKN update to BPP.
postChangeServiceTierConfirm ::
  (Id DP.Person, Id DM.Merchant) ->
  Id DBooking.Booking ->
  ChangeServiceTierConfirmReq ->
  Flow APISuccess
postChangeServiceTierConfirm (_personId, merchantId) bookingId req = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.status == DBS.NEW || booking.status == DBS.CONFIRMED) $
    throwError $ ChangeServiceTierInvalidStatus (show booking.status)
  unless (maybe False isRideOtpTrip booking.tripCategory) $
    throwError ChangeServiceTierNotSupported

  newQuote <- QQuote.findById req.quoteId >>= fromMaybeM (InvalidRequest $ "Quote not found: " <> req.quoteId.getId)
  when (newQuote.vehicleServiceTierType == booking.vehicleServiceTierType) $
    throwError ChangeServiceTierSameTier

  bppBookingId <- booking.bppBookingId & fromMaybeM (InvalidRequest "BPP booking ID not found")
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  msgId <- generateGUID

  let updateBuildReq =
        ACL.UpdateBuildReq
          { bppBookingId = bppBookingId,
            bppId = booking.providerId,
            bppUrl = booking.providerUrl,
            transactionId = booking.transactionId,
            merchant = merchant,
            messageId = msgId,
            city = merchant.defaultCity, -- TODO: Correct during interoperability
            details =
              ACL.UChangeServiceTierBuildReqDetails
                ACL.ChangeServiceTierBuildReqDetails
                  { newVehicleServiceTier = newQuote.vehicleServiceTierType,
                    bppQuoteId = newQuote.itemId
                  }
          }
  -- BAP booking update happens in on_update handler after BPP confirms the change.
  -- No optimistic update here to avoid inconsistency if BPP rejects.
  updateMsg <- ACL.buildUpdateReq updateBuildReq
  void $ CallBPP.updateV2 booking.providerUrl updateMsg
  pure Success
