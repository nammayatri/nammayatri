module Product.Cancel (cancel, onCancel) where

import App.Types
import Beckn.Product.Validation.Context (validateContext)
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.ReqTypes as Common
import qualified Beckn.Types.Core.Taxi.API.OnCancel as OnCancel
import qualified Beckn.Types.Core.Taxi.Cancel as Cancel
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideCancellationReason as QRCR
import qualified Storage.Queries.SearchRequest as MC
import Types.API.Cancel as API
import Types.Error
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.RideCancellationReason as SRCR
import Utils.Common
import qualified Utils.Notifications as Notify

cancel :: Id SRB.RideBooking -> Id Person.Person -> API.CancelReq -> FlowHandler CancelRes
cancel bookingId personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  rideCancellationReasonAPI <- req.rideCancellationReason & fromMaybeM (InvalidRequest "Cancellation reason is not present.")
  rideBooking <- QRB.findById bookingId >>= fromMaybeM RideBookingDoesNotExist
  let quoteId = rideBooking.quoteId
  quote <- QQuote.findById quoteId >>= fromMaybeM QuoteDoesNotExist -- TODO: Handle usecase where multiple productinstances exists for one product
  searchRequest <- MC.findByPersonId personId (quote.requestId) >>= fromMaybeM SearchRequestNotFound
  unless (isRideBookingCancellable rideBooking) $
    throwError $ RideInvalidStatus "Cannot cancel this ride"
  let txnId = getId $ searchRequest.id
  organization <-
    OQ.findOrganizationById (quote.providerId)
      >>= fromMaybeM OrgNotFound
  bapURIs <- asks (.bapSelfURIs)
  bppURI <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  context <- buildTaxiContext txnId bapURIs.cabs (Just bppURI)
  ExternalAPI.cancel bppURI (Common.BecknReq context (Cancel.CancelMessage quote.bppQuoteId.getId Cancel.ByUser))
  DB.runSqlDBTransaction $
    QRCR.create $ makeRideCancelationReason rideBooking.id rideCancellationReasonAPI
  return Success
  where
    makeRideCancelationReason rideBookingId rideCancellationReasonAPI = do
      let RideCancellationReasonAPIEntity {..} = rideCancellationReasonAPI
      SRCR.RideCancellationReason
        { rideBookingId = rideBookingId,
          source = Cancel.ByUser,
          reasonCode = Just reasonCode,
          additionalInfo = additionalInfo
        }

isRideBookingCancellable :: SRB.RideBooking -> Bool
isRideBookingCancellable ride =
  ride.status `elem` [SRB.CONFIRMED, SRB.TRIP_ASSIGNED]

onCancel ::
  SignatureAuthResult ->
  OnCancel.OnCancelReq ->
  FlowHandler AckResponse
onCancel _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext req.context
    case req.contents of
      Right msg -> do
        let bppRideBookingId = Id $ msg.order.id
        -- TODO: Handle usecase where multiple productinstances exists for one product

        rideBooking <- QRB.findByBPPBookingId bppRideBookingId >>= fromMaybeM RideBookingDoesNotExist
        unless (isRideBookingCancellable rideBooking) $
          throwError (RideBookingInvalidStatus (show rideBooking.status))
        mbRide <- QRide.findByRBId rideBooking.id
        let cancellationSource = msg.cancellation_reason_id
        quote <- QQuote.findById rideBooking.quoteId >>= fromMaybeM QuoteDoesNotExist
        let searchRequestId = quote.requestId
        logTagInfo ("txnId-" <> getId searchRequestId) ("Cancellation reason " <> show cancellationSource)
        DB.runSqlDBTransaction $ do
          QRB.updateStatus rideBooking.id SRB.CANCELLED
          whenJust mbRide $ \ride -> QRide.updateStatus ride.id Ride.CANCELLED
          unless (cancellationSource == Cancel.ByUser) $
            QRCR.create $ SRCR.RideCancellationReason rideBooking.id cancellationSource Nothing Nothing
        -- notify customer
        mbPerson <- Person.findById rideBooking.requestorId
        whenJust mbPerson $ \person -> Notify.notifyOnCancel rideBooking person.id person.deviceToken cancellationSource
      Left err -> logTagError "on_cancel req" $ "on_cancel error: " <> show err
    return Ack
