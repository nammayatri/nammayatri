module Product.Cancel (cancel, onCancel) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.Core.API.Cancel as API
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Types.Mobility.Order (CancellationSource (..))
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
import Types.API.Cancel as Cancel
import Types.Error
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.RideCancellationReason as SRCR
import Utils.Common
import qualified Utils.Notifications as Notify

cancel :: Id SRB.RideBooking -> Id Person.Person -> Cancel.CancelReq -> FlowHandler CancelRes
cancel bookingId personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  rideCancellationReasonAPI <- req.rideCancellationReason & fromMaybeM (InvalidRequest "Cancellation reason is not present.")
  rideBooking <- QRB.findById bookingId >>= fromMaybeM RideBookingDoesNotExist
  let quoteId = rideBooking.quoteId
  quote <- QQuote.findById quoteId >>= fromMaybeM QuoteDoesNotExist -- TODO: Handle usecase where multiple productinstances exists for one product
  searchRequest <- MC.findByPersonId personId (quote.requestId) >>= fromMaybeM SearchRequestNotFound
  unless (isRideBookingCancellable rideBooking) $
    throwError $ RideInvalidStatus "Cannot cancel this ride"
  let txnId = getId $ searchRequest.id
  let cancelReqMessage = API.CancelReqMessage (API.CancellationOrder (getId quoteId) Nothing)
  context <- buildContext "cancel" txnId Nothing Nothing
  organization <-
    OQ.findOrganizationById (quote.providerId)
      >>= fromMaybeM OrgNotFound
  baseUrl <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  ExternalAPI.cancel baseUrl (API.CancelReq context cancelReqMessage)
  DB.runSqlDBTransaction $
    QRCR.create $ makeRideCancelationReason rideBooking.id rideCancellationReasonAPI
  return Success
  where
    makeRideCancelationReason rideBookingId rideCancellationReasonAPI = do
      let RideCancellationReasonAPIEntity {..} = rideCancellationReasonAPI
      SRCR.RideCancellationReason
        { rideBookingId = rideBookingId,
          source = ByUser,
          reasonCode = Just reasonCode,
          additionalInfo = additionalInfo
        }

isRideBookingCancellable :: SRB.RideBooking -> Bool
isRideBookingCancellable ride =
  ride.status `elem` [SRB.CONFIRMED, SRB.TRIP_ASSIGNED]

onCancel ::
  SignatureAuthResult Organization.Organization ->
  API.OnCancelReq ->
  FlowHandler API.OnCancelRes
onCancel _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "on_cancel" $ req.context
    case req.contents of
      Right msg -> do
        let quoteId = Id $ msg.order.id
        -- TODO: Handle usecase where multiple productinstances exists for one product

        rideBooking <- QRB.findByQuoteId quoteId >>= fromMaybeM RideBookingDoesNotExist
        unless (isRideBookingCancellable rideBooking) $
          throwError (RideBookingInvalidStatus (show rideBooking.status))
        mbRide <- QRide.findByRBId rideBooking.id
        cancellationSource <- msg.order.cancellation_reason_id & fromMaybeM (InvalidRequest "No cancellation source.")
        quote <- QQuote.findById quoteId >>= fromMaybeM QuoteDoesNotExist
        let searchRequestId = quote.requestId
        logTagInfo ("txnId-" <> getId searchRequestId) ("Cancellation reason " <> show cancellationSource)
        DB.runSqlDBTransaction $ do
          QRB.updateStatus rideBooking.id SRB.CANCELLED
          whenJust mbRide $ \ride -> QRide.updateStatus ride.id Ride.CANCELLED
          unless (cancellationSource == ByUser) $
            QRCR.create $ SRCR.RideCancellationReason rideBooking.id cancellationSource Nothing Nothing
        -- notify customer
        mbPerson <- Person.findById rideBooking.requestorId
        whenJust mbPerson $ \person -> Notify.notifyOnCancel rideBooking person.id person.deviceToken cancellationSource
      Left err -> logTagError "on_cancel req" $ "on_cancel error: " <> show err
    return Ack
