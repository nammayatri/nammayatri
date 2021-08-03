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
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideCancellationReason as QRCR
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as MC
import Types.API.Cancel as Cancel
import Types.Error
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.OldRide as Ride
import qualified Types.Storage.RideCancellationReason as SRCR
import Utils.Common
import qualified Utils.Notifications as Notify

cancel :: Id Ride.Ride -> Id Person.Person -> Cancel.CancelReq -> FlowHandler CancelRes
cancel bookingId personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  rideCancellationReasonAPI <- req.rideCancellationReason & fromMaybeM (InvalidRequest "Cancellation reason is not present.")
  ride <- QRide.findById bookingId >>= fromMaybeM RideDoesNotExist
  let quoteId = ride.quoteId
  quote <- QQuote.findById quoteId >>= fromMaybeM QuoteDoesNotExist -- TODO: Handle usecase where multiple productinstances exists for one product
  searchRequest <- MC.findByPersonId personId (quote.requestId) >>= fromMaybeM SearchRequestNotFound
  unless (isRideCancellable ride) $
    throwError $ RideInvalidStatus "Cannot cancel this ride"
  let txnId = getId $ searchRequest.id
  let cancelReqMessage = API.CancelReqMessage (API.CancellationOrder (getId quoteId) Nothing)
  context <- buildContext "cancel" txnId Nothing Nothing
  organization <-
    OQ.findOrganizationById (quote.organizationId)
      >>= fromMaybeM OrgNotFound
  baseUrl <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  ExternalAPI.cancel baseUrl (API.CancelReq context cancelReqMessage)
  DB.runSqlDBTransaction $
    QRCR.create $ makeRideCancelationReason ride.id rideCancellationReasonAPI
  return Success
  where
    makeRideCancelationReason rideId rideCancellationReasonAPI = do
      let RideCancellationReasonAPIEntity {..} = rideCancellationReasonAPI
      SRCR.RideCancellationReason
        { rideId = rideId,
          source = ByUser,
          reasonCode = Just reasonCode,
          additionalInfo = additionalInfo
        }

isRideCancellable :: Ride.Ride -> Bool
isRideCancellable ride =
  ride.status `elem` [Ride.NEW, Ride.INSTOCK, Ride.CONFIRMED, Ride.TRIP_ASSIGNED]

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

        mbRide <- QRide.findByQuoteId quoteId

        whenJust mbRide $ \ride -> do
          -- TODO what if we update several PI but then get an error?
          -- wrap everything in a transaction
          unless (ride.status `elem` [Ride.NEW, Ride.INSTOCK, Ride.CONFIRMED, Ride.TRIP_ASSIGNED]) $
            throwError (RideInvalidStatus (show ride.status))
          DB.runSqlDBTransaction $
            QRide.updateStatus ride.id Ride.CANCELLED
        quote <- QQuote.findById quoteId >>= fromMaybeM QuoteDoesNotExist
        let searchRequestId = quote.requestId
        -- notify customer
        searchRequest <- MC.findById searchRequestId >>= fromMaybeM SearchRequestNotFound
        cancellationSource <- msg.order.cancellation_reason_id & fromMaybeM (InvalidRequest "No cancellation source.")
        logTagInfo ("txnId-" <> getId searchRequestId) ("Cancellation reason " <> show cancellationSource)
        mbPerson <- Person.findById searchRequest.requestorId
        whenJust mbPerson $ \person -> Notify.notifyOnCancel quote person.id person.deviceToken cancellationSource
        unless (cancellationSource == ByUser) $
          whenJust mbRide $ \ride ->
            DB.runSqlDBTransaction $
              QRCR.create $ SRCR.RideCancellationReason ride.id cancellationSource Nothing Nothing
      Left err -> logTagError "on_cancel req" $ "on_cancel error: " <> show err
    return Ack
