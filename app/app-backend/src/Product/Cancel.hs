module Product.Cancel (cancel) where

import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.Core.ReqTypes as Common
import qualified Beckn.Types.Core.Taxi.Cancel.Req as ReqCancel
import qualified Beckn.Types.Core.Taxi.Common.Context as Context
import Beckn.Types.Id
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.RideBookingCancellationReason as SBCR
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideBookingCancellationReason as QBCR
import qualified Storage.Queries.SearchRequest as MC
import Types.API.Cancel as API
import Types.Error
import Utils.Common

cancel :: Id SRB.RideBooking -> Id Person.Person -> API.CancelReq -> FlowHandler CancelRes
cancel bookingId personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let bookingCancellationReasonAPI = req.bookingCancellationReason
  rideBooking <- QRB.findById bookingId >>= fromMaybeM RideBookingDoesNotExist
  let quoteId = rideBooking.quoteId
  quote <- QQuote.findById quoteId >>= fromMaybeM QuoteDoesNotExist -- TODO: Handle usecase where multiple productinstances exists for one product
  searchRequest <- MC.findByPersonId personId (quote.requestId) >>= fromMaybeM SearchRequestNotFound
  unless (isRideBookingCancellable rideBooking) $
    throwError $ RideInvalidStatus "Cannot cancel this ride"
  let txnId = getId $ searchRequest.id
  bapURIs <- askConfig (.bapSelfURIs)
  bapIDs <- askConfig (.bapSelfIds)
  context <- buildTaxiContext Context.CANCEL txnId bapIDs.cabs bapURIs.cabs (Just quote.providerId) (Just quote.providerUrl)
  void $ ExternalAPI.cancel quote.providerUrl (Common.BecknReq context (ReqCancel.CancelReqMessage quote.bppQuoteId.getId ReqCancel.ByUser))
  rideBookingCancelationReason <- buildRideBookingCancelationReason rideBooking.id bookingCancellationReasonAPI
  DB.runTransaction
    (QBCR.create rideBookingCancelationReason)
    `rethrow` \(SQLRequestError _ _) -> RideInvalidStatus "This ride is already cancelled"
  return Success
  where
    buildRideBookingCancelationReason rideBookingId bookingCancellationReasonAPI = do
      let RideBookingCancellationReasonAPIEntity {..} = bookingCancellationReasonAPI
      id <- generateGUID
      return $
        SBCR.RideBookingCancellationReason
          { rideBookingId = rideBookingId,
            rideId = Nothing,
            source = ReqCancel.ByUser,
            reasonCode = Just reasonCode,
            reasonStage = Just reasonStage,
            additionalInfo = additionalInfo,
            ..
          }

isRideBookingCancellable :: SRB.RideBooking -> Bool
isRideBookingCancellable ride =
  ride.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT, SRB.TRIP_ASSIGNED]
