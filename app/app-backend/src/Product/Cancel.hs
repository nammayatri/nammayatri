module Product.Cancel (cancel) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.Core.ReqTypes as Common
import qualified Beckn.Types.Core.Taxi.Cancel.Req as ReqCancel
import Beckn.Types.Id
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideCancellationReason as QRCR
import qualified Storage.Queries.SearchRequest as MC
import Types.API.Cancel as API
import Types.Error
import qualified Types.Storage.Person as Person
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.RideCancellationReason as SRCR
import Utils.Common

cancel :: Id SRB.RideBooking -> Id Person.Person -> API.CancelReq -> FlowHandler CancelRes
cancel bookingId personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let rideCancellationReasonAPI = req.rideCancellationReason
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
  bapIDs <- asks (.bapSelfIds)
  bppUrl <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  context <- buildTaxiContext txnId bapIDs.cabs bapURIs.cabs (Just organization.shortId.getShortId) (Just bppUrl)
  void $ ExternalAPI.cancel bppUrl (Common.BecknReq context (ReqCancel.CancelReqMessage quote.bppQuoteId.getId ReqCancel.ByUser))
  DB.runSqlDBTransaction
    (QRCR.create $ makeRideCancelationReason rideBooking.id rideCancellationReasonAPI)
    `rethrow` \(SQLRequestError _ _) -> RideInvalidStatus "This ride is already cancelled"
  return Success
  where
    makeRideCancelationReason rideBookingId rideCancellationReasonAPI = do
      let RideCancellationReasonAPIEntity {..} = rideCancellationReasonAPI
      SRCR.RideCancellationReason
        { rideBookingId = rideBookingId,
          source = ReqCancel.ByUser,
          reasonCode = Just reasonCode,
          additionalInfo = additionalInfo
        }

isRideBookingCancellable :: SRB.RideBooking -> Bool
isRideBookingCancellable ride =
  ride.status `elem` [SRB.CONFIRMED, SRB.TRIP_ASSIGNED]
