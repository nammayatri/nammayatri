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
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.RideBookingCancellationReason as SBCR
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideBookingCancellationReason as QBCR
import qualified Storage.Queries.SearchRequest as MC
import Types.API.Cancel as API
import Types.Error
import Utils.Common

cancel :: Id SRB.RideBooking -> Id Person.Person -> API.CancelReq -> FlowHandler CancelRes
cancel bookingId personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let bookingCancellationReasonAPI = req.bookingCancellationReason
  rideBooking <- QRB.findById bookingId >>= fromMaybeM (RideBookingDoesNotExist bookingId.getId)
  searchRequest <- MC.findByPersonId personId (rideBooking.requestId) >>= fromMaybeM (SearchRequestNotFound rideBooking.requestId.getId)
  canCancelRideBooking <- isRideBookingCancellable rideBooking
  unless canCancelRideBooking $
    throwError $ RideInvalidStatus "Cannot cancel this ride"
  let txnId = getId $ searchRequest.id
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.CANCEL txnId bapIDs.cabs bapURIs.cabs (Just rideBooking.providerId) (Just rideBooking.providerUrl)

  when (rideBooking.status == SRB.NEW) $ throwError (RideBookingInvalidStatus "NEW")
  bppOrderId <- fromMaybeM (RideBookingFieldNotPresent "bppBookingId") rideBooking.bppBookingId
  void $ ExternalAPI.cancel rideBooking.providerUrl (Common.BecknReq context (ReqCancel.CancelReqMessage bppOrderId.getId ReqCancel.ByUser))

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

isRideBookingCancellable :: EsqDBFlow m r => SRB.RideBooking -> m Bool
isRideBookingCancellable rideBooking
  | rideBooking.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT] = pure True
  | rideBooking.status == SRB.TRIP_ASSIGNED = do
    ride <- QR.findActiveByRBId rideBooking.id >>= fromMaybeM (RideDoesNotExist rideBooking.id.getId)
    pure (ride.status == Ride.NEW)
  | otherwise = pure False
