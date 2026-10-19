module Domain.Action.UI.RideEvidence
  ( getRideEvidence,
    getRideEvidenceFile,
  )
where

import qualified API.Types.UI.RideEvidence
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import Tools.Error

getRideEvidence ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.Flow [API.Types.UI.RideEvidence.RideEvidenceItem]
  )
getRideEvidence (mbPersonId, merchantId) rideId = do
  (ride, merchant) <- findOwnRideAndMerchant mbPersonId merchantId rideId
  items <- CallBPPInternal.getRideEvidence merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId
  pure $
    map
      ( \item ->
          API.Types.UI.RideEvidence.RideEvidenceItem
            { id = item.evidenceId,
              description = item.evidenceDescription,
              rideStatus = item.evidenceRideStatus,
              createdAt = item.evidenceCreatedAt,
              thumbnailUrl = item.evidenceThumbnailUrl,
              imageUrl = item.evidenceImageUrl,
              urlsExpireAt = item.evidenceUrlsExpireAt
            }
      )
      items

getRideEvidenceFile ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Kernel.Prelude.Text ->
    Environment.Flow Kernel.Prelude.Text
  )
getRideEvidenceFile (mbPersonId, merchantId) rideId evidenceId = do
  (ride, merchant) <- findOwnRideAndMerchant mbPersonId merchantId rideId
  let fetchFile = CallBPPInternal.getRideEvidenceFile merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId evidenceId
  fetched <- withTryCatch "getRideEvidenceFile" fetchFile
  case fetched of
    Right content -> pure content
    Left _ -> do
      -- The driver service refuses a proof that is hidden, unknown or from another ride. Only after a failure is the visible list read, to answer such a proof as a plain not-found. Any other failure is tried again.
      visibleItems <- CallBPPInternal.getRideEvidence merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId
      if any (\item -> item.evidenceId == evidenceId) visibleItems
        then fetchFile
        else throwError (InvalidRequest $ "Evidence does not exist: " <> evidenceId)

-- A customer can only read proofs of a ride booked by their own account.
findOwnRideAndMerchant ::
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) ->
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
  Environment.Flow (Domain.Types.Ride.Ride, Domain.Types.Merchant.Merchant)
findOwnRideAndMerchant mbPersonId merchantId rideId = do
  personId <- fromMaybeM (PersonNotFound "person id not present in token") mbPersonId
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  unless (booking.riderId == personId) $ throwError (RideNotFound rideId.getId)
  merchant <- CQMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  pure (ride, merchant)
