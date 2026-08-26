module Domain.Action.UI.Aarokya where

import qualified Data.Aeson as A
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Kernel.External.PartnerSdk.Interface.Types as PartnerSdk
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import qualified Tools.PartnerSdk as TPartnerSdk

newtype AarokyaContributorTokenReq = AarokyaContributorTokenReq
  { rideId :: Id DRide.Ride
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- | Mint an Aarokya contributor token for the driver of a given ride.
--
-- The caller is the customer (authenticated via their own token). The
-- beneficiary on Aarokya is always a driver, so @beneficiary_identifier@ is the
-- driver's phone number, resolved server-side from the ride (never trusted from
-- the client). The ride must belong to the calling customer.
--
-- The response is Aarokya's raw JSON body, forwarded verbatim: this endpoint is
-- a pass-through proxy for the contributor token and does not re-shape it.
generateContributorToken ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    MonadFlow m
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  AarokyaContributorTokenReq ->
  m A.Value
generateContributorToken (personId, _merchantId) req = do
  ride <- QRide.findById req.rideId >>= fromMaybeM (RideDoesNotExist req.rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  -- Authorisation: only the customer who took the ride may mint a token for its driver.
  unless (booking.riderId == personId) $ throwError (InvalidRequest "Ride does not belong to the requester")
  -- contribution_channel / contributor_ref are traceability metadata stamped into
  -- the issued JWT by Aarokya. Per aarokya PR #465: the channel is the enum value
  -- CUSTOMER_APP, and the ref is a tagged object identifying the contributor —
  -- here the customer, i.e. { "type": "CUSTOMER_ID", "value": <customerId> }.
  let sdkReq =
        PartnerSdk.GenerateContributorTokenReq
          { beneficiaryIdentifier = ride.driverMobileNumber,
            contributionChannel = Just "CUSTOMER_APP",
            contributorRef = Just (PartnerSdk.ContributorRef {refType = "CUSTOMER_ID", refValue = personId.getId})
          }
  -- Config is resolved for the ride's merchant + operating city (booking is the
  -- authoritative source; its merchantOperatingCityId is always present).
  TPartnerSdk.generateContributorToken booking.merchantId booking.merchantOperatingCityId sdkReq
