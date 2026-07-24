module Domain.Action.UI.Aarokya where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Kernel.External.PartnerSdk.Interface.Types as PartnerSdk
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Tools.PartnerSdk as TPartnerSdk

newtype AarokyaContributorTokenReq = AarokyaContributorTokenReq
  { rideId :: Id DRide.Ride
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype AarokyaContributorTokenRes = AarokyaContributorTokenRes
  { contributorToken :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- | Mint an Aarokya contributor token for the driver of a given ride.
--
-- The caller is the customer (authenticated via their own token). The
-- beneficiary on Aarokya is always a driver, so @beneficiary_identifier@ is the
-- driver's phone number, resolved server-side from the ride (never trusted from
-- the client). The ride must belong to the calling customer.
generateContributorToken ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    MonadFlow m
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  AarokyaContributorTokenReq ->
  m AarokyaContributorTokenRes
generateContributorToken (personId, merchantId) req = do
  ride <- QRide.findById req.rideId >>= fromMaybeM (RideDoesNotExist req.rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  -- Authorisation: only the customer who took the ride may mint a token for its driver.
  unless (booking.riderId == personId) $ throwError (InvalidRequest "Ride does not belong to the requester")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
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
  res <- TPartnerSdk.generateContributorToken merchantId person.merchantOperatingCityId sdkReq
  pure $ AarokyaContributorTokenRes {contributorToken = res.contributorToken}
