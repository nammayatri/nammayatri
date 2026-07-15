{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.FRFSBooking
  ( FRFSBookingSearchReq (..),
    upsertRiderPersonByBadge,
    frfsBookingSearch,
    frfsBookingQuote,
    frfsBookingConfirm,
    frfsBookingStatus,
  )
where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketServiceAPI
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils (frfsVehicleCategoryToBecknVehicleCategory)
import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.IntegratedBPPConfig as QIBC
import qualified Storage.Queries.Person as Person
import Tools.Error

-- | Body of the driver-app (BPP) → rider-app (BAP) internal FRFS search call.
--   @feedKey@ is the stable GTFS feed identifier shared across both services;
--   the rider-app resolves its OWN IntegratedBPPConfig (and hence merchant /
--   operating-city) from it, so no driver-side UUIDs are ever trusted.
--   @operatorBadgeToken@ is the conductor's stable GIMS badge — the rider-app
--   keys a dedicated CONDUCTORTOKEN person on it (never a phone), so conductor
--   counter-sales are never merged with a customer account sharing a number.
data FRFSBookingSearchReq = FRFSBookingSearchReq
  { feedKey :: Text,
    vehicleType :: Spec.VehicleCategory,
    operatorBadgeToken :: Text,
    searchReq :: FRFSTicketServiceAPI.FRFSSearchAPIReq
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

-- | Resolve the dedicated rider-app person a conductor's bookings are attached to,
--   keyed on the conductor's operator badge token (GIMS badge). If a person already
--   exists for that badge it is reused; otherwise a phone-less @CONDUCTORTOKEN@
--   rider person is created (the same identity model used by conductor-token login).
--
--   Keying on the badge — not a phone — is deliberate: a phone-keyed person would
--   collide with a real customer sharing that number, attributing counter-sales to
--   the wrong account. @merchantId@ MUST be the rider-app (BAP) id derived from the
--   feedKey-resolved config, never a driver-app (BPP) id.
upsertRiderPersonByBadge ::
  Id DM.Merchant ->
  Text ->
  Flow (Id SP.Person)
upsertRiderPersonByBadge merchantId badgeToken = do
  merchant <- B.runInReplica $ CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  -- Serialize the lookup-then-create on the (merchant, badge) key so two concurrent conductor
  -- searches for the same badge cannot both miss and create duplicate persons: the second call
  -- waits for the lock and then reuses the person the first created.
  Redis.withLockRedisAndReturnValue conductorPersonLockKey 10 $
    Person.findByOperatorBadgeTokenAndMerchantId (Just badgeToken) merchantId
      >>= maybe (createConductorPerson merchant) (pure . (.id))
  where
    conductorPersonLockKey = "frfs:conductorPersonUpsert:" <> merchantId.getId <> ":" <> badgeToken
    createConductorPerson merchant = do
      person <-
        DReg.createPerson
          (buildAuthReq merchant)
          SP.CONDUCTORTOKEN
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          merchant
          Nothing
          (Just badgeToken)
      pure person.id
    buildAuthReq merchant =
      DReg.AuthReq
        { -- No phone is stored for a conductor person: it is only ever resolved by
          -- operatorBadgeToken, and a fabricated number would pollute the
          -- mobileNumberHash secondary key (risking collision with a real customer).
          -- The payment flow substitutes a throwaway phone at order-creation time.
          mobileNumber = Nothing,
          mobileCountryCode = Nothing,
          identifierType = Just SP.CONDUCTORTOKEN,
          merchantId = merchant.shortId,
          deviceToken = Nothing,
          notificationToken = Nothing,
          whatsappNotificationEnroll = Nothing,
          firstName = Nothing,
          middleName = Nothing,
          lastName = Nothing,
          email = Nothing,
          businessEmail = Nothing,
          language = Nothing,
          gender = Nothing,
          otpChannel = Nothing,
          registrationLat = Nothing,
          registrationLon = Nothing,
          enableOtpLessRide = Nothing,
          allowBlockedUserLogin = Nothing,
          isOperatorReq = Nothing,
          reuseToken = Nothing,
          operatorBadgeToken = Just badgeToken,
          deviceSerialNumber = Nothing,
          vehicleType = Nothing
        }

-- | Internal FRFS search: resolve the rider-app config from feedKey, upsert the
--   conductor (by badge) as the rider, and delegate to the standard search handler.
--   The resulting @FRFSSearch@ is stamped with the conductor's riderId, so the
--   subsequent quote / confirm / status calls need only the entity ids.
frfsBookingSearch :: FRFSBookingSearchReq -> Flow FRFSTicketServiceAPI.FRFSSearchAPIRes
frfsBookingSearch req = do
  let vehicleCategory = frfsVehicleCategoryToBecknVehicleCategory req.vehicleType
  -- feedKey is the cross-service anchor, but the rider-side config's platformType varies by
  -- city seed (e.g. chennai_bus has an APPLICATION config, kolkata_bus is MULTIMODAL-only), so
  -- don't assume one: prefer APPLICATION, then fall back to MULTIMODAL for the same feed.
  let findCfg platformType = QIBC.findByFeedKeyAndVehicleCategoryAndPlatformType req.feedKey vehicleCategory platformType
  integratedBPPConfig <-
    findCfg DIBC.APPLICATION
      >>= maybe (findCfg DIBC.MULTIMODAL) (pure . Just)
      >>= fromMaybeM (InvalidRequest $ "IntegratedBPPConfig not found for feedKey: " <> req.feedKey)
  merchantOperatingCity <-
    CQMOC.findById integratedBPPConfig.merchantOperatingCityId
      >>= fromMaybeM (MerchantOperatingCityDoesNotExist integratedBPPConfig.merchantOperatingCityId.getId)
  personId <- upsertRiderPersonByBadge integratedBPPConfig.merchantId req.operatorBadgeToken
  -- A conductor sells tickets on the physical bus in front of them, whose service tier
  -- (e.g. PREMIUM) is normally blacklisted from the customer search UI. Pass the requested
  -- tier through so it is NOT blacklisted for the conductor flow (Nothing here would hide
  -- PREMIUM/SHUTTLE fares); empty keeps all tiers when none is requested.
  let keepServiceTiers = Just (maybeToList req.searchReq.serviceTier)
  FRFSTicketService.postFrfsSearch
    (Just personId, integratedBPPConfig.merchantId)
    (Just merchantOperatingCity.city)
    (Just integratedBPPConfig.id)
    keepServiceTiers
    req.vehicleType
    req.searchReq

-- | Internal FRFS quote fetch: the conductor identity is read straight off the
--   search entity (set during 'frfsBookingSearch'), satisfying the handler's
--   rider-ownership check by construction.
frfsBookingQuote :: Id DFRFSSearch.FRFSSearch -> Flow [FRFSTicketServiceAPI.FRFSQuoteAPIRes]
frfsBookingQuote searchId = do
  search <- QFRFSSearch.findById searchId >>= fromMaybeM (SearchRequestDoesNotExist searchId.getId)
  FRFSTicketService.getFrfsSearchQuote (Just search.riderId, search.merchantId) searchId

-- | Internal FRFS confirm: delegate to the STANDARD v2 confirm so the normal
--   real-payment flow runs (init → on_init creates the Juspay payment order).
--   The conductor identity is read from the quote entity. The returned
--   'FRFSTicketBookingStatusAPIRes' carries @payment.paymentOrder@ (the Juspay
--   SDK payload) that the tablet renders as a QR for the customer to pay.
frfsBookingConfirm ::
  Id DFRFSQuote.FRFSQuote ->
  FRFSTicketServiceAPI.FRFSQuoteConfirmReq ->
  Flow FRFSTicketServiceAPI.FRFSTicketBookingStatusAPIRes
frfsBookingConfirm quoteId req = do
  quote <- QFRFSQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  FRFSTicketService.postFrfsQuoteV2Confirm (Just quote.riderId, quote.merchantId) quoteId Nothing req

-- | Internal FRFS booking status: delegate to the standard status handler, which
--   is an active state-machine driver — each call polls Juspay and advances the
--   booking (payment success → BPP confirm → ticket QR). The conductor identity
--   is read from the booking entity.
frfsBookingStatus ::
  Id DFRFSTicketBooking.FRFSTicketBooking ->
  Flow FRFSTicketServiceAPI.FRFSTicketBookingStatusAPIRes
frfsBookingStatus bookingId = do
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (FRFSTicketBookingDoesNotExist bookingId.getId)
  FRFSTicketService.getFrfsBookingStatus (Just booking.riderId, booking.merchantId) bookingId
