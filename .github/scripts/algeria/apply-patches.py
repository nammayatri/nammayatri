#!/usr/bin/env python3
"""
Apply the Algeria (+213) patches to a checkout of the Namma Yatri backend.

    ./apply-patches.py <source-root>

<source-root> is the directory that contains Backend/ — i.e. the root of a
checkout of nammayatri at the pinned 2023 baseline
(03a753113af1fdcddf3378d9dc2fc31170e385e4).

Upstream hard-codes the Indian dial code in five places. Every one of them
rejects, or silently fails to find, an Algerian number. We replace +91 with
+213 rather than accepting any country code: a permissive check would let
anyone in the world trigger an OTP SMS, which is how SMS-pumping fraud works.
Widening it later is a one-line change.

The script is idempotent — running it twice is a no-op — and it fails loudly
rather than silently skipping a site, because a missed patch produces a binary
that looks fine and still rejects +213 at runtime.
"""

import sys
from pathlib import Path

BACKEND = "Backend"
RIDER_SRC = f"{BACKEND}/app/rider-platform/rider-app/Main/src"
DRIVER_SRC = f"{BACKEND}/app/provider-platform/dynamic-offer-driver-app/Main/src"
RIDER = f"{RIDER_SRC}/Domain/Action/UI"
RIDER_API = f"{RIDER_SRC}/API/UI"
# Shared by both apps. Only ever touched with a field that is optional on
# the wire, so the two binaries can be deployed in either order.
BECKN_SPEC = f"{BACKEND}/lib/beckn-spec/src/Beckn/Types/Core/Taxi"
DRIVER = f"{DRIVER_SRC}/Domain/Action"

# (path, documented line number, old text, new text, note)
#
# The two Registration.hs sites differ in how they name the Regex type, and it
# is not cosmetic:
#
#   rider  Registration.hs:50  import Kernel.Types.Predicate            (unqualified)
#   driver Registration.hs:45  import qualified Kernel.Types.Predicate as P
#
# `Regex` is a type synonym (`type Regex = RE Char`) exported by
# Kernel.Types.Predicate, so it is in scope bare in the rider file and only as
# P.Regex in the driver file. Getting this wrong is a compile error four hours
# into the build. Neither site needs a new import.
PATCHES = [
    (
        f"{RIDER}/Registration.hs",
        81,
        'validateField "mobileCountryCode" mobileCountryCode P.mobileIndianCode',
        'validateField "mobileCountryCode" mobileCountryCode ("+213" :: Regex)',
        "rider: POST /v2/auth country-code validation",
    ),
    (
        f"{DRIVER}/UI/Registration.hs",
        76,
        'validateField "mobileCountryCode" mobileCountryCode P.mobileIndianCode',
        'validateField "mobileCountryCode" mobileCountryCode ("+213" :: P.Regex)',
        "driver: driver login country-code validation",
    ),
    (
        f"{DRIVER}/Dashboard/Driver.hs",
        301,
        'mobileIndianCode = "+91"',
        'mobileIndianCode = "+213"',
        "driver: dashboard driver lookup default country code",
    ),
    (
        f"{DRIVER}/UI/Call.hs",
        61,
        'QPerson.findByMobileNumber "+91" mobileNumberHash',
        'QPerson.findByMobileNumber "+213" mobileNumberHash',
        "driver: Exotel inbound-call driver lookup",
    ),
    (
        f"{DRIVER}/UI/DriverOnboarding/Image.hs",
        189,
        'Person.findByMobileNumber "+91" mobileNumberHash',
        'Person.findByMobileNumber "+213" mobileNumberHash',
        "driver: onboarding document lookup by phone",
    ),
    # ── The car, on the driver's offer ──────────────────────────────────────
    #
    # The client asked, repeatedly, for the passenger to see which car is
    # coming *while he is choosing between drivers*, not after. Today an offer
    # carries driverName, rating, distance, duration and validTill, and nothing
    # about the vehicle: `DriverQuote` has a `vehicleVariant` and no model, and
    # the BPP never looks the vehicle up when it builds on_select.
    #
    # These three patches carry "Renault|Clio|Grey" from the BPP to the BAP.
    #
    # ── Why `descriptor.name` and not a new BECKN field ─────────────────────
    # `OS.ItemDescriptor` already has a `name` that this baseline sets to "" and
    # the rider never reads. Using it means **no change to the shared Beckn
    # types**, which are compiled into the gateway and the registry as well as
    # both apps — so this cannot desynchronise the three binaries. A new field
    # would have been cleaner and far riskier.
    #
    # Pipe-separated rather than JSON so the parse on the far side cannot throw:
    # worst case a field is empty and the passenger sees one less word.
    (
        f"{DRIVER_SRC}/Beckn/ACL/OnSelect.hs",
        27,
        """data DOnSelectReq = DOnSelectReq
  { transporterInfo :: TransporterInfo,
    searchRequest :: SearchRequest,
    quotes :: [DQuote.DriverQuote],
    now :: UTCTime
  }""",
        """data DOnSelectReq = DOnSelectReq
  { transporterInfo :: TransporterInfo,
    searchRequest :: SearchRequest,
    quotes :: [DQuote.DriverQuote],
    -- Algeria: "make|model|colour" for the one driver this offer is from.
    -- Text rather than a Vehicle so this module needs no new import.
    vehicleDesc :: Maybe Text,
    now :: UTCTime
  }""",
        "driver: carry the vehicle description on the on_select request",
    ),
    (
        f"{DRIVER_SRC}/Beckn/ACL/OnSelect.hs",
        101,
        """mkQuoteEntities :: DOnSelectReq -> DQuote.DriverQuote -> QuoteEntities
mkQuoteEntities dReq quote = do
  let fulfillment = mkFulfillment dReq quote
      category = driverOfferCategory
      offer = Nothing
      item = mkItem category.id fulfillment.id quote
  QuoteEntities {..}""",
        """mkQuoteEntities :: DOnSelectReq -> DQuote.DriverQuote -> QuoteEntities
mkQuoteEntities dReq quote = do
  let fulfillment = mkFulfillment dReq quote
      category = driverOfferCategory
      offer = Nothing
      item = mkItem category.id fulfillment.id quote dReq.vehicleDesc
  QuoteEntities {..}""",
        "driver: pass the vehicle description down to the item",
    ),
    (
        f"{DRIVER_SRC}/Beckn/ACL/OnSelect.hs",
        133,
        """mkItem :: OS.FareProductType -> Text -> DQuote.DriverQuote -> OS.Item
mkItem categoryId fulfillmentId q =
  OS.Item
    { id = q.id.getId,
      category_id = categoryId,
      fulfillment_id = fulfillmentId,
      offer_id = Nothing,
      price = price_,
      descriptor =
        OS.ItemDescriptor
          { name = "",""",
        """mkItem :: OS.FareProductType -> Text -> DQuote.DriverQuote -> Maybe Text -> OS.Item
mkItem categoryId fulfillmentId q mbVehicleDesc =
  OS.Item
    { id = q.id.getId,
      category_id = categoryId,
      fulfillment_id = fulfillmentId,
      offer_id = Nothing,
      price = price_,
      descriptor =
        OS.ItemDescriptor
          { name = fromMaybe "" mbVehicleDesc,""",
        "driver: put the vehicle description in the item descriptor",
    ),
    # The lookup itself. `QVeh` and the `EsqDBFlow` pattern are both already in
    # this file — `sendRideAssignedUpdateToBAP` twenty lines above does exactly
    # this — so nothing new is imported and the constraint added is one this
    # module already satisfies elsewhere.
    (
        f"{DRIVER_SRC}/SharedLogic/CallBAP.hs",
        225,
        """sendDriverOffer ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasPrettyLogger m r
  ) =>""",
        """sendDriverOffer ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    EsqDBFlow m r,
    CoreMetrics m,
    HasPrettyLogger m r
  ) =>""",
        "driver: sendDriverOffer may read the database",
    ),
    (
        f"{DRIVER_SRC}/SharedLogic/CallBAP.hs",
        240,
        """    buildOnSelectReq ::
      (MonadTime m, HasPrettyLogger m r) =>
      DM.Merchant ->
      DSR.SearchRequest ->
      [DDQ.DriverQuote] ->
      m ACL.DOnSelectReq
    buildOnSelectReq org searchRequest quotes = do
      now <- getCurrentTime""",
        """    buildOnSelectReq ::
      (MonadTime m, HasPrettyLogger m r, EsqDBFlow m r) =>
      DM.Merchant ->
      DSR.SearchRequest ->
      [DDQ.DriverQuote] ->
      m ACL.DOnSelectReq
    buildOnSelectReq org searchRequest quotes = do
      now <- getCurrentTime
      -- Algeria: the car the passenger will actually get into. Absent is fine
      -- and stays absent -- a missing vehicle must never fail an offer that is
      -- otherwise good, so this is a lookup and not a `fromMaybeM`.
      mbVeh <- QVeh.findById driverQuote.driverId
      let vehicleDesc =
            mbVeh <&> \\veh ->
              T.intercalate "|" [fromMaybe "" veh.make, veh.model, veh.color, veh.registrationNo, getId driverQuote.driverId]""",
        "driver: look the vehicle up when building the offer (make|model|colour|plate|driverId)",
    ),
    (
        f"{DRIVER_SRC}/SharedLogic/CallBAP.hs",
        267,
        """        ACL.DOnSelectReq
          { transporterInfo,
            quotes,
            now,
            searchRequest
          }""",
        """        ACL.DOnSelectReq
          { transporterInfo,
            quotes,
            vehicleDesc,
            now,
            searchRequest
          }""",
        "driver: pass the vehicle description into the on_select request",
    ),
    # ── The rider half of the same chain ────────────────────────────────────
    #
    # The BPP patches above write "Renault|Clio|Grey" into the item descriptor.
    # The rider *already receives it*: `ItemDescriptor` is `{ name, code }` and
    # `buildQuoteInfo` reads only `code`, so upstream parses the field and drops
    # it on the floor. Nothing below widens the BECKN payload; it stops the drop.
    #
    # ── Why a column and not a spare field ──────────────────────────────────
    # `DriverOffer.driverName` was the tempting place — it needs no migration,
    # and it is safe in the narrow sense: `Ride.driverName` comes from
    # `on_update`'s `fulfillment.agent.name`, a different path entirely, so a
    # composite here would never reach the passenger's ride screen. It was
    # rejected anyway. A `driver_name` column reading "Ahmed|Renault|Clio|Grey"
    # is a trap for whoever next opens that table, and this project has been
    # bitten by hidden encodings more than once.
    #
    # The migration is `dev/local-stack/driver-offer-vehicle.sql`, and it is
    # ordinary: add a nullable column, then swap the image. The old binary
    # ignores an extra nullable column, so rollback stays a straight image swap.
    (
        f"{RIDER_SRC}/Beckn/ACL/OnSelect.hs",
        112,
        """  let rating = item.rating
  let bppQuoteId = item.id
  pure $
    DOnSelect.DriverOfferQuoteDetails""",
        """  let rating = item.rating
  let bppQuoteId = item.id
  -- Algeria: "make|model|colour", written by our own BPP a few hundred lines
  -- up this file's mirror image. Upstream sets this descriptor to "" and never
  -- reads it, so empty is the ordinary case and has to stay `Nothing` rather
  -- than become an empty string the app would draw as a blank line under the
  -- driver's name.
  --
  -- Compared with (==) rather than matched against a literal pattern: a Text
  -- literal *pattern* needs OverloadedStrings to be on in this module, and
  -- losing a 36-minute build to a language extension is not a trade worth
  -- making for two fewer lines. No import is needed either way.
  let descName = item.descriptor.name
  let vehicleDesc = if descName == "" then Nothing else Just descName
  pure $
    DOnSelect.DriverOfferQuoteDetails""",
        "rider: read the vehicle out of the on_select item descriptor",
    ),
    (
        f"{RIDER_SRC}/Domain/Action/Beckn/OnSelect.hs",
        65,
        """data DriverOfferQuoteDetails = DriverOfferQuoteDetails
  { driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: HighPrecMeters,
    validTill :: UTCTime,
    rating :: Maybe Centesimal,
    bppDriverQuoteId :: Id DDriverOffer.BPPQuote
  }""",
        """data DriverOfferQuoteDetails = DriverOfferQuoteDetails
  { driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: HighPrecMeters,
    validTill :: UTCTime,
    rating :: Maybe Centesimal,
    -- Algeria: "make|model|colour". `buildDriverOffer` below builds its result
    -- with RecordWildCards, so naming it here is the whole of the wiring.
    vehicleDesc :: Maybe Text,
    bppDriverQuoteId :: Id DDriverOffer.BPPQuote
  }""",
        "rider: carry the vehicle through the on_select action",
    ),
    (
        f"{RIDER_SRC}/Domain/Types/DriverOffer.hs",
        25,
        """data DriverOffer = DriverOffer
  { id :: Id DriverOffer,
    estimateId :: Id DEstimate.Estimate,
    driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: HighPrecMeters,
    validTill :: UTCTime,
    bppQuoteId :: Id BPPQuote,
    rating :: Maybe Centesimal
  }
  deriving (Generic, Show, PrettyShow)

data DriverOfferAPIEntity = DriverOfferAPIEntity
  { driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: HighPrecMeters,
    validTill :: UTCTime,
    rating :: Maybe Centesimal
  }""",
        """data DriverOffer = DriverOffer
  { id :: Id DriverOffer,
    estimateId :: Id DEstimate.Estimate,
    driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: HighPrecMeters,
    validTill :: UTCTime,
    bppQuoteId :: Id BPPQuote,
    rating :: Maybe Centesimal,
    -- Algeria: "make|model|colour", or Nothing when the BPP did not send one --
    -- an older provider, or a driver with no vehicle attached. The app must
    -- treat absence as ordinary and simply show one line less.
    vehicleDesc :: Maybe Text
  }
  deriving (Generic, Show, PrettyShow)

data DriverOfferAPIEntity = DriverOfferAPIEntity
  { driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: HighPrecMeters,
    validTill :: UTCTime,
    rating :: Maybe Centesimal,
    -- The one field the passenger's screen is being rebuilt for. Quote.hs
    -- converts between these two records with RecordWildCards, so adding the
    -- name to both is the entire change.
    vehicleDesc :: Maybe Text
  }""",
        "rider: put the vehicle on the offer the app reads",
    ),
    (
        f"{RIDER_SRC}/Storage/Tabular/DriverOffer.hs",
        40,
        """      bppQuoteId Text
      rating Centesimal Maybe
      Primary id""",
        """      bppQuoteId Text
      rating Centesimal Maybe
      vehicleDesc Text Maybe
      Primary id""",
        "rider: persist the vehicle description",
    ),
    # ── The passenger picks who gets the request ────────────────────────────
    #
    # Today every driver the pool finds is asked, in batches, and the first to
    # answer wins. The client asked for the other thing: the passenger sees the
    # cars near him and sends the request to the one, two or three he wants.
    #
    # ── The channel already exists ──────────────────────────────────────────
    # `select` carries a rider decision to the provider already: a Bool called
    # `auto_assign_enabled`, in `order.fulfillment.tags`, which the provider
    # stores on the search request and the allocator reads back. Nothing new is
    # invented here — a second field rides in the same tags, is stored in the
    # same row and is read at the same moment.
    #
    # `Maybe Text`, comma-separated, and `Maybe` is what makes this deployable
    # in either order: an old provider ignores a JSON key it does not know, and
    # a new provider reading an old rider's payload parses `Nothing`, which
    # means "he did not choose" and asks everyone exactly as before.
    #
    # ── What it deliberately does NOT do ────────────────────────────────────
    # There is no fallback to the full pool. If the two drivers he chose never
    # answer, he gets no offers — and that is the honest outcome of choosing.
    # Quietly widening the search would mean a third driver arriving at his door
    # after he specifically did not pick him. The waiting screen already runs
    # its own clock, so the app is where the "ask everyone" offer belongs.
    (
        f"{BECKN_SPEC}/Select/Fulfillment.hs",
        36,
        """newtype Tags = Tags
  { auto_assign_enabled :: Bool
  }
  deriving (Generic, Show)""",
        """data Tags = Tags
  { auto_assign_enabled :: Bool,
    -- Algeria: comma-separated driver ids the passenger picked, or Nothing
    -- when he did not pick. Optional on purpose -- see apply-patches.py.
    chosen_drivers :: Maybe Text
  }
  deriving (Generic, Show)""",
        "spec: the select tags carry the passenger's shortlist",
    ),
    (
        f"{BECKN_SPEC}/Select/Fulfillment.hs",
        59,
        """    { fieldLabelModifier = \\case
        "auto_assign_enabled" -> "./komn/auto_assign_enabled"
        a -> a
    }""",
        """    { fieldLabelModifier = \\case
        "auto_assign_enabled" -> "./komn/auto_assign_enabled"
        "chosen_drivers" -> "./komn/chosen_drivers"
        a -> a
    }""",
        "spec: name the new tag the way the old one is named",
    ),
    (
        f"{RIDER_SRC}/Domain/Action/UI/Select.hs",
        55,
        """newtype DEstimateSelectReq = DEstimateSelect
  { autoAssignEnabled :: Bool
  }""",
        """data DEstimateSelectReq = DEstimateSelect
  { autoAssignEnabled :: Bool,
    -- Algeria: comma-separated person ids, joined by the app. Text rather than
    -- a list so the shape is identical at every hop -- request body, BECKN tag
    -- and database column -- and there is exactly one place that splits it.
    chosenDrivers :: Maybe Text
  }""",
        "rider: the select body accepts a shortlist",
    ),
    (
        f"{RIDER_API}/Select.hs",
        69,
        """  let req = DSelect.DEstimateSelect {autoAssignEnabled = False}
  dSelectReq <- DSelect.select personId estimateId
  becknReq <- ACL.buildSelectReq dSelectReq req.autoAssignEnabled""",
        """  let req = DSelect.DEstimateSelect {autoAssignEnabled = False, chosenDrivers = Nothing}
  dSelectReq <- DSelect.select personId estimateId
  becknReq <- ACL.buildSelectReq dSelectReq req.autoAssignEnabled req.chosenDrivers""",
        "rider: the bodyless /select asks everyone, as it always did",
    ),
    (
        f"{RIDER_API}/Select.hs",
        76,
        """select2 personId estimateId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  dSelectReq <- DSelect.select personId estimateId
  becknReq <- ACL.buildSelectReq dSelectReq req.autoAssignEnabled""",
        """select2 personId estimateId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  dSelectReq <- DSelect.select personId estimateId
  becknReq <- ACL.buildSelectReq dSelectReq req.autoAssignEnabled req.chosenDrivers""",
        "rider: /select2 forwards the shortlist",
    ),
    (
        f"{RIDER_SRC}/Beckn/ACL/Select.hs",
        31,
        """  DSelect.DSelectRes ->
  Bool ->
  m (BecknReq Select.SelectMessage)
buildSelectReq dSelectReq autoAssignEnabled = do""",
        """  DSelect.DSelectRes ->
  Bool ->
  Maybe Text ->
  m (BecknReq Select.SelectMessage)
buildSelectReq dSelectReq autoAssignEnabled chosenDrivers = do""",
        "rider ACL: take the shortlist",
    ),
    (
        f"{RIDER_SRC}/Beckn/ACL/Select.hs",
        41,
        "  let order = mkOrder dSelectReq autoAssignEnabled",
        "  let order = mkOrder dSelectReq autoAssignEnabled chosenDrivers",
        "rider ACL: pass it to the order",
    ),
    (
        f"{RIDER_SRC}/Beckn/ACL/Select.hs",
        50,
        """mkOrder :: DSelect.DSelectRes -> Bool -> Select.Order
mkOrder req autoAssignEnabled = do""",
        """mkOrder :: DSelect.DSelectRes -> Bool -> Maybe Text -> Select.Order
mkOrder req autoAssignEnabled chosenDrivers = do""",
        "rider ACL: mkOrder takes it too",
    ),
    (
        f"{RIDER_SRC}/Beckn/ACL/Select.hs",
        74,
        """              Select.Tags
                { auto_assign_enabled = autoAssignEnabled
                },""",
        """              Select.Tags
                { auto_assign_enabled = autoAssignEnabled,
                  chosen_drivers = chosenDrivers
                },""",
        "rider ACL: put the shortlist in the tags",
    ),
    (
        f"{DRIVER_SRC}/Beckn/ACL/Select.hs",
        64,
        "        autoAssignEnabled = order.fulfillment.tags.auto_assign_enabled\n      }",
        """        autoAssignEnabled = order.fulfillment.tags.auto_assign_enabled,
        chosenDrivers = order.fulfillment.tags.chosen_drivers
      }""",
        "provider ACL: read the shortlist off the tags",
    ),
    (
        f"{DRIVER_SRC}/Domain/Action/Beckn/Select.hs",
        57,
        """    variant :: Variant,
    autoAssignEnabled :: Bool
  }""",
        """    variant :: Variant,
    autoAssignEnabled :: Bool,
    -- Algeria: comma-separated person ids, or Nothing when the passenger did
    -- not choose. Read in the allocator, not here.
    chosenDrivers :: Maybe Text
  }""",
        "provider: the select request carries the shortlist",
    ),
    (
        f"{DRIVER_SRC}/Domain/Action/Beckn/Select.hs",
        151,
        """        status = DSearchReq.ACTIVE,
        updatedAt = now,
        autoAssignEnabled = sReq.autoAssignEnabled
      }""",
        """        status = DSearchReq.ACTIVE,
        updatedAt = now,
        autoAssignEnabled = sReq.autoAssignEnabled,
        -- Stored rather than passed along, because the allocator job that
        -- actually builds the batches runs later and re-reads this row.
        chosenDrivers = sReq.chosenDrivers
      }""",
        "provider: store the shortlist on the search request",
    ),
    (
        f"{DRIVER_SRC}/Domain/Types/SearchRequest.hs",
        47,
        """    status :: SearchRequestStatus,
    autoAssignEnabled :: Bool
  }
  deriving (Generic, PrettyShow, Show)""",
        """    status :: SearchRequestStatus,
    autoAssignEnabled :: Bool,
    -- Algeria: comma-separated person ids the passenger picked. Nothing means
    -- he did not pick, and every driver in the pool is asked as before.
    chosenDrivers :: Maybe Text
  }
  deriving (Generic, PrettyShow, Show)""",
        "provider: the shortlist on the domain type",
    ),
    (
        f"{DRIVER_SRC}/Storage/Tabular/SearchRequest.hs",
        55,
        """      Primary id
      autoAssignEnabled Bool
      deriving Generic""",
        """      Primary id
      autoAssignEnabled Bool
      chosenDrivers Text Maybe
      deriving Generic""",
        "provider: persist the shortlist",
    ),
    (
        f"{DRIVER_SRC}/SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle/Internal/DriverPool.hs",
        25,
        "import qualified Data.HashMap as HM\nimport Domain.Types.Merchant (Merchant)",
        "import qualified Data.HashMap as HM\nimport qualified Data.Text as T\nimport Domain.Types.Merchant (Merchant)",
        "provider: Data.Text, for the one split",
    ),
    (
        f"{DRIVER_SRC}/SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle/Internal/DriverPool.hs",
        62,
        "prepareDriverPoolBatch ::\n  ( EncFlow m r,",
        """-- | Algeria: keep only the drivers the passenger actually picked.
--
-- `chosenDrivers` is a comma-separated list of person ids, put on the search
-- request by the rider. `Nothing` -- and an empty list, which is the same thing
-- said differently -- means he did not choose, and the pool is returned whole.
-- That is the ordinary case and must stay the cheap one.
--
-- An id that is not in the pool is simply absent from the result. He may have
-- gone offline, taken a ride, or moved out of radius between the app listing
-- him and this request going out. That is deliberately not an error: the pool
-- is allowed to come back empty, the search then finds nobody, and the app --
-- which runs its own clock on the waiting screen -- is where the passenger is
-- offered the choice of asking everyone. Widening it here would put a driver
-- he specifically did not pick at his door.
onlyChosen :: DSR.SearchRequest -> [DriverPoolWithActualDistResult] -> [DriverPoolWithActualDistResult]
onlyChosen searchReq pool =
  case searchReq.chosenDrivers of
    Nothing -> pool
    Just raw -> do
      let wanted = filter (not . T.null) . map T.strip $ T.splitOn "," raw
      if null wanted
        then pool
        else filter (\\dpr -> getId dpr.driverPoolResult.driverId `elem` wanted) pool

prepareDriverPoolBatch ::
  ( EncFlow m r,""",
        "provider: the filter itself",
    ),
    (
        f"{DRIVER_SRC}/SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle/Internal/DriverPool.hs",
        106,
        """      radiusStep <- getPoolRadiusStep searchReq.id
      allNearbyDrivers <- calcDriverPool radiusStep""",
        """      radiusStep <- getPoolRadiusStep searchReq.id
      -- The one line this whole change exists for. Everything below works off
      -- `allNearbyDrivers`, so filtering here filters the batching, the
      -- sorting, the fill and the radius expansion at once.
      allNearbyDrivers <- onlyChosen searchReq <$> calcDriverPool radiusStep""",
        "provider: filter the pool to the passenger's shortlist",
    ),
    # ── The driver rates the passenger ──────────────────────────────────────
    #
    # The client asked for this repeatedly and it was refused three times, for
    # a reason that was true: **this backend could not do it.** The only rating
    # route in the whole driver API is `/beckn/{merchantId}/rating`, which is
    # the provider *receiving* a rating from the rider app, and `rider_details`
    # had five columns with nowhere to put one. So the app shipped a star that
    # points one way and deliberately no "Noter" pill, because a control that
    # cannot do anything is worse than an absent one.
    #
    # These patches give it somewhere to go. Everything stays on the provider
    # side: the driver, the ride, the passenger record and the rating all live
    # in `atlas_driver_offer_bpp`, so nothing crosses BECKN and neither the
    # gateway nor the rider binary needs to know this happened.
    #
    # The migration is `dev/local-stack/passenger-rating.sql`, and it is the
    # ordinary shape: nullable/defaulted columns first, then the image swap. The
    # old binary ignores columns it was never told about, so rollback stays a
    # straight image swap with nothing to undo.
    (
        f"{DRIVER_SRC}/Domain/Types/RiderDetails.hs",
        24,
        """data RiderDetailsE e = RiderDetails
  { id :: Id RiderDetails,
    mobileCountryCode :: Text,
    mobileNumber :: EncryptedHashedField e Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }""",
        """data RiderDetailsE e = RiderDetails
  { id :: Id RiderDetails,
    mobileCountryCode :: Text,
    mobileNumber :: EncryptedHashedField e Text,
    -- Algeria: what drivers have made of this passenger.
    --
    -- `Double` rather than the `Centesimal` the driver's own rating uses. That
    -- type lives in Kernel.Types.Common, which this module does not import,
    -- and adding an import to a file with two encryption instances to save a
    -- decimal place is not a trade worth a failed build.
    --
    -- The running total is kept alongside the average because there is nowhere
    -- to recompute it from: a driver's average is rebuilt by reading every row
    -- in the ratings table, and passenger ratings have no such table. Storing
    -- the count and the sum makes the next average one addition.
    rating :: Maybe Double,
    totalRatings :: Int,
    totalRatingScore :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }""",
        "driver: the passenger record can hold a rating",
    ),
    (
        f"{DRIVER_SRC}/Storage/Tabular/RiderDetails.hs",
        36,
        """      mobileNumberHash DbHash
      createdAt UTCTime
      updatedAt UTCTime""",
        """      mobileNumberHash DbHash
      rating Double Maybe
      totalRatings Int
      totalRatingScore Int
      createdAt UTCTime
      updatedAt UTCTime""",
        "driver: persist the passenger's rating",
    ),
    # `fromTType`/`toTType` in that file are RecordWildCards, so naming the
    # fields above is the whole of the wiring -- the same reason the vehicle
    # patches needed nothing further.
    (
        f"{DRIVER_SRC}/Storage/Queries/RiderDetails.hs",
        34,
        """findByMobileNumber ::
  (MonadThrow m, Log m, Transactionable m, EncFlow m r) =>
  Text ->
  m (Maybe RiderDetails)
findByMobileNumber mobileNumber_ = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  Esq.findOne $ do
    riderDetails <- from $ table @RiderDetailsT
    where_ $ riderDetails ^. RiderDetailsMobileNumberHash ==. val mobileNumberDbHash
    return riderDetails""",
        """findByMobileNumber ::
  (MonadThrow m, Log m, Transactionable m, EncFlow m r) =>
  Text ->
  m (Maybe RiderDetails)
findByMobileNumber mobileNumber_ = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  Esq.findOne $ do
    riderDetails <- from $ table @RiderDetailsT
    where_ $ riderDetails ^. RiderDetailsMobileNumberHash ==. val mobileNumberDbHash
    return riderDetails

-- | Algeria: record what a driver made of a passenger.
--
-- The average is passed in rather than computed here, because the caller has
-- already read the row it is updating and a second read inside the transaction
-- would be a read of its own write. Modelled on `Person.updateAverageRating`,
-- which is the same shape one table over.
--
-- This module imports Kernel.Storage.Esqueleto unqualified as well as
-- qualified, so `update`, `set`, `val`, `where_`, `toKey` and the field
-- constructors are all already in scope.
updateRating :: Id RiderDetails -> Double -> Int -> Int -> SqlDB ()
updateRating riderId newAverage newCount newScore = do
  now <- getCurrentTime
  Esq.update $ \\tbl -> do
    set
      tbl
      [ RiderDetailsRating =. val (Just newAverage),
        RiderDetailsTotalRatings =. val newCount,
        RiderDetailsTotalRatingScore =. val newScore,
        RiderDetailsUpdatedAt =. val now
      ]
    where_ $ tbl ^. RiderDetailsTId ==. val (toKey riderId)""",
        "driver: a query that writes the passenger's rating",
    ),
    # ── The route the driver's app calls ────────────────────────────────────
    #
    # Added to the existing ride API rather than given a module of its own: it
    # is addressed by ride id, it is only legal on a ride the caller drove, and
    # every other verb on a ride already lives here.
    (
        f"{DRIVER_SRC}/API/UI/Ride.hs",
        18,
        """    CancelRideReq (..),
    DRide.DriverRideListRes (..),""",
        """    CancelRideReq (..),
    RateCustomerReq (..),
    DRide.DriverRideListRes (..),""",
        "driver API: export the new request type",
    ),
    (
        f"{DRIVER_SRC}/API/UI/Ride.hs",
        69,
        """           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
           :> "cancel"
           :> ReqBody '[JSON] CancelRideReq
           :> Post '[JSON] APISuccess
       )""",
        """           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
           :> "cancel"
           :> ReqBody '[JSON] CancelRideReq
           :> Post '[JSON] APISuccess
           -- Algeria: the driver rates the passenger. POST on the ride, like
           -- every other verb here, so the ride he drove is the authorisation.
           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
           :> "rateCustomer"
           :> ReqBody '[JSON] RateCustomerReq
           :> Post '[JSON] APISuccess
       )""",
        "driver API: the rateCustomer route",
    ),
    (
        f"{DRIVER_SRC}/API/UI/Ride.hs",
        94,
        """data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)""",
        """data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Algeria: one to five, and nothing else.
--
-- No free-text feedback field. The passenger's side has one and it is written
-- to a table nobody reads; a second write-only box is not worth the column.
newtype RateCustomerReq = RateCustomerReq
  { ratingValue :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)""",
        "driver API: the request body",
    ),
    (
        f"{DRIVER_SRC}/API/UI/Ride.hs",
        109,
        """handler :: FlowServer API
handler =
  listDriverRides
    :<|> arrivedAtPickup
    :<|> startRide
    :<|> endRide
    :<|> cancelRide""",
        """handler :: FlowServer API
handler =
  listDriverRides
    :<|> arrivedAtPickup
    :<|> startRide
    :<|> endRide
    :<|> cancelRide
    :<|> rateCustomer""",
        "driver API: wire the handler",
    ),
    (
        f"{DRIVER_SRC}/API/UI/Ride.hs",
        146,
        """arrivedAtPickup :: Id SP.Person -> Id Ride.Ride -> LatLong -> FlowHandler APISuccess
arrivedAtPickup _ rideId req = withFlowHandlerAPI $ DRide.arrivedAtPickup rideId req""",
        """arrivedAtPickup :: Id SP.Person -> Id Ride.Ride -> LatLong -> FlowHandler APISuccess
arrivedAtPickup _ rideId req = withFlowHandlerAPI $ DRide.arrivedAtPickup rideId req

-- | Algeria: the driver's rating of his passenger.
--
-- The driver id is used, not ignored: this is the one verb here where the
-- caller's identity is the whole authorisation, so the action checks that the
-- ride was his before writing anything.
rateCustomer :: Id SP.Person -> Id Ride.Ride -> RateCustomerReq -> FlowHandler APISuccess
rateCustomer driverId rideId RateCustomerReq {ratingValue} =
  withFlowHandlerAPI $ DRide.rateCustomer driverId rideId ratingValue""",
        "driver API: the handler function",
    ),
    # ── The action behind it, and the rating on the way back out ────────────
    (
        f"{DRIVER_SRC}/Domain/Action/UI/Ride.hs",
        15,
        """module Domain.Action.UI.Ride
  ( DriverRideRes (..),
    DriverRideListRes (..),
    listDriverRides,
    arrivedAtPickup,
  )
where""",
        """module Domain.Action.UI.Ride
  ( DriverRideRes (..),
    DriverRideListRes (..),
    listDriverRides,
    arrivedAtPickup,
    rateCustomer,
  )
where""",
        "driver: export the rating action",
    ),
    (
        f"{DRIVER_SRC}/Domain/Action/UI/Ride.hs",
        51,
        """import qualified Storage.Queries.RideDetails as QRD
import Tools.Error""",
        """import qualified Storage.Queries.RideDetails as QRD
-- Algeria: the passenger record. `QRD` above is *Ride*Details, one letter and
-- an entirely different table, so this one is spelled out.
import qualified Storage.Queries.RiderDetails as QRiderDetails
import Tools.Error""",
        "driver: import the passenger queries",
    ),
    (
        f"{DRIVER_SRC}/Domain/Action/UI/Ride.hs",
        77,
        """    riderName :: Maybe Text,
    tripStartTime :: Maybe UTCTime,""",
        """    riderName :: Maybe Text,
    -- Algeria: what drivers before him have made of this passenger, or Nothing
    -- for someone nobody has rated yet. Never zero -- the scale starts at one,
    -- and a 0 would tell a driver the passenger is the worst there is.
    riderRating :: Maybe Double,
    tripStartTime :: Maybe UTCTime,""",
        "driver: the passenger's rating on the ride response",
    ),
    (
        f"{DRIVER_SRC}/Domain/Action/UI/Ride.hs",
        110,
        """mkDriverRideRes ::
  RD.RideDetails ->
  Maybe Text ->
  Maybe DRating.Rating ->
  (DRide.Ride, DRB.Booking) ->
  DriverRideRes
mkDriverRideRes rideDetails driverNumber rideRating (ride, booking) = do""",
        """mkDriverRideRes ::
  RD.RideDetails ->
  Maybe Text ->
  Maybe DRating.Rating ->
  Maybe Double ->
  (DRide.Ride, DRB.Booking) ->
  DriverRideRes
mkDriverRideRes rideDetails driverNumber rideRating mbRiderRating (ride, booking) = do""",
        "driver: mkDriverRideRes takes the passenger's rating",
    ),
    (
        f"{DRIVER_SRC}/Domain/Action/UI/Ride.hs",
        139,
        """      riderName = booking.riderName,
      tripStartTime = ride.tripStartTime,""",
        """      riderName = booking.riderName,
      riderRating = mbRiderRating,
      tripStartTime = ride.tripStartTime,""",
        "driver: put it on the response",
    ),
    (
        f"{DRIVER_SRC}/Domain/Action/UI/Ride.hs",
        105,
        """    rideRating <- runInReplica $ QR.findRatingForRide ride.id
    driverNumber <- RD.getDriverNumber rideDetail
    pure $ mkDriverRideRes rideDetail driverNumber rideRating (ride, booking)""",
        """    rideRating <- runInReplica $ QR.findRatingForRide ride.id
    -- Algeria: the passenger's own rating. A booking with no rider on record is
    -- ordinary rather than an error -- `riderId` is only filled at confirm --
    -- so this reads Nothing and the row simply shows one thing less.
    mbRiderRating <- case booking.riderId of
      Nothing -> pure Nothing
      Just riderId -> do
        mbRider <- runInReplica $ QRiderDetails.findById riderId
        pure (mbRider >>= (.rating))
    driverNumber <- RD.getDriverNumber rideDetail
    pure $ mkDriverRideRes rideDetail driverNumber rideRating mbRiderRating (ride, booking)""",
        "driver: read the passenger's rating when listing rides",
    ),
    (
        f"{DRIVER_SRC}/Domain/Action/UI/Ride.hs",
        169,
        """  pure Success
  where
    isValidRideStatus status = status == DRide.NEW""",
        """  pure Success
  where
    isValidRideStatus status = status == DRide.NEW

-- | Algeria: the driver rates his passenger, once the ride is over.
--
-- Everything here is on the provider side -- the driver, the ride, the
-- passenger record and the rating -- so nothing crosses BECKN and the rider
-- binary is not involved.
--
-- ── One known limitation, stated rather than hidden ─────────────────────────
-- There is no per-ride record of a passenger rating, so a second POST for the
-- same ride counts twice. A driver's own rating is protected by a `rating` row
-- keyed on the ride; giving passengers the same would mean a second table, and
-- the app disables the control once it has been used. If this is ever abused,
-- that table is the fix -- not a flag on the ride.
rateCustomer ::
  (EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Id DP.Person ->
  Id DRide.Ride ->
  Int ->
  m APISuccess
rateCustomer driverId rideId ratingValue = do
  unless (ratingValue >= 1 && ratingValue <= 5) $
    throwError $ InvalidRequest "Rating must be between 1 and 5."
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  -- His own ride, and a finished one. The token proves who is asking; this
  -- proves he is asking about something that was his.
  unless (ride.driverId == driverId) $
    throwError $ InvalidRequest "This ride is not yours."
  unless (ride.status == DRide.COMPLETED) $
    throwError $ RideInvalidStatus "The ride is not finished."
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  riderId <- booking.riderId & fromMaybeM (InvalidRequest "This ride has no passenger on record.")
  rider <- runInReplica $ QRiderDetails.findById riderId >>= fromMaybeM (InvalidRequest "Passenger not found.")
  let newCount = rider.totalRatings + 1
      newScore = rider.totalRatingScore + ratingValue
      newAverage = fromIntegral newScore / fromIntegral newCount
  Esq.runTransaction $ QRiderDetails.updateRating riderId newAverage newCount newScore
  pure Success""",
        "driver: the rating action itself",
    ),
]


def main() -> int:
    if len(sys.argv) != 2:
        print(__doc__.strip(), file=sys.stderr)
        return 2

    root = Path(sys.argv[1]).resolve()
    if not (root / BACKEND / "stack.yaml").is_file():
        fail(f"{root} does not look like a Namma Yatri checkout "
             f"({BACKEND}/stack.yaml is missing).")

    applied, already = 0, 0
    for rel, want_line, old, new, note in PATCHES:
        path = root / rel
        if not path.is_file():
            fail(f"missing file: {rel}\n"
                 f"  The source ref is probably not the pinned 2023 baseline.")

        text = path.read_text(encoding="utf-8")

        if new in text:
            print(f"  = {rel}  (already patched)")
            already += 1
            continue

        n = text.count(old)
        if n == 0:
            fail(f"{rel}: could not find the text to patch.\n"
                 f"  looking for: {old}\n"
                 f"  This site moved or changed upstream. Re-check the patch "
                 f"list against the source ref before building — a build that "
                 f"skips a site still rejects +213 at runtime.")
        if n > 1:
            fail(f"{rel}: found {n} occurrences of the text to patch, "
                 f"expected exactly 1. Refusing to guess.")

        # Line number is a sanity check only. Drift is a warning, not an
        # error: the content match above is what actually matters.
        got_line = text[: text.index(old)].count("\n") + 1
        if got_line != want_line:
            print(f"  ! {rel}: expected line {want_line}, found line "
                  f"{got_line} — content matched, continuing")

        path.write_text(text.replace(old, new), encoding="utf-8")
        print(f"  + {rel}:{got_line}  {note}")
        applied += 1

    print(f"\nAlgeria patches: {applied} applied, {already} already in place, "
          f"{len(PATCHES)} total")

    # Belt and braces: prove no +91 survives in the five patched files.
    for rel, *_ in PATCHES:
        text = (root / rel).read_text(encoding="utf-8")
        for i, line in enumerate(text.splitlines(), 1):
            if '"+91"' in line or "P.mobileIndianCode" in line:
                fail(f"{rel}:{i} still hard-codes +91 after patching:\n"
                     f"  {line.strip()}")

    return 0


def fail(msg: str):
    print(f"\nFAILED: {msg}", file=sys.stderr)
    sys.exit(1)


if __name__ == "__main__":
    sys.exit(main())
