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
              T.intercalate "|" [fromMaybe "" veh.make, veh.model, veh.color]""",
        "driver: look the vehicle up when building the offer",
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
