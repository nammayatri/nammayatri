
# lts-sync-lint.sh
#
# Linter that ensures every DB update touching a DriverPoolData-relevant
# Beam field has the CORRESPONDING LTSSync field synced within the same
# Haskell function. Catches both missing syncs AND partial syncs (e.g.
# updating active + blocked but only syncing active).
#
# Usage:
#   ./dev/lts-sync-lint.sh              # scan default Extra query files
#   ./dev/lts-sync-lint.sh path/to/File.hs  # scan specific file(s)
#
# Exit code: 0 = clean, 1 = violations found

set -euo pipefail

BACKEND_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DRIVER_APP="$BACKEND_ROOT/app/provider-platform/dynamic-offer-driver-app/Main/src"


DEFAULT_FILES=(
  "$DRIVER_APP/Storage/Queries/DriverInformationExtra.hs"
  "$DRIVER_APP/Storage/Queries/VehicleExtra.hs"
  "$DRIVER_APP/Storage/Queries/PersonExtra.hs"
  "$DRIVER_APP/Storage/Queries/DriverStatsExtra.hs"
  "$DRIVER_APP/Storage/Queries/DriverBankAccountExtra.hs"
  "$DRIVER_APP/Storage/Queries/FleetDriverAssociationExtra.hs"
  "$DRIVER_APP/Storage/Queries/DriverPlanExtra.hs"
  "$DRIVER_APP/Storage/Queries/DriverGoHomeRequestExtra.hs"
)

if [[ $# -gt 0 ]]; then
  FILES=("$@")
else
  FILES=("${DEFAULT_FILES[@]}")
fi

# ── Main logic in awk ────────────────────────────────────────────────
# awk is vastly faster than bash loops with grep for per-line matching.
# We pass the field mapping as a variable and do all work in one pass.

# Format: "BeamPrefix.beamField=ltsField" entries, pipe-separated (BSD awk safe)
FIELD_MAP="BeamDI.active=active|BeamDI.mode=mode|BeamDI.onRide=onRide|BeamDI.onRideTripCategory=onRideTripCategory|BeamDI.hasAdvanceBooking=hasAdvanceBooking|BeamDI.latestScheduledBooking=latestScheduledBooking|BeamDI.latestScheduledPickup=latestScheduledPickup|BeamDI.blocked=blocked|BeamDI.subscribed=subscribed|BeamDI.canSwitchToRental=canSwitchToRental|BeamDI.canSwitchToInterCity=canSwitchToInterCity|BeamDI.canSwitchToIntraCity=canSwitchToIntraCity|BeamDI.forwardBatchingEnabled=forwardBatchingEnabled|BeamDI.isSpecialLocWarrior=isSpecialLocWarrior|BeamDI.tollRouteBlockedTill=tollRouteBlockedTill|BeamDI.softBlockStiers=softBlockStiers|BeamDI.acUsageRestrictionType=acUsageRestrictionType|BeamDI.acRestrictionLiftCount=acRestrictionLiftCount|BeamDI.tripDistanceMinThreshold=tripDistanceMinThreshold|BeamDI.tripDistanceMaxThreshold=tripDistanceMaxThreshold|BeamDI.maxPickupRadius=maxPickupRadius|BeamDI.isPetModeEnabled=isPetModeEnabled|BeamDI.hasRideStarted=hasRideStarted|BeamDI.airConditionScore=airConditionScore|BeamDI.driverTripEndLocationLat=driverTripEndLocation|BeamDI.driverTripEndLocationLon=driverTripEndLocation|BeamV.variant=variant|BeamV.selectedServiceTiers=selectedServiceTiers|BeamV.vehicleTags=vehicleTags|BeamV.mYManufacturing=mYManufacturing|BeamV.airConditioned=airConditioned|BeamV.luggageCapacity=luggageCapacity|BeamV.vehicleRating=vehicleRating|BeamV.registrationNo=registrationNo|BeamP.deviceToken=deviceToken|BeamP.language=language|BeamP.gender=gender|BeamP.driverTag=driverTag|BeamP.clientSdkVersion=clientSdkVersion|BeamP.clientBundleVersion=clientBundleVersion|BeamP.clientConfigVersion=clientConfigVersion|BeamP.clientOsVersion=clientDevice|BeamP.clientOsType=clientDevice|BeamP.clientModelName=clientDevice|BeamP.clientManufacturer=clientDevice|BeamDS.totalRides=totalRides|Beam.chargesEnabled=chargesEnabled|BeamFDVA.fleetOwnerId=fleetOwnerId|BeamDF.enableServiceUsageCharge=safetyPlusEnabled|BeamDHR.status=goHomeStatus"

violations=0
violation_details=""

for filepath in "${FILES[@]}"; do
  if [[ ! -f "$filepath" ]]; then
    continue
  fi
  relpath="${filepath#"$BACKEND_ROOT"/}"

  result=$(awk -v relpath="$relpath" -v field_map="$FIELD_MAP" '
  BEGIN {
    # Parse field map into beam2lts associative array
    n = split(field_map, entries, "|")
    for (i = 1; i <= n; i++) {
      if (entries[i] == "") continue
      eq = index(entries[i], "=")
      beam_key = substr(entries[i], 1, eq - 1)
      lts_field = substr(entries[i], eq + 1)
      beam2lts[beam_key] = lts_field
    }
    violations = 0
    func_name = ""
    func_start = 0
    block = ""
  }

  function is_func_start(line) {
    if (line ~ /^[a-z][a-zA-Z0-9_]*/) {
      if (line !~ /^(module |import |-- |type |class |instance |data |newtype |deriving )/) {
        return 1
      }
    }
    return 0
  }

  function check_block() {
    if (block == "") return

    # Quick check: does block contain "Se.Set" at all?
    if (index(block, "Se.Set") == 0) return

    # Split block into lines
    num_blines = split(block, blines, "\n")

    # Collect required LTS fields and their source locations
    delete required_lts
    delete violation_lines
    num_required = 0

    for (bi = 1; bi <= num_blines; bi++) {
      bline = blines[bi]
      if (index(bline, "Se.Set") == 0) continue

      for (beam_key in beam2lts) {
        lts_field = beam2lts[beam_key]
        # Escape dots for regex
        escaped = beam_key
        gsub(/\./, "\\.", escaped)
        pat = "Se\\.Set " escaped "([^A-Za-z0-9_]|$)"
        if (bline ~ pat) {
          if (!(lts_field in required_lts)) {
            required_lts[lts_field] = 1
            num_required++
          }
          # Store violation detail keyed by lts_field + counter
          line_num = func_start + bi - 1
          trimmed = bline
          gsub(/^[[:space:]]+/, "", trimmed)
          vkey = lts_field SUBSEP beam_key SUBSEP bi
          violation_lines[vkey] = line_num "\t" trimmed "\t" beam_key
        }
      }
    }

    if (num_required == 0) return

    # Check for runPoolFieldUpdate (bypasses per-field check)
    if (index(block, "runPoolFieldUpdate") > 0) return

    # For each required LTS field, check if LTSSync.<field> = LTSSync.Set exists
    for (lts_field in required_lts) {
      pat = "LTSSync\\." lts_field "[[:space:]]*=[[:space:]]*LTSSync\\.Set"
      if (block !~ pat) {
        violations++
        for (vkey in violation_lines) {
          split(vkey, vkparts, SUBSEP)
          if (vkparts[1] == lts_field) {
            split(violation_lines[vkey], dparts, "\t")
            print "  " relpath ":" dparts[1] ": " func_name " -- Se.Set " dparts[3] " without syncing LTSSync." lts_field
            print "    | " dparts[2]
          }
        }
      }
    }
  }

  {
    if (is_func_start($0)) {
      check_block()
      match($0, /^[a-z][a-zA-Z0-9_]*/)
      func_name = substr($0, RSTART, RLENGTH)
      func_start = NR
      block = $0
    } else {
      block = block "\n" $0
    }
  }

  END {
    check_block()
    print "VIOLATION_COUNT=" violations
  }
  ' "$filepath")

  # Extract violation count and details
  count_line=$(echo "$result" | grep "^VIOLATION_COUNT=")
  file_violations="${count_line#VIOLATION_COUNT=}"
  details=$(echo "$result" | grep -v "^VIOLATION_COUNT=" || true)

  violations=$((violations + file_violations))
  if [[ -n "$details" ]]; then
    violation_details="${violation_details}${details}"$'\n'
  fi
done

# ── Output ───────────────────────────────────────────────────────────
echo "LTS Sync Lint: checking pool-relevant DB updates have field-level LTS sync..."
echo ""

if [[ $violations -gt 0 ]]; then
  echo "FAIL: Found $violations pool field update(s) missing their LTS sync:"
  echo ""
  echo "$violation_details"
  echo ""
  echo "Each Se.Set on a pool field MUST have a matching LTSSync.<field> = LTSSync.Set"
  echo "in the syncDriverPoolDataToLTS call within the same function."
  echo ""
  echo "Example:"
  echo "  updateOneWithKV [Se.Set BeamDI.blocked isBlocked]"
  echo "  LTSSync.syncDriverPoolDataToLTS (cast driverId) \$"
  echo "    LTSSync.emptyUpdate {LTSSync.blocked = LTSSync.Set isBlocked}"
  exit 1
else
  echo "PASS: All pool-relevant DB updates have corresponding field-level LTS syncs."
  exit 0
fi
