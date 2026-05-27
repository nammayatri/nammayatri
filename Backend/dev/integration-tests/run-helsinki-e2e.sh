#!/usr/bin/env bash
# =============================================================================
# Helsinki E2E Test Runner (SKELETON / Work in progress)
# =============================================================================
# Chains: config-sync → onboarding → ride combos × 8 → invoice verification.
# Each newman step exports the env file so generated IDs (fleet_owner_id,
# driver_id, vehicle_id, ride_id, ...) flow forward into the next collection.
#
# This is the WIP scaffold for the foundational slice. Sections marked TODO are
# not implemented yet — they will be filled in as Phase A.04+, B, C are built.
#
# Usage:
#   ./run-helsinki-e2e.sh                 # full chain
#   ./run-helsinki-e2e.sh --skip-sync     # assume config-sync already ran
#   ./run-helsinki-e2e.sh --only onboarding   # stop after Phase A
#   ./run-helsinki-e2e.sh --dry-run       # echo commands only
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COLLECTIONS_DIR="${SCRIPT_DIR}/collections"
ONBOARDING_DIR="${COLLECTIONS_DIR}/HelsinkiOnboarding"
RIDES_DIR="${COLLECTIONS_DIR}/InternationalRideBookingFlow"
INVOICES_DIR="${COLLECTIONS_DIR}/HelsinkiInvoices"            # TODO Phase C

CONFIG_SYNC_DIR="${SCRIPT_DIR}/../config-sync"
FEATURE_MIGRATIONS_DIR="${SCRIPT_DIR}/../feature-migrations"
LOCAL_SEED_DIR="${SCRIPT_DIR}/../local-testing-data"

# Run-scoped env file lives in /tmp so re-runs don't dirty the repo
RUN_TS="$(date +%Y%m%d-%H%M%S)"
ENV_BASE="${ONBOARDING_DIR}/Local/Local_BF_Helsinki.postman_environment.json"
ENV_RUN="/tmp/helsinki-e2e-${RUN_TS}.postman_environment.json"
REPORT_DIR="${SCRIPT_DIR}/reports/helsinki-e2e-${RUN_TS}"

SKIP_SYNC=0
ONLY=""
DRY_RUN=0

# ---------- arg parsing ------------------------------------------------------
while [[ $# -gt 0 ]]; do
  case "$1" in
    --skip-sync) SKIP_SYNC=1; shift ;;
    --only)      ONLY="$2"; shift 2 ;;
    --dry-run)   DRY_RUN=1; shift ;;
    -h|--help)
      grep -E '^#( |!|=)' "$0" | sed -E 's/^# ?//'
      exit 0 ;;
    *) echo "Unknown arg: $1" >&2; exit 2 ;;
  esac
done

run() {
  echo "+ $*"
  if [[ "${DRY_RUN}" -eq 0 ]]; then "$@"; fi
}

newman_step() {
  local name="$1" collection="$2"
  local exported="${REPORT_DIR}/env-after-${name}.json"
  echo ""
  echo "─── ${name} ─────────────────────────────────────────────────────────"
  run mkdir -p "${REPORT_DIR}"
  run newman run "${collection}" \
    -e "${ENV_RUN}" \
    --export-environment "${exported}" \
    --reporters cli,json \
    --reporter-json-export "${REPORT_DIR}/${name}.json" \
    --bail \
    --timeout-request 60000
  # subsequent steps read from the freshly-exported env
  ENV_RUN="${exported}"
}

# Non-gating variant: runs the collection but does not halt the chain on failure
# and does not propagate the exported env (used for FLEET-role probes).
newman_step_probe() {
  local name="$1" collection="$2"
  echo ""
  echo "─── ${name} (probe, non-gating) ─────────────────────────────────────"
  run mkdir -p "${REPORT_DIR}"
  if [[ "${DRY_RUN}" -eq 0 ]]; then
    newman run "${collection}" \
      -e "${ENV_RUN}" \
      --reporters cli,json \
      --reporter-json-export "${REPORT_DIR}/${name}.json" \
      --timeout-request 60000 \
      || echo "  (probe failed — informational only)"
  else
    echo "+ newman run ${collection} ..."
  fi
}

# ---------- Phase 0: config sync + seed --------------------------------------
if [[ "${SKIP_SYNC}" -eq 0 ]]; then
  echo "═══ Phase 0: config-sync (master → local) + seeds ═══"
  run python "${CONFIG_SYNC_DIR}/config_transfer.py" \
    --from prod_international --to local
  # local-testing-data SQLs are idempotent; rerun to refresh JUSPAY_ADMIN seed
  for sql in provider-dashboard.sql rider-dashboard.sql; do
    run psql "postgresql://atlas_driver_offer_bpp_user:atlas@localhost:5434/atlas_dev" \
      -f "${LOCAL_SEED_DIR}/${sql}"
  done
  # Helsinki-specific feature flags that aren't (yet) in master config — these
  # toggle dev-only behaviors (cancellation fee logic, invoice generation flag,
  # VAT rates, online payment offers). Master is the source of truth for tables
  # (merchant_operating_city, document_verification_config, merchant_payment_method,
  # supported_operating_cities, etc.) — DO NOT add local seeds for those.
  for sql in \
    0001-helsinki-online-payment-offers.sql \
    0003-enable-invoice-generation-for-helsinki-delhi.sql \
    0005-enable-cancellation-fee-helsinki.sql \
    0007-helsinki-vat-config.sql \
    0011-helsinki-cancellation-and-invoice-extras.sql \
    0012-helsinki-driver-pool-config.sql ; do
    run psql "postgresql://atlas_driver_offer_bpp_user:atlas@localhost:5434/atlas_dev" \
      -f "${FEATURE_MIGRATIONS_DIR}/${sql}"
  done
else
  echo "═══ Phase 0: SKIPPED (--skip-sync) ═══"
fi

# Initialize the run env from the base
run cp "${ENV_BASE}" "${ENV_RUN}"
run mkdir -p "${REPORT_DIR}"
echo "Run env: ${ENV_RUN}"
echo "Reports : ${REPORT_DIR}"

# ---------- Phase A: Onboarding ---------------------------------------------
echo ""
echo "═══ Phase A: Onboarding ═══"
newman_step       "A01-FleetOwnerOnboarding" "${ONBOARDING_DIR}/01-FleetOwnerOnboarding.json"
newman_step       "A02-StripeOnboarding"     "${ONBOARDING_DIR}/02-StripeOnboarding.json"
newman_step       "A03-AddDriver-Admin"      "${ONBOARDING_DIR}/03-AddDriver_AdminPath.json"
newman_step_probe "A03-AddDriver-FleetProbe" "${ONBOARDING_DIR}/03-AddDriver_FleetPath.json"
newman_step       "A04-AddVehicle-Admin"     "${ONBOARDING_DIR}/04-AddVehicle_AdminPath.json"
newman_step_probe "A04-AddVehicle-FleetProbe" "${ONBOARDING_DIR}/04-AddVehicle_FleetPath.json"

if [[ "${ONLY}" == "onboarding" ]]; then
  echo "═══ Stopped after Phase A (--only onboarding) ═══"
  exit 0
fi

# ---------- Phase B: 8 Ride combos ------------------------------------------
echo ""
echo "═══ Phase B: Ride combos (8) ═══"
# 8-combo matrix: Cash/Online × Cancel/Complete × NoDiscount/Discount.
# Existing 5/8 + 3 new files added in Phase B.
RIDE_COMBOS=(
  "01-StripeRideFlow"                              # Online + Complete + NoDiscount
  "02-StripeRideUserCancellation"                  # Online + Cancel   + NoDiscount
  "05-OnlineRideWithDiscountOffer"                 # Online + Complete + Discount
  "06-CashRideFlow"                                # Cash   + Complete + NoDiscount
  "07-CashRideWithDiscountOffer"                   # Cash   + Complete + Discount
  "09-OnlineRideUserCancellationWithDiscountOffer" # Online + Cancel   + Discount
  "10-CashRideUserCancellation"                    # Cash   + Cancel   + NoDiscount
  "11-CashRideUserCancellationWithDiscountOffer"   # Cash   + Cancel   + Discount
  "12-EditDestinationWithDiscountRecompute"        # Online + EditDest + Discount (recompute verification)
)

for combo in "${RIDE_COMBOS[@]}"; do
  newman_step "B-${combo}" "${RIDES_DIR}/${combo}.json"
  newman_step "C-${combo}-Invoices" "${INVOICES_DIR}/01-VerifyInvoices.json"
done

# ---------- Done ------------------------------------------------------------
echo ""
echo "═══ Helsinki E2E run complete ═══"
echo "Reports: ${REPORT_DIR}"
