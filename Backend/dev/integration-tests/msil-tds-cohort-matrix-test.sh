#!/usr/bin/env bash
# MSIL TDS Cohort Matrix Test
#
# Exhaustively verifies the materialized TDS rate for every cohort in the new
# spec table:
#
#   Role × PAN doc_type × PAN-Aadhaar linkage
#
# Each scenario:
#   1. Spins up a fresh fixture (person + driver_information + optional
#      fleet_owner_information + driver_pan_card + image).
#   2. Seeds an idfy_verification row.
#   3. Fires the PAN-Aadhaar-link webhook at driver-app.
#   4. Reads the materialized tds_rate from the role-appropriate table.
#   5. Asserts the expected rate.
#
# The new cohort matrix (taxConfig rates: linked=0.001, not_linked=0.05, invalid=0.20):
#
#   ┌────────────────┬──────────────┬─────────────────────┬──────────┬──────────────────────────┐
#   │ Role           │ PAN doc_type │ Aadhaar linkage     │ Expected │ Materialization target   │
#   ├────────────────┼──────────────┼─────────────────────┼──────────┼──────────────────────────┤
#   │ DRIVER         │ INDIVIDUAL   │ linked              │   0.001  │ driver_information       │
#   │ DRIVER         │ INDIVIDUAL   │ not_linked          │   0.05   │ driver_information       │
#   │ DRIVER         │ INDIVIDUAL   │ aadhaar_to_other    │   0.001  │ driver_information       │
#   │ DRIVER         │ BUSINESS     │ (any → use linked)  │   0.20   │ driver_information       │
#   │ DRIVER         │ INDIVIDUAL*  │ (any → use linked)  │   0.20   │ driver_information       │  *INVALID PAN
#   ├────────────────┼──────────────┼─────────────────────┼──────────┼──────────────────────────┤
#   │ FLEET_OWNER    │ INDIVIDUAL   │ linked              │   0.001  │ fleet_owner_information  │
#   │ FLEET_OWNER    │ INDIVIDUAL   │ not_linked          │   0.05   │ fleet_owner_information  │
#   │ FLEET_OWNER    │ INDIVIDUAL   │ aadhaar_to_other    │   0.001  │ fleet_owner_information  │
#   │ FLEET_OWNER    │ BUSINESS     │ (any → use linked)  │   0.20   │ fleet_owner_information  │
#   │ FLEET_OWNER    │ INDIVIDUAL*  │ (any → use linked)  │   0.20   │ fleet_owner_information  │  *INVALID PAN
#   ├────────────────┼──────────────┼─────────────────────┼──────────┼──────────────────────────┤
#   │ FLEET_BUSINESS │ INDIVIDUAL   │ linked              │   0.001  │ fleet_owner_information  │
#   │ FLEET_BUSINESS │ INDIVIDUAL   │ not_linked          │   0.05   │ fleet_owner_information  │
#   │ FLEET_BUSINESS │ INDIVIDUAL   │ aadhaar_to_other    │   0.001  │ fleet_owner_information  │
#   │ FLEET_BUSINESS │ BUSINESS     │ (any → use linked)  │   0.001  │ fleet_owner_information  │  ← only legit Business-PAN cohort
#   │ FLEET_BUSINESS │ INDIVIDUAL*  │ (any → use linked)  │   0.20   │ fleet_owner_information  │  *INVALID PAN
#   └────────────────┴──────────────┴─────────────────────┴──────────┴──────────────────────────┘
#
# Prerequisites (see msil-tds-integration-test.sh for full runbook):
#   1. , run-mobility-stack-full running.
#   2. transporter_config.tax_config for MSIL MOC has rideGst/subscriptionGst/
#      invalidPanTdsRate/panNotLinkedTdsRate/section194OThreshold.
#   3. merchant_service_config.config_json.secret for Verification_Idfy is the
#      passetto cipher for "xxxxxxx".
#   4. Driver-app restarted after any tax_config / secret patch (TransporterConfig
#      is in-process cached).

set -euo pipefail

# ─────────────────────────────────────────────────────────────────────────────
# Config
# ─────────────────────────────────────────────────────────────────────────────
PG_HOST="${PG_HOST:-127.0.0.1}"
PG_PORT="${PG_PORT:-5434}"
PG_DB="${PG_DB:-atlas_dev}"
PG_USER="${PG_USER:-ayush}"
PG_SCHEMA="atlas_driver_offer_bpp"

DRIVER_APP_URL="${DRIVER_APP_URL:-http://localhost:8016}"
IDFY_WEBHOOK_SECRET="${IDFY_WEBHOOK_SECRET:-xxxxxxx}"

MSIL_MERCHANT_ID="${MSIL_MERCHANT_ID:-7f7896dd-787e-4a0b-8675-e9e6fe93bb8f}"
MSIL_MERCHANT_SHORT_ID="${MSIL_MERCHANT_SHORT_ID:-NAMMA_YATRI_PARTNER}"
MSIL_DELHI_MOC_ID="${MSIL_DELHI_MOC_ID:-f067bccf-5b34-fb51-a5a3-9d6fa6baac26}"
MSIL_DELHI_CITY="${MSIL_DELHI_CITY:-Bangalore}"

RATE_LINKED="0.001"
RATE_NOT_LINKED="0.05"
RATE_INVALID="0.20"

# Colour helpers
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[0;33m'; CYAN='\033[0;36m'; NC='\033[0m'

TS=$(date +%s)
CREATED_IDS=()

PASS_COUNT=0
FAIL_COUNT=0
FAILED_CASES=()

# ─────────────────────────────────────────────────────────────────────────────
# Helpers
# ─────────────────────────────────────────────────────────────────────────────
psql_exec() { PGPASSWORD="${PG_PASSWORD:-}" psql -h "$PG_HOST" -p "$PG_PORT" -d "$PG_DB" -U "$PG_USER" -q -c "$1" >/dev/null; }
psql_q()    { PGPASSWORD="${PG_PASSWORD:-}" psql -h "$PG_HOST" -p "$PG_PORT" -d "$PG_DB" -U "$PG_USER" -At -c "$1"; }

log()     { echo -e "${CYAN}[$(date +%H:%M:%S)]${NC} $*"; }
section() { echo; echo -e "${CYAN}── $* ──${NC}"; }

assert_eq_num() {
  local actual="$1" expected="$2" case_id="$3" what="$4"
  if awk -v a="$actual" -v e="$expected" 'BEGIN { exit (a+0 == e+0) ? 0 : 1 }'; then
    echo -e "  ${GREEN}PASS${NC} [${case_id}] ${what}: ${actual}"
    PASS_COUNT=$((PASS_COUNT+1))
  else
    echo -e "  ${RED}FAIL${NC} [${case_id}] ${what}: expected ${expected}, got ${actual}"
    FAIL_COUNT=$((FAIL_COUNT+1))
    FAILED_CASES+=("${case_id}")
  fi
}

assert_eq() {
  local actual="$1" expected="$2" case_id="$3" what="$4"
  if [[ "$actual" == "$expected" ]]; then
    echo -e "  ${GREEN}PASS${NC} [${case_id}] ${what}: ${actual}"
    PASS_COUNT=$((PASS_COUNT+1))
  else
    echo -e "  ${RED}FAIL${NC} [${case_id}] ${what}: expected ${expected}, got ${actual}"
    FAIL_COUNT=$((FAIL_COUNT+1))
    FAILED_CASES+=("${case_id}")
  fi
}

cleanup() {
  if [[ "${KEEP_FIXTURES:-0}" == "1" ]]; then
    log "KEEP_FIXTURES=1 — leaving rows in place. Created driver_ids:"
    for did in "${CREATED_IDS[@]+"${CREATED_IDS[@]}"}"; do echo "    $did"; done
    return
  fi
  log "Cleanup"
  for did in "${CREATED_IDS[@]+"${CREATED_IDS[@]}"}"; do
    psql_exec "
      DELETE FROM ${PG_SCHEMA}.idfy_verification WHERE driver_id = '${did}';
      DELETE FROM ${PG_SCHEMA}.driver_pan_card WHERE driver_id = '${did}';
      DELETE FROM ${PG_SCHEMA}.fleet_owner_information WHERE fleet_owner_person_id = '${did}';
      DELETE FROM ${PG_SCHEMA}.driver_stats WHERE driver_id = '${did}';
      DELETE FROM ${PG_SCHEMA}.driver_information WHERE driver_id = '${did}';
      DELETE FROM ${PG_SCHEMA}.image WHERE person_id = '${did}';
      DELETE FROM ${PG_SCHEMA}.person WHERE id = '${did}';
    " 2>/dev/null || true
  done
}
trap cleanup EXIT

# Insert person + role-appropriate side-table row + PAN card + image.
# Args: $1 case_id, $2 role, $3 doc_type, $4 verification_status,
#       $5 merchant_id (optional, defaults to MSIL), $6 moc_id (optional)
setup_fixture() {
  local case_id="$1" role="$2" doc_type="$3" verif="$4"
  local mid="${5:-$MSIL_MERCHANT_ID}" moc="${6:-$MSIL_DELHI_MOC_ID}"
  local did="t-${case_id}-${TS}"
  local pan_id="pan-${did}"
  local image_id="img-${did}"

  psql_exec "
    INSERT INTO ${PG_SCHEMA}.person
      (id, merchant_id, merchant_operating_city_id, role, first_name, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier_type, gender, language, is_new, onboarded_from_dashboard, total_earned_coins, used_coins, created_at, updated_at)
    VALUES
      ('${did}', '${mid}', '${moc}', '${role}', 'T-${case_id}', 'dummy-enc', decode(md5('${did}'), 'hex'), '+91', 'MOBILENUMBER', 'UNKNOWN', 'ENGLISH', false, false, 0, 0, now(), now());
  "
  psql_exec "
    INSERT INTO ${PG_SCHEMA}.driver_information
      (driver_id, active, blocked, verified, aadhaar_verified, on_ride, can_downgrade_to_sedan, can_downgrade_to_hatchback, can_downgrade_to_taxi, enabled, payment_pending, subscribed, num_of_locks, ac_restriction_lift_count, created_at, updated_at)
    VALUES
      ('${did}', true, false, true, true, false, false, false, false, true, false, true, 0, 0, now(), now());
  "
  if [[ "$role" == "FLEET_OWNER" || "$role" == "FLEET_BUSINESS" ]]; then
    local fleet_type="INDIVIDUAL_FLEET"
    [[ "$role" == "FLEET_BUSINESS" ]] && fleet_type="BUSINESS_FLEET"
    psql_exec "
      INSERT INTO ${PG_SCHEMA}.fleet_owner_information
        (fleet_owner_person_id, fleet_type, merchant_id, merchant_operating_city_id, enabled, blocked, verified, created_at, updated_at)
      VALUES
        ('${did}', '${fleet_type}', '${mid}', '${moc}', true, false, true, now(), now());
    "
  fi
  psql_exec "
    INSERT INTO ${PG_SCHEMA}.image
      (id, person_id, image_type, s3_path, created_at, updated_at, merchant_id)
    VALUES
      ('${image_id}', '${did}', 'PanCard', 's3://test', now(), now(), '${mid}');
  "
  psql_exec "
    INSERT INTO ${PG_SCHEMA}.driver_pan_card
      (id, driver_id, document_image_id1, doc_type, pan_card_number_encrypted, pan_card_number_hash, verification_status, verified_by, driver_name, failed_rules, consent, consent_timestamp, merchant_id, merchant_operating_city_id, created_at, updated_at)
    VALUES
      ('${pan_id}', '${did}', '${image_id}', '${doc_type}', 'enc-pan', decode(md5('${pan_id}'), 'hex'), '${verif}', 'FRONTEND_SDK', 'T-${case_id}', ARRAY[]::text[], true, now(), '${mid}', '${moc}', now(), now());
  "

  CREATED_IDS+=("$did")
  echo "$did"
}

# Args: $1 driver_id, $2 is_linked (true/false), $3 message (JSON-quoted),
#       $4 status (JSON-quoted), $5 merchant_id (opt), $6 moc_id (opt),
#       $7 merchant_short_id (opt, for URL), $8 city (opt, for URL)
fire_webhook() {
  local did="$1" is_linked="$2" message="$3" status="$4"
  local mid="${5:-$MSIL_MERCHANT_ID}" moc="${6:-$MSIL_DELHI_MOC_ID}"
  local short_id="${7:-$MSIL_MERCHANT_SHORT_ID}" city="${8:-$MSIL_DELHI_CITY}"
  local request_id="req-${did}-$(date +%s%N)"
  local row_id; row_id=$(uuidgen | tr 'A-Z' 'a-z')
  local image_id; image_id=$(psql_q "SELECT id FROM ${PG_SCHEMA}.image WHERE person_id = '${did}' LIMIT 1;")

  psql_exec "
    INSERT INTO ${PG_SCHEMA}.idfy_verification
      (id, driver_id, merchant_id, merchant_operating_city_id, request_id, status, doc_type, document_image_id1, document_number_encrypted, document_number_hash, image_extraction_validation, created_at, updated_at)
    VALUES
      ('${row_id}', '${did}', '${mid}', '${moc}', '${request_id}', 'pending', 'PanAadhaarLinkage', '${image_id}', 'enc-pan', decode(md5('${request_id}'), 'hex'), 'Skipped', now(), now());
  "

  local now; now=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
  local payload; payload=$(cat <<EOF
{
  "action": "verify_with_source",
  "completed_at": "$now",
  "created_at": "$now",
  "group_id": "test-group-${request_id}",
  "request_id": "$request_id",
  "status": "completed",
  "task_id": "test-task-${request_id}",
  "type": "pan_aadhaar_link",
  "result": {
    "source_output": {
      "is_linked": $is_linked,
      "message": $message,
      "status": $status
    }
  }
}
EOF
)
  curl -sS -X POST "${DRIVER_APP_URL}/${short_id}/${city}/service/idfy/verification" \
    -H "Authorization: ${IDFY_WEBHOOK_SECRET}" \
    -H 'Content-Type: application/json' \
    -d "$payload" >/dev/null
  sleep 1
}

# Args: $1 driver_id, $2 role
read_materialized_rate() {
  local did="$1" role="$2"
  if [[ "$role" == "FLEET_OWNER" || "$role" == "FLEET_BUSINESS" ]]; then
    psql_q "SELECT COALESCE(tds_rate::text, 'NULL') FROM ${PG_SCHEMA}.fleet_owner_information WHERE fleet_owner_person_id = '${did}';"
  else
    psql_q "SELECT COALESCE(tds_rate::text, 'NULL') FROM ${PG_SCHEMA}.driver_information WHERE driver_id = '${did}';"
  fi
}

# One test case.
# Args: $1 case_id, $2 role, $3 doc_type, $4 verification_status, $5 is_linked,
#       $6 message (JSON), $7 status (JSON), $8 expected_rate, $9 description
run_case() {
  local case_id="$1" role="$2" doc_type="$3" verif="$4"
  local is_linked="$5" message="$6" status="$7" expected="$8" desc="$9"
  log "  ${case_id}: ${desc}"
  local did; did=$(setup_fixture "$case_id" "$role" "$doc_type" "$verif")
  fire_webhook "$did" "$is_linked" "$message" "$status"
  local actual; actual=$(read_materialized_rate "$did" "$role")
  assert_eq_num "$actual" "$expected" "$case_id" "${role}/${doc_type}/${verif}"
}

# Canonical linkage payloads
LINKED_MSG='"PAN & Aadhaar are linked"';     LINKED_STATUS='"id_found"'
NOT_LINKED_MSG='"PAN and Aadhaar are not linked"'; NOT_LINKED_STATUS='"id_not_found"'
OTHER_PAN_MSG='"Aadhaar linked to some other PAN"'; OTHER_PAN_STATUS='"id_found"'

# ─────────────────────────────────────────────────────────────────────────────
# Scenarios
# ─────────────────────────────────────────────────────────────────────────────
log "MSIL TDS cohort matrix — run id ${TS}"

section "DRIVER role"
run_case D1 DRIVER INDIVIDUAL VALID 'true'  "$LINKED_MSG"     "$LINKED_STATUS"     "$RATE_LINKED"     "Individual + linked → 0.1%"
run_case D2 DRIVER INDIVIDUAL VALID 'false' "$NOT_LINKED_MSG" "$NOT_LINKED_STATUS" "$RATE_NOT_LINKED" "Individual + not linked → 5%"
run_case D3 DRIVER INDIVIDUAL VALID 'false' "$OTHER_PAN_MSG"  "$OTHER_PAN_STATUS"  "$RATE_LINKED"     "Individual + Aadhaar-to-other-PAN → 0.1% (any-Aadhaar rule)"
run_case D4 DRIVER BUSINESS   VALID 'true'  "$LINKED_MSG"     "$LINKED_STATUS"     "$RATE_INVALID"    "Business PAN on DRIVER → 20% (defensive)"
run_case D5 DRIVER INDIVIDUAL MANUAL_VERIFICATION_REQUIRED 'true' "$LINKED_MSG" "$LINKED_STATUS" "$RATE_INVALID" "Invalid PAN → 20%"

section "FLEET_OWNER role (Individual Fleet)"
run_case F1 FLEET_OWNER INDIVIDUAL VALID 'true'  "$LINKED_MSG"     "$LINKED_STATUS"     "$RATE_LINKED"     "Individual + linked → 0.1%"
run_case F2 FLEET_OWNER INDIVIDUAL VALID 'false' "$NOT_LINKED_MSG" "$NOT_LINKED_STATUS" "$RATE_NOT_LINKED" "Individual + not linked → 5%"
run_case F3 FLEET_OWNER INDIVIDUAL VALID 'false' "$OTHER_PAN_MSG"  "$OTHER_PAN_STATUS"  "$RATE_LINKED"     "Individual + Aadhaar-to-other-PAN → 0.1%"
run_case F4 FLEET_OWNER BUSINESS   VALID 'true'  "$LINKED_MSG"     "$LINKED_STATUS"     "$RATE_INVALID"    "Business PAN on Individual Fleet → 20% (defensive)"
run_case F5 FLEET_OWNER INDIVIDUAL MANUAL_VERIFICATION_REQUIRED 'true' "$LINKED_MSG" "$LINKED_STATUS" "$RATE_INVALID" "Invalid PAN → 20%"

section "FLEET_BUSINESS role (Business Fleet)"
run_case B1 FLEET_BUSINESS INDIVIDUAL VALID 'true'  "$LINKED_MSG"     "$LINKED_STATUS"     "$RATE_LINKED"     "Individual + linked → 0.1%"
run_case B2 FLEET_BUSINESS INDIVIDUAL VALID 'false' "$NOT_LINKED_MSG" "$NOT_LINKED_STATUS" "$RATE_NOT_LINKED" "Individual + not linked → 5%"
run_case B3 FLEET_BUSINESS INDIVIDUAL VALID 'false' "$OTHER_PAN_MSG"  "$OTHER_PAN_STATUS"  "$RATE_LINKED"     "Individual + Aadhaar-to-other-PAN → 0.1%"
run_case B4 FLEET_BUSINESS BUSINESS   VALID 'true'  "$LINKED_MSG"     "$LINKED_STATUS"     "$RATE_LINKED"     "Business PAN on Business Fleet → 0.1% (legitimate cohort)"
run_case B5 FLEET_BUSINESS INDIVIDUAL MANUAL_VERIFICATION_REQUIRED 'true' "$LINKED_MSG" "$LINKED_STATUS" "$RATE_INVALID" "Invalid PAN → 20%"

section "Non-MSIL regression"
# N1: a non-MSIL merchant (panNotLinkedTdsRate IS NULL on its tax_config) should
# see no materialization — tds_rate stays NULL — even after a "linked" webhook.
# This guards against spec-mode behavior leaking to merchants that didn't opt in.
NONMSIL_ROW=$(psql_q "
  SELECT m.id, m.short_id, moc.id, moc.city
  FROM ${PG_SCHEMA}.merchant m
  JOIN ${PG_SCHEMA}.merchant_operating_city moc ON moc.merchant_id = m.id
  JOIN ${PG_SCHEMA}.transporter_config tc ON tc.merchant_operating_city_id = moc.id
  WHERE m.short_id <> '${MSIL_MERCHANT_SHORT_ID}'
    AND (tc.tax_config ->> 'panNotLinkedTdsRate') IS NULL
  LIMIT 1;
")
if [[ -z "$NONMSIL_ROW" ]]; then
  echo -e "  ${YELLOW}SKIP${NC} [N1] no non-MSIL merchant with panNotLinkedTdsRate=NULL found in local DB"
else
  IFS=$'|' read -r NM_MID NM_SHORT NM_MOC NM_CITY <<< "$NONMSIL_ROW"
  log "  N1: non-MSIL DRIVER (${NM_SHORT}, ${NM_CITY}) — tds_rate stays NULL"
  NM_DID=$(setup_fixture N1 DRIVER INDIVIDUAL VALID "$NM_MID" "$NM_MOC")
  fire_webhook "$NM_DID" 'true' "$LINKED_MSG" "$LINKED_STATUS" "$NM_MID" "$NM_MOC" "$NM_SHORT" "$NM_CITY"
  NM_RATE=$(psql_q "SELECT COALESCE(tds_rate::text, 'NULL') FROM ${PG_SCHEMA}.driver_information WHERE driver_id = '${NM_DID}';")
  assert_eq "$NM_RATE" "NULL" "N1" "non-MSIL driver_information.tds_rate stays NULL"
fi

# ─────────────────────────────────────────────────────────────────────────────
# Summary
# ─────────────────────────────────────────────────────────────────────────────
echo
TOTAL=$((PASS_COUNT + FAIL_COUNT))
if [[ $FAIL_COUNT -eq 0 ]]; then
  echo -e "${GREEN}All ${TOTAL} cases passed.${NC}"
else
  echo -e "${RED}${FAIL_COUNT}/${TOTAL} cases failed:${NC} ${FAILED_CASES[*]}"
  exit 1
fi
