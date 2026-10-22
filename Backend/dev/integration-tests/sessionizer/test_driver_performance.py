# /// script
# requires-python = ">=3.11,<3.14"
# dependencies = ["confluent-kafka>=2.3", "redis>=5", "psycopg[binary]>=3.1"]
# ///
"""Scenario tests for the DRIVER_PERFORMANCE consumer (performance_analytics metrics).

Produces synthetic CDC rows onto the six topics the driver drainer publishes,
in the exact drainer format (flat full row, snake_case columns, epoch-second ints,
"True"/"False" strings, every column present with nulls, HighPrecMoney in
aeson's number format, e.g. 5.0e-2):

  adob-sessionizer-searchrequestfordriver   pop-ups
  adob-sessionizer-ride                     rides
  adob-sessionizer-booking                  auto-accept flag, quote fare params
  adob-sessionizer-bookingcancellationreason
  adob-sessionizer-fareparameters           driver addition, tip (the quote's)
  adob-sessionizer-rating

plus a few driver-pool-state events for the DRIVER_POOL_STATE writer: the
consumer keys every pop-up and ride by the driver's state at the event's own
time (the state columns are part of the row key).

Every run uses fresh ids and its own city. Messages go out in four phases, and
by default the script waits for a consumer flush (Redis `perf:watermark`)
between phases, so facts that arrive before or after their ride are joined
through the consumer's Redis state, not only inside one poll batch. Timestamps
sit inside the last hour, so no state TTL interferes.

It then checks atlas_kafka.driver_analytics_5m for the run's drivers:
  1. per scenario: totals against hand-written expectations
  2. per (bucket, city, driver): against a reference model that applies the
     metric definitions to the FINAL state of every entity (written from the
     spec, independent of the Haskell code)
  3. malformed messages ({} / garbage / partial rows) are skipped without
     stalling: every later scenario still lands
  4. state rows: per (bucket, city, driver, state) against the model, where
     the state is the writer's history entry in force at the event time (SRD /
     ride created_at); a pop-up answered after the driver's state changed
     still lands in the row of its first version; drivers without writer
     history get state_known = 0

--e2e: after a real ride run, compares the newest driver's totals in
atlas_kafka.driver_performance_5m with the same metrics computed from local
Postgres.

--selfcheck: runs only the reference model on the plan (no Kafka, ClickHouse
or Redis) and asserts it agrees with the hand-written expectations and
that every message has the drainer format.

Usage (from Backend/dev/integration-tests, with the stack running):
  uv run sessionizer/test_driver_performance.py              # ~5 min: 4 flush waits
  uv run sessionizer/test_driver_performance.py --fast       # no flush between phases
  uv run sessionizer/test_driver_performance.py --e2e        # after a ride run
  uv run sessionizer/test_driver_performance.py --selfcheck

Exits 1 if any check fails.
"""

import argparse
import json
import os
import sys
import time
import urllib.parse
import urllib.request
import uuid
import zlib
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import UTC, datetime
from decimal import Decimal

T = {
    "srd": "adob-sessionizer-searchrequestfordriver",
    "ride": "adob-sessionizer-ride",
    "booking": "adob-sessionizer-booking",
    "bcr": "adob-sessionizer-bookingcancellationreason",
    "fp": "adob-sessionizer-fareparameters",
    "rating": "adob-sessionizer-rating",
}
TABLE_OF = {v: k for k, v in T.items()}
STATE_TOPIC = "driver-pool-state"                  # the DRIVER_POOL_STATE writer's input, not a CDC table
GROUP = "driver-performance"
WATERMARK_KEY = "perf:watermark"
SENTINEL_DRIVER = "drivers-not-found"

COUNTS = ["popups_sent", "popups_accepted", "popups_rejected", "popups_neglected", "response_secs_sum",
          "auto_accept_search_requests", "total_bookings", "finished_rides",
          "inprogress_rides", "cancelled_rides", "rides_cancelled_by_driver", "rides_cancelled_by_user",
          "auto_accept_bookings", "auto_accept_finished_rides", "auto_accept_cancelled_rides",
          "auto_accept_rides_cancelled_by_driver", "auto_accept_rides_cancelled_by_user",
          "rides_with_rating", "rating_sum"]
MONEY = ["driver_addition", "customer_tip", "gross_earnings"]
MEASURES = COUNTS + MONEY

RIDE_RANK = {"NEW": 0, "UPCOMING": 0, "INPROGRESS": 1, "COMPLETED": 2, "CANCELLED": 2}
BUCKET_S = 300

# Fields a row must carry for the consumer to decode it; the model skips rows without them.
REQUIRED = {
    "srd": ["id", "driver_id", "created_at"],
    "ride": ["id", "booking_id", "driver_id", "status", "created_at"],
    "booking": ["id", "fare_parameters_id"],
    "bcr": ["booking_id", "source"],
    "fp": ["id"],
    "rating": ["id", "ride_id", "rating_value"],
}


# ─── drainer format ──────────────────────────────────────────────────────────

def aeson_num(x) -> str:
    """A Double/HighPrecMoney as aeson prints it: integral values as integers,
    otherwise Data.Scientific's Generic format (exponent form when the decimal
    exponent is < 0 or > 7: 0.05 -> 5.0e-2, 12.5 -> 12.5)."""
    d = Decimal(repr(x)) if isinstance(x, float) else Decimal(x)
    if d == d.to_integral_value():
        return str(int(d))
    sign, digits, exp = d.normalize().as_tuple()
    ds = "".join(map(str, digits))
    e = len(ds) + exp                      # value = 0.<ds> x 10^e
    s = "-" if sign else ""
    if e < 0 or e > 7:
        return f"{s}{ds[0]}.{ds[1:] or '0'}e{e - 1}"
    return f"{s}0.{ds}" if e == 0 else f"{s}{ds[:e]}.{ds[e:]}"


def enc(v) -> str:
    if isinstance(v, float):
        return aeson_num(v)
    if isinstance(v, dict):
        return "{" + ",".join(f"{json.dumps(k)}:{enc(x)}" for k, x in v.items()) + "}"
    if isinstance(v, list):
        return "[" + ",".join(enc(x) for x in v) + "]"
    return json.dumps(v)


def cdc(row: dict) -> bytes:
    # Top-level keys come out sorted (aeson KeyMap), as in the real messages.
    return enc(dict(sorted(row.items()))).encode()


def b(flag) -> str | None:
    return None if flag is None else ("True" if flag else "False")


# Every nullable column of each table (from the Beam definitions), present as null.
NULLABLE = {
    "ride": """
        backend_app_version backend_config_version billing_category cancellation_additional_info
        cancellation_charges_on_cancel cancellation_commission cancellation_consequence_row_id_if_cancelled
        cancellation_fault_rule cancellation_fault_verdict cancellation_fee_if_cancelled
        cancellation_fee_tax_if_cancelled cancellation_reason_code cancelled_by chargeable_distance
        client_bundle_version client_config_version client_manufacturer client_model_name client_os_type
        client_os_version client_id client_sdk_version cloud_type commission currency delivery_file_ids
        destination_reached_at discount_amount distance_calculation_failed distance_to_pickup distance_unit
        driver_arrival_time driver_cancellation_penalty_amount driver_cancellation_penalty_fee_id
        driver_cancellation_penalty_waived_reason driver_deviated_from_route driver_deviated_to_toll_route
        driver_go_home_request_id driver_gps_turned_off enable_frequent_location_updates enable_otp_less_ride
        end_odometer_reading_file_id end_odometer_reading_value end_otp estimated_end_time_range_end
        estimated_end_time_range_start estimated_toll_charges estimated_toll_ids estimated_toll_names fare
        fare_amount fare_parameters_id final_fare_policy_id fleet_owner_id has_stops insured_amount
        is_advance_booking is_air_conditioned is_driver_special_loc_warrior is_free_ride is_insured
        is_pet_ride is_pickup_or_destination_edited merchant_id merchant_operating_city_id number_of_deviation
        number_of_osrm_snap_to_road_calls number_of_self_tuned number_of_snap_to_road_calls online_payment
        passed_through_destination payment_charge payment_charge_bearer pickup_behaviour pickup_dark_seconds
        pickup_distance_calculation_failed pickup_drop_outside_of_threshold pickup_fault_seconds
        previous_ride_trip_end_lat previous_ride_trip_end_lon previous_ride_trip_end_time react_bundle_version
        recompute_reason referral_flag_reason return_otp return_started_at ride_ended_by ride_tags
        scheduled_acceptance_mode sos_id start_odometer_reading_file_id start_odometer_reading_value
        subscription_purchase_ids tip_amount toll_charges toll_confidence toll_ids toll_names trip_category
        trip_end_lat trip_end_lon trip_end_time trip_start_lat trip_start_lon trip_start_time
        ui_distance_calculation_with_accuracy ui_distance_calculation_without_accuracy
        vehicle_service_tier_air_conditioned vehicle_service_tier_name vehicle_service_tier_seating_capacity
        vehicle_variant""",
    "booking": """
        add_on_data area bap_city bap_country billing_category booking_deposit business_email_domain
        cancellation_commission coins_rewarded_on_gold_tier_ride commission config_in_experiment_versions
        currency customer_language disability_tag discount_amount display_booking_id distance_to_pickup
        distance_unit dq_duration_to_pickup dynamic_pricing_logic_version email_domain estimate_id
        estimated_congestion_charge estimated_distance estimated_duration estimated_static_duration
        exotel_declined_call_status_receiving_time fare_settlement_type finance_invoice_id from_loc_geohash
        from_location_id has_stops initiated_as insured_amount is_air_conditioned is_auto_accepted
        is_dashboard_request is_insured is_pet_ride is_pickup_or_destination_edited is_referred_ride
        is_safety_plus is_scheduled ledger_write_mode max_estimated_distance merchant_operating_city_id
        number_of_luggages number_share_consent parcel_quantity parcel_type payment_charge
        payment_charge_bearer payment_id payment_instrument payment_method_id payment_mode payment_url
        pickup_gate_id preference_match_score receiver_id receiver_name receiver_primary_exophone
        reconciliation_status return_time rider_id rider_name round_trip search_try_id sender_id sender_name
        sender_primary_exophone special_location_name special_location_tag special_zone_otp_code
        stop_location_id to_loc_geohash to_location_id toll_charges toll_ids toll_names trip_category
        vehicle_service_tier_air_conditioned vehicle_service_tier_name vehicle_service_tier_seating_capacity""",
    "bcr": """
        additional_info created_at distance_unit driver_cancellation_location_lat
        driver_cancellation_location_lon driver_dist_to_pickup driver_id merchant_id
        merchant_operating_city_id ondc_cancellation_reason_id reason_code ride_id updated_at""",
    "fp": """
        driver_selected_fare customer_extra_fee negative_fare_adjustment waiting_charge ride_extra_time_fare
        night_shift_charge base_fare_amount pet_charges driver_allowance airport_convenience_fee
        business_discount personal_discount should_apply_business_discount should_apply_personal_discount
        priority_charges driver_selected_fare_amount customer_extra_fee_amount negative_fare_adjustment_amount
        waiting_charge_amount ride_extra_time_fare_amount night_shift_charge_amount
        night_shift_rate_if_applies service_charge stop_charges service_charge_amount parking_charge
        govt_charges govt_charges_amount customer_cancellation_dues toll_charges congestion_charge
        congestion_charge_via_dp congestion_charge_amount insurance_charge card_charge_on_fare
        fixed_card_charge luggage_charge return_fee_charge booth_charge scheduling_charge platform_fee sgst
        cgst tds_amount tds_rate tds_processed_at platform_fee_charges_by conditional_charges
        customer_gate_fee_items currency driver_cancellation_not_allowed updated_at merchant_id
        merchant_operating_city_id payment_processing_fee payment_processing_fee_vat is_vat_tax_type
        commission discount_applicable_ride_fare_tax_exclusive discount_applicable_ride_fare_tax
        non_discount_applicable_ride_fare_tax_exclusive non_discount_applicable_ride_fare_tax
        toll_fare_tax_exclusive toll_vat cancellation_fee_tax_exclusive cancellation_tax
        parking_charge_tax_exclusive parking_charge_tax fare_settlement_type negotiated_fare_delta
        add_on_charges buffered_fare""",
    "rating": """
        feedback_details is_favourite is_safe issue_id media_id was_offered_assistance merchant_id
        merchant_operating_city_id""",
}

# A real SRD message from the local app (KV-disabled table, app's direct push path).
SRD_TEMPLATE = {
    "acceptance_ratio": None, "actual_distance_to_pickup": 0, "air_conditioned": None,
    "auto_assign_max_pickup_distance": None, "backend_app_version": "DEV", "backend_config_version": None,
    "base_fare": 151, "base_fare_amount": 151.084, "batch_number": 0, "batching_mode": "OFF",
    "cancellation_ratio": None, "client_bundle_version": None, "client_config_version": None,
    "client_manufacturer": None, "client_model_name": None, "client_os_type": None, "client_os_version": None,
    "client_sdk_version": None, "coins_rewarded_on_gold_tier_ride": None, "commission_charges": None,
    "conditional_charges": "[]", "created_at": 0, "currency": "INR", "customer_cancellation_dues": 0,
    "customer_tags": {}, "distance_unit": "Meter", "driver_available_time": None,
    "driver_cancellation_not_allowed": None, "driver_default_step_fee": None,
    "driver_default_step_fee_amount": None, "driver_id": "", "driver_max_extra_fee": None,
    "driver_max_extra_fee_amount": None, "driver_min_extra_fee": None, "driver_min_extra_fee_amount": None,
    "driver_speed": None, "driver_step_fee": None, "driver_step_fee_amount": None, "driver_tag_score": 0,
    "driver_tags": {"NormalDriver": True, "SafetyCohort": "New"}, "duration_to_pickup": 0, "estimate_id": "",
    "fleet_owner_id": None, "from_loc_geohash": "tdr1w", "go_home_request_id": None,
    "has_applicable_preferences": "False", "has_available_for_rides_tag": "False", "id": "",
    "is_auto_accepted": "False", "is_favourite": None, "is_forward_request": "False",
    "is_part_of_intelligent_pool": "False", "is_safety_plus": "False", "keep_hidden_for_seconds": 0,
    "lat": 12.935200426583755, "lon": 77.62449949979782, "merchant_id": "", "merchant_operating_city_id": "",
    "middle_stop_count": 0, "mode": "ONLINE", "notification_source": None, "parallel_search_request_count": 1,
    "parcel_quantity": None, "parcel_type": None, "pickup_zone": "False", "pooling_config_version": 9,
    "pooling_logic_version": 23, "preference_match_score": 1, "previous_drop_geo_hash": None,
    "react_bundle_version": None, "rendered_at": None, "responded_at": None, "response": None,
    "ride_frequency_score": None, "ride_request_popup_delay_duration": 0, "search_request_id": "",
    "search_request_valid_till": 0, "search_try_id": "", "start_time": 0, "status": "Active",
    "straight_line_distance_to_pickup": 0, "total_rides": 0, "trip_estimated_distance": 5753,
    "trip_estimated_duration": 358, "updated_at": 0, "upgrade_cab_request": "False", "vehicle_age": None,
    "vehicle_category": "AUTO_CATEGORY", "vehicle_number": "KA19TG5468", "vehicle_service_tier": "AUTO_RICKSHAW",
    "vehicle_service_tier_name": "Auto", "vehicle_variant": "AUTO_RICKSHAW",
}


def floor5(s: int) -> int:
    return int(s) - int(s) % BUCKET_S


def hms(s: int) -> str:
    return time.strftime("%H:%M:%S", time.gmtime(s))


# ─── plan ────────────────────────────────────────────────────────────────────

@dataclass
class Msg:
    phase: int
    topic: str
    key: str | None
    value: bytes


@dataclass
class Scenario:
    name: str
    what: str
    expect: dict = field(default_factory=dict)   # hand-written totals over the scenario's drivers
    drivers: list[str] = field(default_factory=list)


class Plan:
    def __init__(self, base_s: int, run_id: str):
        self.base = base_s
        self.run_id = run_id
        self.msgs: list[Msg] = []
        self.scenarios: list[Scenario] = []
        self.merchant = "perf-test-merchant"
        self.city = f"perf-test-{run_id}"
        self.phase = 0

    def t(self, off: int) -> int:
        return self.base + off

    def scenario(self, name, what, **expect) -> Scenario:
        sc = Scenario(name, what, expect)
        self.scenarios.append(sc)
        self.phase = 0
        return sc

    def driver(self, sc: Scenario) -> str:
        d = f"pt-{self.run_id}-{len(self.scenarios):02d}-{len(sc.drivers)}-{uuid.uuid4().hex[:6]}"
        sc.drivers.append(d)
        return d

    def raw(self, topic: str, value: bytes, key: str | None = "driver-db-sync-stream{shard-0}"):
        self.msgs.append(Msg(self.phase, topic, key, value))

    def state(self, d, ts_ms: int, **changes):
        """A driver-pool-state event, as the driver app publishes it."""
        ev = {"driverId": d, "ts": ts_ms, "changes": changes}
        self.msgs.append(Msg(self.phase, STATE_TOPIC, d, json.dumps(ev).encode()))

    def send(self, table: str, row: dict, times: int = 1):
        pk = row["booking_id"] if table == "bcr" else row["id"]
        shard = zlib.crc32(pk.encode()) % 128
        key = (f"searchRequestForDriver_id_{pk}{{shard-{shard}}}" if table == "srd"
               else f"driver-db-sync-stream{{shard-{shard}}}")
        for _ in range(times):
            self.msgs.append(Msg(self.phase, T[table], key, cdc(row)))

    def row(self, table: str, **cols) -> dict:
        r = dict.fromkeys(NULLABLE[table].split(), None)
        r.update(cols)
        return r

    # One builder per table; offsets are seconds from base. Returns the row, so a
    # later version can be sent as {**row, ...}.
    def srd(self, d, off, response=None, rendered=None, responded=None, auto=False, status="Inactive",
            city="", sid=None, times=1):
        c = self.t(off)
        r = {**SRD_TEMPLATE, "id": sid or str(uuid.uuid4()), "driver_id": d,
             "merchant_id": self.merchant, "merchant_operating_city_id": self.city if city == "" else city,
             "created_at": c, "start_time": c - 9, "search_request_valid_till": c + 60,
             "updated_at": c + max(responded or 0, rendered or 0),
             "response": response, "rendered_at": None if rendered is None else c + rendered,
             "responded_at": None if responded is None else c + responded,
             "is_auto_accepted": b(auto), "status": status,
             "search_request_id": str(uuid.uuid4()), "search_try_id": str(uuid.uuid4()),
             "estimate_id": str(uuid.uuid4())}
        self.send("srd", r, times)
        return r

    def ride(self, rid, bid, d, off, status, fare=None, toll=None, fp_id=None, upd=0, times=1):
        c = self.t(off)
        r = self.row("ride", id=rid, booking_id=bid, driver_id=d, status=status, created_at=c, updated_at=c + upd,
                     otp="4321", safety_alert_triggered="False", short_id=rid[:10],
                     tracking_url=f"http://localhost:8016/ui/driver/location/{rid}", traveled_distance=0,
                     merchant_id=self.merchant, merchant_operating_city_id=self.city, currency="INR",
                     distance_unit="Meter", vehicle_variant="AUTO_RICKSHAW", fare=fare,
                     fare_amount=None if fare is None else float(fare), toll_charges=toll,
                     fare_parameters_id=fp_id or str(uuid.uuid5(uuid.NAMESPACE_OID, rid)),
                     trip_start_time=c + 60 if status in ("INPROGRESS", "COMPLETED") else None,
                     trip_end_time=c + 600 if status == "COMPLETED" else None)
        self.send("ride", r, times)
        return r

    def dbooking(self, bid, off, auto=False, fp_id=None):
        c = self.t(off)
        r = self.row("booking", id=bid, is_auto_accepted=b(auto), fare_parameters_id=fp_id or str(uuid.uuid4()),
                     bap_id="localhost/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51",
                     bap_uri="http://localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51",
                     created_at=c, updated_at=c, start_time=c, estimated_fare=100.0,
                     primary_exophone="+917777777777", provider_id=self.merchant, quote_id=str(uuid.uuid4()),
                     status="TRIP_ASSIGNED", transaction_id=str(uuid.uuid4()), booking_type="NormalBooking",
                     vehicle_variant="AUTO_RICKSHAW", merchant_operating_city_id=self.city, currency="INR",
                     distance_unit="Meter")
        self.send("booking", r)
        return r

    def bcr(self, bid, rid, d, off, source):
        c = self.t(off)
        r = self.row("bcr", booking_id=bid, ride_id=rid, source=source, driver_id=d, merchant_id=self.merchant,
                     merchant_operating_city_id=self.city, created_at=c, updated_at=c, reason_code="OTHER",
                     distance_unit="Meter")
        self.send("bcr", r)
        return r

    def fp(self, fid, driver_selected_fare=None, customer_extra_fee=None, parking=None):
        r = self.row("fp", id=fid, base_fare=50, base_fare_amount=50.0, fare_parameters_type="Progressive",
                     driver_selected_fare=driver_selected_fare,
                     driver_selected_fare_amount=None if driver_selected_fare is None else float(driver_selected_fare),
                     customer_extra_fee=customer_extra_fee,
                     customer_extra_fee_amount=None if customer_extra_fee is None else float(customer_extra_fee),
                     parking_charge=parking, currency="INR", merchant_id=self.merchant,
                     merchant_operating_city_id=self.city, updated_at=self.t(0))
        self.send("fp", r)
        return r

    def rating(self, rtid, rid, d, off, value):
        c = self.t(off)
        r = self.row("rating", id=rtid, ride_id=rid, driver_id=d, rating_value=value, created_at=c, updated_at=c,
                     merchant_id=self.merchant, merchant_operating_city_id=self.city)
        self.send("rating", r)
        return r

def ids(n: int) -> list[str]:
    return [str(uuid.uuid4()) for _ in range(n)]


def build_plan(base_s: int, run_id: str) -> Plan:
    p = Plan(base_s, run_id)
    edge = floor5(p.t(300)) + BUCKET_S
    edge_off = edge - base_s

    sc = p.scenario("malformed", "{} / garbage / partial / non-object messages on every topic are skipped")
    for topic in T.values():
        p.raw(topic, b"{}")
        p.raw(topic, b"{not json")
        p.raw(topic, b"[]")
        p.raw(topic, b"null")
        p.raw(topic, json.dumps({"id": str(uuid.uuid4()), "booking_id": str(uuid.uuid4())}).encode())

    # ── pop-ups (SRD) ──
    sc = p.scenario("srd_accept", "Active create, then Accept 5 s after rendering",
                    popups_sent=1, popups_accepted=1, response_secs_sum=5)
    d = p.driver(sc)
    p.phase = 0
    r = p.srd(d, 60, status="Active")
    p.phase = 1
    p.srd(d, 60, "Accept", 2, 7, sid=r["id"])

    sc = p.scenario("srd_reject", "Reject 3 s after rendering",
                    popups_sent=1, popups_rejected=1, response_secs_sum=3)
    d = p.driver(sc)
    p.phase = 0
    p.srd(d, 70, "Reject", 1, 4)

    sc = p.scenario("srd_neglected", "rendered, never answered", popups_sent=1, popups_neglected=1)
    d = p.driver(sc)
    r = p.srd(d, 80, status="Active")
    p.srd(d, 80, None, 1, None, sid=r["id"])

    sc = p.scenario("srd_pulled_retraction", "Pulled counts nothing; a counted pop-up that becomes Pulled is retracted")
    d = p.driver(sc)
    p.phase = 0
    r1 = p.srd(d, 90, status="Active")                  # counted neglected, flushed ...
    r2 = p.srd(d, 95, "Reject", 1, 3)                   # counted rejected + response time, flushed ...
    p.srd(d, 100, "Pulled", 1, None)                    # never counted
    p.phase = 1
    p.srd(d, 90, "Pulled", 1, None, sid=r1["id"])       # ... then retracted
    p.srd(d, 95, "Pulled", 1, 3, sid=r2["id"])

    sc = p.scenario("srd_auto_accept", "auto-accepted Accept counts; Accept with False/null flag or auto Reject don't",
                    popups_sent=4, popups_accepted=3, popups_rejected=1, auto_accept_search_requests=1,
                    response_secs_sum=4)
    d = p.driver(sc)
    p.phase = 0
    p.srd(d, 110, "Accept", 1, 1, auto=True)            # responded == rendered: no response time
    p.srd(d, 111, "Accept", 1, 2, auto=False)
    p.srd(d, 112, "Accept", None, None, auto=None)
    p.srd(d, 113, "Reject", 1, 4, auto=True)

    sc = p.scenario("srd_response_time_rules", "response time only when rendered and responded are set and ordered",
                    popups_sent=4, popups_rejected=4, response_secs_sum=10)
    d = p.driver(sc)
    p.srd(d, 120, "Reject", None, 5)                    # never rendered
    p.srd(d, 121, "Reject", 5, 3)                       # responded before rendered
    p.srd(d, 122, "Reject", 2, 2)                       # same second
    p.srd(d, 123, "Reject", 1, 11)                      # 10 s

    sc = p.scenario("srd_sentinel_skip", f"driver_id '{SENTINEL_DRIVER}' marker rows are skipped")
    sc.drivers.append(SENTINEL_DRIVER)
    p.srd(SENTINEL_DRIVER, 130, None, 1, None)
    p.srd(SENTINEL_DRIVER, 131, "Reject", 1, 2)

    sc = p.scenario("srd_duplicates", "one Accept delivered 3x, then replayed after two flushes: counted once",
                    popups_sent=1, popups_accepted=1, response_secs_sum=4)
    d = p.driver(sc)
    p.phase = 0
    r = p.srd(d, 140, "Accept", 1, 5, times=3)
    for ph in (1, 2):
        p.phase = ph
        p.send("srd", r)
    p.phase = 0

    sc = p.scenario("srd_bucket_edge", "created exactly on a 5-min edge and 1 s before: two buckets",
                    popups_sent=2, popups_neglected=2)
    d = p.driver(sc)
    p.srd(d, edge_off, None, 1, None)
    p.srd(d, edge_off - 1, None, 1, None)

    sc = p.scenario("srd_no_city", "null merchant_operating_city_id lands under city ''",
                    popups_sent=1, popups_neglected=1)
    d = p.driver(sc)
    p.srd(d, 150, None, 1, None, city=None)

    sc = p.scenario("driver_state_stamped", "a pop-up is keyed by the driver's state at its created_at (on a ride, "
                    "2 tiers); the driver goes OFFLINE before answering, and the answer still lands in that same row",
                    popups_sent=1, popups_accepted=1, response_secs_sum=2)
    d = p.driver(sc)
    start = floor5(p.t(180))
    created = start + 150                                # the pop-up's created_at, mid-bucket
    p.phase = 0                                          # the writer has the state a flush before the pop-up
    p.state(d, start * 1000 + 10_000, mode="ONLINE", onRide=False, selectedServiceTiers=["AUTO_RICKSHAW"])
    p.state(d, start * 1000 + 100_000, onRide=True, selectedServiceTiers=["AUTO_RICKSHAW", "AUTO_LITE"])
    p.phase = 1
    r = p.srd(d, created - base_s, status="Active")      # first sight: neglected, in the on-ride state row
    p.phase = 2
    p.state(d, (created + 30) * 1000, mode="OFFLINE", onRide=False)   # after created_at, same bucket
    p.phase = 3
    p.srd(d, created - base_s, "Accept", 1, 3, sid=r["id"])           # -neglected +accepted: same row

    # ── rides ──
    sc = p.scenario("ride_lifecycle", "NEW -> INPROGRESS -> COMPLETED (fare 200), booking a flush before the ride",
                    total_bookings=1, finished_rides=1, gross_earnings=200)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.dbooking(bid, 600, auto=False)
    p.phase = 1
    p.ride(rid, bid, d, 600, "NEW")
    p.ride(rid, bid, d, 600, "INPROGRESS", upd=60)
    p.phase = 2
    p.ride(rid, bid, d, 600, "COMPLETED", fare=200, upd=600)

    sc = p.scenario("ride_inprogress", "NEW -> INPROGRESS only", total_bookings=1, inprogress_rides=1)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.ride(rid, bid, d, 610, "NEW")
    p.phase = 1
    p.ride(rid, bid, d, 610, "INPROGRESS", upd=60)

    sc = p.scenario("cancel_by_driver_bcr_before", "ByDriver reason seen before the ride",
                    total_bookings=1, cancelled_rides=1, rides_cancelled_by_driver=1)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.bcr(bid, rid, d, 620, "ByDriver")
    p.dbooking(bid, 620)
    p.phase = 1
    p.ride(rid, bid, d, 620, "NEW")
    p.ride(rid, bid, d, 620, "CANCELLED", upd=30)

    sc = p.scenario("cancel_by_driver_bcr_after", "ByDriver reason arrives two flushes after the cancel",
                    total_bookings=1, cancelled_rides=1, rides_cancelled_by_driver=1)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.ride(rid, bid, d, 630, "NEW")
    p.ride(rid, bid, d, 630, "CANCELLED", upd=30)
    p.phase = 2
    p.bcr(bid, rid, d, 630, "ByDriver")

    sc = p.scenario("cancel_by_user_auto", "auto-accepted booking cancelled ByUser; reason has no ride_id (joined by booking)",
                    total_bookings=1, cancelled_rides=1, rides_cancelled_by_user=1, auto_accept_bookings=1,
                    auto_accept_cancelled_rides=1, auto_accept_rides_cancelled_by_user=1)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.dbooking(bid, 640, auto=True)
    p.ride(rid, bid, d, 640, "NEW")
    p.ride(rid, bid, d, 640, "CANCELLED", upd=30)
    p.phase = 1
    p.bcr(bid, None, d, 640, "ByUser")

    sc = p.scenario("cancel_by_user_bcr_before", "ByUser reason with no ride_id a flush before the ride (joined by booking)",
                    total_bookings=1, cancelled_rides=1, rides_cancelled_by_user=1)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.bcr(bid, None, d, 645, "ByUser")
    p.phase = 1
    p.ride(rid, bid, d, 645, "NEW")
    p.ride(rid, bid, d, 645, "CANCELLED", upd=30)

    sc = p.scenario("cancel_by_application", "ByApplication: cancelled, neither by driver nor by user",
                    total_bookings=1, cancelled_rides=1)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.bcr(bid, rid, d, 650, "ByApplication")
    p.ride(rid, bid, d, 650, "NEW")
    p.ride(rid, bid, d, 650, "CANCELLED", upd=30)

    sc = p.scenario("auto_accept_completed", "auto-accept flag arrives after a completed ride",
                    total_bookings=1, finished_rides=1, gross_earnings=100, auto_accept_bookings=1,
                    auto_accept_finished_rides=1)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.ride(rid, bid, d, 660, "NEW")
    p.ride(rid, bid, d, 660, "COMPLETED", fare=100, upd=600)
    p.phase = 1
    p.dbooking(bid, 660, auto=True)

    sc = p.scenario("auto_accept_cancel_by_driver", "auto-accepted booking cancelled ByDriver",
                    total_bookings=1, cancelled_rides=1, rides_cancelled_by_driver=1, auto_accept_bookings=1,
                    auto_accept_cancelled_rides=1, auto_accept_rides_cancelled_by_driver=1)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.dbooking(bid, 670, auto=True)
    p.ride(rid, bid, d, 670, "NEW")
    p.ride(rid, bid, d, 670, "CANCELLED", upd=30)
    p.bcr(bid, rid, d, 670, "ByDriver")

    sc = p.scenario("ride_versions", "lower-rank versions after COMPLETED are ignored (same batch and after a flush); "
                    "a later COMPLETED replaces the fare; duplicates count once",
                    total_bookings=1, finished_rides=1, gross_earnings=160)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.ride(rid, bid, d, 680, "NEW")
    p.ride(rid, bid, d, 680, "COMPLETED", fare=150, upd=600)
    p.ride(rid, bid, d, 680, "INPROGRESS", upd=60)
    p.phase = 1
    p.ride(rid, bid, d, 680, "NEW", times=2)
    p.ride(rid, bid, d, 680, "COMPLETED", fare=160, upd=601, times=2)
    p.phase = 2
    p.ride(rid, bid, d, 680, "INPROGRESS", upd=60)

    sc = p.scenario("toll_and_parking_ignored", "the ride's toll 12.5 and its own fare params' parking 20.25 "
                    "(a flush before the ride) change nothing: gross = fare 300",
                    total_bookings=1, finished_rides=1, gross_earnings=300)
    d = p.driver(sc)
    rid, bid, fpr = ids(3)
    p.phase = 0
    p.fp(fpr, parking=20.25)
    p.phase = 1
    p.ride(rid, bid, d, 690, "NEW", fp_id=fpr)
    p.ride(rid, bid, d, 690, "COMPLETED", fare=300, toll=12.5, fp_id=fpr, upd=600)

    sc = p.scenario("driver_addition_tip", "quote fare params (booking.fare_parameters_id), before and after the ride, any status",
                    total_bookings=2, finished_rides=1, gross_earnings=90, driver_addition=25, customer_tip=10)
    d = p.driver(sc)
    rid_a, bid_a, fq_a, rid_b, bid_b, fq_b, fr_b = ids(7)
    p.phase = 0
    p.dbooking(bid_a, 720, fp_id=fq_a)
    p.fp(fq_a, driver_selected_fare=20, customer_extra_fee=10)
    p.phase = 1
    p.ride(rid_a, bid_a, d, 720, "NEW")
    p.phase = 0
    p.dbooking(bid_b, 725, fp_id=fq_b)
    p.fp(fr_b)                                           # the ride's own fare params, all null: no effect
    p.ride(rid_b, bid_b, d, 725, "NEW", fp_id=fr_b)
    p.ride(rid_b, bid_b, d, 725, "COMPLETED", fare=90, fp_id=fr_b, upd=600)
    p.phase = 2
    p.fp(fq_b, driver_selected_fare=5)

    sc = p.scenario("quote_fare_params_updated", "quote fare params joined from pending, then updated: "
                    "the booking's next version keeps the update",
                    total_bookings=1, finished_rides=1, gross_earnings=80, driver_addition=15)
    d = p.driver(sc)
    rid, bid, fq = ids(3)
    p.phase = 0
    p.fp(fq, driver_selected_fare=20, customer_extra_fee=10)    # pending: no ride knows it yet
    bk = p.dbooking(bid, 727, fp_id=fq)
    p.phase = 1
    p.ride(rid, bid, d, 727, "NEW")
    p.ride(rid, bid, d, 727, "COMPLETED", fare=80, upd=600)
    p.phase = 2
    p.fp(fq, driver_selected_fare=15)                             # joined through the index; tip 10 -> null
    p.phase = 3                                                   # a flush after the update, not racing it
    p.send("booking", {**bk, "status": "COMPLETED", "updated_at": bk["updated_at"] + 600})

    sc = p.scenario("ratings", "rating before the ride, rating after it, and a rating updated 5 -> 3",
                    total_bookings=2, finished_rides=2, gross_earnings=100, rides_with_rating=2, rating_sum=7)
    d = p.driver(sc)
    rid_a, bid_a, rt_a, rid_b, bid_b, rt_b = ids(6)
    p.phase = 0
    p.rating(rt_a, rid_a, d, 730, 4)
    p.ride(rid_b, bid_b, d, 735, "NEW")
    p.ride(rid_b, bid_b, d, 735, "COMPLETED", fare=50, upd=600)
    p.phase = 1
    p.ride(rid_a, bid_a, d, 730, "NEW")
    p.ride(rid_a, bid_a, d, 730, "COMPLETED", fare=50, upd=600)
    p.phase = 2
    p.rating(rt_b, rid_b, d, 1400, 5)
    p.rating(rt_b, rid_b, d, 1401, 3)

    sc = p.scenario("ride_row_key_change", "same-rank version moves the ride to another driver: old row retracted",
                    total_bookings=1, finished_rides=1, gross_earnings=60)
    d1 = p.driver(sc)
    d2 = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.ride(rid, bid, d1, 750, "NEW")
    p.phase = 1
    p.ride(rid, bid, d2, 750, "NEW", upd=5)
    p.ride(rid, bid, d2, 750, "COMPLETED", fare=60, upd=600)

    # ── extreme / edge cases ──
    sc = p.scenario("edge_srd_answer_then_unanswered", "a late unanswered copy after the answer is ignored",
                    popups_sent=1, popups_accepted=1, response_secs_sum=3)
    d = p.driver(sc)
    p.phase = 0
    r = p.srd(d, 160, status="Active")
    p.srd(d, 160, "Accept", 1, 4, sid=r["id"])
    p.phase = 1
    p.send("srd", r)                                     # the original Active version, redelivered late

    sc = p.scenario("edge_srd_accept_then_pulled", "an answer replaced by Pulled is fully retracted")
    d = p.driver(sc)
    p.phase = 0
    r = p.srd(d, 161, "Accept", 1, 2)
    p.phase = 1
    p.send("srd", {**r, "response": "Pulled"})

    sc = p.scenario("edge_srd_unknown_response", "an unknown response value counts as sent, not neglected",
                    popups_sent=1)
    d = p.driver(sc)
    p.srd(d, 162, "Expired", 1, None)

    sc = p.scenario("edge_srd_empty_driver", "driver_id '' is counted under driver ''", popups_sent=1, popups_neglected=1)
    sc.drivers.append("")
    p.srd("", 163, None, 1, None)

    sc = p.scenario("edge_srd_epoch_and_far_future", "created_at 0 (1970) and 2100 land in their own buckets",
                    popups_sent=2, popups_neglected=2)
    d = p.driver(sc)
    for c in (0, 4102444800):
        r = p.srd(d, 164, None, None, None)
        p.msgs.pop()                                     # resend with the extreme created_at instead
        p.send("srd", {**r, "created_at": c, "start_time": c, "updated_at": c, "search_request_valid_till": c})

    sc = p.scenario("edge_srd_many_in_one_bucket", "500 pop-ups for one driver in one bucket",
                    popups_sent=400, popups_accepted=100, popups_rejected=200, popups_neglected=100,
                    response_secs_sum=500)
    d = p.driver(sc)
    for i in range(500):
        kind = i % 5
        if kind == 0:
            p.srd(d, 170, "Accept", 1, 4)
        elif kind in (1, 2):
            p.srd(d, 170, "Reject", 1, 2)
        elif kind == 3:
            p.srd(d, 170, None, 1, None)
        else:
            p.srd(d, 170, "Pulled", 1, None)

    sc = p.scenario("edge_srd_huge_message", "a 200 KB message with an extra column still counts",
                    popups_sent=1, popups_rejected=1, response_secs_sum=1)
    d = p.driver(sc)
    r = p.srd(d, 171, "Reject", 1, 2)
    p.msgs.pop()
    p.send("srd", {**r, "zz_padding": "x" * 200_000})

    sc = p.scenario("edge_ride_completed_then_cancelled", "a same-rank CANCELLED after COMPLETED replaces it",
                    total_bookings=1, cancelled_rides=1, rides_cancelled_by_driver=1)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.ride(rid, bid, d, 760, "NEW")
    p.ride(rid, bid, d, 760, "COMPLETED", fare=100, upd=600)
    p.phase = 1
    p.ride(rid, bid, d, 760, "CANCELLED", upd=700)
    p.bcr(bid, rid, d, 760, "ByDriver")

    sc = p.scenario("edge_ride_unknown_status", "a ride version with an unknown status is skipped",
                    total_bookings=1, finished_rides=1, gross_earnings=70)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.ride(rid, bid, d, 765, "NEW")
    p.ride(rid, bid, d, 765, "TELEPORTED", upd=10)
    p.ride(rid, bid, d, 765, "COMPLETED", fare=70, upd=600)

    sc = p.scenario("edge_ride_null_fare", "COMPLETED with fare null: finished, earns 0",
                    total_bookings=1, finished_rides=1)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.ride(rid, bid, d, 766, "COMPLETED", fare=None, upd=600)

    sc = p.scenario("edge_money_huge", "fare 2,500,000,000.5 (sent as 2.5000000005e9) is exact",
                    total_bookings=1, finished_rides=1, gross_earnings=Decimal("2500000000.5"))
    d = p.driver(sc)
    rid, bid = ids(2)
    p.ride(rid, bid, d, 767, "COMPLETED", fare=2_500_000_000.5, upd=600)

    sc = p.scenario("edge_money_tiny", "fare 0.0001 (sent as 1.0e-4) keeps 4 decimals",
                    total_bookings=1, finished_rides=1, gross_earnings=Decimal("0.0001"))
    d = p.driver(sc)
    rid, bid = ids(2)
    p.ride(rid, bid, d, 768, "COMPLETED", fare=0.0001, upd=600)

    sc = p.scenario("edge_bcr_source_changes", "a later cancellation-reason version changes ByUser to ByDriver",
                    total_bookings=1, cancelled_rides=1, rides_cancelled_by_driver=1)
    d = p.driver(sc)
    rid, bid = ids(2)
    p.phase = 0
    p.ride(rid, bid, d, 770, "CANCELLED", upd=30)
    p.bcr(bid, rid, d, 770, "ByUser")
    p.phase = 1
    p.bcr(bid, rid, d, 771, "ByDriver")

    sc = p.scenario("edge_ratings_extremes", "ratings 0 and 5 on one ride plus 1 on another (two rating ids per ride)",
                    total_bookings=2, finished_rides=2, gross_earnings=20, rides_with_rating=3, rating_sum=6)
    d = p.driver(sc)
    rid_a, bid_a, rid_b, bid_b = ids(4)
    p.ride(rid_a, bid_a, d, 772, "COMPLETED", fare=10, upd=600)
    p.ride(rid_b, bid_b, d, 772, "COMPLETED", fare=10, upd=600)
    p.rating(str(uuid.uuid4()), rid_a, d, 900, 0)
    p.rating(str(uuid.uuid4()), rid_a, d, 901, 5)
    p.rating(str(uuid.uuid4()), rid_b, d, 902, 1)

    sc = p.scenario("edge_orphans", "facts whose ride never arrives (bookings, fare params, rating, reason) add nothing")
    d = p.driver(sc)
    rid, bid, fq = ids(3)
    p.dbooking(bid, 773, auto=True, fp_id=fq)
    p.fp(fq, driver_selected_fare=50, customer_extra_fee=50)
    p.rating(str(uuid.uuid4()), rid, d, 900, 5)
    p.bcr(bid, rid, d, 773, "ByDriver")

    sc = p.scenario("edge_booking_fare_params_switch", "the booking's quote fare params id changes: the new one wins",
                    total_bookings=1, finished_rides=1, gross_earnings=30, driver_addition=3)
    d = p.driver(sc)
    rid, bid, fq1, fq2 = ids(4)
    p.phase = 0
    p.fp(fq1, driver_selected_fare=10, customer_extra_fee=10)
    p.fp(fq2, driver_selected_fare=3)
    bk = p.dbooking(bid, 774, fp_id=fq1)
    p.ride(rid, bid, d, 774, "COMPLETED", fare=30, upd=600)
    p.phase = 1
    p.send("booking", {**bk, "fare_parameters_id": fq2, "updated_at": bk["updated_at"] + 60})

    sc = p.scenario("edge_weird_ids", "ids and city with quotes, backslashes, unicode and 300 chars",
                    total_bookings=1, finished_rides=1, gross_earnings=11, popups_sent=1, popups_accepted=1,
                    response_secs_sum=1)
    d = f"pt-{run_id}-weird-'\"\\-ಕನ್ನಡ-日本-" + "x" * 300
    sc.drivers.append(d)
    rid = "ride-'\"\\-" + str(uuid.uuid4())
    bid = "bk-'\"\\-ಕ-" + str(uuid.uuid4())
    p.srd(d, 775, "Accept", 1, 2)
    p.ride(rid, bid, d, 775, "COMPLETED", fare=11, upd=600)

    p.phase = 0
    p.msgs.sort(key=lambda m: m.phase)                  # stable: in-phase order is production order
    return p


# ─── reference model ─────────────────────────────────────────────────────────

def dec(x) -> Decimal:
    return Decimal(0) if x is None else Decimal(str(x))


def truthy(x) -> bool:
    return x in (True, "True", "true")


def zero() -> dict:
    return {**dict.fromkeys(COUNTS, 0), **dict.fromkeys(MONEY, Decimal(0))}


def srd_contribution(v):
    if v["driver_id"] == SENTINEL_DRIVER or v.get("response") == "Pulled":
        return None
    resp = v.get("response")
    c = zero()
    c["popups_sent"] = 1
    c["popups_accepted"] = int(resp == "Accept")
    c["popups_rejected"] = int(resp == "Reject")
    c["popups_neglected"] = int(resp is None)
    ra, rs = v.get("rendered_at"), v.get("responded_at")
    if ra is not None and rs is not None and ra > 0 and rs > 0 and rs > ra:
        c["response_secs_sum"] = rs - ra
    c["auto_accept_search_requests"] = int(resp == "Accept" and truthy(v.get("is_auto_accepted")))
    return (floor5(v["created_at"]), v.get("merchant_operating_city_id") or "", v["driver_id"]), c


def ride_contribution(ride, dbk, bcr, fp_quote, ratings):
    st = ride["status"]
    done, cancelled = st == "COMPLETED", st == "CANCELLED"
    source = bcr["source"] if bcr else None
    by_driver, by_user = cancelled and source == "ByDriver", cancelled and source == "ByUser"
    auto = dbk is not None and truthy(dbk.get("is_auto_accepted"))
    c = zero()
    c["total_bookings"] = 1
    c["finished_rides"] = int(done)
    c["inprogress_rides"] = int(st == "INPROGRESS")
    c["cancelled_rides"] = int(cancelled)
    c["rides_cancelled_by_driver"] = int(by_driver)
    c["rides_cancelled_by_user"] = int(by_user)
    c["auto_accept_bookings"] = int(auto)
    c["auto_accept_finished_rides"] = int(auto and done)
    c["auto_accept_cancelled_rides"] = int(auto and cancelled)
    c["auto_accept_rides_cancelled_by_driver"] = int(auto and by_driver)
    c["auto_accept_rides_cancelled_by_user"] = int(auto and by_user)
    if done:
        c["gross_earnings"] = dec(ride.get("fare"))
    if fp_quote:
        c["driver_addition"] = dec(fp_quote.get("driver_selected_fare"))
        c["customer_tip"] = dec(fp_quote.get("customer_extra_fee"))
    c["rides_with_rating"] = len(ratings)
    c["rating_sum"] = sum(int(r["rating_value"]) for r in ratings)
    return (floor5(ride["created_at"]), ride.get("merchant_operating_city_id") or "", ride["driver_id"]), c


def add(rows, kc):
    if kc is None:
        return
    k, c = kc
    row = rows[k]
    for m in MEASURES:
        row[m] += c[m]


def final_states(msgs: list[Msg]) -> dict[str, dict]:
    """Last decodable version of every entity, per topic, in production order.
    Rides keep a version only if its status rank is >= the stored one."""
    latest: dict[str, dict] = defaultdict(dict)
    for m in msgs:
        table = TABLE_OF.get(m.topic)
        if table is None:
            continue                                     # driver-pool-state: see state_model
        try:
            v = json.loads(m.value)
        except json.JSONDecodeError:
            continue
        if not isinstance(v, dict) or any(v.get(f) is None for f in REQUIRED[table]):
            continue
        pk = v["booking_id"] if table == "bcr" else v["id"]
        if table == "srd":
            old = latest["srd"].get(pk)
            if old is not None and old.get("response") is not None and v.get("response") is None:
                continue                                 # an answer is never undone by an unanswered copy
        if table == "ride" and v["status"] not in RIDE_RANK:
            continue                                     # unknown status: undecodable for the consumer
        if table == "ride":
            old = latest["ride"].get(pk)
            if old is not None and RIDE_RANK[v["status"]] < RIDE_RANK[old["status"]]:
                continue
        latest[table][pk] = v
    return latest


def contributions(latest: dict[str, dict], stamp=None) -> dict:
    """Rows keyed (bucket, city, driver). With stamp(driver, event_ms) -> (state key,
    entry ts), keyed (bucket, city, driver, state key) instead: each pop-up and
    ride by the driver's state at the event's own time (SRD / ride created_at);
    ratings and joined facts go with their ride. Known-state rows also carry
    state_at, the newest entry time credited to them."""
    rows = defaultdict(zero)

    def put(kc, event_s):
        if kc is None:
            return
        k, c = kc
        if stamp is not None:
            sk, at = stamp(k[2], event_s * 1000)
            k = (*k, sk)
            rows[k]["state_at"] = max(rows[k].get("state_at", 0), at)
        add(rows, (k, c))

    for v in latest["srd"].values():
        put(srd_contribution(v), v["created_at"])
    bcrs = list(latest["bcr"].values())
    for ride in latest["ride"].values():
        dbk = latest["booking"].get(ride["booking_id"])
        bcr = (next((x for x in bcrs if x.get("ride_id") == ride["id"]), None)
               or next((x for x in bcrs if x.get("ride_id") is None and x["booking_id"] == ride["booking_id"]), None))
        fp_quote = latest["fp"].get(dbk["fare_parameters_id"]) if dbk else None
        ratings = [r for r in latest["rating"].values() if r["ride_id"] == ride["id"]]
        put(ride_contribution(ride, dbk, bcr, fp_quote, ratings), ride["created_at"])
    return rows


def model(plan: Plan) -> dict:
    return contributions(final_states(plan.msgs))


def state_history(msgs: list[Msg]) -> dict[str, list[tuple[int, dict]]]:
    """Per driver, the writer's history: the merged state after each driver-pool-state
    event that changed it, at the event ts."""
    current: dict[str, dict] = defaultdict(dict)
    history: dict[str, list[tuple[int, dict]]] = defaultdict(list)
    for m in msgs:
        if m.topic == STATE_TOPIC:
            v = json.loads(m.value)
            current[v["driverId"]].update(v["changes"])
            h = history[v["driverId"]]
            if not h or h[-1][1] != current[v["driverId"]]:
                h.append((v["ts"], dict(current[v["driverId"]])))
    return {d: sorted(h, key=lambda e: e[0]) for d, h in history.items()}


# The state columns the checks group by: (mode, onRide, selectedServiceTiers).
STATE_COLS = ("mode", "onRide", "selectedServiceTiers")


def state_key(st: dict) -> tuple:
    return st.get("mode"), st.get("onRide"), tuple(st.get("selectedServiceTiers") or [])


def stamper(history: dict):
    """stamp(driver, at_ms) -> (state key, entry ts) of the newest history entry
    at or before at_ms; (None, 0) when there is none (state_known = 0)."""
    def stamp(d, at_ms):
        in_force = [(t, st) for t, st in history.get(d, []) if t <= at_ms]
        return (state_key(in_force[-1][1]), in_force[-1][0]) if in_force else (None, 0)
    return stamp


def state_model(plan: Plan) -> tuple[dict, dict]:
    """Expected state rows: (bucket, city, driver, state key or None) -> non-zero
    measures, and state_at per known-state row."""
    rows = contributions(final_states(plan.msgs), stamper(state_history(plan.msgs)))
    return norm_rows(rows), {k: v["state_at"] for k, v in rows.items() if k[3] is not None and norm(v)}


# ─── comparison helpers ──────────────────────────────────────────────────────

def norm(row: dict) -> dict:
    """Non-zero measures only, as Decimals."""
    return {m: dec(row.get(m)) for m in MEASURES if dec(row.get(m)) != 0}


def norm_rows(rows: dict) -> dict:
    return {k: n for k, v in rows.items() if (n := norm(v))}


def total(rows: dict, drivers) -> dict:
    t: dict = defaultdict(zero)
    for k, v in rows.items():
        if k[2] in drivers:
            add(t, ("all", {**zero(), **v}))
    return norm(t["all"])


def fmt(n: dict) -> str:
    return " ".join(f"{m}={v}" for m, v in n.items()) or "nothing"


class Report:
    def __init__(self):
        self.failures = 0

    def check(self, ok: bool, label: str, detail: str = ""):
        self.failures += 0 if ok else 1
        print(f"  {'PASS' if ok else 'FAIL'}  {label}" + (f"\n        {detail}" if detail and not ok else ""))


def check_scenarios(rep: Report, plan: Plan, got: dict, exp: dict, label: str):
    for sc in plan.scenarios:
        if not sc.drivers:
            continue
        want = norm(sc.expect)
        have = total(got, set(sc.drivers))
        rep.check(have == want, f"{sc.name}: {sc.what}", f"want {fmt(want)}\n        got  {fmt(have)}")
        mine_exp = {k: v for k, v in exp.items() if k[2] in sc.drivers}
        mine_got = {k: v for k, v in got.items() if k[2] in sc.drivers}
        if mine_exp != mine_got:
            rep.check(False, f"{sc.name}: per bucket and city matches the {label}",
                      "\n        ".join(f"{hms(k[0])} city={k[1] or '-'} {k[2][-8:]}  model: {fmt(mine_exp.get(k, {}))}"
                                         f"  |  got: {fmt(mine_got.get(k, {}))}"
                                         for k in sorted(set(mine_exp) | set(mine_got))))


# ─── self-check ──────────────────────────────────────────────────────────────

def run_selfcheck() -> int:
    rep = Report()
    print("aeson number format")
    for x, want in ((0.05, "5.0e-2"), (12.5, "12.5"), (0.5, "0.5"), (151.084, "151.084"), (100.0, "100"),
                    (12345678.5, "1.23456785e7"), (20.25, "20.25"), (-0.05, "-5.0e-2")):
        rep.check(aeson_num(x) == want, f"{x!r} -> {want}", f"got {aeson_num(x)}")
        rep.check(float(json.loads(aeson_num(x))) == x, f"{want} decodes back to {x!r}")

    now = int(time.time())
    for i, base in enumerate(range(now - 3000, now - 3000 + 300, 37)):   # many alignments against the 5-min edge
        plan = build_plan(base, f"self{i}")
        exp = norm_rows(model(plan))
        if i == 0:
            print(f"plan: {len(plan.scenarios)} scenarios, {len(plan.msgs)} messages, {len(exp)} expected rows")
            print("drainer format")
            keysets: dict[str, set] = {}
            ok_keys = ok_bool = ok_ts = ok_sorted = True
            for m in plan.msgs:
                if m.topic == STATE_TOPIC:
                    continue                             # checked below
                try:
                    v = json.loads(m.value)
                except json.JSONDecodeError:
                    continue
                if not isinstance(v, dict) or len(v) < 5 or "zz_padding" in v:
                    continue                             # the oversized-message edge case adds a column on purpose
                table = TABLE_OF[m.topic]
                ok_keys &= keysets.setdefault(table, set(v)) == set(v)
                ok_bool &= not any(isinstance(x, bool) for x in v.values())
                ok_ts &= all(isinstance(v.get(c), int) for c in ("created_at", "updated_at", "start_time", "timestamp")
                             if c in v and v[c] is not None)
                ok_sorted &= list(v) == sorted(v)
            rep.check(ok_keys, "every message of a table carries the same full column set (nulls present)")
            rep.check(ok_bool, "no JSON booleans at top level (bools are \"True\"/\"False\" strings)")
            rep.check(ok_ts, "timestamps are integer epoch seconds")
            rep.check(ok_sorted, "top-level keys sorted, as aeson emits them")
            rep.check(set(keysets["srd"]) == set(SRD_TEMPLATE),
                      f"SRD rows have the {len(SRD_TEMPLATE)} columns of the real message")
            rep.check(any(b'"fare":1.0e-4' in m.value for m in plan.msgs if m.topic == T["ride"]),
                      "ride fare 0.0001 is sent as 1.0e-4")
            evs = [json.loads(m.value) for m in plan.msgs if m.topic == STATE_TOPIC]
            rep.check(bool(evs) and all(set(e) == {"driverId", "ts", "changes"} and isinstance(e["ts"], int)
                                        and m.key == e["driverId"] for e, m in
                                        zip(evs, (m for m in plan.msgs if m.topic == STATE_TOPIC))),
                      "driver-pool-state events are {driverId, ts (ms), changes}, keyed by driver id")
            print("reference model vs hand-written expectations")
        for sc in plan.scenarios:
            if not sc.drivers:
                continue
            want, have = norm(sc.expect), total(exp, set(sc.drivers))
            if i == 0 or want != have:
                rep.check(want == have, f"{sc.name} (base {hms(base)})", f"want {fmt(want)}\n        model {fmt(have)}")
        if i == 0:
            sentinel = [k for k in exp if k[2] == SENTINEL_DRIVER]
            rep.check(not sentinel, "no row for the sentinel driver")
            edge_rows = sorted(k[0] for k in exp if k[2] in next(s for s in plan.scenarios
                                                                 if s.name == "srd_bucket_edge").drivers)
            rep.check(len(edge_rows) == 2 and edge_rows[1] - edge_rows[0] == BUCKET_S,
                      "bucket-edge pop-ups land in two adjacent buckets")
            key_change = next(s for s in plan.scenarios if s.name == "ride_row_key_change")
            rep.check(not any(k[2] == key_change.drivers[0] for k in exp), "a reassigned ride leaves nothing on the old driver")
            print("state rows (keyed by the driver's state at the event time)")
        srows, sat = state_model(plan)
        summed: dict = defaultdict(zero)
        for k, v in srows.items():
            add(summed, (k[:3], {**zero(), **v}))
        if i == 0 or norm_rows(summed) != exp:
            rep.check(norm_rows(summed) == exp, f"state rows sum to the per-driver rows (base {hms(base)})")
        stamped = next(s for s in plan.scenarios if s.name == "driver_state_stamped")
        d = stamped.drivers[0]
        srd_msgs = [(m.phase, json.loads(m.value)) for m in plan.msgs if m.topic == T["srd"] and d.encode() in m.value]
        created = srd_msgs[0][1]["created_at"]
        first_ph, answer_ph = srd_msgs[0][0], srd_msgs[-1][0]
        changes = [(m.phase, json.loads(m.value)) for m in plan.msgs if m.topic == STATE_TOPIC and m.key == d]
        between = [e for ph, e in changes if first_ph < ph < answer_ph and e["ts"] > created * 1000
                   and floor5(e["ts"] // 1000) == floor5(created)]
        ok = (srd_msgs[0][1]["response"] is None and srd_msgs[-1][1]["response"] == "Accept" and len(between) == 1
              and between[0]["changes"].get("mode") == "OFFLINE")
        if i == 0 or not ok:
            rep.check(ok, "driver_state_stamped: the state changes (OFFLINE) after the pop-up's created_at, in its "
                      "bucket, between the Active version and the answer", f"srd {srd_msgs} changes {changes}")
        key = ("ONLINE", True, ("AUTO_RICKSHAW", "AUTO_LITE"))
        want = {(floor5(created), plan.city, d, key): norm(stamped.expect)}
        have = {k: v for k, v in srows.items() if k[2] == d}
        if i == 0 or have != want:
            rep.check(have == want, f"driver_state_stamped: one state row, the state at created_at (ONLINE, on ride, "
                      f"2 tiers): accepted 1, neglected 0, nothing in another state row (base {hms(base)})",
                      f"want {want}\n        model {have}")
        at = sat.get((floor5(created), plan.city, d, key))
        if i == 0 or at != (floor5(created) + 100) * 1000:
            rep.check(at == (floor5(created) + 100) * 1000, "driver_state_stamped: state_at = the on-ride entry's time",
                      f"model {at}")
        others = [k for k in srows if k[2] != d and k[3] is not None]
        if i == 0 or others:
            rep.check(not others, "rows of every other driver (no writer history) have state_known = 0",
                      f"{others[:3]}")
    # Order independence across topics: facts may reach the consumer before or after the ride.
    plan = build_plan(now - 3000, "selfperm")
    exp = norm_rows(model(plan))
    by_topic = defaultdict(list)
    for m in plan.msgs:
        by_topic[m.topic].append(m)
    reordered = [m for t in sorted(by_topic, reverse=True) for m in by_topic[t]]
    rep.check(norm_rows(contributions(final_states(reordered))) == exp,
              "model result does not depend on the interleaving of topics")
    print(f"\n{'ALL PASS' if not rep.failures else f'{rep.failures} FAILED'}  (selfcheck)")
    return 1 if rep.failures else 0


# ─── I/O ─────────────────────────────────────────────────────────────────────

def ch(url: str, sql: str) -> list[dict]:
    q = urllib.parse.urlencode({"query": sql + " FORMAT JSONEachRow"})
    with urllib.request.urlopen(f"{url}/?{q}", timeout=30) as r:
        return [json.loads(line, parse_float=Decimal) for line in r.read().decode("utf-8", errors="replace").splitlines() if line]


def pg_connect(args):
    import psycopg
    from psycopg.rows import dict_row
    user, schema = "atlas_driver_offer_bpp_user", "atlas_driver_offer_bpp"
    conn = psycopg.connect(host=args.pg_host, port=args.pg_port, dbname=args.pg_db, user=user,
                           password=args.pg_password, connect_timeout=5, autocommit=True, row_factory=dict_row)
    conn.execute("SET TIME ZONE 'UTC'")
    conn.execute(f"SET search_path TO {schema}")
    return conn


def preflight(args) -> bool:
    """Creates missing topics, then waits until the driver-performance group has
    all its topics assigned (a running consumer only picks up a new topic on its next
    metadata refresh, and would start it at the latest offset), plus a few
    seconds: a partition the group never committed gets that latest offset only
    after its assignment, and anything produced before is skipped."""
    from confluent_kafka.admin import AdminClient, NewTopic
    admin = AdminClient({"bootstrap.servers": args.bootstrap})
    try:
        existing = admin.list_topics(timeout=10).topics
    except Exception as e:  # noqa: BLE001
        print(f"FAIL  Kafka at {args.bootstrap} unreachable: {e}")
        return False
    missing = [t for t in [*T.values(), STATE_TOPIC] if t not in existing]
    if missing:
        print(f"  creating topics: {', '.join(missing)}")
        for f in admin.create_topics([NewTopic(t, 1, 1) for t in missing]).values():
            try:
                f.result(10)
            except Exception as e:  # noqa: BLE001 - already exists is fine
                print(f"  note: {e}")
    deadline = time.time() + args.assign_timeout
    assigned: set[str] = set()
    while time.time() < deadline:
        try:
            desc = admin.describe_consumer_groups([GROUP])[GROUP].result(10)
            assigned = {tp.topic for mb in desc.members for tp in mb.assignment.topic_partitions}
        except Exception as e:  # noqa: BLE001
            print(f"\r  describe group {GROUP}: {e}", end="")
        if set(T.values()) <= assigned:
            time.sleep(5)
            return True
        print(f"\r  waiting for group '{GROUP}' to be assigned all {len(T)} topics "
              f"({len(assigned & set(T.values()))}/{len(T)}) ... "
              f"{int(deadline - time.time())}s left ", end="", flush=True)
        time.sleep(5)
    print(f"\nFAIL  group '{GROUP}' is not consuming {sorted(set(T.values()) - assigned)}: "
          "is kafka-driver-performance-exe running? Restart it so it subscribes to the new topics.")
    return False


def produce(prod, msgs: list[Msg]):
    for m in msgs:
        prod.produce(m.topic, key=m.key.encode() if m.key else None, value=m.value)
        prod.poll(0)
    prod.flush(30)


def wait_for_flush(r, after_ms: int, timeout_s: int, what: str) -> bool:
    deadline = time.time() + timeout_s
    while time.time() < deadline:
        if int(r.get(WATERMARK_KEY) or 0) >= after_ms:
            print(f"\r  {what}: flushed                                        ")
            return True
        print(f"\r  {what}: waiting for the consumer flush ... {int(deadline - time.time())}s left ", end="", flush=True)
        time.sleep(2)
    print()
    return False


def sql_str(x: str) -> str:
    return "'" + x.replace("\\", "\\\\").replace("'", "\\'") + "'"


def query_rows(url: str, where: str) -> dict:
    rows = ch(url, f"""
        SELECT toUnixTimestamp(bucket) AS b, merchant_operating_city_id AS c, driver_id AS d,
               {", ".join(f"sum({m}) AS {m}" for m in MEASURES)}
        FROM atlas_kafka.driver_analytics_5m WHERE {where} GROUP BY b, c, d""")
    out = {}
    for x in rows:
        out[(int(x["b"]), x["c"], x["d"])] = {m: dec(x[m]) for m in MEASURES}
    return norm_rows(out)


def query_state_rows(url: str, where: str) -> tuple[dict, dict]:
    """(bucket, city, driver, state key or None) -> non-zero measures, and state_at per known-state row."""
    rows = ch(url, f"""
        SELECT toUnixTimestamp(bucket) AS b, merchant_operating_city_id AS c, driver_id AS d,
               state_known, {", ".join(STATE_COLS)},
               {", ".join(f"sum({m}) AS {m}" for m in MEASURES)}, max(state_at) AS state_at
        FROM atlas_kafka.driver_analytics_5m WHERE {where}
        GROUP BY b, c, d, state_known, {", ".join(STATE_COLS)}""")
    out, at = {}, {}
    for x in rows:
        sk = state_key(x)
        if int(x["state_known"]) == 0:              # must also have every state column NULL / []
            sk = None if sk == (None, None, ()) else ("state_known=0 with values", *sk)
        k = (int(x["b"]), x["c"], x["d"], sk)
        row = {m: dec(x[m]) for m in MEASURES}
        if norm(row):
            out[k] = norm(row)
            if sk is not None:
                at[k] = int(x["state_at"])
    return out, at


# ─── run modes ───────────────────────────────────────────────────────────────

def run_scenarios(args) -> int:
    import redis
    from confluent_kafka import Producer

    r = redis.Redis(host=args.redis_host, port=args.redis_port, db=args.redis_db, decode_responses=True)
    run_id = uuid.uuid4().hex[:8]
    base = int(time.time()) - 50 * 60
    plan = build_plan(base, run_id)
    exp = norm_rows(model(plan))
    drivers = sorted({d for sc in plan.scenarios for d in sc.drivers})

    print(f"run {run_id}: {sum(1 for s in plan.scenarios if s.drivers)} scenarios, {len(plan.msgs)} messages, "
          f"city {plan.city}")
    print("preflight")
    if not preflight(args):
        return 1
    prod = Producer({"bootstrap.servers": args.bootstrap, "linger.ms": 5})
    for ph in sorted({m.phase for m in plan.msgs}):
        msgs = [m for m in plan.msgs if m.phase == ph]
        produce(prod, msgs)
        sent_ms = int(time.time() * 1000)
        print(f"  phase {ph}: sent {len(msgs)} messages")
        last = ph == max(m.phase for m in plan.msgs)
        if args.fast and not last:
            time.sleep(2)
        elif not wait_for_flush(r, sent_ms + args.grace * 1000, args.flush_timeout, f"phase {ph}"):
            print(f"FAIL  no flush in {args.flush_timeout}s; is kafka-driver-performance-exe running "
                  f"(it sets {WATERMARK_KEY} after each flush)?")
            return 1

    # The sentinel driver is matched through the test city only (other runs and real data use it too).
    id_list = ",".join(sql_str(d) for d in drivers if d not in (SENTINEL_DRIVER, ""))
    where = f"driver_id IN ({id_list}) OR merchant_operating_city_id = '{plan.city}'"
    got = query_rows(args.clickhouse, where)
    rep = Report()
    print("driver_analytics_5m vs hand-written totals and the reference model")
    check_scenarios(rep, plan, got, exp, "reference model")
    stray = {k: v for k, v in got.items() if k[2] not in drivers}
    rep.check(not stray, f"no rows for other drivers in the test city {plan.city}",
              "; ".join(f"{k[2]}: {fmt(v)}" for k, v in stray.items()))
    rep.check(got == exp, "every row matches the reference model, bucket by bucket")

    print("state rows (driver_analytics_5m, keyed by the driver's state at the event time)")
    exp_srows, exp_at = state_model(plan)
    got_srows, got_at = query_state_rows(args.clickhouse, where)
    bad = sorted({k for k in set(exp_srows) | set(got_srows) if exp_srows.get(k) != got_srows.get(k)}, key=repr)
    rep.check(not bad, "every (bucket, city, driver, state) row matches the model",
              "\n        ".join(f"{hms(k[0])} {k[2][-8:]} {k[3]}  model: {fmt(exp_srows.get(k, {}))}"
                                 f"  |  got: {fmt(got_srows.get(k, {}))}" for k in bad[:20]))
    stamped = next(s for s in plan.scenarios if s.name == "driver_state_stamped")
    mine = {k: v for k, v in got_srows.items() if k[2] in stamped.drivers}
    want = {k: v for k, v in exp_srows.items() if k[2] in stamped.drivers}
    rep.check(mine == want and len(mine) == 1, f"{stamped.name}: {stamped.what}",
              f"want {want}\n        got  {mine}")
    rep.check({k: v for k, v in got_at.items() if k[2] in stamped.drivers}
              == {k: v for k, v in exp_at.items() if k[2] in stamped.drivers},
              f"{stamped.name}: state_at = the history entry in force at created_at",
              f"want {exp_at}\n        got  {got_at}")
    others = {k: v for k, v in got_srows.items() if k[2] not in stamped.drivers and k[3] is not None}
    rep.check(not others, "rows of every other driver (no writer history) have state_known = 0",
              "; ".join(f"{k[2]}: {k[3]}" for k in list(others)[:5]))

    print("consumer state (Redis)")
    # -1 is "no expiry"; -2 is a key gone since the scan (perf:flush_inflight is deleted after each flush)
    no_ttl = [k for k in r.scan_iter("perf:*", count=1000) if k != WATERMARK_KEY and r.ttl(k) == -1]
    rep.check(not no_ttl, "every perf:* state key has a TTL", f"{len(no_ttl)} without: {no_ttl[:5]}")
    too_long = [k for k in r.scan_iter("perf:*", count=1000) if r.ttl(k) > 8 * 86400 + 60]
    rep.check(not too_long, "no perf:* key outlives 8 days", f"{too_long[:5]}")
    print(f"\n{'ALL PASS' if not rep.failures else f'{rep.failures} FAILED'}  (run {run_id})")
    return 1 if rep.failures else 0


VIEW_COLS = {m: m for m in MEASURES if m not in ("response_secs_sum", "rating_sum")}


def run_e2e(args) -> int:
    """The newest driver of a real ride run: driver_performance_5m vs the same
    metrics computed from local Postgres with the reference model."""
    try:
        conn = pg_connect(args)
    except Exception as e:  # noqa: BLE001
        print(f"FAIL  cannot reach local Postgres: {e}")
        return 1
    newest = conn.execute("SELECT trim(driver_id) AS d, created_at FROM ride ORDER BY created_at DESC LIMIT 1").fetchone()
    if not newest and not args.driver:
        print("FAIL  no ride in local Postgres; run a ride test first")
        return 1
    d = args.driver or newest["d"]
    since = conn.execute("SELECT coalesce(max(created_at), now()) - make_interval(hours => %s) AS s"
                         " FROM ride WHERE driver_id = %s", (args.hours, d)).fetchone()["s"]
    # Whole 5-minute buckets on both sides: ClickHouse is read from bucket since_b on.
    since_b = floor5(int(since.timestamp()))
    # A second early, as the CDC rounds timestamps to whole seconds; model rows whose
    # bucket still comes out before since_b are dropped below.
    lower = datetime.fromtimestamp(since_b - 1, UTC)
    print(f"driver {d}: entities from {datetime.fromtimestamp(since_b, UTC):%Y-%m-%d %H:%M:%S} UTC")

    def epoch(x):
        # timestamp-without-time-zone columns (Beam LocalTime) hold UTC wall-clock time
        return None if x is None else round((x if x.tzinfo else x.replace(tzinfo=UTC)).timestamp())

    rows = defaultdict(zero)
    for s in conn.execute("SELECT * FROM search_request_for_driver WHERE driver_id = %s AND created_at >= %s",
                          (d, lower)).fetchall():
        add(rows, srd_contribution({"driver_id": s["driver_id"], "merchant_operating_city_id": s["merchant_operating_city_id"],
                                    "created_at": epoch(s["created_at"]), "response": s["response"],
                                    "rendered_at": epoch(s["rendered_at"]), "responded_at": epoch(s["responded_at"]),
                                    "is_auto_accepted": b(s["is_auto_accepted"])}))
    rides = conn.execute("SELECT * FROM ride WHERE driver_id = %s AND created_at >= %s", (d, lower)).fetchall()
    rids = [x["id"].strip() for x in rides]
    bids = [x["booking_id"].strip() for x in rides]
    bookings = {x["id"]: x for x in conn.execute("SELECT * FROM booking WHERE id = ANY(%s)", (bids,)).fetchall()}
    bcrs = conn.execute("SELECT * FROM booking_cancellation_reason WHERE ride_id = ANY(%s) OR booking_id = ANY(%s)",
                        (rids, bids)).fetchall()
    fp_ids = [x["fare_parameters_id"] for x in bookings.values()]
    fps = {x["id"]: x for x in conn.execute("SELECT * FROM fare_parameters WHERE id = ANY(%s)", (fp_ids,)).fetchall()}
    ratings = conn.execute("SELECT ride_id, rating_value FROM rating WHERE ride_id = ANY(%s)", (rids,)).fetchall()
    for x in rides:
        rid, bid = x["id"].strip(), x["booking_id"].strip()
        ride = {"id": rid, "booking_id": bid, "driver_id": d, "status": x["status"],
                "merchant_operating_city_id": (x["merchant_operating_city_id"] or "").strip(),
                "created_at": epoch(x["created_at"]), "fare": x["fare"]}
        dbk = bookings.get(bid)
        dbk_c = dbk and {"is_auto_accepted": b(dbk["is_auto_accepted"]), "fare_parameters_id": dbk["fare_parameters_id"]}
        bcr = (next((c for c in bcrs if c["ride_id"] == rid), None)
               or next((c for c in bcrs if c["ride_id"] is None and c["booking_id"] == bid), None))
        add(rows, ride_contribution(
            ride, dbk_c, bcr, fps.get(dbk["fare_parameters_id"]) if dbk else None,
            [r for r in ratings if r["ride_id"] == rid]))
    conn.close()
    want = total({k: v for k, v in rows.items() if k[0] >= since_b}, {d})

    import redis
    r = redis.Redis(host=args.redis_host, port=args.redis_port, db=args.redis_db, decode_responses=True)
    if not wait_for_flush(r, int(time.time() * 1000) + args.grace * 1000, args.flush_timeout, "CDC up to now"):
        print(f"FAIL  no flush in {args.flush_timeout}s; is kafka-driver-performance-exe running?")
        return 1
    view = ch(args.clickhouse, f"SELECT {', '.join(f'sum({c}) AS {c}' for c in sorted(set(VIEW_COLS.values())))} "
                               f"FROM atlas_kafka.driver_performance_5m "
                               f"WHERE driver_id = '{d}' AND bucket >= toDateTime({since_b})")[0]
    base_rows = query_rows(args.clickhouse, f"driver_id = '{d}' AND bucket >= toDateTime({since_b})")
    base_tot = total(base_rows, {d})
    got = {m: dec(view[c]) for m, c in VIEW_COLS.items()}
    for m in ("response_secs_sum", "rating_sum"):
        got[m] = base_tot.get(m, Decimal(0))
    got = norm(got)

    def close(x, y) -> bool:
        # Postgres doubles vs Decimal(18,4) in ClickHouse
        return abs(dec(x) - dec(y)) <= Decimal("0.001")

    rep = Report()
    print("driver_performance_5m (+ driver_analytics_5m sums) vs Postgres")
    for m in MEASURES:
        rep.check(close(got.get(m, 0), want.get(m, 0)), f"{m}: {want.get(m, 0)}", f"ClickHouse {got.get(m, 0)}")
    rep.check(want.get("total_bookings", 0) > 0 or want.get("popups_sent", 0) > 0,
              "the driver has rides or pop-ups in the window")
    print(f"\n{'ALL PASS' if not rep.failures else f'{rep.failures} FAILED'}  (driver {d})")
    return 1 if rep.failures else 0


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--bootstrap", default="127.0.0.1:29092")
    ap.add_argument("--clickhouse", default="http://localhost:8123")
    ap.add_argument("--redis-host", default="localhost")
    ap.add_argument("--redis-port", type=int, default=6379)
    ap.add_argument("--redis-db", type=int, default=2, help="the consumer's Redis DB (driverStateRedisCfg)")
    ap.add_argument("--pg-host", default="localhost")
    ap.add_argument("--pg-port", type=int, default=int(os.environ.get("DB_PRIMARY_PORT", 5434)))
    ap.add_argument("--pg-db", default="atlas_dev")
    ap.add_argument("--pg-password", default="atlas")
    ap.add_argument("--flush-timeout", type=int, default=180, help="seconds to wait for each consumer flush")
    ap.add_argument("--grace", type=int, default=5, help="a flush counts if it happened this many s after sending")
    ap.add_argument("--assign-timeout", type=int, default=330, help="seconds to wait for the group to get all topics")
    ap.add_argument("--fast", action="store_true", help="don't wait for a flush between phases")
    ap.add_argument("--e2e", action="store_true", help="check the newest driver of a real ride run instead")
    ap.add_argument("--driver", help="with --e2e: the driver id (default: driver of the newest ride)")
    ap.add_argument("--hours", type=int, default=2, help="with --e2e: window before the driver's newest ride")
    ap.add_argument("--selfcheck", action="store_true", help="run only the reference model against the expectations")
    args = ap.parse_args()
    sys.exit(run_selfcheck() if args.selfcheck else run_e2e(args) if args.e2e else run_scenarios(args))


if __name__ == "__main__":
    main()
