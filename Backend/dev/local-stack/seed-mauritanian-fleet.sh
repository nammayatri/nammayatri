#!/usr/bin/env bash
#
# A test fleet in Nouakchott — two drivers per sellable vehicle type.
#
# The Mauritanian replacement for `setup.sh drivers`, which places drivers in
# Algiers and is kept for the day the pilot goes back.
#
# ── Why two per type and not one ────────────────────────────────────────────
# One driver per row means a whole category goes dead the moment he picks up a
# ride, and that happened minutes after a real person first used the Algerian
# stack. It also means the offers screen — the one where the passenger chooses
# between drivers — is never actually exercised as designed.
#
# ── The three things that make a driver invisible ───────────────────────────
# Each of these has cost an afternoon, and none of them produces an error. A
# search simply returns no price, which reads like broken dispatch.
#
#   1. **`point`, not lat/lon.** `driver_location` carries lat, lon AND a
#      PostGIS `point`. The pool does its distance test on `point`; lat and lon
#      are carried along for display. Updating only lat/lon looks completely
#      correct in psql and changes nothing at all.
#
#   2. **`coordinates_calculated_at`.** A position older than the freshness
#      window is skipped. Rows nobody updates go stale overnight and the stack
#      that worked yesterday returns empty arrays today, HTTP 200, nothing in
#      any log. `drivers-keepalive.sh` exists for exactly this and is
#      location-agnostic, so it keeps these fresh with no change.
#
#   3. **Spread.** The pool searches outward from the pickup. Scatter the fleet
#      wider than the search radius and it is empty again. +/-0.005 degrees is
#      roughly +/-550 m, which sits inside every configuration this stack has
#      had.
#
# ── The rows are created by signing in, not by INSERT ────────────────────────
# `person` has a long tail of NOT NULL columns and hand-writing an INSERT for
# it is how you get a driver that exists and does not work. `POST /ui/auth` on
# loopback creates the row the way the backend means it to be created; this
# script then approves it the way the agency would.
#
#   ./seed-mauritanian-fleet.sh          # create or refresh the fleet
#   ./seed-mauritanian-fleet.sh place    # only re-place and re-stamp
#   ./seed-mauritanian-fleet.sh show     # who is out there
#
set -uo pipefail
cd "$(dirname "$0")"

D=http://localhost:8017
MER=favorit0-0000-0000-0000-00000favorit

# Nouakchott, Carrefour Madrid end of Tevragh Zeina. Same centre the app opens
# on, so the fleet is where the passenger is looking.
LAT=18.0858
LON=-15.9582

pg() { docker exec ny-postgres psql -U postgres -d atlas_dev -At -c "$1"; }
say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }
ok()  { printf '   \033[1;32mok  \033[0m%s\n' "$*"; }
bad() { printf '   \033[1;31mBAD \033[0m%s\n' "$*"; }

# number | name | variant | make | model | colour | plate | code
#
# Numbers are in real Chinguitel range (22xxxxxx): first digit 2, second not 5.
# Plates are the Mauritanian shape -- four digits, two letters, then the wilaya,
# and 00 is Nouakchott for every one of them.
FLEET='
22100001|Mohamed Vall|SEDAN|Toyota|Corolla|Blanc|2145 AC 00|100001
22100002|Ahmed Salem|SEDAN|Hyundai|Accent|Gris|3182 BM 00|100002
22100003|Cheikh Ould Baba|HATCHBACK|Renault|Clio|Bleu|4027 CD 00|100003
22100004|Brahim Ould Sidi|HATCHBACK|Dacia|Sandero|Rouge|5391 DK 00|100004
22100005|Moustapha Ould Cheikh|SUV|Toyota|Land Cruiser|Blanc|6248 EL 00|100005
22100006|Abdallahi Ould Mohamed|SUV|Nissan|Patrol|Noir|7135 FM 00|100006
22100007|Yahya Ould Bakar|AUTO_RICKSHAW|Piaggio|Ape|Jaune|8460 GN 00|100007
22100008|Elhadj Ould Amar|AUTO_RICKSHAW|Piaggio|Ape|Vert|9573 HP 00|100008
'

create_one() {
  local num="$1" name="$2" variant="$3" make="$4" model="$5" colour="$6" plate="$7"

  local did
  did=$(pg "SELECT id FROM atlas_driver_offer_bpp.person
             WHERE unencrypted_mobile_number = '$num'")

  if [ -z "$did" ]; then
    curl -s -o /dev/null -X POST "$D/ui/auth" -H 'content-type: application/json' \
      -d "{\"mobileCountryCode\":\"+222\",\"mobileNumber\":\"$num\",\"merchantId\":\"$MER\"}"
    did=$(pg "SELECT id FROM atlas_driver_offer_bpp.person
               WHERE unencrypted_mobile_number = '$num'")
  fi
  [ -n "$did" ] || { bad "$num: no person row appeared"; return 1; }

  # The name as the agency would record it: given name in first_name, the rest
  # in last_name. Not both in one field -- that is the duplication bug of
  # 2026-09-02, and this file is where a fleet of it would be created at once.
  local first last
  first="${name%% *}"
  last="${name#* }"
  [ "$last" = "$name" ] && last=''

  pg "UPDATE atlas_driver_offer_bpp.person
         SET first_name = '$first', last_name = $( [ -n "$last" ] && echo "'$last'" || echo NULL )
       WHERE id = '$did'" >/dev/null
  pg "UPDATE atlas_driver_offer_bpp.driver_information
         SET enabled = true, verified = true, blocked = false, active = true, on_ride = false
       WHERE driver_id = '$did'" >/dev/null
  pg "INSERT INTO atlas_driver_offer_bpp.driver_stats (driver_id) VALUES ('$did')
       ON CONFLICT (driver_id) DO NOTHING" >/dev/null

  if [ "$(pg "SELECT count(*) FROM atlas_driver_offer_bpp.vehicle WHERE driver_id = '$did'")" = "0" ]; then
    # vehicle_class '3WT' is copied from the rows dispatch is known to accept.
    # It reads wrong for a sedan and is an upstream artefact; dispatch matches
    # on `variant`, so it is left alone deliberately.
    pg "INSERT INTO atlas_driver_offer_bpp.vehicle
          (driver_id, capacity, make, model, variant, color, registration_no,
           merchant_id, vehicle_class, created_at, updated_at)
        VALUES ('$did', 4, '$make', '$model', '$variant', '$colour', '$plate',
                '$MER', '3WT', now(), now())" >/dev/null
  else
    pg "UPDATE atlas_driver_offer_bpp.vehicle
           SET make = '$make', model = '$model', variant = '$variant',
               color = '$colour', registration_no = '$plate', updated_at = now()
         WHERE driver_id = '$did'" >/dev/null
  fi

  ok "$num  $name  $variant  $plate"
}

place_all() {
  # Both the coordinates and the `point` in one statement, so they cannot
  # disagree -- see note 1 at the top.
  pg "UPDATE atlas_driver_offer_bpp.driver_location dl
         SET lat = $LAT + (random() - 0.5) * 0.01,
             lon = $LON + (random() - 0.5) * 0.01,
             coordinates_calculated_at = now(),
             updated_at = now()
        FROM atlas_driver_offer_bpp.person p
       WHERE p.id = dl.driver_id
         AND p.unencrypted_mobile_number LIKE '221000%'" >/dev/null

  # Some rows do not exist yet: a driver who has never sent a position has no
  # driver_location at all, and the UPDATE above silently touches nothing.
  #
  # `point` goes in the INSERT itself because the column is NOT NULL -- setting
  # it in a later statement fails the insert outright, which is how the first
  # run of this script created no positions at all while printing "placed".
  pg "INSERT INTO atlas_driver_offer_bpp.driver_location
         (driver_id, lat, lon, point, coordinates_calculated_at, created_at, updated_at)
       SELECT p.id, c.lat, c.lon,
              ST_SetSRID(ST_Point(c.lon, c.lat), 4326),
              now(), now(), now()
         FROM atlas_driver_offer_bpp.person p
        CROSS JOIN LATERAL (
              SELECT $LAT + (random() - 0.5) * 0.01 AS lat,
                     $LON + (random() - 0.5) * 0.01 AS lon
        ) c
        WHERE p.unencrypted_mobile_number LIKE '221000%'
          AND NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.driver_location dl
                           WHERE dl.driver_id = p.id)" >/dev/null

  pg "UPDATE atlas_driver_offer_bpp.driver_location dl
         SET point = ST_SetSRID(ST_Point(dl.lon, dl.lat), 4326)
        FROM atlas_driver_offer_bpp.person p
       WHERE p.id = dl.driver_id
         AND p.unencrypted_mobile_number LIKE '221000%'" >/dev/null

  # Assert rather than announce. A seeder that reports success while creating
  # nothing is how a demo fails at the worst possible moment.
  local placed want
  placed=$(pg "SELECT count(*) FROM atlas_driver_offer_bpp.driver_location dl
                 JOIN atlas_driver_offer_bpp.person p ON p.id = dl.driver_id
                WHERE p.unencrypted_mobile_number LIKE '221000%'
                  AND dl.point IS NOT NULL
                  AND dl.coordinates_calculated_at > now() - interval '2 minutes'")
  want=$(pg "SELECT count(*) FROM atlas_driver_offer_bpp.person
              WHERE unencrypted_mobile_number LIKE '221000%'")
  if [ "$placed" != "$want" ]; then
    bad "only $placed of $want drivers have a fresh position -- the pool will be short"
    return 1
  fi
  ok "$placed of $want drivers positioned, with a point and a fresh timestamp"
}

show() {
  say "the fleet, and how far each car is from the centre of Nouakchott"
  pg "SELECT '   ' || rpad(p.unencrypted_mobile_number, 10) ||
             rpad(coalesce(p.first_name,'') || ' ' || coalesce(p.last_name,''), 24) ||
             rpad(v.variant, 15) ||
             rpad(v.registration_no, 12) ||
             lpad(round(ST_Distance(dl.point::geography,
                    ST_SetSRID(ST_Point($LON, $LAT), 4326)::geography)::numeric)::text, 5) || ' m' ||
             '  active=' || di.active
        FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = p.id
        JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
        JOIN atlas_driver_offer_bpp.driver_location dl ON dl.driver_id = p.id
       WHERE p.unencrypted_mobile_number LIKE '221000%'
       ORDER BY v.variant, p.unencrypted_mobile_number"
}

case "${1:-seed}" in
  place) place_all; show; exit 0 ;;
  show)  show; exit 0 ;;
esac

say "creating the fleet"
echo "$FLEET" | while IFS='|' read -r num name variant make model colour plate code; do
  [ -z "$num" ] && continue
  create_one "$num" "$name" "$variant" "$make" "$model" "$colour" "$plate"
done

say "enrolling them in the guard, so any of them can also sign in"
echo "$FLEET" | while IFS='|' read -r num name variant make model colour plate code; do
  [ -z "$num" ] && continue
  ./enrol-driver.sh --set "$num" "$code" "$name" >/dev/null 2>&1 \
    && ok "$num  code $code" || bad "$num  enrolment failed"
done

say "putting them on the map"
place_all || exit 1

say "standing the Algerian fleet down"
# Not deleted. They are outside the Mauritanian geofence and unreachable
# anyway, but leaving them `active` means the pool carries rows it can never
# use, and a future reader would wonder why. One UPDATE brings them back.
n=$(pg "UPDATE atlas_driver_offer_bpp.driver_information di
           SET active = false
          FROM atlas_driver_offer_bpp.driver_location dl
         WHERE dl.driver_id = di.driver_id
           AND dl.lat BETWEEN 36.0 AND 37.5
           AND di.active
        RETURNING 1" | wc -l)
ok "$n Algerian driver(s) set inactive"

show
