"""The corporate shuttle GTFS naming convention, in one place.

Agreed with the shared-route foundation (Backend/dev/local-testing-data/shared-route-extra-setup.sql):

  feed / agency   corp_<city>            one feed per city, shared by every partner company
  route           CORP-<ORG>-<n>         <ORG> = the company's code, <n> = 1, 2, ...
  stop            CORP_<ORG>_S<n>        numbered per company, in order of first use
  trip            <route>-<HHMM>         the departure time from the route's first stop
  calendar        service days           one service per distinct (days, start, end)

The org code inside every id is what keeps companies apart in the shared city feed.
"""

import hashlib
import re
import uuid

ORG_RE = re.compile(r"^[A-Z0-9]+$")
CITY_RE = re.compile(r"^[a-z][a-z0-9_]*$")
ROUTE_RE = re.compile(r"^CORP-([A-Z0-9]+)-([1-9][0-9]*)$")
STOP_RE = re.compile(r"^CORP_([A-Z0-9]+)_S([1-9][0-9]*)$")
TRIP_RE = re.compile(r"^(CORP-[A-Z0-9]+-[1-9][0-9]*)-([0-9]{4})$")
HHMM_RE = re.compile(r"^([0-9]{2}):([0-9]{2})$")

DAYS = ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]
DAY_CODES = {"monday": "MON", "tuesday": "TUE", "wednesday": "WED", "thursday": "THU", "friday": "FRI", "saturday": "SAT", "sunday": "SUN"}


def check_city(city):
    if not CITY_RE.match(city):
        raise ValueError(f"city must be lower-case letters, digits or _ (e.g. chennai): {city!r}")
    return city


def check_org(org):
    if not ORG_RE.match(org or ""):
        raise ValueError(f"org code must be upper-case letters and digits only (e.g. ACME): {org!r}")
    return org


def feed_id(city):
    return "corp_" + check_city(city)


def agency_id(city):
    return feed_id(city)


def feed_dir_name(city):
    """The folder inside the zip and the zip's name, in the nandi/assets layout (<city>.<mode>.gtfs)."""
    return f"{check_city(city)}.corp_shuttle.gtfs"


def route_id(org, n):
    if not isinstance(n, int) or isinstance(n, bool) or n < 1:
        raise ValueError(f"route number must be a positive integer: {n!r}")
    return f"CORP-{check_org(org)}-{n}"


def stop_id(org, n):
    if not isinstance(n, int) or n < 1:
        raise ValueError(f"stop number must be a positive integer: {n!r}")
    return f"CORP_{check_org(org)}_S{n}"


def parse_hhmm(text):
    """'08:30' -> minutes after midnight."""
    m = HHMM_RE.match(text or "")
    if not m or int(m.group(1)) > 23 or int(m.group(2)) > 59:
        raise ValueError(f"time must be HH:MM (00:00-23:59): {text!r}")
    return int(m.group(1)) * 60 + int(m.group(2))


def trip_id(route, departure_hhmm):
    parse_hhmm(departure_hhmm)
    return f"{route}-{departure_hhmm.replace(':', '')}"


def service_id(days, start_date, end_date):
    """One GTFS service per distinct (days, start, end): e.g. MON_TUE_WED_THU_FRI_20270101_20271231."""
    ordered = [d for d in DAYS if d in days]
    if not ordered:
        raise ValueError("a route needs at least one service day")
    return "_".join(DAY_CODES[d] for d in ordered) + f"_{start_date}_{end_date}"


def gtfs_time(minutes):
    """Minutes after midnight -> HH:MM:SS (GTFS allows 24:00:00 and later for trips past midnight)."""
    return f"{minutes // 60:02d}:{minutes % 60:02d}:00"


def pg_md5_uuid(seed):
    """The id Postgres gives for md5('<seed>')::uuid::text, so generated SQL matches the fixture's ids."""
    return str(uuid.UUID(hashlib.md5(seed.encode()).hexdigest()))
