"""Load the control centre's interest export.

Input: one or more saved responses of the dashboard API
  GET .../shuttleInterest/list?passOrganizationId=<company>&limit=100&offset=<0,100,...>
(the list is capped at 100 rows a call, so save each page; a bare JSON list of items works too). Every row is one
employee; a person appearing on two pages is kept once.
"""

import json

import naming

TRIP_TYPES = ("TO_OFFICE", "TO_HOME", "ROUND_TRIP")


def _minutes(t):
    """'08:30:00' (or '08:30', '08:30:00.000') -> minutes after midnight; None stays None."""
    if t is None:
        return None
    parts = str(t).split(":")
    return int(parts[0]) * 60 + int(parts[1])


def load(paths):
    rows = []
    for path in paths:
        with open(path) as f:
            data = json.load(f)
        items = data.get("interests", []) if isinstance(data, dict) else data
        if not isinstance(items, list):
            raise ValueError(f"{path}: expected the shuttleInterest/list response or a list of its items")
        rows.extend(items)
    people = {}
    for r in rows:
        pid = r.get("personId")
        if not pid:
            raise ValueError("an interest row has no personId")
        trip = r.get("tripType")
        if trip not in TRIP_TYPES:
            raise ValueError(f"interest {pid}: unknown tripType {trip!r}")
        days = [str(d).lower() for d in r.get("daysOfWeek") or []]
        people[pid] = {
            "personId": pid,
            "name": r.get("employeeName") or "",
            "company": r.get("companyName") or r.get("companyDomain") or "",
            "homeLat": float(r["pickupLat"]),
            "homeLon": float(r["pickupLon"]),
            "officeLat": float(r["dropLat"]),
            "officeLon": float(r["dropLon"]),
            "officeAddress": r.get("dropAddress") or "",
            "toOffice": trip in ("TO_OFFICE", "ROUND_TRIP"),
            "toHome": trip in ("TO_HOME", "ROUND_TRIP"),
            "pickupMin": _minutes(r.get("pickupTime")),
            "dropMin": _minutes(r.get("dropTime")),
            "days": [d for d in naming.DAYS if d in days],
        }
    return [people[k] for k in sorted(people)]
