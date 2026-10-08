#!/usr/bin/env python3
"""What year will the propositions screen print for each car we actually have?

The client accepted `118` on 2026-08-25 and, seeing it on a real screen the same
day, asked for `2018`. He is right: the middle group of an Algerian plate is
**[genre][YY]**, not a year -- the first digit is the category of vehicle (1
tourism, 2 lorry, 3 van, 4 bus, 9 motorcycle) and only the last two are the year
of first registration.

This mirrors `plateYear()` in the app exactly and runs it over every plate in the
fleet, so the change can be checked against real registrations instead of two
examples. Run it ON the VPS.
"""
import datetime
import subprocess
import sys

NOW = datetime.date.today().year

GENRE = {
    "1": "tourisme", "2": "camion", "3": "camionnette", "4": "autocar",
    "5": "tracteur routier", "6": "tracteur agricole", "7": "engin spécial",
    "8": "remorque", "9": "moto",
}


def plate_year(plate):
    """Byte-for-byte the app's rule, so a disagreement here is a real one."""
    groups = plate.strip().split()
    if len(groups) != 3:
        return None, "not three groups"
    middle = groups[1]
    if not (len(middle) == 3 and middle.isdigit()):
        return None, "middle group is not three digits"
    yy = int(middle[1:])
    year = 2000 + yy if 2000 + yy <= NOW else 1900 + yy
    if not (1960 <= year <= NOW):
        return None, f"decodes to {year}, outside 1960..{NOW}"
    return year, GENRE.get(middle[0], "genre " + middle[0])


rows = subprocess.run(
    ["docker", "exec", "ny-postgres", "psql", "-U", "postgres", "-d", "atlas_dev", "-tAc",
     "SELECT registration_no || '|' || coalesce(variant,'?') "
     "FROM atlas_driver_offer_bpp.vehicle ORDER BY registration_no;"],
    capture_output=True, text=True, timeout=60).stdout.strip().splitlines()

shown = skipped = 0
print(f"{'plate':<18} {'variant':<14} what the row will say")
print("-" * 72)
for line in rows:
    if "|" not in line:
        continue
    plate, variant = line.rsplit("|", 1)
    year, why = plate_year(plate)
    if year:
        shown += 1
        print(f"{plate:<18} {variant:<14} {year}   ({why})")
    else:
        skipped += 1
        print(f"{plate:<18} {variant:<14} —  falls back to the vehicle name ({why})")

print("-" * 72)
print(f"{shown} plates print a year, {skipped} fall back.")
print("A fallback is not a failure: upstream's seeded vehicles carry things like")
print("'4810' with no groups at all, and naming the vehicle beats printing 4810.")
sys.exit(0)
