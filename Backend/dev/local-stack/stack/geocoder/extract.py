#!/usr/bin/env python3
"""
Algeria OSM extract -> the rows our search index is built from.

WHY THIS EXISTS
---------------
The rider-app asks Google for autocomplete, place details and reverse
geocoding. We do not have Google, and mock-google answers with Indian
addresses. We do have the same Algeria .osm.pbf that already feeds OSRM and
the tile server, so the names are already on the box -- they just are not in a
form anything can search.

WHY NOT NOMINATIM / PHOTON / PELIAS
-----------------------------------
All three are address-first geocoders, and addresses are the one thing this
data does not have: measured over Algiers, 42,108 road ways carry only 5,204
`addr:housenumber` tags. "12 Rue Didouche Mourad" cannot resolve and never
will. What the data does have is 3,490 distinct street names and 7,956 named
POIs, 97% and 96% of them reachable by someone typing Latin characters. So the
index is landmark- and street-first by construction, which is also how people
in Algiers give directions.

The operational argument points the same way: those geocoders each want their
own datastore (Nominatim a dedicated PostgreSQL and a multi-hour import;
Photon and Pelias an Elasticsearch). The box has 11 GB and 17 containers on it
already. The whole Algiers dataset is ~13k rows.

WHAT IT DOES NOT DO
-------------------
 * No relations. Named multipolygons (a few large parks, some campuses) are
   skipped -- assembling areas costs a second pass for a small tail.
 * No house numbers, deliberately. See above.
 * A street becomes one row per way here; ways are collapsed by name and
   locality later, in SQL, where the locality is known.

Usage:  python3 extract.py <input.osm.pbf> <output.csv>
"""

import csv
import sys

import osmium

# ─── which names we show ────────────────────────────────────────────────────
# Measured on Algiers: 74% of distinct street names and 78% of POI names are
# already Latin, and ~86% of the Arabic-only ones carry one of these tags. A
# rider typing French finds 97% of streets and 96% of POIs -- but only if we
# look for the alternative rather than the primary `name`.
LATIN_TAGS = ("name:fr", "int_name", "name:latin", "name:en")

# Every tag we will accept as a searchable name, primary or not. All of them go
# into the match text, so typing Arabic works too.
NAME_TAGS = ("name", "name:fr", "name:ar", "name:en", "name:latin", "int_name",
             "alt_name", "old_name", "short_name", "official_name", "loc_name")

# ─── what counts as a place ─────────────────────────────────────────────────
STREET_HIGHWAYS = {
    "motorway": 0.50, "trunk": 0.55, "primary": 0.55, "secondary": 0.50,
    "tertiary": 0.45, "unclassified": 0.40, "residential": 0.40,
    "living_street": 0.38, "pedestrian": 0.45, "road": 0.35, "service": 0.30,
    "footway": 0.30, "path": 0.28, "steps": 0.25, "cycleway": 0.28,
}

PLACE_RANKS = {
    "city": 1.00, "town": 0.90, "borough": 0.85, "suburb": 0.80,
    "quarter": 0.76, "neighbourhood": 0.74, "village": 0.70,
    "hamlet": 0.60, "locality": 0.55, "isolated_dwelling": 0.45,
}

# A rider's destination is a landmark far more often than a shop, so the
# ordering here is about what people actually ask a driver for.
POI_RANKS = {
    "aerodrome": 0.95, "station": 0.85, "bus_station": 0.82, "hospital": 0.82,
    "university": 0.80, "stadium": 0.80, "mall": 0.78, "college": 0.74,
    "townhall": 0.74, "police": 0.72, "marketplace": 0.72, "clinic": 0.68,
    "mosque": 0.66, "school": 0.64, "place_of_worship": 0.64, "museum": 0.64,
    "hotel": 0.62, "bank": 0.60, "pharmacy": 0.60, "fuel": 0.58,
    "post_office": 0.58, "supermarket": 0.58, "restaurant": 0.52,
    "cafe": 0.50, "bus_stop": 0.42, "tram_stop": 0.48, "subway_entrance": 0.46,
}

# Keys that make something a point of interest at all. `building` is last on
# purpose: a named building is only interesting when nothing better applies.
POI_KEYS = ("aeroway", "railway", "amenity", "shop", "tourism", "leisure",
            "office", "healthcare", "public_transport", "historic", "craft",
            "military", "building")

# Named, but never a destination. Cheap to drop here, awkward to filter later.
SKIP_VALUES = {
    "yes", "house", "residential", "apartments", "hut", "shed", "garage",
    "garages", "roof", "wall", "fence", "tree", "bench", "waste_basket",
    "drinking_water", "bicycle_parking", "surveillance", "street_lamp",
    "level_crossing", "crossing", "traffic_signals", "give_way", "stop",
    "turning_circle", "milestone", "elevator", "vending_machine",
}


def pick(tags, keys):
    for k in keys:
        v = tags.get(k)
        if v and v.strip():
            return v.strip()
    return None


def display_name(tags):
    """
    The single string a rider reads. French first, then any Latin alternative,
    then whatever the primary name is -- an Arabic-only name is still far
    better than dropping the place.
    """
    return pick(tags, LATIN_TAGS) or pick(tags, ("name",))


def all_names(tags):
    seen, out = set(), []
    for k in NAME_TAGS:
        v = tags.get(k)
        if v and v.strip() and v.strip() not in seen:
            seen.add(v.strip())
            out.append(v.strip())
    return out


def classify(tags):
    """(kind, class, subclass, importance) or None to skip."""
    place = tags.get("place")
    if place in PLACE_RANKS:
        return "place", "place", place, PLACE_RANKS[place]

    highway = tags.get("highway")
    if highway in STREET_HIGHWAYS:
        return "street", "highway", highway, STREET_HIGHWAYS[highway]

    for key in POI_KEYS:
        value = tags.get(key)
        if not value or value in SKIP_VALUES:
            continue
        kind = "transport" if key in ("aeroway", "railway", "public_transport") else "poi"
        # A named building with no other tag is the weakest thing we keep.
        base = 0.35 if key == "building" else 0.45
        return kind, key, value, POI_RANKS.get(value, base)

    return None


class Collector(osmium.SimpleHandler):
    def __init__(self, writer):
        super().__init__()
        self.writer = writer
        self.counts = {"node": 0, "way": 0, "skipped_no_location": 0}

    def emit(self, osm_type, osm_id, tags, lat, lon):
        shown = display_name(tags)
        if not shown:
            return
        klass = classify(tags)
        if klass is None:
            return
        kind, cls, subclass, importance = klass

        self.writer.writerow([
            osm_type, osm_id, kind, cls, subclass,
            tags.get("name", ""), tags.get("name:fr", ""), tags.get("name:en", ""),
            pick(tags, ("name:latin", "int_name")) or "",
            shown,
            "|".join(all_names(tags)),
            f"{importance:.3f}",
            f"{lat:.7f}", f"{lon:.7f}",
        ])
        self.counts[osm_type] = self.counts.get(osm_type, 0) + 1

    def node(self, n):
        if not n.tags:
            return
        self.emit("node", n.id, dict(n.tags), n.location.lat, n.location.lon)

    def way(self, w):
        if not w.tags or len(w.nodes) == 0:
            return
        try:
            if w.is_closed():
                # A named closed way is a building, park or campus: the middle
                # of the outline is meaningless, the centre of it is not.
                lats = [n.location.lat for n in w.nodes if n.location.valid()]
                lons = [n.location.lon for n in w.nodes if n.location.valid()]
                if not lats:
                    raise osmium.InvalidLocationError()
                lat, lon = sum(lats) / len(lats), sum(lons) / len(lons)
            else:
                # A street: its midpoint is a point actually on the street,
                # which the centroid of a bend is not.
                usable = [n.location for n in w.nodes if n.location.valid()]
                if not usable:
                    raise osmium.InvalidLocationError()
                middle = usable[len(usable) // 2]
                lat, lon = middle.lat, middle.lon
        except osmium.InvalidLocationError:
            self.counts["skipped_no_location"] += 1
            return
        self.emit("way", w.id, dict(w.tags), lat, lon)


def main():
    if len(sys.argv) != 3:
        sys.exit(f"usage: {sys.argv[0]} <input.osm.pbf> <output.csv>")
    src, dst = sys.argv[1], sys.argv[2]

    with open(dst, "w", newline="", encoding="utf-8") as fh:
        writer = csv.writer(fh)
        writer.writerow([
            "osm_type", "osm_id", "kind", "class", "subclass",
            "name", "name_fr", "name_en", "name_latin",
            "display_name", "alt_names", "importance", "lat", "lon",
        ])
        handler = Collector(writer)
        # `locations=True` keeps every node's position in memory so a way can
        # be given a point. flex_mem is the sparse index -- roughly a gigabyte
        # for a country this size, which the box has.
        handler.apply_file(src, locations=True, idx="flex_mem")

    print(f"nodes    {handler.counts.get('node', 0):>8}")
    print(f"ways     {handler.counts.get('way', 0):>8}")
    print(f"no-loc   {handler.counts['skipped_no_location']:>8}  (ways outside the extract)")
    print(f"-> {dst}")


if __name__ == "__main__":
    main()
