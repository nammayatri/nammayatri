"""python3 -m unittest discover -s tests   (from Backend/dev/corporate-shuttle-routes)"""

import argparse
import copy
import json
import os
import sys
import tempfile

os.environ["OSRM_URL"] = "http://127.0.0.1:9"  # no OSRM in tests: the planner falls back to straight-line estimates
import unittest
import zipfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, ROOT)

import build_feed  # noqa: E402
import interests  # noqa: E402
import naming  # noqa: E402
import plan as plans  # noqa: E402
import validate  # noqa: E402

ACME_PLAN = os.path.join(ROOT, "plans", "chennai", "ACME.json")


def acme():
    return plans.load_plan(ACME_PLAN)


class NamingTest(unittest.TestCase):
    def test_convention(self):
        self.assertEqual(naming.feed_id("chennai"), "corp_chennai")
        self.assertEqual(naming.route_id("ACME", 1), "CORP-ACME-1")
        self.assertEqual(naming.stop_id("ACME", 5), "CORP_ACME_S5")
        self.assertEqual(naming.trip_id("CORP-ACME-1", "08:30"), "CORP-ACME-1-0830")
        self.assertEqual(naming.service_id(["friday", "monday"], "20270101", "20271231"), "MON_FRI_20270101_20271231")
        self.assertEqual(naming.feed_dir_name("chennai"), "chennai.corp_shuttle.gtfs")

    def test_rejects_bad_codes(self):
        for bad in ("acme", "AC-ME", "", None):
            with self.assertRaises(ValueError):
                naming.check_org(bad)
        with self.assertRaises(ValueError):
            naming.route_id("ACME", 0)
        with self.assertRaises(ValueError):
            naming.parse_hhmm("24:00")

    def test_md5_uuid_matches_the_fixture(self):
        # md5('e2e-corp-shuttle-ibc')::uuid::text in Postgres
        self.assertEqual(naming.pg_md5_uuid("e2e-corp-shuttle-ibc"), "5190da6e-3400-8d10-9f5a-62af9dbf7dff")


class PlanTest(unittest.TestCase):
    def raw(self):
        with open(ACME_PLAN) as f:
            return json.load(f)

    def test_acme_resolves_to_the_fixture_ids(self):
        company = acme()
        _, route_stops = plans.resolve_stop_ids(company)
        self.assertEqual(route_stops["CORP-ACME-1"], [f"CORP_ACME_S{i}" for i in range(1, 6)])
        self.assertEqual(company["routes"][0]["offsets"], [0, 10, 20, 30, 40])

    def test_rejections(self):
        cases = {
            "one stop": lambda r: r["routes"][0].update(stops=r["routes"][0]["stops"][:1]),
            "repeated departure": lambda r: r["routes"][0].update(departures=["08:30", "08:30"]),
            "no days": lambda r: r["routes"][0].update(serviceDays=[]),
            "decreasing offsets": lambda r: r["routes"][0].pop("minutesBetweenStops") and r["routes"][0].update(stopOffsetsMinutes=[0, 10, 5, 20, 30]),
            "zero fare": lambda r: r["routes"][0].update(fareInr=0),
            "bad org": lambda r: r.update(org="acme"),
            "end before start": lambda r: r["routes"][0].update(startDate="20271231", endDate="20270101"),
            "no such date": lambda r: r["routes"][0].update(startDate="20270230", endDate="20271231"),
            "shape latitude out of range": lambda r: r["routes"][0].update(shape=[[100, 80.2], [12.9, 80.3]]),
            "shape point not a number": lambda r: r["routes"][0].update(shape=[["x", 80.2], [12.9, 80.3]]),
        }
        for name, mutate in cases.items():
            raw = self.raw()
            mutate(raw)
            with self.subTest(name), self.assertRaises(ValueError):
                plans.check_plan(raw)

    def test_shared_stop_gets_one_id(self):
        raw = self.raw()
        evening = copy.deepcopy(raw["routes"][0])
        evening.update(n=2, direction="TO_HOME", stops=list(reversed(evening["stops"]))[:3], departures=["18:00"])
        raw["routes"].append(evening)
        stop_ids, route_stops = plans.resolve_stop_ids(plans.check_plan(raw))
        self.assertEqual(len(stop_ids), 5)
        self.assertEqual(route_stops["CORP-ACME-2"], ["CORP_ACME_S5", "CORP_ACME_S4", "CORP_ACME_S3"])


class BuildTest(unittest.TestCase):
    def test_build_validate_and_determinism(self):
        config = plans.load_city_config("chennai")
        with tempfile.TemporaryDirectory() as tmp:
            first = build_feed.build("chennai", out_dir=tmp)
            with open(first["zip"], "rb") as f:
                bytes1 = f.read()
            second = build_feed.build("chennai", out_dir=tmp)
            with open(second["zip"], "rb") as f:
                self.assertEqual(bytes1, f.read(), "a rebuild with no plan change must be byte-identical")
            with zipfile.ZipFile(first["zip"]) as zf:
                self.assertTrue(all(n.startswith("chennai.corp_shuttle.gtfs/") for n in zf.namelist()))
            with open(first["sql"]) as f:
                sql = f.read()
            result = validate.check("chennai", validate.load_zip(first["zip"]), sql, config)
            self.assertEqual({k: v for k, v in result.items() if v}, {})
            self.assertEqual(sql.count("'ADULT'"), 10)  # 5 stops -> 10 forward pairs

    def test_two_companies_share_one_feed(self):
        config = plans.load_city_config("chennai")
        other = plans.check_plan(
            {
                "org": "GLOBEX",
                "routes": [
                    {
                        "n": 1,
                        "name": "Adyar - Globex",
                        "stops": [{"name": "Adyar", "lat": 13.0067, "lon": 80.2573}, {"name": "Globex", "lat": 12.97, "lon": 80.25}],
                        "minutesBetweenStops": 15,
                        "departures": ["09:00"],
                        "serviceDays": ["monday"],
                        "startDate": "20261201",
                        "endDate": "20271130",
                        "fareInr": 30,
                    }
                ],
            }
        )
        companies = [acme(), other]
        files = build_feed.build_files("chennai", config, companies)
        sql = build_feed.build_fares_sql("chennai", config, companies)
        result = validate.check("chennai", files, sql, config)
        self.assertEqual({k: v for k, v in result.items() if v}, {})
        tables = validate.read_tables(files)
        self.assertEqual(sorted(r["route_id"] for r in tables["routes"]), ["CORP-ACME-1", "CORP-GLOBEX-1"])
        self.assertEqual([r["feed_id"] for r in tables["feed_info"]], ["corp_chennai"])

    def test_selftest(self):
        self.assertTrue(validate.selftest())


class ValidateTest(unittest.TestCase):
    def test_a_trip_without_stop_times_fails(self):
        t = {"routes": [], "trips": [{"trip_id": "CORP-ACME-1-0830"}, {"trip_id": "CORP-ACME-1-1800"}], "stop_times": [
            {"trip_id": "CORP-ACME-1-0830", "stop_sequence": "1", "departure_time": "08:30:00"},
            {"trip_id": "CORP-ACME-1-0830", "stop_sequence": "2", "departure_time": "08:40:00"},
        ]}
        self.assertIn("trip CORP-ACME-1-1800 has fewer than 2 stops", validate.rule_trips("chennai", t))

    def test_gtfstidy_failing_silently_is_an_error(self):
        from unittest import mock
        silent = mock.Mock(returncode=-9, stdout="", stderr="")
        with mock.patch.object(validate.shutil, "which", return_value="/bin/gtfstidy"), mock.patch.object(validate.subprocess, "run", return_value=silent):
            errors, _ = validate.rule_gtfstidy("/tmp")
        self.assertEqual(errors, ["gtfstidy exited with code -9 and printed nothing"])


class PlannerTest(unittest.TestCase):
    OFFICE = {"lat": 12.9279, "lon": 77.6271}

    def person(self, pid, lat, lon, pickup="08:30", drop="18:30", days=("monday", "tuesday")):
        row = {"personId": pid, "companyName": "ACME Corp", "pickupLat": lat, "pickupLon": lon, "dropLat": 12.9279, "dropLon": 77.6271,
               "tripType": "ROUND_TRIP", "pickupTime": pickup + ":00", "dropTime": drop + ":00", "daysOfWeek": list(days)}
        path = os.path.join(tempfile.mkdtemp(), "rows.json")
        with open(path, "w") as f:
            json.dump([row], f)
        return interests.load([path])[0]

    def people(self):
        # stop A (5 people within 500 m: 3 at 08:30, 2 at 09:30), stop B 1.5 km on (2 at 08:30, 3 at 09:30), stop C across town
        a = [self.person(f"a{i}", 12.9600 + i * 0.0005, 77.6400, "08:30" if i < 3 else "09:30") for i in range(5)]
        b = [self.person(f"b{i}", 12.9480 + i * 0.0005, 77.6380, "08:30" if i < 2 else "09:30") for i in range(5)]
        c = [self.person(f"c{i}", 12.8900 + i * 0.0005, 77.5800) for i in range(2)]
        return a + b + c

    def test_nearby_stops_share_a_route_and_the_same_place_gets_a_shuttle_per_time(self):
        import planner
        routes = planner.plan_direction(self.people(), "ACME Corp", self.OFFICE, "MORNING")
        self.assertEqual(len(routes), 2)
        ab = next(r for r in routes if len(r["stops"]) == 3)
        self.assertEqual([s["kind"] for s in ab["stops"]], ["SHARED", "SHARED", "OFFICE"])
        self.assertEqual([len(ids) for _, ids in ab["trips"]["monday"]], [5, 5])
        self.assertEqual(ab["trips"]["monday"][0][0], 8 * 60 + 30)  # at stop A, the first stop, at 08:30
        self.assertEqual(ab["trips"]["monday"], ab["trips"]["tuesday"])
        self.assertNotIn("wednesday", ab["trips"])

    def test_evening_is_the_same_layout_reversed(self):
        import planner
        people = self.people()
        morning = planner.plan_direction(people, "ACME Corp", self.OFFICE, "MORNING")
        evening = planner.plan_direction(people, "ACME Corp", self.OFFICE, "EVENING")
        self.assertEqual([[(s["lat"], s["lon"]) for s in r["stops"]][::-1] for r in morning], [[(s["lat"], s["lon"]) for s in r["stops"]] for r in evening])
        self.assertTrue(all(r["stops"][0]["kind"] == "OFFICE" for r in evening))

    def test_shuttles_keep_to_the_seats(self):
        import planner
        sizes = lambda wishes: [(t, len(ids)) for t, ids in planner.shuttles(wishes)]
        self.assertEqual(sizes([(510, f"p{i}") for i in range(10)]), [(510, 5), (515, 5)])  # 10 at 08:30: two of 5
        self.assertEqual(sizes([(510, "a"), (510, "b"), (510, "c"), (540, "d"), (540, "e")]), [(510, 5)])  # 3 + 2: one
        self.assertEqual(sizes([(510, f"p{i}") for i in range(9)] + [(525, "x"), (525, "y")]), [(510, 5), (515, 6)])
        self.assertEqual(sizes([(510, "a"), (600, "b")]), [(510, 1), (600, 1)])  # too far apart: light, still run

    def test_stop_names_never_use_an_address(self):
        import planner
        self.assertEqual(planner._label({"kind": "SHARED", "street": "Rajiv Gandhi Salai"}, "ACME Corp"), "Rajiv Gandhi Salai")
        self.assertEqual(planner._label({"kind": "SHARED", "street": ""}, "ACME Corp"), "Pickup point")
        self.assertEqual(planner._label({"kind": "OFFICE"}, "ACME Corp"), "ACME Corp office")

    def test_plan_files_are_valid_and_carry_the_days(self):
        import plan_routes
        args = argparse.Namespace(start="20261201", end="20271130", fare=40)
        company, report = plan_routes.plan_company(self.people(), "ACME Corp", self.OFFICE, "ACME", args)
        checked = plans.check_plan(company)
        self.assertEqual(len(checked["routes"]), 4)  # 2 routes each way
        self.assertTrue(all(r["routeId"].startswith("CORP-ACME-") for r in checked["routes"]))
        self.assertTrue(all(d["days"] == ["monday", "tuesday"] for r in checked["routes"] for d in r["departures"]))
        self.assertEqual(len(report), 4)

    def test_org_codes_follow_the_convention(self):
        import plan_routes
        self.assertEqual(plan_routes.org_code("Namma Yatri", {}), "NY")
        self.assertEqual(plan_routes.org_code("Juspay", {"orgs": {}}), "JUSPAY")
        self.assertEqual(plan_routes.org_code("Juspay", {"orgs": {"Juspay": "JP"}}), "JP")

    def test_clashing_org_codes_stop_the_run(self):
        import plan_routes
        with self.assertRaises(SystemExit):
            plan_routes.org_codes(["acme.com", "acme.org"], {})
        self.assertEqual(plan_routes.org_codes(["acme.com", "acme.org"], {"orgs": {"acme.org": "ACMEORG"}}), {"acme.com": "ACME", "acme.org": "ACMEORG"})


if __name__ == "__main__":
    unittest.main()
