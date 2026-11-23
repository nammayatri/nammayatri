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
import demand_check  # noqa: E402
import interests  # noqa: E402
import naming  # noqa: E402
import plan as plans  # noqa: E402
import validate  # noqa: E402

EXAMPLE_INTERESTS = os.path.join(ROOT, "examples", "chennai_acme_interests.json")
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


class DemandTest(unittest.TestCase):
    def test_check_against_the_example(self):
        people = interests.load([EXAMPLE_INTERESTS])
        results = demand_check.evaluate(acme(), people)
        morning = [r for _, leg, r in results if leg == "TO_OFFICE"]
        self.assertEqual(len(morning), 11)
        self.assertEqual(sum(demand_check.covered(r) for r in morning), 9)
        evening = [r for _, leg, r in results if leg == "TO_HOME"]
        self.assertTrue(all(r["noRoute"] for r in evening))  # the fixture plan has no TO_HOME route

    def test_time_is_checked_day_by_day(self):
        # 18:30 on Tuesday/Friday, 18:45 on the other weekdays: a Mon-Fri rider wanting 18:40 +-15 fits every day
        route = {"offsets": [0, 10], "departures": [
            {"time": "18:30", "days": ["tuesday", "friday"]},
            {"time": "18:45", "days": ["monday", "wednesday", "thursday"]},
        ]}
        week = ["monday", "tuesday", "wednesday", "thursday", "friday"]
        self.assertTrue(all(demand_check.fits_on(route, 0, d, 18 * 60 + 40, 15) for d in week))
        self.assertFalse(demand_check.fits_on(route, 0, "saturday", 18 * 60 + 40, 15))
        self.assertFalse(demand_check.fits_on(route, 0, "tuesday", 18 * 60 + 40, 5))

    def test_other_companies_are_left_out(self):
        people = interests.load([EXAMPLE_INTERESTS])
        other = dict(people[0], personId="someone-else", company="Other Co")
        mine, others = demand_check.of_company(acme(), people + [other])
        self.assertEqual(others, 1)
        self.assertNotIn("someone-else", [p["personId"] for p in mine])



class PlannerTest(unittest.TestCase):
    def people(self):
        return interests.load([EXAMPLE_INTERESTS])

    def test_every_rider_is_placed_on_a_short_route(self):
        import planner
        people = self.people()
        office = {"lat": 12.8251, "lon": 80.2182}
        for direction in ("MORNING", "EVENING"):
            routes = planner.plan_day(people, "ACME Corp", office, direction, "monday")
            riding = planner.riders_for(people, direction, "monday")
            placed = [r["personId"] for route in routes for r in route["riders"]]
            self.assertEqual(sorted(placed), sorted(r["personId"] for r in riding), direction)
            for route in routes:
                end = route["stops"][-1] if direction == "MORNING" else route["stops"][0]
                self.assertEqual(end["kind"], "OFFICE")
                self.assertTrue(all(b > a for a, b in zip(route["offsets"], route["offsets"][1:])))

    def test_departures_fit_everyone(self):
        import planner
        self.assertEqual(planner.pick_departures([(510, 15), (515, 15), (630, 15)], 2), [510, 630])
        self.assertEqual(planner.pick_departures([(512, 0)], 2), [512])
        # 08:00 and 09:00, each +-30: the 08:30 slot inside both windows serves both riders with one departure
        self.assertEqual(planner.pick_departures([(480, 30), (540, 30)], 2), [510])

    def test_routes_keep_the_stop_limit(self):
        import planner
        people = self.people()
        office = {"lat": 12.8251, "lon": 80.2182}
        for max_stops in (1, 2, 3):
            for direction in ("MORNING", "EVENING"):
                routes = planner.plan_day(people, "ACME Corp", office, direction, "monday", 1, max_stops)
                riding = planner.riders_for(people, direction, "monday")
                placed = [r["personId"] for route in routes for r in route["riders"]]
                self.assertEqual(sorted(placed), sorted(r["personId"] for r in riding), (direction, max_stops))
                for route in routes:
                    pickups = [s for s in route["stops"] if s["kind"] != "OFFICE"]
                    self.assertLessEqual(len(pickups), max_stops, (direction, max_stops))

    def test_stop_names_never_use_an_address(self):
        import planner
        people = self.people()
        for p in people:
            self.assertNotIn("area", p)
        self.assertEqual(planner._label({"kind": "SHARED", "street": "Rajiv Gandhi Salai"}, "ACME Corp"), "Rajiv Gandhi Salai")
        self.assertEqual(planner._label({"kind": "DOORSTEP", "street": ""}, "ACME Corp"), "Pickup point")
        self.assertEqual(planner._label({"kind": "OFFICE"}, "ACME Corp"), "ACME Corp office")

    def test_plan_files_are_valid_and_merge_days(self):
        import argparse
        import plan_routes
        people = self.people()
        office = {"lat": 12.8251, "lon": 80.2182}
        args = argparse.Namespace(min_riders=2, max_stops=6, start="20261201", end="20271130", fare=40)
        company, report = plan_routes.plan_company(people, "ACME Corp", office, "ACME", args)
        checked = plans.check_plan(company)
        self.assertTrue(checked["routes"])
        self.assertTrue(all(r["routeId"].startswith("CORP-ACME-") for r in checked["routes"]))
        # weekday riders: their departures run on several days, as one route
        self.assertTrue(any(len(d["days"]) > 1 for r in checked["routes"] for d in r["departures"]))
        self.assertTrue(report)

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

    def test_days_merge_only_with_the_same_timings(self):
        import plan_routes
        stops = [{"lat": 12.9, "lon": 80.2}, {"lat": 12.8, "lon": 80.2}]
        self.assertEqual(plan_routes._key("MORNING", {"stops": stops, "offsets": [0, 10]}), plan_routes._key("MORNING", {"stops": stops, "offsets": [0, 10]}))
        self.assertNotEqual(plan_routes._key("MORNING", {"stops": stops, "offsets": [0, 10]}), plan_routes._key("MORNING", {"stops": stops, "offsets": [0, 14]}))


if __name__ == "__main__":
    unittest.main()
