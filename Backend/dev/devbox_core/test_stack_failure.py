"""Unit tests for the stack failure-detection helpers (pure, no ssh)."""
from . import stack


def test_root_failure_picks_the_crashed_service_not_the_cascade():
    # today's real incident: driver app exits 1 with restarts; allocator Skipped waiting on it.
    procs = [
        {"name": "rider-app-exe", "status": "Running", "is_running": True, "restarts": 0, "exit_code": 0},
        {"name": "dynamic-offer-driver-app-exe", "status": "Completed", "is_running": False, "restarts": 4, "exit_code": 1},
        {"name": "driver-offer-allocator-exe", "status": "Skipped", "is_running": False, "restarts": 0, "exit_code": 1},
        {"name": "producer-exe", "status": "Pending", "is_running": False, "restarts": 0, "exit_code": 0},
    ]
    bad, skipped = stack._root_failure(procs)
    assert bad["name"] == "dynamic-offer-driver-app-exe"
    assert skipped == ["driver-offer-allocator-exe"]


def test_root_failure_detects_a_crash_loop_by_restarts():
    procs = [{"name": "notification-service", "status": "Restarting", "is_running": True, "restarts": 5, "exit_code": 1}]
    bad, _ = stack._root_failure(procs)
    assert bad is not None and bad["name"] == "notification-service"


def test_root_failure_none_when_all_healthy():
    procs = [
        {"name": "rider-app-exe", "status": "Running", "is_running": True, "restarts": 0, "exit_code": 0},
        {"name": "nammayatri-init", "status": "Completed", "is_running": False, "restarts": 0, "exit_code": 0},
    ]
    assert stack._root_failure(procs) == (None, [])


def test_hint_for_known_patterns():
    assert "dhall" in stack._hint_for("Error: Expression doesn't match annotation")
    assert "CONCURRENTLY" in stack._hint_for("SqlError {sqlState = \"25001\"")
    assert "race" in stack._hint_for("cabal: Text file busy")
    assert stack._hint_for("just some benign log") is None
