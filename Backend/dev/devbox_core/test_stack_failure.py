"""Tests for the stack failure detection and the GitHub error annotation.

Run from Backend/dev:  python3 -m pytest devbox_core/test_stack_failure.py
No dev-box needed: ssh.run is replaced by a local bash run or a fake."""
import subprocess

import pytest

from . import output, stack


def P(name, status="Running", running=True, restarts=0, exit_code=0):
    return {"name": name, "status": status, "is_running": running,
            "restarts": restarts, "exit_code": exit_code}


# --- which service broke -------------------------------------------------------

def test_driver_app_crash_loop_is_the_root_not_the_skipped_cascade():
    procs = [
        P("rider-app-exe"),
        P("dynamic-offer-driver-app-exe", "Completed", False, 4, 1),
        P("driver-offer-allocator-exe", "Skipped", False, 0, 1),
        P("producer-exe", "Pending", False),
    ]
    bad, skipped = stack._root_failure(procs)
    assert bad["name"] == "dynamic-offer-driver-app-exe"
    assert skipped == ["driver-offer-allocator-exe"]


def test_init_failure_is_caught():
    bad, _ = stack._root_failure([P("nammayatri-init", "Completed", False, 0, 1),
                                  P("rider-app-exe", "Pending", False)])
    assert bad["name"] == "nammayatri-init"


def test_rider_app_restarting_three_times_is_a_crash_loop():
    bad, _ = stack._root_failure([P("rider-app-exe", "Restarting", True, 3, 1)])
    assert bad["name"] == "rider-app-exe"


def test_one_or_two_app_restarts_are_not_a_failure():
    assert stack._root_failure([P("rider-app-exe", "Running", True, 2, 1)]) == (None, [])


def test_designed_restarts_of_other_services_are_ignored():
    procs = [
        P("mock-registry", "Restarting", True, 12, 1),        # restart = always, liveness probe
        P("notification-service", "Restarting", True, 5, 1),  # waits for its deps
        P("metabase-setup", "Completed", False, 0, 1),         # optional tool
        P("rider-app-exe"), P("nammayatri-init", "Completed", False, 0, 0),
    ]
    assert stack._root_failure(procs) == (None, [])


# --- error titles and GitHub escaping ------------------------------------------

@pytest.mark.parametrize("stage,title", [
    ("preflight", "Stack setup failed"), ("nix-eval", "Stack setup failed"),
    ("cache-restore", "Build failed"), ("build", "Build failed"),
    ("services", "Start failed"), ("whatever", "Release failed"),
])
def test_title_follows_the_stage(stage, title):
    assert stack.StackFailure(stage, "x").title == title


def test_annotation_is_escaped_and_carries_the_reason(capsys):
    output.annotate("Build failed: init, step 2", "nammayatri-init exited with code 1\n"
                    "SqlError 100% broken\r\nline 3")
    first, *rest = capsys.readouterr().out.splitlines()
    assert first == ("::error title=Build failed%3A init%2C step 2::"
                     "nammayatri-init exited with code 1%0ASqlError 100%25 broken%0Aline 3")
    assert rest[0] == "nammayatri-init exited with code 1"   # full message in the log too


# --- reading the failed service's log (real bash on temp files) -----------------

@pytest.fixture
def local_ssh(monkeypatch):
    def run(t, script, timeout=20):
        return subprocess.run(["bash", "-c", script], capture_output=True, text=True, timeout=timeout)
    monkeypatch.setattr(stack.ssh, "run", run)


def test_service_errors_reads_the_service_log_under_backend(tmp_path, local_ssh):
    (tmp_path / "Backend").mkdir()
    (tmp_path / "Backend" / "nammayatri-init.log").write_text(
        "starting\nExecuting: 0042-foo.sql\n\x1b[31mSqlError {sqlState = \"42P01\"}\x1b[0m\ndone\n")
    out = stack.service_errors(None, str(tmp_path), "nammayatri-init")
    assert out.splitlines() == ["Executing: 0042-foo.sql", 'SqlError {sqlState = "42P01"}']


def test_service_errors_falls_back_to_the_stack_log(tmp_path, local_ssh):
    (tmp_path / "data").mkdir()
    (tmp_path / "data" / "stack.log").write_text(
        "[rider-app-exe\t] Error: Expression doesn't match annotation\n"
        "[mock-registry\t] error: unrelated\n")
    out = stack.service_errors(None, str(tmp_path), "rider-app-exe")
    assert out == "[rider-app-exe\t] Error: Expression doesn't match annotation"


def test_service_errors_shows_the_last_lines_when_nothing_looks_like_an_error(tmp_path, local_ssh):
    (tmp_path / "rider-app-exe.log").write_text("".join(f"line {i}\n" for i in range(30)))
    out = stack.service_errors(None, str(tmp_path), "rider-app-exe", n=3)
    assert out.splitlines() == ["line 27", "line 28", "line 29"]


# --- wait_ready fails fast, in any stage after the build starts ----------------

def _fake_stack(monkeypatch, stage, procs, errs="Executing: 0042-foo.sql\nSqlError x"):
    monkeypatch.setattr(stack, "status", lambda t, w: {"alive": True, "stage": stage, "buildFailed": False})
    monkeypatch.setattr(stack, "services", lambda t, w: procs)
    monkeypatch.setattr(stack, "service_errors", lambda t, w, n: errs)
    monkeypatch.setattr(stack, "build_progress", lambda t, w: "")
    monkeypatch.setattr(stack, "office_base", lambda h, n: "http://x/")
    monkeypatch.setattr(stack, "_answers", lambda u: False)
    monkeypatch.setattr(stack.time, "sleep", lambda s: pytest.fail("should have failed, not waited"))


def test_init_crash_during_build_stage_fails_fast_as_build_failed(monkeypatch):
    # the apps never print, so the stage stays "build"; before, this waited out the timeout
    _fake_stack(monkeypatch, "build", [P("nammayatri-init", "Completed", False, 0, 1),
                                       P("rider-app-exe", "Skipped", False)])
    with pytest.raises(stack.StackFailure) as e:
        stack.wait_ready(type("T", (), {"host": "h"})(), "ws", "/w", out=lambda s: None)
    assert e.value.title == "Build failed"
    msg = str(e.value)
    assert msg.startswith("nammayatri-init exited with code 1\nHint: a DB migration failed")
    assert "Did not start (waiting on it): rider-app-exe" in msg


def test_app_crash_loop_during_services_is_start_failed(monkeypatch):
    _fake_stack(monkeypatch, "services", [P("rider-app-exe", "Restarting", True, 4, 1)],
                errs="Error: Expression doesn't match annotation")
    with pytest.raises(stack.StackFailure) as e:
        stack.wait_ready(type("T", (), {"host": "h"})(), "ws", "/w", out=lambda s: None)
    assert e.value.title == "Start failed"
    assert str(e.value).startswith("rider-app-exe keeps crashing (4 restarts)\nHint: dhall config")


def test_hint_for_known_patterns():
    assert "dhall" in stack._hint_for("Error: Expression doesn't match annotation")
    assert "CONCURRENTLY" in stack._hint_for("SqlError {sqlState = \"25001\"")
    assert "race" in stack._hint_for("cabal: Text file busy")
    assert stack._hint_for("just some benign log") is None
