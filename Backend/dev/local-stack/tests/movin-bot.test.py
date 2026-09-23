#!/usr/bin/env python3
"""
Does the bot say the right thing, once, and never invent a fault?

Everything outside the process is stubbed: `docker` is a script on PATH that
prints the rows a query would have returned, Telegram is a local HTTP server
that records what it was asked to send, and the server snapshot and the
certificate directory are temporary files. No database, no network, no bot.

The three properties that matter, and each has cost somebody a night somewhere:
  * an alert is sent once, not on every tick;
  * an alert can fire again after its cause has cleared and returned;
  * a database that does not answer is never reported as "zero".
"""
import http.server
import json
import os
import subprocess
import sys
import tempfile
import threading
import urllib.parse

HERE = os.path.dirname(os.path.abspath(__file__))
BOT = os.path.join(HERE, "..", "movin-bot.py")

sent = []
fails = []


class Telegram(http.server.BaseHTTPRequestHandler):
    def do_POST(self):                                        # noqa: N802
        n = int(self.headers.get("Content-Length", 0))
        body = urllib.parse.parse_qs(self.rfile.read(n).decode())
        if self.path.endswith("/sendMessage"):
            sent.append(body.get("text", [""])[0])
            out = {"ok": True, "result": {}}
        else:                                    # getUpdates and anything else
            out = {"ok": True, "result": []}
        raw = json.dumps(out).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(raw)))
        self.end_headers()
        self.wfile.write(raw)

    def log_message(self, *_):
        pass


def ok(name, cond, detail=""):
    print(f"   {'PASS' if cond else '**FAIL**'}  {name}  {detail}")
    if not cond:
        fails.append(name)


def run_bot(work, rows, env=None):
    """One --once pass with `rows` as everything psql returns."""
    with open(os.path.join(work, "rows"), "w", encoding="utf-8") as fh:
        fh.write(rows)
    e = dict(os.environ)
    e.update({
        "PATH": os.path.join(work, "bin") + ":" + os.environ["PATH"],
        "TELEGRAM_BOT_TOKEN": "t", "TELEGRAM_CHAT_ID": "1",
        "BOT_TELEGRAM_API": f"http://127.0.0.1:{PORT}",
        "BOT_STATE": os.path.join(work, "state.json"),
        "BOT_SERVER_STATE": os.path.join(work, "server.json"),
        "BOT_CERT_DIR": os.path.join(work, "certs"),
        "BOT_GUARD_HEALTH": f"http://127.0.0.1:{PORT}/guard",
        "BOT_API_HEALTH": f"http://127.0.0.1:{PORT}/api",
        "ROWS_FILE": os.path.join(work, "rows"),
        # Out of quiet hours and away from digest hour, so a test at 3am and a
        # test at noon behave identically.
        "BOT_QUIET_START": "3", "BOT_QUIET_END": "4", "BOT_DIGEST_HOUR": "25",
    })
    e.update(env or {})
    subprocess.run([sys.executable, BOT, "--once"], env=e,
                   capture_output=True, text=True, timeout=60)


srv = http.server.HTTPServer(("127.0.0.1", 0), Telegram)
PORT = srv.server_address[1]
threading.Thread(target=srv.serve_forever, daemon=True).start()

work = tempfile.mkdtemp()
os.makedirs(os.path.join(work, "bin"))
os.makedirs(os.path.join(work, "certs"))
with open(os.path.join(work, "bin", "docker"), "w", encoding="utf-8") as fh:
    fh.write('#!/usr/bin/env bash\ncat "$ROWS_FILE"\n')
os.chmod(os.path.join(work, "bin", "docker"), 0o755)
with open(os.path.join(work, "server.json"), "w", encoding="utf-8") as fh:
    json.dump({"containers": [{"name": "ny-edge", "status": "Up 3 weeks"}],
               "backups": {"timer": {"last_result": "success"}, "archives": []},
               "measured_at": "now"}, fh)

# One pending driver, tab-free: the bot splits on the unit separator.
US = "\x1f"
PENDING = US.join(["id-aaa", "Yas Kara", "36664750", "0", "2", "1"])

print("1. A driver appears")
run_bot(work, PENDING)
first = len([m for m in sent if "nouvelle inscription" in m])
ok("announced once", first == 1, f"{first} message(s)")

print("\n2. Three more ticks, same driver")
before = len(sent)
for _ in range(3):
    run_bot(work, PENDING)
ok("silent while nothing changed", len(sent) == before,
   f"{len(sent) - before} extra")

print("\n3. He is validated: the queue empties")
sent.clear()
run_bot(work, "")
ok("nothing said about an empty queue", len(sent) == 0, f"{len(sent)} message(s)")

print("\n4. He comes back later — the same fault must be reportable again")
sent.clear()
run_bot(work, PENDING)
ok("announced again after clearing",
   len([m for m in sent if "nouvelle inscription" in m]) == 1)

print("\n5. The database does not answer")
sent.clear()
with open(os.path.join(work, "bin", "docker"), "w", encoding="utf-8") as fh:
    fh.write('#!/usr/bin/env bash\nexit 1\n')     # psql fails, like a dead pool
os.chmod(os.path.join(work, "bin", "docker"), 0o755)
run_bot(work, "")
invented = [m for m in sent if "aucune course" in m or "aucun chauffeur" in m]
ok("silence is not reported as a fleet outage", not invented,
   f"{len(invented)} invented alert(s)")

srv.shutdown()
print()
if fails:
    print("FAILED:", ", ".join(fails))
    sys.exit(1)
print("ALL PASSED")
