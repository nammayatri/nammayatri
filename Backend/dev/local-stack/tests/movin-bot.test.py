#!/usr/bin/env python3
"""
Does the bot say the right thing, once, and never invent a fault?

Everything outside the process is stubbed: `docker` is a script on PATH that
prints the rows a query would have returned -- the rows in `tables/<name>` when
the query names that table, else the default rows -- Telegram is a local HTTP server
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
BOT = os.path.join(HERE, "..", "stack", "movin-bot.py")

sent = []
fails = []
# What the guard's /healthz answers; None is a guard that does not answer.
GUARD = None


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

    def do_GET(self):                                         # noqa: N802
        if self.path == "/guard" and GUARD is not None:
            raw = json.dumps(GUARD).encode()
            self.send_response(200)
        else:
            raw = b"{}"
            self.send_response(404)
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


def run_bot(work, rows, env=None, tables=None):
    """One --once pass with `rows` as what psql returns, except for a query
    naming a table in `tables`, which gets that table's rows."""
    with open(os.path.join(work, "rows"), "w", encoding="utf-8") as fh:
        fh.write(rows)
    tdir = os.path.join(work, "tables")
    for f in os.listdir(tdir):
        os.remove(os.path.join(tdir, f))
    for name, body in (tables or {}).items():
        with open(os.path.join(tdir, name), "w", encoding="utf-8") as fh:
            fh.write(body)
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
        "ROWS_DIR": tdir,
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
os.makedirs(os.path.join(work, "tables"))
DOCKER = """#!/usr/bin/env bash
sql="${@: -1}"
for f in "$ROWS_DIR"/*; do
  [ -e "$f" ] || continue
  case "$sql" in *"$(basename "$f")"*) cat "$f"; exit 0 ;; esac
done
cat "$ROWS_FILE"
"""
with open(os.path.join(work, "bin", "docker"), "w", encoding="utf-8") as fh:
    fh.write(DOCKER)
os.chmod(os.path.join(work, "bin", "docker"), 0o755)
with open(os.path.join(work, "server.json"), "w", encoding="utf-8") as fh:
    json.dump({"containers": [{"name": "ny-edge", "status": "Up 3 weeks"}],
               "backups": {"timer": {"last_result": "success"}, "archives": []},
               "measured_at": "now"}, fh)

# One pending driver, tab-free: the bot splits on the unit separator.
US = "\x1f"
MR = "favorit0-0000-0000-0000-00000favorit"
DZ = "algeria0-0000-0000-0000-00000algeria"
PENDING = US.join(["id-aaa", "Yas Kara", "36664750", "0", "2", "1", MR, ""])

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

print("\n5. A driver registers in ALGERIA (2026-09-27: never announced before)")
sent.clear()
run_bot(work, US.join(["id-dz1", "Amine Test", "0666123456", "0", "0", "0", DZ, ""]))
dz = [m for m in sent if "nouvelle inscription" in m]
ok("announced", len(dz) == 1, f"{len(dz)} message(s)")
ok("and says Algeria", bool(dz) and "Algérie" in dz[0], dz[:1])

print("\n5b. A refused driver sends his file again")
sent.clear()
run_bot(work, US.join(["id-rs1", "Sidi Test", "36664751", "0", "2", "1", MR, "resubmitted"]))
rs = [m for m in sent if "renvoyé après refus" in m]
ok("announced as a resubmission, not a new sign-up", len(rs) == 1, sent[:1])

print("\n6. A passenger reports a driver -- at night too")
sent.clear()
REPORT = US.join(["41", "Il roulait trop vite", "AB12CD", "Karim B", "0666123456",
                  "00001 116 16", DZ, "Sara", "0555123456"])
run_bot(work, "", tables={"movin.ride_report": REPORT},
        env={"BOT_QUIET_START": "0", "BOT_QUIET_END": "23"})   # always quiet
rep_ = [m for m in sent if "SIGNALEMENT" in m]
ok("announced through quiet hours", len(rep_) == 1, f"{len(rep_)} message(s)")
ok("with the text, both people and the country",
   bool(rep_) and all(x in rep_[0] for x in
                      ("Il roulait trop vite", "Karim B", "Sara", "Algérie")), rep_[:1])
before = len(sent)
run_bot(work, "", tables={"movin.ride_report": REPORT})
ok("and only once", len(sent) == before, f"{len(sent) - before} extra")

print("\n7. Somebody asks for his account to be deleted")
sent.clear()
DEL = US.join(["7", "0555123456", "rider", "2026-09-27", "2026-10-27", "30", "Je pars"])
run_bot(work, "", tables={"movin.deletion_request": DEL})
d = [m for m in sent if "suppression de compte" in m]
ok("announced, passenger, Algeria, with the reason",
   len(d) == 1 and all(x in d[0] for x in ("passager", "Algérie", "Je pars")), d[:1])
DEL_MR = US.join(["8", "36664750", "driver", "2026-09-27", "2026-10-27", "30", ""])
sent.clear()
run_bot(work, "", tables={"movin.deletion_request": DEL_MR})
d = [m for m in sent if "suppression de compte" in m]
ok("a Mauritanian driver's says Mauritania",
   len(d) == 1 and "chauffeur" in d[0] and "Mauritanie" in d[0], d[:1])

print("\n8. A driver is suspended from the console, and one is closed")
sent.clear()
BLOCKED = "\n".join([
    US.join(["id-s1", "0666000001", "Test", DZ, "suspend", "dangerous_driving",
             "2026-09-28 10:34"]),
    # Closed: no end date, so the LAST column is empty -- the row that the
    # old strip() used to drop.
    US.join(["id-s2", "36664750", "Yas", MR, "close", "fraud", ""]),
])
run_bot(work, "", tables={"movin.driver_sanction": BLOCKED})
b = [m for m in sent if "chauffeur suspendu" in m or "chauffeur fermé" in m]
ok("both announced", len(b) == 2, f"{len(b)} message(s)")
ok("suspension: reason, end, Algeria",
   any(all(x in m for x in ("suspendu", "Conduite dangereuse", "2026-09-28", "Algérie"))
       for m in b), b)
ok("closure: reason, Mauritania",
   any(all(x in m for x in ("compte chauffeur fermé", "Fraude", "Mauritanie")) for m in b), b)

print("\n8b. The office SMS phone goes quiet, then comes back (2026-09-29)")
from datetime import datetime, timedelta, timezone          # noqa: E402


def pulse(minutes_ago, countries=("+213",)):
    at = datetime.now(timezone.utc) - timedelta(minutes=minutes_ago)
    return {"smsInbox": {"countries": list(countries),
                         "lastAt": at.isoformat().replace("+00:00", "Z")}}


def phone_msgs():
    return [m for m in sent if "téléphone SMS" in m]


sent.clear()
GUARD = pulse(5)
run_bot(work, "")
ok("a heartbeat 5 min ago is not an outage", not phone_msgs(), phone_msgs())
GUARD = pulse(20)
run_bot(work, "")
down = phone_msgs()
ok("20 min of silence: said once, loud, naming Algeria and the way round",
   len(down) == 1 and "ne répond plus" in down[0] and "Algérie" in down[0]
   and "WhatsApp" in down[0], down)
run_bot(work, "")
ok("and not again while it stays silent", len(phone_msgs()) == 1, phone_msgs())
GUARD = pulse(0)
run_bot(work, "")
back = phone_msgs()
ok("back: said once", len(back) == 2 and "répond de nouveau" in back[1], back)
run_bot(work, "")
ok("and nothing more once it is back", len(phone_msgs()) == 2, phone_msgs())
GUARD = pulse(60, countries=())
run_bot(work, "")
ok("no country on a SIM: a silent phone matters to nobody", len(phone_msgs()) == 2, phone_msgs())
GUARD = None

print("\n9. The database does not answer")
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
