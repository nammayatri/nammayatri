#!/usr/bin/env python3
"""
The Movin bot — what the console would tell you if it could reach your pocket.

── Why this exists ─────────────────────────────────────────────────────────
The admin console holds twelve screens and answers every question about the
fleet, and none of it reaches anybody who is not looking at it. Audited on
2026-09-23: the validation queue held two registrations six and two days old,
and the last decision on the whole system was three weeks before that. Nothing
was broken. Nobody had been told.

That generalises past validation. This stack's own CLAUDE.md records three
faults that produced **no error anywhere** — stale driver positions, the BECKN
negative-coordinate bug, and Redis-cached merchant rows — each of which looked
exactly like "the app is broken" and cost an afternoon. A check that notices
silence is worth more here than one that reads an error log, because the
expensive failures on this box do not write to one.

── What it is, and what it deliberately is not ─────────────────────────────
It **reads and it tells**. It has no route that writes to the fleet: the owner
chose read-only commands on 2026-09-23, and that is what keeps a bot on a
phone from being a way to enable a driver or message a country by mistake.
Sending, validating and the tariff stay in the console, behind a login.

── Why Python on the host, of all the options ──────────────────────────────
There is no Node on this box, Postgres is not reachable from a container
without either the docker socket or a client library, and adding an admin
service account would give a chat process a console session. The host already
runs `server-state.py` under systemd for the same class of job, and Python's
standard library covers everything needed: `urllib` for Telegram, `subprocess`
for psql through the container that already has it, `json` for state. No
dependency is installed for this, which is the same rule auth-guard keeps.

── Telegram, and why not SMS ───────────────────────────────────────────────
Moorsyl only delivers to `+222` and the person reading these has an Algerian
number — the same constraint the guard's SMS_BYPASS list exists for — and
there is no SMTP on this box.

Configuration, all optional, all in /opt/ny/local-stack/.env:

    TELEGRAM_BOT_TOKEN, TELEGRAM_CHAT_ID    required; without them this exits
    BOT_CHECK_INTERVAL_S      300    how often the checks run
    BOT_QUIET_START/END       22/7   local hours where only money, law and
                                     "everything is down" may wake somebody
    BOT_DIGEST_HOUR           8      daily summary
    BOT_WEEKLY_DAY            0      Monday
    BOT_PATIENCE_H            4      a pending driver waited this long
    BOT_NO_RIDES_H            6      no ride in this many working hours
    BOT_STALE_POSITION_MIN    20     driver positions older than this
    BOT_DISK_PCT              85     root filesystem this full
    BOT_CERT_DAYS             14     certificate expires within this
"""
import json
import os
import re
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timedelta, timezone

HERE = os.path.dirname(os.path.abspath(__file__))
STATE_PATH = os.environ.get("BOT_STATE", "/opt/ny/local-stack/movin-bot.state.json")
# Overridable so the test suite can point every outside call at a local stub.
# Nothing here is a secret and the defaults are the real thing, so an
# unconfigured run behaves exactly as production does.
STATE_JSON = os.environ.get("BOT_SERVER_STATE",
                            "/opt/ny/local-stack/server-state/state.json")
CERT_DIR = os.environ.get("BOT_CERT_DIR", "/opt/ny/local-stack/edge-certs/live")
GUARD_HEALTH = os.environ.get("BOT_GUARD_HEALTH", "http://127.0.0.1:8031/healthz")
API_HEALTH = os.environ.get("BOT_API_HEALTH", "https://api.movinapp.net/healthz")
TELEGRAM_API = os.environ.get("BOT_TELEGRAM_API", "https://api.telegram.org")
MR = "favorit0-0000-0000-0000-00000favorit"


# ── configuration ───────────────────────────────────────────────────────────

def load_env():
    """Read .env the way the shell scripts do, without sourcing it."""
    path = os.path.join(HERE, ".env")
    if not os.path.exists(path):
        path = "/opt/ny/local-stack/.env"
    try:
        with open(path, "r", encoding="utf-8") as fh:
            for line in fh:
                line = line.strip()
                if not line or line.startswith("#") or "=" not in line:
                    continue
                k, v = line.split("=", 1)
                os.environ.setdefault(k.strip(), v.strip())
    except OSError:
        pass


def num(name, default):
    try:
        return int(os.environ.get(name, default))
    except ValueError:
        return int(default)


load_env()
TOKEN = os.environ.get("TELEGRAM_BOT_TOKEN", "").strip()
CHAT = os.environ.get("TELEGRAM_CHAT_ID", "").strip()
INTERVAL = num("BOT_CHECK_INTERVAL_S", 300)
QUIET_START = num("BOT_QUIET_START", 22)
QUIET_END = num("BOT_QUIET_END", 7)
DIGEST_HOUR = num("BOT_DIGEST_HOUR", 8)
WEEKLY_DAY = num("BOT_WEEKLY_DAY", 0)
PATIENCE_H = num("BOT_PATIENCE_H", 4)
NO_RIDES_H = num("BOT_NO_RIDES_H", 6)
STALE_MIN = num("BOT_STALE_POSITION_MIN", 20)
DISK_PCT = num("BOT_DISK_PCT", 85)
CERT_DAYS = num("BOT_CERT_DAYS", 14)


# ── the two things it talks to ──────────────────────────────────────────────

def psql(sql):
    """Rows as lists of strings, through the container that already has psql.

    Postgres is published on 127.0.0.1:5434, but reaching it from Python would
    mean a driver library, and this file installs nothing. `docker exec` costs
    a process every five minutes, which is nothing, and keeps the dependency
    count at zero.
    """
    try:
        out = subprocess.run(
            ["docker", "exec", "ny-postgres", "psql", "-U", "postgres",
             "-d", "atlas_dev", "-At", "-F", "\x1f", "-c", sql],
            capture_output=True, text=True, timeout=30,
        )
        if out.returncode != 0:
            log(f"psql failed: {out.stderr.strip()[:200]}")
            return None
        return [r.split("\x1f") for r in out.stdout.strip().split("\n") if r]
    except Exception as exc:                                  # noqa: BLE001
        log(f"psql error: {exc}")
        return None


def one(sql, default=None):
    """A single scalar, or `default` when the query could not be answered.

    None and the default are kept distinct on purpose: "the database did not
    answer" must never be reported as "the number is zero", which is exactly
    how a monitoring system invents an outage.
    """
    rows = psql(sql)
    if rows is None or not rows or not rows[0]:
        return default
    return rows[0][0]


def tg(method, **params):
    if not TOKEN:
        return None
    url = f"{TELEGRAM_API}/bot{TOKEN}/{method}"
    data = urllib.parse.urlencode(params).encode()
    try:
        with urllib.request.urlopen(url, data=data, timeout=60) as r:
            return json.loads(r.read().decode())
    except urllib.error.HTTPError as e:
        log(f"telegram {method}: HTTP {e.code} {e.read()[:160]!r}")
    except Exception as exc:                                  # noqa: BLE001
        log(f"telegram {method}: {exc}")
    return None


def send(text):
    """True only when Telegram accepted it, so a failure is never recorded as
    delivered and the next run says it again."""
    r = tg("sendMessage", chat_id=CHAT, text=text,
           disable_web_page_preview="true")
    return bool(r and r.get("ok"))


def log(msg):
    print(f"movin-bot: {msg}", flush=True)


# ── state: what has already been said ───────────────────────────────────────

def read_state():
    try:
        with open(STATE_PATH, "r", encoding="utf-8") as fh:
            return json.load(fh)
    except Exception:                                         # noqa: BLE001
        return {}


def write_state(state):
    tmp = STATE_PATH + ".tmp"
    try:
        with open(tmp, "w", encoding="utf-8") as fh:
            json.dump(state, fh)
        os.replace(tmp, STATE_PATH)
    except OSError as exc:
        log(f"cannot write state: {exc}")


# ── helpers ─────────────────────────────────────────────────────────────────

def now():
    return datetime.now(timezone.utc)


def quiet_hours():
    """Local night. Only money, law and a dead stack may speak through it —
    a notifier that wakes somebody for a three-star rating gets muted, and a
    muted notifier is worse than none because everybody believes it works."""
    h = datetime.now().hour
    if QUIET_START == QUIET_END:
        return False
    if QUIET_START < QUIET_END:
        return QUIET_START <= h < QUIET_END
    return h >= QUIET_START or h < QUIET_END


def http_json(url, timeout=8):
    try:
        with urllib.request.urlopen(url, timeout=timeout) as r:
            return json.loads(r.read().decode())
    except Exception:                                         # noqa: BLE001
        return None


def server_state():
    try:
        with open(STATE_JSON, "r", encoding="utf-8") as fh:
            return json.load(fh)
    except Exception:                                         # noqa: BLE001
        return None


# ═══════════════════════════════════════════════════════════════════════════
#  The checks.
#
#  Each returns a list of (key, urgency, text). `key` is what dedupes it:
#  the same key is said once and not again until it has cleared. `urgency` is
#  "loud" for money, law and a dead stack — the only three that pass through
#  quiet hours — and "normal" for everything else.
# ═══════════════════════════════════════════════════════════════════════════

def check_registrations(_):
    """A driver is waiting. The reason this whole file exists."""
    rows = psql(f"""
      SELECT p.id,
             coalesce(nullif(trim(coalesce(p.first_name,'') || ' ' ||
                                  coalesce(p.last_name,'')), ''), 'Nom non renseigné'),
             p.unencrypted_mobile_number,
             round(extract(epoch from (now() - p.created_at)) / 3600)::int,
             (SELECT count(*) FROM movin.driver_document d WHERE d.driver_id = p.id),
             (SELECT count(*) FROM movin.driver_declaration dd WHERE dd.driver_id = p.id)
        FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
       WHERE p.merchant_id = '{MR}' AND NOT di.enabled AND NOT di.blocked
       ORDER BY p.created_at""")
    if rows is None:
        return []
    out = []
    for pid, name, number, hours, docs, decl in rows:
        hours = int(hours or 0)
        papers = f"{docs} papier(s) reçu(s)" if int(docs or 0) else "aucun papier reçu"
        if int(decl or 0):
            papers += ", véhicule déclaré"
        out.append((f"reg:new:{pid}", "normal",
                    f"Movin · nouvelle inscription chauffeur\n\n{name}\n{number}\n"
                    f"{papers}\nÀ l'instant\n\nhttps://admin.movinapp.net"))
        if hours >= PATIENCE_H:
            out.append((f"reg:waited:{pid}", "normal",
                        f"Movin · chauffeur toujours en attente\n\n{name}\n{number}\n"
                        f"{papers}\nEn attente depuis {hours} h\n\n"
                        f"https://admin.movinapp.net"))
    return out


def check_sms_budget(_):
    """1 · The bill. The guard warns into a log nobody reads; this is the log
    nobody reads, read."""
    h = http_json(GUARD_HEALTH)
    if not h:
        return []
    b = (h.get("gateway") or {}).get("budget") or {}
    hour, hmax = b.get("hour"), b.get("hourMax")
    day, dmax = b.get("day"), b.get("dayMax")
    if None in (hour, hmax, day, dmax):
        return []
    if b.get("exhausted"):
        return [("sms:exhausted", "loud",
                 f"Movin · BUDGET SMS ÉPUISÉ\n\n{hour}/{hmax} cette heure, "
                 f"{day}/{dmax} aujourd'hui.\n\nPlus aucun code n'est envoyé : "
                 f"les nouvelles inscriptions échouent jusqu'à ce que la limite "
                 f"soit relevée ou que l'heure passe.")]
    if hour * 5 >= hmax * 4 or day * 5 >= dmax * 4:
        return [("sms:low", "loud",
                 f"Movin · budget SMS bientôt atteint\n\n{hour}/{hmax} cette heure, "
                 f"{day}/{dmax} aujourd'hui.")]
    return []


def check_sms_gateway(state):
    """2 · Moorsyl refusing. Only when the message changes, or a flapping
    gateway would send one of these every five minutes."""
    h = http_json(GUARD_HEALTH)
    if not h:
        return []
    err = (h.get("gateway") or {}).get("lastError")
    if not err:
        return []
    seen = state.get("sms:lasterror")
    if seen == err:
        return []
    state["sms:lasterror"] = err
    return [("sms:gateway:" + str(abs(hash(err)) % 10**8), "loud",
             f"Movin · la passerelle SMS a refusé un envoi\n\n{str(err)[:300]}")]


def check_wallet(_):
    """3 · Money in and money wrong."""
    out = []
    neg = psql("""
      SELECT w.driver_id, p.unencrypted_mobile_number, w.balance
        FROM movin.wallet w
        JOIN atlas_driver_offer_bpp.person p ON p.id = w.driver_id
       WHERE w.balance < 0""")
    for did, number, bal in (neg or []):
        out.append((f"wallet:neg:{did}", "normal",
                    f"Movin · porte-monnaie négatif\n\n{number}\nSolde : {bal} MRU"))
    tops = psql("""
      SELECT t.transaction_id, p.unencrypted_mobile_number, t.amount, t.currency
        FROM movin.wallet_topup t
        JOIN atlas_driver_offer_bpp.person p ON p.id = t.driver_id
       WHERE t.credited_at IS NOT NULL
         AND t.credited_at > now() - interval '2 days'""")
    for txn, number, amount, cur in (tops or []):
        out.append((f"wallet:topup:{txn}", "normal",
                    f"Movin · rechargement reçu\n\n{number}\n{amount} {cur or 'MRU'}"))
    return out


def check_deletions(_):
    """4 and 5 · The queue with a legal clock on it. `delete_by` is a promise
    with a date; the console shows it and nothing else does."""
    rows = psql("""
      SELECT id, phone, side, requested_at::date,
             delete_by::date,
             (delete_by::date - now()::date) AS days_left
        FROM movin.deletion_request
       WHERE status NOT IN ('done','withdrawn','anonymised')""")
    if rows is None:
        return []
    out = []
    for rid, phone, side, asked, due, left in rows:
        left = int(left or 0)
        who = "passager" if (side or "").lower().startswith("rider") else "chauffeur"
        out.append((f"del:new:{rid}", "normal",
                    f"Movin · demande de suppression de compte\n\n{phone} ({who})\n"
                    f"Demandé le {asked}\nÀ traiter avant le {due}\n\n"
                    f"https://admin.movinapp.net"))
        if left < 0:
            out.append((f"del:overdue:{rid}", "loud",
                        f"Movin · SUPPRESSION EN RETARD\n\n{phone} ({who})\n"
                        f"L'échéance du {due} est dépassée de {abs(left)} jour(s).\n\n"
                        f"https://admin.movinapp.net"))
        elif left <= 3:
            out.append((f"del:soon:{rid}", "loud",
                        f"Movin · suppression à traiter\n\n{phone} ({who})\n"
                        f"Échéance le {due} — {left} jour(s)."))
    return out


def check_no_rides(_):
    """6 · Silence. The best single catch-all for this stack, because its
    documented failures produce no error at all — just nothing happening."""
    if quiet_hours():
        return []
    # Only meaningful once this stack normally carries traffic: on a pilot that
    # has never had a busy day, "no rides for six hours" is the truth and not
    # a fault, and saying it daily is how a notifier gets muted.
    week = one("SELECT count(*) FROM atlas_driver_offer_bpp.ride "
               "WHERE created_at > now() - interval '7 days'")
    if week is None or int(week) < 20:
        return []
    last = one("SELECT round(extract(epoch from (now() - max(created_at)))/3600, 1) "
               "FROM atlas_driver_offer_bpp.ride")
    if last is None:
        return []
    try:
        hours = float(last)
    except (TypeError, ValueError):
        return []
    if hours < NO_RIDES_H:
        return []
    return [("rides:silent", "normal",
             f"Movin · aucune course depuis {hours:.0f} h\n\n"
             f"Si c'est inhabituel à cette heure, cela ressemble aux pannes "
             f"silencieuses connues : positions périmées, dispatch, ou recherche "
             f"qui ne renvoie aucun prix.")]


def check_zero_estimates(_):
    """7 · Searches that found nobody. The exact symptom of the three silent
    faults: an empty array, HTTP 200, and nothing in any log."""
    if quiet_hours():
        return []
    rows = psql("""
      SELECT count(*) FILTER (WHERE q.search_request_id IS NULL), count(*)
        FROM atlas_driver_offer_bpp.search_request s
        LEFT JOIN atlas_driver_offer_bpp.driver_quote q
               ON q.search_request_id = s.id
       WHERE s.created_at > now() - interval '1 hour'""")
    if not rows or len(rows[0]) < 2:
        return []
    try:
        empty, total = int(rows[0][0]), int(rows[0][1])
    except (TypeError, ValueError):
        return []
    if total < 3 or empty < total:
        return []
    return [("search:zero", "normal",
             f"Movin · {total} recherche(s) dans l'heure, aucune offre\n\n"
             f"Chaque recherche est repartie sans un seul prix. C'est le symptôme "
             f"d'un dispatch en panne, pas d'une absence de clients.")]


def check_stale_positions(_):
    """8 · The documented trap: the pool ignores old positions, so search
    returns nothing and no component reports a fault."""
    if quiet_hours():
        return []
    mins = one(f"""
      SELECT round(extract(epoch from (now() - max(dl.updated_at)))/60)::int
        FROM atlas_driver_offer_bpp.driver_location dl
        JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = dl.driver_id
       WHERE di.active AND NOT di.blocked""")
    if mins is None:
        return []
    try:
        mins = int(mins)
    except (TypeError, ValueError):
        return []
    if mins < STALE_MIN:
        return []
    return [("fleet:stale", "normal",
             f"Movin · positions chauffeurs périmées\n\n"
             f"La plus récente date de {mins} min. Au-delà, le dispatch cesse de "
             f"voir la flotte et les recherches ne renvoient aucun prix.")]


def check_nobody_online(_):
    """9 · The whole fleet offline in working hours."""
    if quiet_hours():
        return []
    n = one(f"""
      SELECT count(*) FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
       WHERE p.merchant_id = '{MR}' AND di.active AND di.enabled AND NOT di.blocked""")
    if n is None:
        return []
    if int(n) > 0:
        return []
    return [("fleet:empty", "normal",
             "Movin · aucun chauffeur en ligne\n\n"
             "Personne ne peut recevoir de course en Mauritanie en ce moment.")]


def check_containers(_):
    """10 · Something stopped."""
    s = server_state()
    if not s:
        return []
    bad = []
    for c in s.get("containers", []):
        name = c.get("name", "?")
        # `state` is docker's own word ("running", "exited", "restarting").
        # `health` is null for every container without a healthcheck, which is
        # most of them -- reading null as unhealthy reported the whole stack as
        # down on the first dry run, which is what dry runs are for.
        state = (c.get("state") or "").lower()
        health = (c.get("health") or "").lower()
        if state != "running" or health == "unhealthy":
            bad.append(f"{name} — {c.get('state') or '?'}"
                       + (f" ({health})" if health else ""))
    if not bad:
        return []
    return [("infra:containers:" + ",".join(sorted(b.split(" — ")[0] for b in bad)),
             "loud",
             "Movin · conteneur(s) en panne\n\n" + "\n".join(bad))]


def check_api(_):
    """11 · What a phone actually sees. This is the check that would have
    caught the nginx mistake of 2026-09-23 in five minutes instead of 33."""
    code = None
    try:
        req = urllib.request.Request(API_HEALTH, method="GET")
        with urllib.request.urlopen(req, timeout=12) as r:
            code = r.status
    except urllib.error.HTTPError as e:
        code = e.code                 # an answer is an answer: the edge is up
    except Exception:                                         # noqa: BLE001
        code = None
    if code is not None:
        return []
    return [("infra:api", "loud",
             "Movin · l'API ne répond pas\n\n"
             f"{API_HEALTH} est injoignable depuis le serveur lui-même. "
             "C'est ce que verrait un téléphone.")]


def check_disk(_):
    """12 · Documents land on a volume with no quota."""
    try:
        st = os.statvfs("/")
        used = 100 - (st.f_bavail * 100 // st.f_blocks)
    except Exception:                                         # noqa: BLE001
        return []
    if used < DISK_PCT:
        return []
    free_gb = st.f_bavail * st.f_frsize / 1e9
    return [("infra:disk", "loud",
             f"Movin · disque à {used} %\n\nIl reste {free_gb:.0f} Go. "
             f"Les papiers des chauffeurs et les sauvegardes écrivent ici.")]


def check_certs(_):
    """13 · certbot renews on its own; this says when it has not."""
    out = []
    if not os.path.isdir(CERT_DIR):
        return out
    for name in sorted(os.listdir(CERT_DIR)):
        pem = os.path.join(CERT_DIR, name, "fullchain.pem")
        if not os.path.exists(pem):
            continue
        try:
            res = subprocess.run(["openssl", "x509", "-enddate", "-noout", "-in", pem],
                                 capture_output=True, text=True, timeout=10)
            m = re.search(r"notAfter=(.+)", res.stdout.strip())
            if not m:
                continue
            end = datetime.strptime(m.group(1).strip(), "%b %d %H:%M:%S %Y %Z")
            end = end.replace(tzinfo=timezone.utc)
        except Exception:                                     # noqa: BLE001
            continue
        left = (end - now()).days
        if left <= CERT_DAYS:
            out.append((f"infra:cert:{name}:{end.date()}", "loud",
                        f"Movin · certificat TLS bientôt expiré\n\n{name}\n"
                        f"Expire le {end.date()} — {left} jour(s).\n"
                        f"certbot devrait renouveler seul ; s'il ne l'a pas fait, "
                        f"c'est à regarder."))
    return out


def check_backups(_):
    """14 · backup.sh runs at 02:30 and reports to nobody."""
    s = server_state()
    if not s:
        return []
    b = s.get("backups") or {}
    out = []
    timer = b.get("timer") or {}
    if timer.get("last_result") not in (None, "success"):
        out.append((f"infra:backup:result:{timer.get('last_result')}", "loud",
                    f"Movin · la sauvegarde a échoué\n\n"
                    f"Dernier résultat : {timer.get('last_result')}"))
    archives = b.get("archives") or []
    if archives:
        try:
            newest = max(a.get("taken_at", "") for a in archives)
            taken = datetime.strptime(newest, "%Y-%m-%dT%H:%M:%SZ").replace(
                tzinfo=timezone.utc)
            age_h = (now() - taken).total_seconds() / 3600
            if age_h > 36:
                out.append((f"infra:backup:stale:{taken.date()}", "loud",
                            f"Movin · aucune sauvegarde depuis {age_h:.0f} h\n\n"
                            f"La plus récente date du {taken.date()}."))
        except Exception:                                     # noqa: BLE001
            pass
    return out


def check_low_ratings(_):
    """17 · One or two stars, with something written. A complaint somebody
    took the trouble to type is worth reading the same day."""
    rows = psql("""
      SELECT r.id, r.rating_value, coalesce(r.feedback_details,''),
             coalesce(p.first_name,'')
        FROM atlas_driver_offer_bpp.rating r
        LEFT JOIN atlas_driver_offer_bpp.person p ON p.id = r.driver_id
       WHERE r.rating_value <= 2
         AND coalesce(r.feedback_details,'') <> ''
         AND r.created_at > now() - interval '2 days'""")
    out = []
    for rid, stars, text, driver in (rows or []):
        out.append((f"rating:{rid}", "normal",
                    f"Movin · note basse avec commentaire\n\n"
                    f"{stars}/5 — chauffeur {driver or '?'}\n« {text[:300]} »\n\n"
                    f"https://admin.movinapp.net"))
    return out


def check_cancellations(_):
    """18 · A run of cancellations above the recent normal. Compared against
    this fleet's own last fortnight rather than a number invented here."""
    if quiet_hours():
        return []
    rows = psql("""
      SELECT
        (SELECT count(*) FROM atlas_driver_offer_bpp.booking_cancellation_reason c
           JOIN atlas_driver_offer_bpp.booking b ON b.id = c.booking_id
          WHERE b.created_at > now() - interval '3 hours'),
        (SELECT round(count(*) / 112.0, 2) FROM atlas_driver_offer_bpp.booking_cancellation_reason c
           JOIN atlas_driver_offer_bpp.booking b ON b.id = c.booking_id
          WHERE b.created_at > now() - interval '14 days')""")
    if not rows or len(rows[0]) < 2:
        return []
    try:
        recent, per3h = int(rows[0][0]), float(rows[0][1] or 0)
    except (TypeError, ValueError):
        return []
    # Needs a real baseline and a real excess: with almost no history every
    # quiet afternoon looks like a spike.
    if per3h < 1 or recent < 5 or recent < per3h * 3:
        return []
    return [("rides:cancels", "normal",
             f"Movin · beaucoup d'annulations\n\n{recent} en 3 h, "
             f"contre {per3h:.1f} habituellement sur la même durée.")]


def check_blocked_drivers(_):
    """19 · Somebody was blocked — by validation, or by the wallet gate."""
    rows = psql(f"""
      SELECT p.id, p.unencrypted_mobile_number,
             coalesce(nullif(trim(coalesce(p.first_name,'')),''),'?')
        FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
       WHERE p.merchant_id = '{MR}' AND di.blocked""")
    out = []
    for pid, number, name in (rows or []):
        out.append((f"driver:blocked:{pid}", "normal",
                    f"Movin · chauffeur bloqué\n\n{name}\n{number}\n\n"
                    f"https://admin.movinapp.net"))
    return out


CHECKS = [
    check_registrations, check_sms_budget, check_sms_gateway, check_wallet,
    check_deletions, check_no_rides, check_zero_estimates,
    check_stale_positions, check_nobody_online, check_containers, check_api,
    check_disk, check_certs, check_backups, check_low_ratings,
    check_cancellations, check_blocked_drivers,
]


# ── 15 and 16 · the digests ─────────────────────────────────────────────────

def numbers(window):
    """The same figures the console's Aperçu screen answers with."""
    r = psql(f"""
      SELECT
        (SELECT count(*) FROM atlas_driver_offer_bpp.ride
          WHERE created_at > now() - interval '{window}'),
        (SELECT coalesce(sum(fare),0) FROM atlas_driver_offer_bpp.ride
          WHERE created_at > now() - interval '{window}' AND status = 'COMPLETED'),
        (SELECT count(*) FROM atlas_driver_offer_bpp.person p
           JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
          WHERE p.merchant_id = '{MR}' AND NOT di.enabled AND NOT di.blocked),
        (SELECT count(*) FROM atlas_driver_offer_bpp.person p
           JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
          WHERE p.merchant_id = '{MR}' AND di.enabled AND NOT di.blocked),
        (SELECT count(*) FROM atlas_driver_offer_bpp.person p
           JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
          WHERE p.merchant_id = '{MR}' AND p.created_at > now() - interval '{window}'),
        (SELECT coalesce(sum(amount),0) FROM movin.wallet_topup
          WHERE credited_at > now() - interval '{window}')""")
    if not r:
        return None
    v = r[0]
    return {"rides": v[0], "fare": v[1], "pending": v[2], "fleet": v[3],
            "new_drivers": v[4], "topups": v[5]}


def digest(window, title):
    n = numbers(window)
    if not n:
        return None
    return (f"Movin · {title}\n\n"
            f"Courses            {n['rides']}\n"
            f"Encaissé           {n['fare']} MRU\n"
            f"Rechargements      {n['topups']} MRU\n"
            f"Nouveaux chauffeurs {n['new_drivers']}\n"
            f"Flotte active      {n['fleet']}\n"
            f"En attente         {n['pending']}\n\n"
            f"https://admin.movinapp.net")


# ── the commands ────────────────────────────────────────────────────────────

HELP = """Movin · commandes

/file        qui attend la validation
/jour        les chiffres du jour
/semaine     les chiffres de la semaine
/chauffeur <numéro>   un chauffeur en particulier
/flotte      qui est en ligne
/serveur     l'état de la machine
/budget      le budget SMS
/aide        ce message

Le bot lit seulement. Valider, refuser, écrire aux chauffeurs
et changer un tarif restent dans la console."""


def cmd_file(_):
    rows = psql(f"""
      SELECT coalesce(nullif(trim(coalesce(p.first_name,'') || ' ' ||
                                  coalesce(p.last_name,'')),''),'Nom non renseigné'),
             p.unencrypted_mobile_number,
             round(extract(epoch from (now() - p.created_at))/3600)::int,
             (SELECT count(*) FROM movin.driver_document d WHERE d.driver_id = p.id)
        FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
       WHERE p.merchant_id = '{MR}' AND NOT di.enabled AND NOT di.blocked
       ORDER BY p.created_at""")
    if rows is None:
        return "La base n'a pas répondu."
    if not rows:
        return "Personne n'attend. La file est vide."
    lines = [f"{n}\n  {num_} · {h} h · {d} papier(s)"
             for n, num_, h, d in rows]
    return (f"Movin · {len(rows)} en attente\n\n" + "\n\n".join(lines)
            + "\n\nhttps://admin.movinapp.net")


def cmd_jour(_):
    return digest("24 hours", "les dernières 24 h") or "La base n'a pas répondu."


def cmd_semaine(_):
    return digest("7 days", "les 7 derniers jours") or "La base n'a pas répondu."


def cmd_chauffeur(arg):
    digits = re.sub(r"\D", "", arg or "")
    if len(digits) < 6:
        return "Donnez un numéro : /chauffeur 36664750"
    rows = psql(f"""
      SELECT coalesce(nullif(trim(coalesce(p.first_name,'') || ' ' ||
                                  coalesce(p.last_name,'')),''),'Nom non renseigné'),
             p.unencrypted_mobile_number, di.enabled, di.blocked, di.active,
             coalesce(v.variant,'—'), coalesce(v.registration_no,'—'),
             coalesce((SELECT balance::text FROM movin.wallet w
                        WHERE w.driver_id = p.id),'—'),
             (SELECT count(*) FROM movin.driver_document d WHERE d.driver_id = p.id),
             p.created_at::date
        FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
        LEFT JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = p.id
       WHERE p.unencrypted_mobile_number LIKE '%{digits}%'
       LIMIT 3""")
    if rows is None:
        return "La base n'a pas répondu."
    if not rows:
        return f"Aucun chauffeur avec {digits}."
    out = []
    for name, number, en, bl, ac, var, plate, bal, docs, since in rows:
        state = ("bloqué" if bl == "t" else
                 "en ligne" if (en == "t" and ac == "t") else
                 "actif" if en == "t" else "en attente de validation")
        out.append(f"{name}\n{number}\nÉtat : {state}\nVéhicule : {var} {plate}\n"
                   f"Porte-monnaie : {bal} MRU\nPapiers : {docs}\nInscrit le {since}")
    return "Movin · chauffeur\n\n" + "\n\n———\n\n".join(out)


def cmd_flotte(_):
    rows = psql(f"""
      SELECT coalesce(v.variant,'sans véhicule'), count(*)
        FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
        LEFT JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = p.id
       WHERE p.merchant_id = '{MR}' AND di.enabled AND NOT di.blocked
       GROUP BY 1 ORDER BY 1""")
    if rows is None:
        return "La base n'a pas répondu."
    online = one(f"""
      SELECT count(*) FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
       WHERE p.merchant_id = '{MR}' AND di.active AND di.enabled AND NOT di.blocked""",
                 "?")
    body = "\n".join(f"{v:<16} {n}" for v, n in rows) or "aucun"
    return f"Movin · flotte\n\n{body}\n\nEn ligne maintenant : {online}"


def cmd_serveur(_):
    s = server_state()
    if not s:
        return "Pas d'instantané du serveur."
    # `state`, like check_containers -- the snapshot has no `status` field and
    # reading the missing one reported every container as down.
    cs = s.get("containers", [])
    up = sum(1 for c in cs if (c.get("state") or "").lower() == "running")
    total = len(cs)
    bad = [c.get("name") for c in cs if (c.get("state") or "").lower() != "running"]
    b = s.get("backups") or {}
    archives = b.get("archives") or []
    newest = max((a.get("taken_at", "") for a in archives), default="—")
    try:
        st = os.statvfs("/")
        disk = f"{100 - (st.f_bavail * 100 // st.f_blocks)} % utilisé"
    except Exception:                                         # noqa: BLE001
        disk = "?"
    return (f"Movin · serveur\n\n"
            f"Conteneurs   {up}/{total}" + (f"\n  en panne : {', '.join(bad)}" if bad else "")
            + f"\nDisque       {disk}\n"
            f"Sauvegarde   {newest}\n"
            f"Relevé       {s.get('measured_at','?')}")


def cmd_budget(_):
    h = http_json(GUARD_HEALTH)
    if not h:
        return "Le garde n'a pas répondu."
    g = h.get("gateway") or {}
    b = g.get("budget") or {}
    return (f"Movin · budget SMS\n\n"
            f"Cette heure  {b.get('hour','?')}/{b.get('hourMax','?')}\n"
            f"Aujourd'hui  {b.get('day','?')}/{b.get('dayMax','?')}\n"
            f"Épuisé       {'oui' if b.get('exhausted') else 'non'}\n"
            f"Envoyés      {g.get('sent','?')} depuis le démarrage\n"
            f"Dernière erreur : {g.get('lastError') or 'aucune'}")


COMMANDS = {
    "/file": cmd_file, "/jour": cmd_jour, "/semaine": cmd_semaine,
    "/chauffeur": cmd_chauffeur, "/flotte": cmd_flotte,
    "/serveur": cmd_serveur, "/budget": cmd_budget,
    "/aide": lambda _: HELP, "/start": lambda _: HELP, "/help": lambda _: HELP,
}


def handle_command(text):
    parts = (text or "").strip().split(maxsplit=1)
    if not parts:
        return None
    word = parts[0].split("@")[0].lower()      # /jour@movin_bot
    fn = COMMANDS.get(word)
    if not fn:
        return None
    try:
        return fn(parts[1] if len(parts) > 1 else "")
    except Exception as exc:                                  # noqa: BLE001
        log(f"command {word} failed: {exc}")
        return "Cette commande a échoué. Regardez le journal du bot."


# ── the loop ────────────────────────────────────────────────────────────────

def run_checks(state, seed=False):
    """Every check, then the digests. One message per new key, and a key is
    forgotten once its condition clears so it can fire again next time."""
    seen = set()
    for check in CHECKS:
        try:
            alerts = check(state) or []
        except Exception as exc:                              # noqa: BLE001
            log(f"{check.__name__} failed: {exc}")
            continue
        for key, urgency, text in alerts:
            seen.add(key)
            if key in state.get("sent", {}):
                continue
            if seed:
                # First run: record what is already true and stay quiet. A bot
                # switched on beside a fleet that has been running for weeks
                # would otherwise open with a wall of history.
                state.setdefault("sent", {})[key] = now().isoformat()
                continue
            if urgency != "loud" and quiet_hours():
                continue            # held, not dropped: it fires in the morning
            if send(text):
                state.setdefault("sent", {})[key] = now().isoformat()
                log(f"told about {key}")

    # Forget what has cleared, so the same fault can be reported again later.
    # Registration keys are kept while the driver is still in the queue, which
    # `seen` already carries.
    state["sent"] = {k: v for k, v in state.get("sent", {}).items() if k in seen}

    # Digests, on their own schedule rather than on a condition.
    local = datetime.now()
    stamp = local.strftime("%Y-%m-%d")
    if local.hour == DIGEST_HOUR and state.get("digest:day") != stamp:
        msg = digest("24 hours", "hier en chiffres")
        if msg and send(msg):
            state["digest:day"] = stamp
    week = local.strftime("%Y-W%W")
    if (local.weekday() == WEEKLY_DAY and local.hour == DIGEST_HOUR
            and state.get("digest:week") != week):
        msg = digest("7 days", "la semaine en chiffres")
        if msg and send(msg):
            state["digest:week"] = week


def main():
    if not TOKEN or not CHAT:
        log("no TELEGRAM_BOT_TOKEN/TELEGRAM_CHAT_ID — nothing to do")
        return 0
    if "--once" in sys.argv or "--seed" in sys.argv:
        state = read_state()
        run_checks(state, seed="--seed" in sys.argv)
        write_state(state)
        return 0

    log(f"started · checks every {INTERVAL}s · quiet {QUIET_START}h-{QUIET_END}h "
        f"· digest at {DIGEST_HOUR}h")
    state = read_state()
    if not state.get("sent") and not state.get("seeded"):
        log("first run: learning the current state without announcing it")
        run_checks(state, seed=True)
        state["seeded"] = now().isoformat()
        write_state(state)
    offset = state.get("tg_offset", 0)
    last_check = time.time()          # seeded just now; next check is a full interval away

    while True:
        # Commands first, with a long poll: it is what makes the bot answer in
        # a second instead of on the next tick, and it costs one idle request.
        upd = tg("getUpdates", offset=offset, timeout=20, allowed_updates='["message"]')
        if upd and upd.get("ok"):
            for u in upd.get("result", []):
                offset = max(offset, u.get("update_id", 0) + 1)
                msg = u.get("message") or {}
                if str((msg.get("chat") or {}).get("id")) != str(CHAT):
                    continue        # only the configured chat is ever obeyed
                answer = handle_command(msg.get("text"))
                if answer:
                    send(answer)
            state["tg_offset"] = offset

        if time.time() - last_check >= INTERVAL:
            run_checks(state)
            last_check = time.time()

        write_state(state)
        time.sleep(1)


if __name__ == "__main__":
    sys.exit(main())
