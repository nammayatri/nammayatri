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
DZ = "algeria0-0000-0000-0000-00000algeria"

# ── Two countries, 2026-09-27 ───────────────────────────────────────────────
# Until then every driver query here said `merchant_id = MR`, so a driver who
# registered in Algeria was never announced -- while his Chargily top-up WAS,
# because the top-up query had no merchant filter at all. Found by the owner
# registering himself in Algeria and hearing nothing. Every driver query now
# names both merchants, and every message says which country it is about.
#
# A driver's country is his merchant; a passenger's is his number (the
# console's own rule, apps/api/src/shared/country.ts in the website repo).
COUNTRY = {
    MR: {"code": "MR", "label": "🇲🇷 Mauritanie", "cur": "MRU", "dial": "+222",
         "mobile": r"^[2-4][0-46-9][0-9]{6}$"},
    DZ: {"code": "DZ", "label": "🇩🇿 Algérie", "cur": "DA", "dial": "+213",
         "mobile": r"^0[5-7][0-9]{8}$"},
}
MERCHANTS = f"('{MR}', '{DZ}')"
# wallet_topup.currency is ISO 4217; the office reads dinars as DA.
CURRENCY_LABEL = {"DZD": "DA", "MRU": "MRU"}
# The website's SANCTION_REASON_LABEL (packages/domain/src/sanctions.ts).
SANCTION_REASON = {
    "dangerous_driving": "Conduite dangereuse",
    "behaviour": "Comportement inacceptable",
    "harassment": "Harcèlement ou agression",
    "fraud": "Fraude ou prix abusif",
    "vehicle": "Véhicule non conforme ou dangereux",
    "impersonation": "Ce n’est pas le titulaire du compte qui conduit",
    "other": "Autre motif",
}


def country_of(merchant=None, phone=None):
    """The country's entry, by merchant first, then by a stored national
    number. None when it is neither -- upstream's seed accounts."""
    if merchant in COUNTRY:
        return COUNTRY[merchant]
    digits = re.sub(r"\D", "", phone or "")
    for c in COUNTRY.values():
        if re.match(c["mobile"], digits):
            return c
    return None


def where(merchant=None, phone=None):
    c = country_of(merchant, phone)
    return c["label"] if c else "Pays inconnu"


def one_line(sql_expr):
    """A free-text column made safe for the row splitter: psql rows are split
    on newlines, so a report written over two lines would become two rows."""
    return f"regexp_replace(coalesce({sql_expr}, ''), E'[\\n\\r\\x1f]+', ' ', 'g')"


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
        # Split on newlines only. `str.strip()` counts \x1f as whitespace, so
        # stripping the output ate the separator before an EMPTY LAST COLUMN
        # and the row came back one field short -- and was then skipped by
        # the unpacking. A deletion with no reason vanished that way.
        return [r.split("\x1f") for r in out.stdout.split("\n") if r]
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
             (SELECT count(*) FROM movin.driver_declaration dd WHERE dd.driver_id = p.id),
             p.merchant_id,
             -- A refused driver who corrected his file and sent it again from
             -- the app (2026-09-28) is back in this queue; say so.
             coalesce((SELECT dv.decision FROM movin.driver_validation dv
                        WHERE dv.driver_id = p.id
                        ORDER BY dv.decided_at DESC LIMIT 1), '')
        FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
       WHERE p.merchant_id IN {MERCHANTS} AND NOT di.enabled AND NOT di.blocked
       ORDER BY p.created_at""")
    if rows is None:
        return []
    out = []
    for pid, name, number, hours, docs, decl, merchant, last in rows:
        land = where(merchant)
        headline = ("dossier renvoyé après refus" if last == "resubmitted"
                    else "nouvelle inscription chauffeur")
        hours = int(hours or 0)
        papers = f"{docs} papier(s) reçu(s)" if int(docs or 0) else "aucun papier reçu"
        if int(decl or 0):
            papers += ", véhicule déclaré"
        out.append((f"reg:new:{pid}", "normal",
                    f"Movin · {headline}\n{land}\n\n{name}\n{number}\n"
                    f"{papers}\nÀ l'instant\n\nhttps://admin.movinapp.net"))
        if hours >= PATIENCE_H:
            out.append((f"reg:waited:{pid}", "normal",
                        f"Movin · chauffeur toujours en attente\n{land}\n\n{name}\n{number}\n"
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


# Minutes of silence before the office SMS phone is called gone. Its
# forwarder sends a heartbeat (an empty list) every 5 minutes, so 15 is three
# missed in a row -- a phone briefly out of signal is not an outage.
SMS_PHONE_SILENT_MIN = num("BOT_SMS_PHONE_SILENT_MIN", 15)


def check_sms_phone(state):
    """2b · The office SMS phone (2026-09-29). Where a country signs in by
    texting that SIM, a phone that is off, flat or offline means everyone who
    picks « Confirmer en nous envoyant un SMS » waits for nothing -- and the
    server cannot tell, only notice the silence. Said once when it goes quiet,
    and once when it is back."""
    h = http_json(GUARD_HEALTH)
    if not h:
        return []
    inbox = h.get("smsInbox") or {}
    countries = inbox.get("countries") or []
    if not countries:
        state.pop("smsphone:down", None)
        state.pop("smsphone:nopulse", None)
        return []                     # no country depends on the phone
    t = now()
    last = inbox.get("lastAt")
    if last:
        state.pop("smsphone:nopulse", None)
        heard = datetime.fromisoformat(last.replace("Z", "+00:00"))
    else:
        # The guard restarted and nothing has come since: count from the first
        # time the bot noticed, not from the epoch.
        heard = datetime.fromisoformat(state.setdefault("smsphone:nopulse", t.isoformat()))
    silent = (t - heard).total_seconds() / 60
    labels = {c["dial"]: c["label"] for c in COUNTRY.values()}
    names = ", ".join(labels.get(c, c) for c in countries)
    if silent >= SMS_PHONE_SILENT_MIN:
        state["smsphone:down"] = heard.isoformat()
        return [("smsphone:silent", "loud",
                 f"Movin · le téléphone SMS de l'agence ne répond plus\n\n"
                 f"Aucun message ni signe de vie depuis {int(silent)} min "
                 f"(dernier : {heard.astimezone().strftime('%H:%M')}).\n\n"
                 f"{names} : ceux qui choisissent « Confirmer en nous envoyant un "
                 f"SMS » attendront pour rien. Vérifier la batterie, le réseau, "
                 f"internet et l'application de transfert. WhatsApp fonctionne "
                 f"toujours.")]
    if state.pop("smsphone:down", None):
        return [("smsphone:back:" + str(last), "loud",
                 "Movin · le téléphone SMS de l'agence répond de nouveau ✅")]
    return []


def check_wallet(_):
    """3 · Money in and money wrong."""
    out = []
    neg = psql("""
      SELECT w.driver_id, p.unencrypted_mobile_number, w.balance, p.merchant_id
        FROM movin.wallet w
        JOIN atlas_driver_offer_bpp.person p ON p.id = w.driver_id
       WHERE w.balance < 0""")
    for did, number, bal, merchant in (neg or []):
        c = country_of(merchant)
        out.append((f"wallet:neg:{did}", "normal",
                    f"Movin · porte-monnaie négatif\n{where(merchant)}\n\n{number}\n"
                    f"Solde : {bal} {c['cur'] if c else ''}".rstrip()))
    tops = psql("""
      SELECT t.transaction_id, p.unencrypted_mobile_number, t.amount, t.currency,
             p.merchant_id
        FROM movin.wallet_topup t
        JOIN atlas_driver_offer_bpp.person p ON p.id = t.driver_id
       WHERE t.credited_at IS NOT NULL
         AND t.credited_at > now() - interval '2 days'""")
    for txn, number, amount, cur, merchant in (tops or []):
        out.append((f"wallet:topup:{txn}", "normal",
                    f"Movin · rechargement reçu\n{where(merchant)}\n\n{number}\n"
                    f"{amount} {CURRENCY_LABEL.get(cur, cur or '')}".rstrip()))
    return out


def check_deletions(_):
    """4 and 5 · The queue with a legal clock on it. `delete_by` is a promise
    with a date; the console shows it and nothing else does."""
    rows = psql(f"""
      SELECT id, phone, side, requested_at::date,
             delete_by::date,
             (delete_by::date - now()::date) AS days_left,
             {one_line("reason")}
        FROM movin.deletion_request
       WHERE status NOT IN ('done','withdrawn','anonymised')""")
    if rows is None:
        return []
    out = []
    for rid, phone, side, asked, due, left, reason in rows:
        left = int(left or 0)
        who = "passager" if (side or "").lower().startswith("rider") else "chauffeur"
        why = f"\nMotif : « {reason[:300]} »" if reason else ""
        out.append((f"del:new:{rid}", "normal",
                    f"Movin · demande de suppression de compte\n{where(phone=phone)}\n\n"
                    f"{phone} ({who}){why}\n"
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
    """9 · A whole country's fleet offline in working hours -- each country
    on its own, since a busy Nouakchott says nothing about Algiers."""
    if quiet_hours():
        return []
    rows = psql(f"""
      SELECT m.id, count(di.driver_id)
        FROM unnest(ARRAY['{MR}', '{DZ}']) AS m(id)
        LEFT JOIN atlas_driver_offer_bpp.person p ON p.merchant_id = m.id
        LEFT JOIN atlas_driver_offer_bpp.driver_information di
               ON di.driver_id = p.id AND di.active AND di.enabled AND NOT di.blocked
       GROUP BY m.id""")
    out = []
    for merchant, n in (rows or []):
        c = country_of(merchant)
        if not c or int(n or 0) > 0:
            continue
        out.append((f"fleet:empty:{c['code']}", "normal",
                    f"Movin · aucun chauffeur en ligne\n{c['label']}\n\n"
                    f"Personne ne peut recevoir de course dans ce pays en ce moment."))
    return out


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
      SELECT r.id, r.rating_value, """ + one_line("r.feedback_details") + """,
             coalesce(p.first_name,''), coalesce(p.merchant_id,'')
        FROM atlas_driver_offer_bpp.rating r
        LEFT JOIN atlas_driver_offer_bpp.person p ON p.id = r.driver_id
       WHERE r.rating_value <= 2
         AND coalesce(r.feedback_details,'') <> ''
         AND r.created_at > now() - interval '2 days'""")
    out = []
    for rid, stars, text, driver, merchant in (rows or []):
        out.append((f"rating:{rid}", "normal",
                    f"Movin · note basse avec commentaire\n{where(merchant)}\n\n"
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
    """19 · Somebody was blocked -- suspended or closed from the console, or
    by upstream. The console's latest sanction says why and until when."""
    rows = psql(f"""
      SELECT p.id, p.unencrypted_mobile_number,
             coalesce(nullif(trim(coalesce(p.first_name,'')),''),'?'),
             p.merchant_id,
             coalesce(s.action, ''), coalesce(s.reason, ''),
             coalesce(to_char(s.until AT TIME ZONE 'UTC', 'YYYY-MM-DD HH24:MI'), '')
        FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
        LEFT JOIN LATERAL (
          SELECT action, reason, until FROM movin.driver_sanction ds
           WHERE ds.driver_id = p.id
           ORDER BY ds.decided_at DESC, ds.id DESC LIMIT 1) s ON true
       WHERE p.merchant_id IN {MERCHANTS} AND di.blocked""")
    out = []
    for pid, number, name, merchant, action, reason, until in (rows or []):
        title = {"suspend": "chauffeur suspendu",
                 "close": "compte chauffeur fermé"}.get(action, "chauffeur bloqué")
        detail = ""
        if reason:
            detail += f"\nMotif : {SANCTION_REASON.get(reason, reason)}"
        if action == "suspend":
            detail += f"\nJusqu'au {until} UTC" if until else "\nSans date de fin"
        out.append((f"driver:blocked:{pid}", "normal",
                    f"Movin · {title}\n{where(merchant)}\n\n{name}\n{number}"
                    f"{detail}\n\nhttps://admin.movinapp.net"))
    return out


def check_reports(_):
    """20 · A passenger reported a ride (« Signaler », 2026-09-27). Every one,
    at once and through quiet hours: the owner asked to hear about each report
    as it happens, and one may be about a driver who is still on the road.

    A report's country is its driver's merchant, else the passenger's number
    -- the console's rule. Two days back, like the ratings, so a restart never
    replays the history."""
    rows = psql(f"""
      SELECT rr.id::text,
             {one_line("rr.body")},
             coalesce(rr.ride_short_id, ''),
             coalesce(nullif(trim(concat_ws(' ', p.first_name, p.last_name)), ''),
                      rr.driver_name, '?'),
             coalesce(p.unencrypted_mobile_number, ''),
             coalesce(rr.vehicle_number, ''),
             coalesce(p.merchant_id, ''),
             coalesce(nullif(trim(concat_ws(' ', ap.first_name, ap.last_name)), ''), '?'),
             coalesce(ap.unencrypted_mobile_number, '')
        FROM movin.ride_report rr
        LEFT JOIN atlas_driver_offer_bpp.person p ON p.id = rr.driver_id
        LEFT JOIN atlas_app.person ap ON ap.id = rr.rider_id
       WHERE rr.created_at > now() - interval '2 days'
       ORDER BY rr.created_at""")
    out = []
    for rid, body, ride, driver, dphone, plate, merchant, rider, rphone in (rows or []):
        car = f" · {plate}" if plate else ""
        course = f"\nCourse {ride}" if ride else ""
        out.append((f"report:{rid}", "loud",
                    f"Movin · SIGNALEMENT d'un passager\n{where(merchant, rphone)}\n\n"
                    f"Chauffeur : {driver} {dphone}{car}\n"
                    f"Passager : {rider} {rphone}{course}\n\n"
                    f"« {body[:600]} »\n\nhttps://admin.movinapp.net"))
    return out


CHECKS = [
    check_registrations, check_sms_budget, check_sms_gateway, check_sms_phone,
    check_wallet,
    check_deletions, check_no_rides, check_zero_estimates,
    check_stale_positions, check_nobody_online, check_containers, check_api,
    check_disk, check_certs, check_backups, check_low_ratings,
    check_cancellations, check_blocked_drivers, check_reports,
]


# ── 15 and 16 · the digests ─────────────────────────────────────────────────

def numbers(window, merchant):
    """The same figures the console's Aperçu screen answers with, for one
    country: a ride belongs to its driver's merchant, and ouguiyas are never
    added to dinars."""
    r = psql(f"""
      SELECT
        (SELECT count(*) FROM atlas_driver_offer_bpp.ride r
           JOIN atlas_driver_offer_bpp.person p ON p.id = r.driver_id
          WHERE p.merchant_id = '{merchant}'
            AND r.created_at > now() - interval '{window}'),
        (SELECT coalesce(sum(r.fare),0) FROM atlas_driver_offer_bpp.ride r
           JOIN atlas_driver_offer_bpp.person p ON p.id = r.driver_id
          WHERE p.merchant_id = '{merchant}'
            AND r.created_at > now() - interval '{window}' AND r.status = 'COMPLETED'),
        (SELECT count(*) FROM atlas_driver_offer_bpp.person p
           JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
          WHERE p.merchant_id = '{merchant}' AND NOT di.enabled AND NOT di.blocked),
        (SELECT count(*) FROM atlas_driver_offer_bpp.person p
           JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
          WHERE p.merchant_id = '{merchant}' AND di.enabled AND NOT di.blocked),
        (SELECT count(*) FROM atlas_driver_offer_bpp.person p
           JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
          WHERE p.merchant_id = '{merchant}' AND p.created_at > now() - interval '{window}'),
        (SELECT coalesce(sum(t.amount),0) FROM movin.wallet_topup t
           JOIN atlas_driver_offer_bpp.person p ON p.id = t.driver_id
          WHERE p.merchant_id = '{merchant}'
            AND t.credited_at > now() - interval '{window}')""")
    if not r or len(r[0]) < 6:
        return None
    v = r[0]
    return {"rides": v[0], "fare": v[1], "pending": v[2], "fleet": v[3],
            "new_drivers": v[4], "topups": v[5]}


def digest(window, title):
    parts = []
    for merchant, c in COUNTRY.items():
        n = numbers(window, merchant)
        if not n:
            return None     # half a digest would read as the other half being zero
        parts.append(f"{c['label']}\n"
                     f"Courses            {n['rides']}\n"
                     f"Encaissé           {n['fare']} {c['cur']}\n"
                     f"Rechargements      {n['topups']} {c['cur']}\n"
                     f"Nouveaux chauffeurs {n['new_drivers']}\n"
                     f"Flotte active      {n['fleet']}\n"
                     f"En attente         {n['pending']}")
    return (f"Movin · {title}\n\n" + "\n\n".join(parts)
            + "\n\nhttps://admin.movinapp.net")


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
             (SELECT count(*) FROM movin.driver_document d WHERE d.driver_id = p.id),
             p.merchant_id
        FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
       WHERE p.merchant_id IN {MERCHANTS} AND NOT di.enabled AND NOT di.blocked
       ORDER BY p.created_at""")
    if rows is None:
        return "La base n'a pas répondu."
    if not rows:
        return "Personne n'attend. La file est vide."
    lines = [f"{n}  {where(m)}\n  {num_} · {h} h · {d} papier(s)"
             for n, num_, h, d, m in rows]
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
             p.created_at::date, p.merchant_id
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
    for name, number, en, bl, ac, var, plate, bal, docs, since, merchant in rows:
        c = country_of(merchant)
        state = ("bloqué" if bl == "t" else
                 "en ligne" if (en == "t" and ac == "t") else
                 "actif" if en == "t" else "en attente de validation")
        out.append(f"{name}\n{where(merchant)}\n{number}\nÉtat : {state}\n"
                   f"Véhicule : {var} {plate}\n"
                   f"Porte-monnaie : {bal} {c['cur'] if c else ''}\nPapiers : {docs}\n"
                   f"Inscrit le {since}")
    return "Movin · chauffeur\n\n" + "\n\n———\n\n".join(out)


def cmd_flotte(_):
    sections = []
    for merchant, c in COUNTRY.items():
        rows = psql(f"""
          SELECT coalesce(v.variant,'sans véhicule'), count(*)
            FROM atlas_driver_offer_bpp.person p
            JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
            LEFT JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = p.id
           WHERE p.merchant_id = '{merchant}' AND di.enabled AND NOT di.blocked
           GROUP BY 1 ORDER BY 1""")
        if rows is None:
            return "La base n'a pas répondu."
        online = one(f"""
          SELECT count(*) FROM atlas_driver_offer_bpp.person p
            JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
           WHERE p.merchant_id = '{merchant}'
             AND di.active AND di.enabled AND NOT di.blocked""", "?")
        body = "\n".join(f"{v:<16} {n}" for v, n in rows) or "aucun"
        sections.append(f"{c['label']}\n{body}\nEn ligne maintenant : {online}")
    return "Movin · flotte\n\n" + "\n\n".join(sections)


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
