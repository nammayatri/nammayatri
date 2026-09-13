'use strict';
//
// The lock on the OTP check, for both sides of the stack.
//
// ── The hole ────────────────────────────────────────────────────────────────
// `POST /v2/auth` answers with `attempts: 3`, and the backend does not enforce
// it. Measured on this stack: ~62 ms per guess, 62 consecutive wrong codes, the
// counter never moved, and the same auth session still accepted the right code
// afterwards. Four digits is 10,000 possibilities -- about ten minutes
// single-threaded, far less in parallel.
//
// That is harmless while every port but SSH is shut. It stops being harmless
// the moment the API is published on 443, which is why this went in before the
// edge did.
//
// ── The second hole, which is worse ─────────────────────────────────────────
// The code is not merely guessable, it is *fixed*: `useFakeSms = Some 7891` in
// dhall-configs/dev/, on the rider app and the driver app alike. Measured
// against the driver app on 2026-08-18: 0000 refused, 1234 refused, 7891
// accepted. That setting is still there and is not going away: turning it off
// means no code is delivered at all, because the gateway it would then look for
// is a dead port, and changing which gateway the *binary* calls is a rebuild.
//
// A fixed code is survivable on the rider side of a pilot. On the driver side
// it is not: publishing /ui/ with a code the whole internet knows means anyone
// who knows a driver's phone number owns that driver's account, his shift, and
// his earnings. The same probe also created a driver record for a number nobody
// approved, just by asking -- so self-enrolment is open too.
//
// So this guard now does one more thing for the driver side: it holds a
// PERSONAL CODE per approved number, checks the driver's code itself, and only
// then rewrites the body to the fixed code the backend expects. 7891 stops
// working from the internet, because the guard never forwards it. See
// `driver-codes.json` and `enrol-driver.sh`.
//
// That was not a workaround waiting to be replaced -- it was the same shape the
// real thing has. Since 2026-09-06 the guard also generates a random code per
// sign-in and sends it through Moorsyl, and substitutes exactly as before; only
// the source of the code changed, which is what this file predicted.
//
// ── So where does the code come from now ────────────────────────────────────
// This process, and nowhere else. On a sign-in it makes a random code, texts
// it, and remembers it against the authId. On verify it checks what was typed
// and forwards 7891 upstream regardless. The backend therefore still believes
// in its fixed code and has never been told otherwise -- 7891 is no longer a
// password anybody holds, it is an internal detail between these two processes.
//
// Two consequences worth knowing before changing anything here:
//
//   • The code lives in memory. A restart of this container invalidates every
//     sign-in in flight, and those riders start over. That is seconds, and it
//     is the same trade the session counters already make.
//   • The driver's personal code still works alongside the texted one. That is
//     deliberate: the fleet must not be grounded by an outage at a third party
//     or by an unpaid balance. `enrol-driver.sh` still governs who may sign in
//     at all -- an unlisted number is refused before it costs an SMS.
//
// ── Why here and not in the backend ─────────────────────────────────────────
// That is where it belongs: the `attempts` counter already exists in the
// response, and enforcing it in Haskell would be a few lines. But this stack
// runs *prebuilt* binaries from a CI job with a 350-minute budget and a cache
// that accumulates across runs. Rebuilding to change a counter means a
// multi-hour cycle and a real chance of ending up with binaries that differ
// from the ones every test so far has run against.
//
// So the rule is enforced in front, in code we can deploy in seconds and revert
// just as fast. When the backend is next rebuilt for another reason, the check
// should move into it and this guard should become belt-and-braces.
//
// ── Everything goes through here ────────────────────────────────────────────
// The edge proxies *all* of /v2/ and *all* of /ui/ to this process, not just
// the auth paths. A guard you can route around is not a guard, and one mistyped
// nginx `location` is all it would take.
//
// It is not a general proxy: a path matching no route below is refused here as
// well as at the edge. The driver binary also serves 41 `/dashboard/` routes --
// the office API, which enables drivers and attaches vehicles -- and two
// independent refusals is the right number for that.
//
// State is in memory on purpose. An auth session lives ten minutes; there is
// one replica; and an external store would be one more thing to be down. A
// restart clears the counters, which is why nginx also rate-limits by IP -- that
// layer survives a restart of this one. If this ever runs as more than one
// process, the counters must move to Redis, and that is the moment to notice.
//
// No dependencies: Node's built-in http, crypto and fs, plus global fetch.

const http = require('http');
const fs = require('fs');
const crypto = require('crypto');

const PORT = Number(process.env.PORT || 8031);

const strip = (u) => String(u).replace(/\/$/, '');
const RIDER_URL = strip(process.env.UPSTREAM_URL || 'http://127.0.0.1:8013');
const DRIVER_URL = strip(process.env.DRIVER_UPSTREAM_URL || 'http://127.0.0.1:8016');

/** Wrong codes allowed per auth session. The number the backend already claims. */
const MAX_ATTEMPTS = Number(process.env.MAX_ATTEMPTS || 3);

/**
 * How long an auth session may be used at all, in ms. The backend never expires
 * one -- an authId issued yesterday still verifies today, which turns every
 * abandoned sign-in into a permanent guessing target.
 */
const AUTH_TTL_MS = Number(process.env.AUTH_TTL_MS || 10 * 60 * 1000);

/** How long a locked-out session stays locked. */
const LOCK_MS = Number(process.env.LOCK_MS || 15 * 60 * 1000);

/**
 * How many sign-ins one phone number may start per window.
 *
 * Locking a *session* is worthless on its own: an attacker just asks for a new
 * authId, spends three guesses on it, and repeats. Three in ten thousand per
 * session means about 3,300 sessions for an even chance -- minutes, if starting
 * one is free. This is the control that makes the session lock mean something.
 *
 * It is also what stops someone burning our SMS credit the day a real gateway
 * exists, which is the more expensive version of the same request.
 */
const MAX_STARTS = Number(process.env.MAX_STARTS || 5);
const START_WINDOW_MS = Number(process.env.START_WINDOW_MS || 60 * 60 * 1000);

/**
 * Resends allowed per session.
 *
 * A resend legitimately clears the wrong-code count -- a new code was sent, so
 * the old count describes nothing. Uncapped, that is also a way to walk around
 * the lockout: three guesses, resend, three guesses, forever. Probed on
 * 2026-08-18 and found unreachable for an unrelated reason (the backend answers
 * resend with 500 on this stack, there being no gateway to resend through), so
 * this closes a hole that is currently boarded up by an accident. It costs one
 * counter and it will still be right when resend starts working.
 */
const MAX_RESENDS = Number(process.env.MAX_RESENDS || 3);

const UPSTREAM_TIMEOUT_MS = 20000;

/**
 * Biggest body accepted. Was 1 MB, which was right when the only callers were
 * sign-in and booking. The driver app posts licence and registration photos to
 * /ui/driver/register/validateImage as base64 -- a 1.5 MB phone photo is ~2 MB
 * encoded -- so a 1 MB cap here would reject document upload with a 413 that
 * looks like a network fault from the phone. nginx has a matching limit; both
 * have to be raised or neither means anything.
 */
const MAX_BODY = Number(process.env.MAX_BODY || 8 * 1024 * 1024);

/* ────────────────────────────────────────────────────────────────────────────
   The gateway

   Moorsyl, Mauritanian. `POST /api/sms`, an `x-api-key` header, a body of
   { to, from, body }. Measured against the live API on 2026-09-06:

     • Validation runs BEFORE authentication, so a 400 says nothing about
       whether the key is good. Only a well-formed request ever reaches the key
       check -- which is also why the probes that mapped this contract cost
       nothing to run.
     • `to` must match /^\+222[234]\d{7}$/: the country code, a mobile prefix,
       then seven digits. That is the same rule the app enforces before it calls
       us, so a number which got this far already fits.
     • Success is `{ accepted: true, messageId }`, and `accepted` means queued,
       not delivered. The only delivery signal is a webhook we do not consume,
       so "sent" here always means "the gateway took it".

   The key is never in git, never in the database and never in a log line. It
   arrives as an environment variable from /opt/ny/secrets/moorsyl.env.
   ──────────────────────────────────────────────────────────────────────────── */

/**
 * Which of Moorsyl's two products delivers the code.
 *
 *   'verify'  Moorsyl makes the code, sends it under its own sender and
 *             template, and checks it for us. Codes are exactly 6 characters.
 *   'sms'     We make the code and send it as plain SMS under our own sender
 *             name, with our own French wording.
 *
 * `sms` is the one we want and it is written and tested, but on 2026-09-06 this
 * account could not use it: POST /api/sms answers 403 COMPLIANCE_REQUIRED for
 * every sender -- "Movin", "moorsyl", and none at all -- so it is the account
 * that is not cleared, not the name. Measured, not inferred. /verify/send on
 * the same key at the same moment answered 200.
 *
 * The likely reason is the one that applies to branded SMS everywhere: a sender
 * ID has to be registered with the operators before it may be used, whereas
 * Verify sends under Moorsyl's own already-registered sender. So this is a mode
 * and not a rewrite -- when the client clears compliance, SMS_MODE=sms is the
 * whole switch, and riders get the Movin-branded French message instead.
 *
 * Both modes use a 6-character code so that the app is built once and the
 * switch is invisible to it.
 */
const SMS_MODE = (process.env.SMS_MODE || 'verify').toLowerCase();

const SMS_URL = process.env.SMS_URL || 'https://api.moorsyl.com/api/sms';
const VERIFY_SEND_URL = process.env.VERIFY_SEND_URL || 'https://api.moorsyl.com/api/verify/send';
const VERIFY_CHECK_URL = process.env.VERIFY_CHECK_URL || 'https://api.moorsyl.com/api/verify/check';
const SMS_KEY = process.env.MOORSYL_API_KEY || '';
const SMS_SENDER = process.env.SMS_SENDER || 'Movin';
const SMS_TIMEOUT_MS = Number(process.env.SMS_TIMEOUT_MS || 15000);

/** For /healthz. Never holds the key or a code. */
let lastSmsError = null;
let smsSent = 0;

/**
 * Numbers that skip the gateway entirely and keep the fixed code.
 *
 * Not a convenience -- without it this change locks the people building the app
 * out of it. The gateway only accepts real Mauritanian mobiles, and everyone
 * testing from Algeria signs in as an invented +222 number. Once every code is
 * texted, an invented number gets a message that is delivered nowhere, and the
 * sign-in fails with no way round it.
 *
 * So: a short, explicit list, in full international form
 * (`SMS_BYPASS=+22222778899,+22222778800`). These numbers cost no credit, send
 * nothing, and still verify with the fixed code exactly as every number did
 * before today.
 *
 * It is a hole, and it is meant to be an obvious one. It is printed at startup
 * and counted on /healthz so that nobody has to read this file to discover that
 * some numbers are exempt. Empty it the day the pilot has real riders -- that
 * is the same instruction TEST_OTP carries in the app, and they go together.
 */
const SMS_BYPASS = new Set(
  (process.env.SMS_BYPASS || '').split(',').map((s) => s.trim()).filter(Boolean),
);

/**
 * What an exempt number types instead of a code it never received.
 *
 * It cannot be the backend's own 7891. The app's input is six characters wide
 * now, because Moorsyl's Verify codes are exactly six, so a four-character code
 * can no longer be typed in full — an exempt number would be locked out by the
 * very screen built to let it in. So the guard accepts this instead and
 * substitutes the backend's fixed code exactly as it does for a real one.
 *
 * Six ones, because it should be impossible to mistake for a real code in a
 * screenshot or a log.
 */
const SMS_BYPASS_CODE = process.env.SMS_BYPASS_CODE || '111111';

/**
 * One GSM-7 segment, so one message and one charge. Accented characters are in
 * the GSM alphabet and would be safe; it is emoji and the like that silently
 * force UCS-2 and halve the room. There are none here.
 */
const smsText = (code) =>
  `Movin : votre code est ${code}. Ne le communiquez a personne. ` +
  'Il expire dans 10 minutes.';

/**
 * A code, uniformly at random.
 *
 * `Math.random()` is neither uniform after a modulo nor unpredictable, and this
 * string is now the whole secret -- it is what 7891 used to be, except that it
 * differs per session and nobody but the holder of the phone is told it.
 * Leading zeros are kept: the app compares strings of a fixed length, and
 * dropping them would quietly shrink the keyspace.
 */
function mkCode(digits) {
  let out = '';
  for (let i = 0; i < digits; i += 1) out += String(crypto.randomInt(0, 10));
  return out;
}

/** Constant-time, so a wrong code leaks nothing through how long it took. */
function sameCode(given, want) {
  const a = Buffer.from(String(given));
  const b = Buffer.from(String(want));
  return a.length === b.length && crypto.timingSafeEqual(a, b);
}

async function sendSms(number, code) {
  if (!SMS_KEY) {
    lastSmsError = 'no API key configured';
    console.error('[guard] sms: MOORSYL_API_KEY is empty -- nothing can be sent');
    return { ok: false };
  }

  let res;
  let text;
  try {
    res = await fetch(SMS_URL, {
      method: 'POST',
      headers: { 'content-type': 'application/json', 'x-api-key': SMS_KEY },
      body: JSON.stringify({ to: number, from: SMS_SENDER, body: smsText(code) }),
      signal: AbortSignal.timeout(SMS_TIMEOUT_MS),
    });
    text = await res.text();
  } catch (err) {
    // A failure here is a rider watching a screen, so record which failure it
    // was rather than a bare "could not send".
    lastSmsError = `${err.name}: ${err.message}`;
    console.error(`[guard] sms to ${number} failed -- ${lastSmsError}`);
    return { ok: false };
  }

  // The gateway quotes parts of a request it rejects. Nothing that echoes our
  // body reaches the log with the code still legible in it.
  const safe = text.split(code).join('****').slice(0, 300);

  let accepted = false;
  try {
    accepted = JSON.parse(text).accepted === true;
  } catch { /* not JSON: not a success either, and `safe` says what it was */ }

  if (!res.ok || !accepted) {
    lastSmsError = `HTTP ${res.status} ${safe}`;
    console.error(`[guard] sms to ${number} refused -- ${lastSmsError}`);
    return { ok: false };
  }

  smsSent += 1;
  console.log(`[guard] sms to ${number} accepted by the gateway`);
  return { ok: true };
}

/** POST to Moorsyl and say what came back. Never logs the key. */
async function moorsyl(url, payload) {
  if (!SMS_KEY) {
    lastSmsError = 'no API key configured';
    console.error('[guard] MOORSYL_API_KEY is empty -- nothing can be sent');
    return { reachable: false };
  }
  try {
    const res = await fetch(url, {
      method: 'POST',
      headers: { 'content-type': 'application/json', 'x-api-key': SMS_KEY },
      body: JSON.stringify(payload),
      signal: AbortSignal.timeout(SMS_TIMEOUT_MS),
    });
    const text = await res.text();
    let json = null;
    try { json = JSON.parse(text); } catch { /* keep the text for the log */ }
    return { reachable: true, status: res.status, ok: res.ok, json, text };
  } catch (err) {
    lastSmsError = `${err.name}: ${err.message}`;
    return { reachable: false };
  }
}

/**
 * Ask Moorsyl to send a code, in Verify mode.
 *
 * We never learn the code -- checking it is a second call. That is the trade
 * for not needing a registered sender ID.
 */
async function verifySend(number) {
  const r = await moorsyl(VERIFY_SEND_URL, { to: number });
  if (!r.reachable) {
    console.error(`[guard] verify to ${number} failed -- ${lastSmsError}`);
    return { ok: false };
  }
  const id = r.json && r.json.verificationId;
  if (!r.ok || !id) {
    lastSmsError = `HTTP ${r.status} ${String(r.text).slice(0, 300)}`;
    console.error(`[guard] verify to ${number} refused -- ${lastSmsError}`);
    return { ok: false };
  }
  smsSent += 1;
  console.log(`[guard] verify to ${number} accepted by the gateway`);
  return { ok: true, verificationId: id };
}

/**
 * Check a typed code against a verification.
 *
 * `reachable` is separated from `approved` on purpose. A wrong code and an
 * outage at Moorsyl must not look the same: the first spends an attempt, the
 * second must not, or a bad afternoon at the gateway silently locks people out
 * of their accounts. A 400 here is the API rejecting the code's *shape*, which
 * is a wrong code and nothing more.
 */
async function verifyCheck(verificationId, code) {
  const r = await moorsyl(VERIFY_CHECK_URL, { verificationId, code });
  if (!r.reachable || (r.status >= 500)) {
    console.error(`[guard] verify check unavailable -- ${lastSmsError || `HTTP ${r.status}`}`);
    return { reachable: false, approved: false };
  }
  return { reachable: true, approved: !!(r.json && r.json.status === 'approved') };
}

/**
 * A new code for this session, sent.
 *
 * In `sms` mode the code lives in this process and nowhere else; in `verify`
 * mode we hold only the id and Moorsyl holds the code. Either way the session
 * is only marked as having one if the gateway actually took it -- a session
 * carrying a code nobody received would also refuse the personal code, and lock
 * a driver out of his own account.
 */
async function issueCode(route, s, number) {
  if (SMS_MODE === 'verify') {
    const sent = await verifySend(number);
    if (sent.ok) {
      s.verificationId = sent.verificationId;
      s.smsCode = null;
    }
    return sent;
  }
  const code = mkCode(route.codeDigits);
  const sent = await sendSms(number, code);
  if (sent.ok) {
    s.smsCode = code;
    s.verificationId = null;
  }
  return sent;
}

/* ────────────────────────────────────────────────────────────────────────────
   Routes

   The prefix decides the upstream and whether personal codes apply. Order
   matters only in that the first match wins; the two prefixes are disjoint.
   ──────────────────────────────────────────────────────────────────────────── */

const ROUTES = [
  {
    name: 'rider',
    prefix: '/v2/',
    upstream: RIDER_URL,
    // No personal codes on this side: a rider is whoever holds the phone, and
    // the SMS below is what proves it. There is no agency to issue him one.
    codesFile: null,
    // Now that a code is really delivered, the rider side substitutes too --
    // otherwise the code we texted would be forwarded to a backend that accepts
    // only 7891, and every correct code would come back refused.
    fixedOtp: process.env.RIDER_FIXED_OTP || '7891',
    // Six, matching CODE_LENGTH in the app's config.ts. It was four until
    // 2026-09-06: Moorsyl's Verify takes a code of exactly six characters, and
    // the `sms` mode uses six as well so that the app is built once and the
    // mode switch is invisible to it. A mismatch here is a code that cannot be
    // typed in full, and on screen that looks like the SMS was wrong.
    codeDigits: 6,
    sms: true,
  },
  {
    name: 'driver',
    prefix: '/ui/',
    upstream: DRIVER_URL,
    codesFile: process.env.DRIVER_CODES || '/app/driver-codes.json',
    // What the backend accepts, and what the guard substitutes once it has
    // checked the driver's own code. Kept out of the log on purpose.
    fixedOtp: process.env.DRIVER_FIXED_OTP || '7891',
    codeDigits: 6,
    // Both work here, deliberately. The texted code is the way in; the personal
    // code stays valid so that an outage at the gateway -- or an unpaid balance
    // -- does not ground the fleet. It is no weaker than it was yesterday, and
    // it is the only credential that does not depend on a third party being up.
    sms: true,
  },
];

function routeFor(pathname) {
  return ROUTES.find((r) => pathname.startsWith(r.prefix)) || null;
}

/* ────────────────────────────────────────────────────────────────────────────
   Personal codes

   File shape:
     { "codes": { "+2130551234567": { "salt": hex, "hash": hex, "note": str } } }

   hash = sha256(`${salt}:${number}:${code}`). Salted per number so the file is
   not a rainbow-table lookup, and so two drivers who pick the same code do not
   share a hash. No pepper: one more secret to lose, for a pilot whose whole
   list is ten numbers, and the file is already handled as a secret.

   Re-read when its mtime changes, so enrolling a driver needs no restart. The
   directory is bind-mounted, not the file, so replacing the file inside it is
   safe -- the trap where `tar -x` unlinks a bind-mounted inode and the
   container keeps serving the old one does not apply here.
   ──────────────────────────────────────────────────────────────────────────── */

const codeCache = new Map(); // path -> { mtimeMs, codes }

function loadCodes(path) {
  if (!path) return null;
  let stat;
  try {
    stat = fs.statSync(path);
  } catch {
    // Absent means nobody is enrolled, which must read as "refuse everyone",
    // never as "let everyone through". A typo in the mount path has to fail
    // closed.
    return {};
  }
  const seen = codeCache.get(path);
  if (seen && seen.mtimeMs === stat.mtimeMs) return seen.codes;
  let codes = {};
  try {
    const parsed = JSON.parse(fs.readFileSync(path, 'utf8'));
    codes = parsed && typeof parsed.codes === 'object' ? parsed.codes : {};
    console.log(`[guard] loaded ${Object.keys(codes).length} personal codes from ${path}`);
  } catch (err) {
    // Same reasoning: a malformed file refuses everyone rather than admitting
    // everyone. Loud, because it means an enrolment did not take effect.
    console.error(`[guard] cannot read ${path}: ${err.message} -- refusing all sign-ins`);
  }
  codeCache.set(path, { mtimeMs: stat.mtimeMs, codes });
  return codes;
}

function codeMatches(entry, number, code) {
  if (!entry || !entry.salt || !entry.hash) return false;
  const want = Buffer.from(String(entry.hash), 'hex');
  const got = crypto.createHash('sha256')
    .update(`${entry.salt}:${number}:${code}`)
    .digest();
  // Equal length is a precondition of timingSafeEqual, and a truncated hash in
  // the file would otherwise throw rather than refuse.
  return want.length === got.length && crypto.timingSafeEqual(want, got);
}

/* ────────────────────────────────────────────────────────────────────────────
   Session bookkeeping
   ──────────────────────────────────────────────────────────────────────────── */

/** `${route}:${authId}` -> { born, attempts, resends, lockedUntil, number } */
const sessions = new Map();

/** `${route}:${number}` -> [timestamps of sign-ins started] */
const starts = new Map();

// Sweep, so a long-running process does not accumulate dead sessions. Cheap:
// these maps hold one entry per sign-in attempt in the last few minutes.
setInterval(() => {
  const now = Date.now();
  for (const [id, s] of sessions) {
    if (now - s.born > AUTH_TTL_MS && now > (s.lockedUntil || 0)) sessions.delete(id);
  }
  for (const [key, times] of starts) {
    const live = times.filter((t) => now - t < START_WINDOW_MS);
    if (live.length) starts.set(key, live);
    else starts.delete(key);
  }
}, 60_000).unref();

/** Records a sign-in start and says whether this number has had too many. */
function tooManyStarts(key) {
  const now = Date.now();
  const times = (starts.get(key) || []).filter((t) => now - t < START_WINDOW_MS);
  times.push(now);
  starts.set(key, times);
  return times.length > MAX_STARTS;
}

/* ────────────────────────────────────────────────────────────────────────────
   Talking upstream
   ──────────────────────────────────────────────────────────────────────────── */

function readBody(req) {
  return new Promise((resolve, reject) => {
    const chunks = [];
    let size = 0;
    req.on('data', (c) => {
      size += c.length;
      // Refusing early keeps a hostile body from becoming a memory problem.
      if (size > MAX_BODY) { reject(new Error('body too large')); req.destroy(); return; }
      chunks.push(c);
    });
    req.on('end', () => resolve(Buffer.concat(chunks)));
    req.on('error', reject);
  });
}

async function forward(route, req, body) {
  const headers = {};
  for (const [k, v] of Object.entries(req.headers)) {
    // Hop-by-hop headers, and the ones the upstream must set itself.
    if (['host', 'connection', 'content-length', 'transfer-encoding'].includes(k)) continue;
    headers[k] = v;
  }
  const res = await fetch(`${route.upstream}${req.url}`, {
    method: req.method,
    headers,
    body: ['GET', 'HEAD'].includes(req.method) ? undefined : body,
    signal: AbortSignal.timeout(UPSTREAM_TIMEOUT_MS),
  });
  return { status: res.status, type: res.headers.get('content-type'), text: await res.text() };
}

function send(res, status, obj, extra = {}) {
  const body = JSON.stringify(obj);
  res.writeHead(status, {
    'content-type': 'application/json;charset=utf-8',
    'content-length': Buffer.byteLength(body),
    ...extra,
  });
  res.end(body);
}

/**
 * The backend's own error shape, so the app needs no special case: it already
 * treats any 4xx here as "that code was not accepted".
 */
const refusal = (code) => ({ errorPayload: null, errorCode: code, errorMessage: null });

/* ────────────────────────────────────────────────────────────────────────────
   The rules
   ──────────────────────────────────────────────────────────────────────────── */

const rx = {
  verify: (p) => new RegExp(`^${p}auth/([^/?]+)/verify/?$`),
  resend: (p) => new RegExp(`^${p}auth/otp/([^/?]+)/resend/?$`),
  start: (p) => new RegExp(`^${p}auth/?$`),
};

/**
 * Which countries may sign in at all, by dialling code.
 *
 * Two countries since 2026-09-13, and only one of them open: Algeria has no SMS
 * provider yet, so a +213 sign-in is refused here with `COUNTRY_NOT_OPEN` —
 * which the app shows as "not open in Algeria yet" rather than as a wrong
 * number. Refused BEFORE forwarding, so no person row is created and no code
 * is sent for a country that cannot receive one.
 *
 * A setting and not code, so opening Algeria is `OPEN_COUNTRIES=+222,+213` and
 * a restart — no build, and no new APK: the app already has both countries.
 *
 * Numbers on SMS_BYPASS pass regardless. That is how an Algerian test account
 * can sign in from a real phone before the country opens.
 */
const OPEN_COUNTRIES = new Set(
  (process.env.OPEN_COUNTRIES || '+222').split(',').map((s) => s.trim()).filter(Boolean),
);

async function handle(req, res) {
  const pathname = req.url.split('?')[0];

  if (pathname === '/healthz') {
    return send(res, 200, {
      ok: true,
      routes: ROUTES.map((r) => ({
        prefix: r.prefix,
        upstream: r.upstream,
        personalCodes: r.codesFile ? Object.keys(loadCodes(r.codesFile)).length : null,
        sms: !!r.sms,
        codeDigits: r.codeDigits,
      })),
      sessions: sessions.size,
      numbers: starts.size,
      // Enough to tell "the gateway is down" from "the key was never mounted"
      // without opening a shell. The key itself is only ever a boolean here.
      gateway: {
        configured: !!SMS_KEY,
        mode: SMS_MODE,
        // Only meaningful in `sms` mode; in `verify` mode Moorsyl's own sender
        // is used and this is ignored.
        sender: SMS_MODE === 'sms' ? SMS_SENDER : null,
        sent: smsSent,
        lastError: lastSmsError,
        // Counted, not listed: enough to notice the exemption exists without
        // publishing which numbers can be signed into with a known code.
        bypassNumbers: SMS_BYPASS.size,
      },
    });
  }

  const route = routeFor(pathname);
  if (!route) {
    // Not a proxy. /dashboard/ in particular is the office API and has no
    // business being reachable from a phone.
    return send(res, 404, refusal('NOT_FOUND'));
  }

  let body;
  try {
    body = await readBody(req);
  } catch {
    return send(res, 413, refusal('REQUEST_TOO_LARGE'));
  }

  const codes = loadCodes(route.codesFile);
  const key = (id) => `${route.name}:${id}`;
  const verify = rx.verify(route.prefix).exec(pathname);
  const resend = rx.resend(route.prefix).exec(pathname);
  const isStart = rx.start(route.prefix).test(pathname);

  /* ── starting a sign-in ──────────────────────────────────────────────────
     Checked before forwarding, so a throttled or unknown number never reaches
     the backend: it costs no SMS once there is a gateway, and -- the reason
     this matters on the driver side -- it creates no driver record. Asking for
     a code is enough to bring a `person` row into existence otherwise, which
     was measured, not assumed. */
  if (isStart && req.method === 'POST') {
    let number = null;
    let dialCode = null;
    try {
      const parsed = JSON.parse(body.toString('utf8'));
      dialCode = typeof parsed.mobileCountryCode === 'string' ? parsed.mobileCountryCode : null;
      number = `${parsed.mobileCountryCode || ''}${parsed.mobileNumber || ''}`;
    } catch { /* malformed: let the backend give its own 400 */ }

    // First, before enrolment and throttling: a closed country is not a
    // question about this number at all. See OPEN_COUNTRIES.
    if (dialCode && !OPEN_COUNTRIES.has(dialCode) && !SMS_BYPASS.has(number)) {
      console.warn(`[guard] ${route.name}: ${dialCode} is not open for sign-in`);
      return send(res, 403, refusal('COUNTRY_NOT_OPEN'));
    }

    if (codes && number && !codes[number]) {
      console.warn(`[guard] ${route.name}: ${number} is not enrolled`);
      // Deliberately the same shape and status for "never approved" and
      // "approved but removed": the caller learns that this number cannot sign
      // in here, and not whether it is one the agency knows.
      return send(res, 403, refusal('NOT_REGISTERED'));
    }

    if (number && tooManyStarts(key(number))) {
      console.warn(`[guard] ${route.name}: throttled sign-ins for ${number}`);
      return send(res, 429, refusal('TOO_MANY_REQUESTS'),
        { 'retry-after': String(Math.ceil(START_WINDOW_MS / 1000)) });
    }

    let up;
    try {
      up = await forward(route, req, body);
    } catch (err) {
      console.error(`[guard] ${route.name} upstream: ${err.message}`);
      return send(res, 502, refusal('UPSTREAM_UNAVAILABLE'));
    }

    let authId = null;
    if (up.status === 200) {
      try {
        ({ authId } = JSON.parse(up.text));
        // Recording the birth is what makes expiry possible at all -- the
        // backend never expires an auth id, so without this an abandoned
        // sign-in stays guessable indefinitely. Recording the number is what
        // makes the personal code checkable: the verify request carries only
        // the authId, so this is the guard's only chance to learn who it is for.
        if (authId) {
          sessions.set(key(authId),
            {
            born: Date.now(),
            attempts: 0,
            resends: 0,
            lockedUntil: 0,
            number,
            smsCode: null,
            verificationId: null,
          });
        }
      } catch { /* not JSON we recognise; nothing to remember */ }
    }

    /* ── the code the caller will have to type ──────────────────────────────
       Sent after the throttle, so a number being hammered costs no credit, and
       after the upstream 200, so no code goes out for a session the backend
       declined to open. */
    if (authId && route.sms && SMS_BYPASS.has(number)) {
      // Given the test code directly, so it travels the same path a real one
      // does -- checked here, and the backend's fixed code substituted before
      // forwarding. Logged every time: an exempt number should never be a
      // surprise when reading why somebody got in.
      const s = sessions.get(key(authId));
      if (s) s.smsCode = SMS_BYPASS_CODE;
      console.log(`[guard] ${route.name}: ${number} is exempt, test code accepted`);
    } else if (authId && route.sms) {
      const s = sessions.get(key(authId));
      const sent = await issueCode(route, s, number);
      if (!sent.ok) {
        if (route.codesFile) {
          // The driver still has his permanent code, so this is a warning and
          // not a refusal. Losing the gateway must not also ground the fleet.
          console.warn(`[guard] ${route.name}: no SMS for ${number}, personal code still stands`);
        } else {
          // A rider has nothing else to sign in with. Saying so beats a screen
          // that waits for a message which is not coming, and the session is
          // dropped so the number is not left with a guessable open session.
          sessions.delete(key(authId));
          return send(res, 502, refusal('SMS_SEND_FAILED'));
        }
      }
    }

    res.writeHead(up.status, { 'content-type': up.type || 'application/json' });
    return res.end(up.text);
  }

  /* ── the guarded path ────────────────────────────────────────────────────── */
  if (verify && req.method === 'POST') {
    const id = decodeURIComponent(verify[1]);
    const now = Date.now();
    const known = sessions.get(key(id));

    // An id this process has not seen -- it restarted, or the session began
    // before the guard did.
    //
    // On an uncoded route: start counting from now rather than waving it
    // through. We lose the age, we do not lose the attempt limit.
    //
    // On a coded route it has to be a refusal, and this is the hinge of the
    // whole design: without the remembered number there is nothing to check
    // the personal code against, and forwarding anyway would hand the raw body
    // to a backend that accepts 7891 from anyone. The cost is that a guard
    // restart makes drivers mid-sign-in start over, which is ten seconds.
    if (!known && (codes || route.sms)) {
      console.warn(`[guard] ${route.name}: unknown session ${id}`);
      return send(res, 400, refusal('INVALID_AUTH_DATA'));
    }
    const s = known || { born: now, attempts: 0, resends: 0, lockedUntil: 0, number: null };
    sessions.set(key(id), s);

    if (now < s.lockedUntil) {
      const after = Math.ceil((s.lockedUntil - now) / 1000);
      console.warn(`[guard] ${route.name}: locked ${id} (${after}s left)`);
      return send(res, 429, refusal('TOO_MANY_ATTEMPTS'), { 'retry-after': String(after) });
    }

    if (now - s.born > AUTH_TTL_MS) {
      console.warn(`[guard] ${route.name}: expired ${id}`);
      sessions.delete(key(id));
      return send(res, 400, refusal('INVALID_AUTH_DATA'));
    }

    // Counting a wrong code, and locking on the third. Shared by the personal
    // code check and the upstream's own verdict so the two cannot drift.
    const countWrong = () => {
      s.attempts += 1;
      if (s.attempts >= MAX_ATTEMPTS) {
        s.lockedUntil = now + LOCK_MS;
        console.warn(`[guard] ${route.name}: LOCKED ${id} after ${s.attempts} wrong codes`);
        return send(res, 429, refusal('TOO_MANY_ATTEMPTS'),
          { 'retry-after': String(Math.ceil(LOCK_MS / 1000)) });
      }
      console.log(`[guard] ${route.name}: wrong code ${id} (${s.attempts}/${MAX_ATTEMPTS})`);
      // The shape the backend gives for a wrong code, so the app's existing
      // handling applies unchanged.
      return send(res, 400, refusal('INVALID_AUTH_DATA'));
    };

    let outgoing = body;

    // ── the code ───────────────────────────────────────────────────────────
    // Two things can open a session and either one is enough: the code texted
    // for this session, and -- on the driver side -- the permanent code the
    // agency issued. Neither is ever forwarded. What goes upstream is always
    // the fixed code the deployed binary was built with, which is how 7891
    // stops being a password anybody has: it becomes an internal detail
    // between this process and a backend that costs 45 minutes to change.
    if (codes || s.smsCode || s.verificationId) {
      let given = null;
      let parsed = null;
      try {
        parsed = JSON.parse(body.toString('utf8'));
        given = parsed.otp == null ? null : String(parsed.otp);
      } catch { /* handled below */ }

      if (given === null) return send(res, 400, refusal('INVALID_REQUEST'));

      // Both are evaluated -- no short-circuit -- so how long this takes does
      // not say which of the two the caller got closer to.
      const bySms = s.smsCode ? sameCode(given, s.smsCode) : false;
      const byPersonal = codes ? codeMatches(codes[s.number], s.number, given) : false;

      // Only asked when nothing local already opened the session: it is a
      // network round trip, and in Verify mode Moorsyl counts the attempt at
      // its end too. A driver who used his own code should not spend one.
      let byVerify = false;
      if (s.verificationId && !bySms && !byPersonal) {
        const checked = await verifyCheck(s.verificationId, given);
        if (!checked.reachable) {
          // An outage is not a wrong code. Saying so costs the caller nothing
          // and keeps a bad afternoon at the gateway from locking accounts.
          return send(res, 502, refusal('SMS_CHECK_FAILED'));
        }
        byVerify = checked.approved;
      }

      if (!bySms && !byPersonal && !byVerify) return countWrong();

      parsed.otp = route.fixedOtp;
      outgoing = Buffer.from(JSON.stringify(parsed));
    }

    let up;
    try {
      const proxied = Object.create(req);
      proxied.headers = { ...req.headers, 'content-length': String(outgoing.length) };
      up = await forward(route, proxied, outgoing);
    } catch (err) {
      console.error(`[guard] ${route.name} upstream: ${err.message}`);
      return send(res, 502, refusal('UPSTREAM_UNAVAILABLE'));
    }

    if (up.status === 200) {
      // Spent. Nothing more can be tried against it.
      sessions.delete(key(id));
      console.log(`[guard] ${route.name}: verified ${id}`);
    } else {
      return countWrong();
    }

    res.writeHead(up.status, { 'content-type': up.type || 'application/json' });
    return res.end(up.text);
  }

  /* ── resend ──────────────────────────────────────────────────────────────
     A new code was sent, so the old wrong-code count describes nothing and is
     cleared. Capped, because clearing it without limit is a way around the
     lockout. On a coded route there is nothing to resend -- the driver's code
     does not change -- so it is refused outright rather than forwarded to a
     backend that would answer 500 anyway. */
  if (resend && req.method === 'POST') {
    const id = decodeURIComponent(resend[1]);
    const s = sessions.get(key(id));

    /* With a gateway, a resend is this process's job and not the backend's:
       another code, sent, and the wrong-code count cleared -- that count
       describes a code which no longer opens anything. The backend is never
       asked. Its own resend answers 500 on this stack, there being no gateway
       behind it, and that 500 is what made the button look broken. */
    if (route.sms) {
      if (!s || !s.number) return send(res, 400, refusal('INVALID_AUTH_DATA'));
      if (s.resends >= MAX_RESENDS) {
        console.warn(`[guard] ${route.name}: resend cap on ${id}`);
        return send(res, 429, refusal('TOO_MANY_REQUESTS'),
          { 'retry-after': String(Math.ceil(LOCK_MS / 1000)) });
      }
      const sent = await issueCode(route, s, s.number);
      if (!sent.ok) return send(res, 502, refusal('SMS_SEND_FAILED'));
      s.resends += 1;
      s.attempts = 0;
      s.lockedUntil = 0;
      s.born = Date.now();
      console.log(`[guard] ${route.name}: resent to ${s.number} (${s.resends}/${MAX_RESENDS})`);
      // The shape the app reads back -- it takes the authId from the reply
      // rather than assuming the one it sent.
      return send(res, 200, { authId: id });
    }

    if (codes) {
      console.log(`[guard] ${route.name}: resend refused, codes are permanent`);
      return send(res, 400, refusal('RESEND_NOT_SUPPORTED'));
    }
    if (s && s.resends >= MAX_RESENDS) {
      console.warn(`[guard] ${route.name}: resend cap on ${id}`);
      return send(res, 429, refusal('TOO_MANY_REQUESTS'),
        { 'retry-after': String(Math.ceil(LOCK_MS / 1000)) });
    }

    let up;
    try {
      up = await forward(route, req, body);
    } catch (err) {
      console.error(`[guard] ${route.name} upstream: ${err.message}`);
      return send(res, 502, refusal('UPSTREAM_UNAVAILABLE'));
    }
    if (up.status === 200) {
      sessions.set(key(id), {
        born: Date.now(),
        attempts: 0,
        resends: (s ? s.resends : 0) + 1,
        lockedUntil: 0,
        number: s ? s.number : null,
      });
    }
    res.writeHead(up.status, { 'content-type': up.type || 'application/json' });
    return res.end(up.text);
  }

  /* ── everything else: forwarded unchanged ───────────────────────────────── */
  let up;
  try {
    up = await forward(route, req, body);
  } catch (err) {
    console.error(`[guard] ${route.name} ${req.url}: ${err.message}`);
    return send(res, 502, refusal('UPSTREAM_UNAVAILABLE'));
  }

  res.writeHead(up.status, { 'content-type': up.type || 'application/json' });
  res.end(up.text);
}

http.createServer((req, res) => {
  handle(req, res).catch((err) => {
    console.error(`[guard] ${err.stack || err.message}`);
    if (!res.headersSent) send(res, 500, refusal('GUARD_ERROR'));
  });
}).listen(PORT, () => {
  for (const r of ROUTES) {
    const n = r.codesFile ? Object.keys(loadCodes(r.codesFile)).length : null;
    const how = [
      r.sms
        ? `${r.codeDigits}-digit code via ${SMS_MODE === 'verify' ? 'Moorsyl Verify' : 'our own SMS'}`
        : 'no SMS',
      n === null ? null : `${n} personal codes`,
    ].filter(Boolean).join(', ');
    console.log(`auth-guard  ${r.prefix} -> ${r.upstream}  (${how})`);
  }
  console.log(
    `auth-guard on :${PORT}  ` +
    `${MAX_ATTEMPTS} attempts, session ${AUTH_TTL_MS / 60000} min, lock ${LOCK_MS / 60000} min, ` +
    `${MAX_STARTS} sign-ins per number per ${START_WINDOW_MS / 60000} min, ` +
    `${MAX_RESENDS} resends, body ${Math.round(MAX_BODY / 1024)} kB`,
  );
  // Loud, because without a key the rider side refuses every sign-in rather
  // than falling back to something -- there is nothing to fall back to.
  if (!SMS_KEY) {
    console.error('auth-guard  WARNING: no MOORSYL_API_KEY -- riders cannot sign in at all');
  } else if (SMS_MODE === 'verify') {
    console.log(`auth-guard  gateway ${VERIFY_SEND_URL} (Moorsyl Verify, its sender and template)`);
  } else {
    console.log(`auth-guard  gateway ${SMS_URL} as "${SMS_SENDER}"`);
  }
  // Printed in full, on purpose. These numbers can be signed into by anyone who
  // knows the fixed code, and that should be impossible to forget about.
  if (SMS_BYPASS.size) {
    console.warn(`auth-guard  ${SMS_BYPASS.size} number(s) EXEMPT from SMS, ` +
      `code "${SMS_BYPASS_CODE}" accepted for: ${[...SMS_BYPASS].join(', ')}`);
  }
});
