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
// accepted. There is no SMS gateway yet, so that setting cannot simply be
// turned off -- without it no code is delivered at all and nobody signs in.
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
// That is not a workaround waiting to be replaced -- it is the same shape the
// real thing will have. When an SMS gateway exists, the guard generates a
// random code, sends it, and substitutes exactly as it does now; only the
// source of the code changes.
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
   Routes

   The prefix decides the upstream and whether personal codes apply. Order
   matters only in that the first match wins; the two prefixes are disjoint.
   ──────────────────────────────────────────────────────────────────────────── */

const ROUTES = [
  {
    name: 'rider',
    prefix: '/v2/',
    upstream: RIDER_URL,
    // No personal codes: the client and his testers sign in with the fixed code
    // every day, and switching them to per-number codes mid-pilot would lock
    // them out of their own app. The mechanism below is prefix-agnostic, so the
    // day the rider side gets the same treatment it is one line here.
    codesFile: null,
    fixedOtp: null,
  },
  {
    name: 'driver',
    prefix: '/ui/',
    upstream: DRIVER_URL,
    codesFile: process.env.DRIVER_CODES || '/app/driver-codes.json',
    // What the backend accepts, and what the guard substitutes once it has
    // checked the driver's own code. Kept out of the log on purpose.
    fixedOtp: process.env.DRIVER_FIXED_OTP || '7891',
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

async function handle(req, res) {
  const pathname = req.url.split('?')[0];

  if (pathname === '/healthz') {
    return send(res, 200, {
      ok: true,
      routes: ROUTES.map((r) => ({
        prefix: r.prefix,
        upstream: r.upstream,
        personalCodes: r.codesFile ? Object.keys(loadCodes(r.codesFile)).length : null,
      })),
      sessions: sessions.size,
      numbers: starts.size,
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
    try {
      const parsed = JSON.parse(body.toString('utf8'));
      number = `${parsed.mobileCountryCode || ''}${parsed.mobileNumber || ''}`;
    } catch { /* malformed: let the backend give its own 400 */ }

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

    if (up.status === 200) {
      try {
        const { authId } = JSON.parse(up.text);
        // Recording the birth is what makes expiry possible at all -- the
        // backend never expires an auth id, so without this an abandoned
        // sign-in stays guessable indefinitely. Recording the number is what
        // makes the personal code checkable: the verify request carries only
        // the authId, so this is the guard's only chance to learn who it is for.
        if (authId) {
          sessions.set(key(authId),
            { born: Date.now(), attempts: 0, resends: 0, lockedUntil: 0, number });
        }
      } catch { /* not JSON we recognise; nothing to remember */ }
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
    if (!known && codes) {
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

    // ── the personal code ──────────────────────────────────────────────────
    if (codes) {
      let given = null;
      let parsed = null;
      try {
        parsed = JSON.parse(body.toString('utf8'));
        given = parsed.otp == null ? null : String(parsed.otp);
      } catch { /* handled below */ }

      if (given === null) return send(res, 400, refusal('INVALID_REQUEST'));

      if (!codeMatches(codes[s.number], s.number, given)) {
        // Never forwarded. This is what actually retires the fixed code: a
        // caller who submits 7891 without knowing the driver's own code spends
        // an attempt here and the backend never hears about it.
        return countWrong();
      }

      // Right code. Swap in what the backend is configured to accept. The
      // driver's own code has now done its work and goes no further than this
      // process.
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
    console.log(`auth-guard  ${r.prefix} -> ${r.upstream}  ` +
      (n === null ? '(fixed code, as the backend has it)' : `(${n} personal codes)`));
  }
  console.log(
    `auth-guard on :${PORT}  ` +
    `${MAX_ATTEMPTS} attempts, session ${AUTH_TTL_MS / 60000} min, lock ${LOCK_MS / 60000} min, ` +
    `${MAX_STARTS} sign-ins per number per ${START_WINDOW_MS / 60000} min, ` +
    `${MAX_RESENDS} resends, body ${Math.round(MAX_BODY / 1024)} kB`,
  );
});
