'use strict';
//
// The lock on the OTP check.
//
// ── The hole ────────────────────────────────────────────────────────────────
// `POST /v2/auth` answers with `attempts: 3`, and the backend does not enforce
// it. Measured on this stack: ~62 ms per guess, 62 consecutive wrong codes, the
// counter never moved, and the same auth session still accepted the right code
// afterwards. Four digits is 10,000 possibilities -- about ten minutes
// single-threaded, far less in parallel.
//
// That is harmless while every port but SSH is shut. It stops being harmless
// the moment the API is published on 443, which is the point of the change this
// ships with. So this guard exists to make that exposure safe, and it goes in
// first.
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
// The edge proxies *all* of /v2/ to this process, not just the auth paths. A
// guard you can route around is not a guard, and one mistyped nginx `location`
// is all it would take.
//
// State is in memory on purpose. An auth session lives ten minutes; there is
// one replica; and an external store would be one more thing to be down. A
// restart clears the counters, which is why nginx also rate-limits by IP -- that
// layer survives a restart of this one. If this ever runs as more than one
// process, the counters must move to Redis, and that is the moment to notice.
//
// No dependencies: Node's built-in http plus global fetch.

const http = require('http');

const PORT       = Number(process.env.PORT || 8031);
const UPSTREAM   = (process.env.UPSTREAM_URL || 'http://127.0.0.1:8013').replace(/\/$/, '');

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

const UPSTREAM_TIMEOUT_MS = 20000;

/* ────────────────────────────────────────────────────────────────────────────
   Session bookkeeping
   ──────────────────────────────────────────────────────────────────────────── */

/** authId -> { born, attempts, lockedUntil } */
const sessions = new Map();

/** phone number -> [timestamps of sign-ins started] */
const starts = new Map();

// Sweep, so a long-running process does not accumulate dead sessions. Cheap:
// these maps hold one entry per sign-in attempt in the last few minutes.
setInterval(() => {
  const now = Date.now();
  for (const [id, s] of sessions) {
    if (now - s.born > AUTH_TTL_MS && now > (s.lockedUntil || 0)) sessions.delete(id);
  }
  for (const [number, times] of starts) {
    const live = times.filter((t) => now - t < START_WINDOW_MS);
    if (live.length) starts.set(number, live);
    else starts.delete(number);
  }
}, 60_000).unref();

/** Records a sign-in start and says whether this number has had too many. */
function tooManyStarts(number) {
  const now = Date.now();
  const times = (starts.get(number) || []).filter((t) => now - t < START_WINDOW_MS);
  times.push(now);
  starts.set(number, times);
  return times.length > MAX_STARTS;
}

/* ────────────────────────────────────────────────────────────────────────────
   Talking to rider-app
   ──────────────────────────────────────────────────────────────────────────── */

function readBody(req) {
  return new Promise((resolve, reject) => {
    const chunks = [];
    let size = 0;
    req.on('data', (c) => {
      size += c.length;
      // Nothing this API accepts is large; refusing early keeps a hostile body
      // from becoming a memory problem.
      if (size > 1_000_000) { reject(new Error('body too large')); req.destroy(); return; }
      chunks.push(c);
    });
    req.on('end', () => resolve(Buffer.concat(chunks)));
    req.on('error', reject);
  });
}

async function forward(req, body) {
  const headers = {};
  for (const [k, v] of Object.entries(req.headers)) {
    // Hop-by-hop headers, and the ones the upstream must set itself.
    if (['host', 'connection', 'content-length', 'transfer-encoding'].includes(k)) continue;
    headers[k] = v;
  }
  const res = await fetch(`${UPSTREAM}${req.url}`, {
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

const VERIFY = /^\/v2\/auth\/([^/?]+)\/verify\b/;
const RESEND = /^\/v2\/auth\/otp\/([^/?]+)\/resend\b/;
const START  = /^\/v2\/auth\b/;

async function handle(req, res) {
  if (req.url === '/healthz') {
    return send(res, 200, {
      ok: true, upstream: UPSTREAM,
      sessions: sessions.size, numbers: starts.size,
    });
  }

  let body;
  try {
    body = await readBody(req);
  } catch {
    return send(res, 413, refusal('REQUEST_TOO_LARGE'));
  }

  const verify = VERIFY.exec(req.url);

  // ── starting a sign-in ────────────────────────────────────────────────────
  // Checked before forwarding, so a throttled number never reaches the backend
  // and -- once there is a gateway -- never costs us an SMS.
  if (!verify && req.method === 'POST' && START.test(req.url) && !RESEND.test(req.url)) {
    let number = null;
    try {
      const parsed = JSON.parse(body.toString('utf8'));
      number = `${parsed.mobileCountryCode || ''}${parsed.mobileNumber || ''}`;
    } catch { /* malformed: let the backend give its own 400 */ }

    if (number && tooManyStarts(number)) {
      console.warn(`[guard] throttled sign-ins for ${number}`);
      return send(res, 429, refusal('TOO_MANY_REQUESTS'),
        { 'retry-after': String(Math.ceil(START_WINDOW_MS / 1000)) });
    }
  }

  // ── the guarded path ──────────────────────────────────────────────────────
  if (verify && req.method === 'POST') {
    const id = decodeURIComponent(verify[1]);
    const now = Date.now();
    // An id this process has not seen -- it restarted, or the session began
    // before the guard did. Start counting from now rather than waving it
    // through: we lose the age, we do not lose the attempt limit.
    const s = sessions.get(id) || { born: now, attempts: 0, lockedUntil: 0 };
    sessions.set(id, s);

    if (now < s.lockedUntil) {
      const after = Math.ceil((s.lockedUntil - now) / 1000);
      console.warn(`[guard] locked ${id} (${after}s left)`);
      return send(res, 429, refusal('TOO_MANY_ATTEMPTS'), { 'retry-after': String(after) });
    }

    if (now - s.born > AUTH_TTL_MS) {
      console.warn(`[guard] expired ${id}`);
      sessions.delete(id);
      return send(res, 400, refusal('INVALID_AUTH_DATA'));
    }

    let up;
    try {
      up = await forward(req, body);
    } catch (err) {
      console.error(`[guard] upstream: ${err.message}`);
      return send(res, 502, refusal('UPSTREAM_UNAVAILABLE'));
    }

    if (up.status === 200) {
      // Spent. Nothing more can be tried against it.
      sessions.delete(id);
      console.log(`[guard] verified ${id}`);
    } else {
      s.attempts += 1;
      if (s.attempts >= MAX_ATTEMPTS) {
        s.lockedUntil = now + LOCK_MS;
        console.warn(`[guard] LOCKED ${id} after ${s.attempts} wrong codes`);
        // Answer the attempt that crossed the line as a lockout, not as a
        // wrong code -- otherwise the rider is told to try again by a screen
        // that will refuse them.
        const after = Math.ceil(LOCK_MS / 1000);
        return send(res, 429, refusal('TOO_MANY_ATTEMPTS'), { 'retry-after': String(after) });
      }
      console.log(`[guard] wrong code ${id} (${s.attempts}/${MAX_ATTEMPTS})`);
    }

    res.writeHead(up.status, { 'content-type': up.type || 'application/json' });
    return res.end(up.text);
  }

  // ── everything else: forwarded, some of it remembered ─────────────────────
  let up;
  try {
    up = await forward(req, body);
  } catch (err) {
    console.error(`[guard] upstream ${req.url}: ${err.message}`);
    return send(res, 502, refusal('UPSTREAM_UNAVAILABLE'));
  }

  if (up.status === 200 && req.method === 'POST') {
    const resend = RESEND.exec(req.url);
    if (resend) {
      // A new code was sent, so the old count no longer describes anything.
      const id = decodeURIComponent(resend[1]);
      sessions.set(id, { born: Date.now(), attempts: 0, lockedUntil: 0 });
    } else if (START.test(req.url) && !VERIFY.test(req.url)) {
      try {
        const { authId } = JSON.parse(up.text);
        // Recording the birth is what makes expiry possible at all -- the
        // backend never expires an auth id, so without this an abandoned
        // sign-in stays guessable indefinitely.
        if (authId) sessions.set(authId, { born: Date.now(), attempts: 0, lockedUntil: 0 });
      } catch { /* not JSON we recognise; nothing to remember */ }
    }
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
  console.log(
    `auth-guard on :${PORT} -> ${UPSTREAM}  ` +
    `(${MAX_ATTEMPTS} attempts, session ${AUTH_TTL_MS / 60000} min, lock ${LOCK_MS / 60000} min, ` +
    `${MAX_STARTS} sign-ins per number per ${START_WINDOW_MS / 60000} min)`,
  );
});
