'use strict';
// What a signed-in driver may do, decided on the way through (phase 5 split,
// 2026-10-06 -- moved out of server.js unchanged): the wallet gate (no top-up,
// no work), the rating he gives a passenger reported to the console, and the
// bound on a reply to the office. tests/auth-guard-routes.test.js and
// auth-guard-wallet-gate.test.js hold the answers it must keep.

const strip = (u) => String(u).replace(/\/$/, '');

/**
 * ── The wallet gate, 2026-09-14 ─────────────────────────────────────────────
 * The client's rule: **no top-up, no work.** The wallet holds only what a
 * driver loads through Chargily or Moosyl -- never ride money, Movin takes 0 %
 * on rides -- so a driver without the credit for a day (and no day already
 * paid for) may neither go online nor accept a ride, however much he earned.
 *
 * The app refuses as well, but only here does it hold for an older APK too.
 * Asked of maps-shim with the driver's OWN token, so it is his wallet and
 * nobody else's, and `canWork` is the same expression dispatch uses.
 *
 * Fails OPEN: a wallet we cannot read is our failure, and it must never be
 * what grounds a driver who has paid. Dispatch still skips him if he is unpaid.
 */
const WALLET_URL = strip(process.env.WALLET_URL || 'http://127.0.0.1:8030');

/**
 * ── Driver → passenger ratings, 2026-09-27 ──────────────────────────────────
 * `POST /ui/driver/ride/{rideId}/rateCustomer` adds the stars to a running
 * total on the passenger's `rider_details` and keeps no row: which driver,
 * which ride and how many stars are gone once it returns. The owner wanted the
 * console's Notes to show them, so they are caught here, on the way through.
 *
 * After the driver backend answers 2xx, admin-api is told the ride and the
 * stars on loopback (it reads who drove and who rode from the ride itself).
 * Fire and forget: the driver's answer is never held for it, and admin-api
 * being down costs a console row, never the rating.
 */
const RATINGS_URL = strip(process.env.RATINGS_URL || 'http://127.0.0.1:8040');
const RATE_CUSTOMER = /^\/ui\/driver\/ride\/([^/]+)\/rateCustomer\/?$/;

function noteDriverRating(pathname, body, status) {
  if (status < 200 || status >= 300) return;
  const m = RATE_CUSTOMER.exec(pathname);
  if (!m) return;
  let stars;
  try {
    stars = JSON.parse(body.toString('utf8')).ratingValue;
  } catch {
    return;
  }
  fetch(`${RATINGS_URL}/internal/driver-rating`, {
    method: 'POST',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ rideId: decodeURIComponent(m[1]), stars }),
    signal: AbortSignal.timeout(5000),
  })
    .then((r) => {
      if (r.status !== 204) console.warn(`[guard] driver rating not recorded: ${r.status}`);
    })
    .catch((err) => console.warn(`[guard] driver rating not recorded: ${err.message}`));
}

/**
 * ── A driver's reply to an office message has an end, 2026-09-27 ───────────
 * `PUT /ui/message/{id}/response` stores `reply` in message_report.reply, a
 * `text` column with no length: every other field a person types lands in a
 * varchar(255) that refuses more, and this one took whatever nginx let
 * through (1 MB) and showed it on the console's Messages screen. The app caps
 * the box at 500; this is the line for anything that is not the app.
 */
const REPLY_MAX = 1000;
const MESSAGE_REPLY = /^\/ui\/message\/[^/]+\/response\/?$/;

/** A refusal code when this request's body is out of bounds, else null. */
function outOfBounds(pathname, method, body) {
  if (method !== 'PUT' || !MESSAGE_REPLY.test(pathname)) return null;
  let parsed;
  try {
    parsed = JSON.parse(body.toString('utf8'));
  } catch {
    return 'INVALID_REQUEST';
  }
  const reply = parsed && parsed.reply;
  if (typeof reply !== 'string') return 'INVALID_REQUEST';
  return reply.length > REPLY_MAX ? 'REPLY_TOO_LONG' : null;
}

/** Is this request a driver starting to work: going online, or accepting? */
function isWork(pathname, url, body) {
  if (pathname === '/ui/driver/setActivity') {
    return new URLSearchParams(url.split('?')[1] || '').get('active') === 'true';
  }
  if (pathname === '/ui/driver/searchRequest/quote/respond') {
    try {
      return JSON.parse(body.toString('utf8')).response === 'Accept';
    } catch {
      return false;
    }
  }
  return false;
}

/** true / false from the wallet; null when it could not be asked. */
async function walletAllows(token) {
  if (!token) return null;
  try {
    const r = await fetch(`${WALLET_URL}/wallet/status`, {
      headers: { token: String(token) },
      signal: AbortSignal.timeout(4000),
    });
    if (!r.ok) return null;
    const w = await r.json();
    return typeof w.canWork === 'boolean' ? w.canWork : null;
  } catch {
    return null;
  }
}

module.exports = { WALLET_URL, noteDriverRating, outOfBounds, isWork, walletAllows };
