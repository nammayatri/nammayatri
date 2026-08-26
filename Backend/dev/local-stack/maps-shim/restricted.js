'use strict';
/**
 * Who dispatch should skip, published where the driver binary can read it.
 *
 * ── The rule, from the client on 2026-08-26 ────────────────────────────────
 * A driver who has not paid **stays online**, but a request only reaches him
 * when no paying driver is in the pool. Plus a cap on rides per paid period:
 * past it, he is treated the same way.
 *
 * ── Why the policy lives here and not in the binary ────────────────────────
 * The Haskell patch reads one Redis key holding a JSON array of driver ids and
 * prefers everybody else. That is the whole of its knowledge -- it never learns
 * what a subscription is, what 3 000 DA is, or what 300 rides is. All of that
 * is decided in this file, which can change in the time it takes to restart a
 * container. Putting the numbers in the binary would mean a 45-minute build
 * and a new set of binaries every time the client changed his mind about one.
 *
 * ── The key, measured rather than assumed ──────────────────────────────────
 * Hedis prefixes keys with the app name. The binary calls `Redis.get
 * "movin:restricted"` without `withCrossAppRedis`, so the real key is
 * `dynamic-offer-driver-app:movin:restricted`. Confirmed against the live
 * Redis, which carries `dynamic-offer-driver-app:DriverLocation:...` for plain
 * calls and `driver-offer:DriverPool:...` for cross-app ones. Get this wrong
 * and nothing fails: the binary reads a missing key, restricts nobody, and the
 * whole feature is silently off.
 *
 * ── Failure means nobody is restricted, always ─────────────────────────────
 * A query that throws, a Redis that refuses, a shim that has never run: every
 * one of them leaves the key absent or stale-but-valid, and dispatch behaves
 * as it does today. The failure worth designing against is the other one --
 * restricting a driver who has paid -- and no path here produces it.
 */

const net = require('net');

/** 0 disables the cap entirely. */
const RIDE_CAP = Number(process.env.SUBSCRIPTION_RIDE_CAP || 300);
const EVERY_MS = Number(process.env.RESTRICTED_REFRESH_MS || 5 * 60 * 1000);
const REDIS_HOST = process.env.REDIS_HOST || 'localhost';
const REDIS_PORT = Number(process.env.REDIS_PORT || 6379);
/** The key the binary reads, prefix included. See the header. */
const KEY = 'dynamic-offer-driver-app:movin:restricted';

/**
 * Who owes us something.
 *
 * ── The two halves ─────────────────────────────────────────────────────────
 * `paid_until IS NULL` is a driver who has never paid, and `<= now()` is one
 * whose month ran out. Both are restricted, even though the app words them
 * very differently -- to dispatch they are the same driver.
 *
 * ── What "the period" means for the ride cap ───────────────────────────────
 * The month he is currently inside, which is the payment whose window contains
 * today. Not the last 30 days: paying early stacks, so a driver can be 45 days
 * paid up, and counting a rolling window would charge him rides against a month
 * he has not started. Falls back to when the subscription row was created,
 * which is the free month given to the fleet already on the road -- it has no
 * payment behind it by design, and would otherwise get an unlimited cap.
 */
const SQL = `
  WITH period AS (
    SELECT s.driver_id,
           s.paid_until,
           coalesce(
             (SELECT max(sp.covers_from)
                FROM movin.subscription_payment sp
               WHERE sp.driver_id = s.driver_id
                 AND sp.applied_at IS NOT NULL
                 AND sp.covers_until > now()),
             s.created_at
           ) AS started
      FROM movin.subscription s
  )
  SELECT p.id
    FROM atlas_driver_offer_bpp.person p
    LEFT JOIN period pd ON pd.driver_id = p.id
   WHERE p.role = 'DRIVER'
     AND (
       pd.paid_until IS NULL
       OR pd.paid_until <= now()
       OR ($1 > 0 AND (
            SELECT count(*)
              FROM atlas_driver_offer_bpp.ride r
             WHERE r.driver_id = p.id
               AND r.status = 'COMPLETED'
               AND r.created_at >= pd.started
          ) >= $1)
     )`;

async function compute(pool) {
  const q = await pool.query(SQL, [RIDE_CAP]);
  return q.rows.map((r) => r.id);
}

/**
 * SET the key, speaking RESP directly.
 *
 * No Redis client library: adding one means rebuilding the shim image for a
 * single SET, and the protocol for one command is ten lines. Written as an
 * array of bulk strings, which is the only form redis-cli itself uses.
 */
function publish(ids) {
  return new Promise((resolve) => {
    const value = JSON.stringify(ids);
    const parts = ['SET', KEY, value];
    const wire =
      `*${parts.length}\r\n` +
      parts.map((p) => `$${Buffer.byteLength(p)}\r\n${p}\r\n`).join('');

    const sock = net.createConnection({ host: REDIS_HOST, port: REDIS_PORT });
    let done = false;
    const finish = (ok) => {
      if (done) return;
      done = true;
      sock.destroy();
      resolve(ok);
    };
    sock.setTimeout(5000, () => finish(false));
    sock.on('error', (e) => {
      console.error('[restricted] redis:', e.message);
      finish(false);
    });
    sock.on('connect', () => sock.write(wire));
    sock.on('data', (buf) => finish(buf.toString('utf8').startsWith('+OK')));
  });
}

/**
 * Recompute and publish. Safe to call at any time and from anywhere.
 *
 * Called on a timer, and again the instant a webhook applies a payment -- a
 * driver who has just paid must not wait five minutes to start receiving work
 * again. That immediacy is the whole reason this is exported rather than
 * purely periodic.
 */
async function refresh(pool, why = 'timer') {
  if (!pool) return null;
  let ids;
  try {
    ids = await compute(pool);
  } catch (e) {
    // Leaves the previous value in place, which is the safe direction: an
    // out-of-date list restricts the wrong driver for minutes, a wrong list
    // restricts him wrongly for as long as nobody notices.
    console.error('[restricted] query failed, keeping the last published list:', e.message);
    return null;
  }
  const ok = await publish(ids);
  console.log(`[restricted] ${ids.length} driver(s) restricted (${why})${ok ? '' : ' -- NOT published'}`);
  return ids;
}

/** Publish now, then keep it current. */
function start(pool) {
  if (!pool) {
    console.error('[restricted] no database; dispatch will restrict nobody');
    return;
  }
  void refresh(pool, 'startup');
  const timer = setInterval(() => void refresh(pool, 'timer'), EVERY_MS);
  // Never hold the process open for this alone.
  if (typeof timer.unref === 'function') timer.unref();
}

module.exports = { start, refresh, RIDE_CAP, KEY };
