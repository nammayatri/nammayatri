'use strict';
/**
 * Who dispatch should skip, published where the driver binary can read it.
 *
 * ── The rule, from the client on 2026-09-14 ────────────────────────────────
 * **No top-up, no work.** The wallet holds only what a driver loads through
 * Chargily or Moosyl -- never ride money, Movin takes 0 % on rides -- and a
 * driver on this list is never offered a job (`movinOnlyPaying`), is refused
 * going online and accepting by the auth guard, and is taken offline by the
 * app. It replaced the 2026-08-26 rule, under which he still got a request
 * when no paying driver was in the pool. Plus a cap on rides per paid day:
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
/** What one day costs, and therefore the credit needed to start one. Read from
    the same environment variable `wallet.js` uses, so the gate and the charge
    can never disagree about the price. */
const DAY_PRICE = Number(process.env.WALLET_DAY_PRICE || 30);
/** Two countries since 2026-09-13, each with its own day: 30 MRU in Mauritania,
    100 DA in Algeria. A driver's country is his merchant. Both read from the
    same variables wallet.js uses, for the same reason as above. */
const DAY_PRICE_DZ = Number(process.env.WALLET_DAY_PRICE_DZ || 100);
const DZ_MERCHANT = 'algeria0-0000-0000-0000-00000algeria';
const EVERY_MS = Number(process.env.RESTRICTED_REFRESH_MS || 5 * 60 * 1000);
const REDIS_HOST = process.env.REDIS_HOST || 'localhost';
const REDIS_PORT = Number(process.env.REDIS_PORT || 6379);
/** The key the binary reads, prefix included. See the header. */
const KEY = 'dynamic-offer-driver-app:movin:restricted';
/**
 * The same list under the name the HARD gate reads (binary built 2026-09-14,
 * `movinOnlyPaying`: an unpaid driver is never offered a job). Both are written
 * so the old binary and the new one each find their key during the swap, and a
 * rollback needs nothing here.
 */
const KEY_UNPAID = 'dynamic-offer-driver-app:movin:unpaid';

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
/**
 * ── The wallet rule, since 2026-09-06 ──────────────────────────────────────
 * The monthly subscription is gone. A driver owes us nothing until he drives:
 * 30 MRU comes off at his first ride of a day and covers the next 24 hours.
 *
 * **The rule is NOT "has an active day", and getting that wrong would be
 * invisible.** The day only begins at the first ride, so a driver who has just
 * topped up has no day yet — gating on one would deprioritise him out of ever
 * getting the ride that would start it. He would watch a full wallet do
 * nothing, and every number on his screen would look right.
 *
 * So: restricted when he has neither an active day nor the credit to open one.
 * `wallet.balance` is a cache of the ledger, and it is the correct thing to
 * read here — the ledger is the audit trail, not the hot path.
 *
 * A driver with no wallet row at all has never topped up, which is the same
 * position as an empty one. The LEFT JOIN's NULLs are handled by `coalesce`
 * rather than by a second arm, so there is one expression to get right.
 *
 * The ride cap survives unchanged. It counts completed rides inside the day he
 * is currently paying for, falling back to the day's start.
 */
const SQL = `
  SELECT p.id
    FROM atlas_driver_offer_bpp.person p
    LEFT JOIN movin.wallet w ON w.driver_id = p.id
   WHERE p.role = 'DRIVER'
     AND (
       (
         coalesce(w.day_until, to_timestamp(0)) <= now()
         -- The price of a day in HIS country. One price for everyone would let
         -- an Algerian driver holding 40 DA count as able to work, because 40
         -- is more than Mauritania's 30.
         -- Typed explicitly. Without the casts Postgres resolves the CASE's
         -- two parameters as text and refuses "integer < text" -- measured on
         -- the first deploy, 2026-09-13, where it kept the old list silently.
         -- (No backticks anywhere in this SQL: it lives inside a JS template
         -- string, and one backtick ends it -- which crash-looped the shim.)
         AND coalesce(w.balance, 0) < CASE WHEN p.merchant_id = $3::text THEN $4::int ELSE $2::int END
       )
       OR ($1 > 0 AND w.day_until > now() AND (
            SELECT count(*)
              FROM atlas_driver_offer_bpp.ride r
             WHERE r.driver_id = p.id
               AND r.status = 'COMPLETED'
               AND r.created_at >= w.day_until - interval '24 hours'
          ) >= $1)
     )`;

async function compute(pool) {
  const q = await pool.query(SQL, [RIDE_CAP, DAY_PRICE, DZ_MERCHANT, DAY_PRICE_DZ]);
  return q.rows.map((r) => r.id);
}

/**
 * SET the key, speaking RESP directly.
 *
 * No Redis client library: adding one means rebuilding the shim image for a
 * single SET, and the protocol for one command is ten lines. Written as an
 * array of bulk strings, which is the only form redis-cli itself uses.
 */
function publish(key, ids) {
  return new Promise((resolve) => {
    const value = JSON.stringify(ids);
    const parts = ['SET', key, value];
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
  const old = await publish(KEY, ids);
  const hard = await publish(KEY_UNPAID, ids);
  const ok = old && hard;
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

/**
 * One driver's rides inside the month he is currently paying for.
 *
 * ── Why the app needs this and not just the restriction ────────────────────
 * A driver over his cap is restricted exactly like a lapsed one, and to
 * dispatch they are the same driver. **To him they are opposites.** The lapsed
 * driver owes money and paying fixes it; the capped driver has paid, is fully
 * up to date, and paying again fixes nothing -- his next payment buys the
 * *next* month, and the cap counts against the one he is in.
 *
 * Without this, his rides thin out while the screen says "actif, 12 jours
 * restants" and offers him a Payer button that would take 3 000 DA and change
 * nothing. That is the exact failure the whole subscription was designed
 * around: rides that quietly stop, and a driver who rings the office.
 *
 * The same period definition as the restriction list, deliberately in one
 * place: if the number he reads and the number that restricts him were
 * computed differently, the screen would eventually contradict the dispatch.
 */
async function ridesInPeriod(pool, driverId) {
  const q = await pool.query(
    `WITH p AS (
       SELECT coalesce(
                (SELECT max(sp.covers_from)
                   FROM movin.subscription_payment sp
                  WHERE sp.driver_id = s.driver_id
                    AND sp.applied_at IS NOT NULL
                    AND sp.covers_until > now()),
                s.created_at) AS started,
              s.paid_until
         FROM movin.subscription s
        WHERE s.driver_id = $1)
     SELECT p.started,
            p.paid_until,
            (SELECT count(*) FROM atlas_driver_offer_bpp.ride r
              WHERE r.driver_id = $1 AND r.status = 'COMPLETED'
                AND r.created_at >= p.started) AS used
       FROM p`,
    [String(driverId)],
  );
  const row = q.rows[0];
  if (!row) return { used: 0, cap: RIDE_CAP, since: null, resetsAt: null, capped: false };
  const used = Number(row.used || 0);
  return {
    used,
    cap: RIDE_CAP,
    since: row.started,
    /* When the count goes back to zero: the end of the month he is inside.
       Named rather than derived in the app, because "when does this lift" is
       the only question a capped driver has. */
    resetsAt: row.paid_until,
    capped: RIDE_CAP > 0 && used >= RIDE_CAP,
  };
}

module.exports = { start, refresh, ridesInPeriod, RIDE_CAP, KEY, KEY_UNPAID };
