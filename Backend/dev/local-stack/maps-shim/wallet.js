'use strict';
/**
 * The driver's wallet: 30 MRU a day, taken at his first ride.
 *
 * ── The model, from the client on 2026-09-06 ───────────────────────────────
 * Replaces the monthly subscription entirely. A driver loads credit -- never
 * less than 30 MRU, as much above as he likes -- and **nothing is taken until
 * he works**. At his first ride of a day 30 MRU comes off and he is covered for
 * 24 hours; every ride inside that window is free.
 *
 * That removes more than it adds. Nobody is ever charged without driving, so
 * the whole "pay-then-extend, never auto-debit" apparatus that Algerian cards
 * forced on `subscription.js` has nothing left to guard. Moosyl does have a
 * full subscriptions API with automatic billing, and needing none of it is the
 * safer half.
 *
 * Two rules the client confirmed, because both are about someone's money:
 *
 *   1. The 30 comes off when a ride **starts**, not when it is accepted. A
 *      driver who accepted a ride the passenger then cancelled drove nothing.
 *   2. A driver who starts a ride under 30 **goes negative** rather than being
 *      cut off. The dispatch restriction is soft by design -- an unpaid driver
 *      still gets requests when no paying driver is free -- so this case is
 *      reachable, and the answer cannot be "the app stops working while there
 *      is a passenger in his car".
 *
 * ── The gate is not what it looks like ─────────────────────────────────────
 * Dispatch deprioritises drivers who owe us. The rule is **not** "has an active
 * day": the day only begins at the first ride, so gating on it would stop a
 * driver who has just topped up from ever getting the ride that starts it. It
 * is `day_until > now() OR balance >= PRICE`. See `restricted.js`.
 *
 * ── The Moosyl contract, measured rather than read ─────────────────────────
 * Their published OpenAPI is wrong about the two things this file needs, so
 * both were established by calling the API on 2026-09-06:
 *
 *   * `POST /checkout-session` returns **`checkoutUrl` at the top level**,
 *     outside `data`. The schema only describes `data`, so it looks absent.
 *   * **The status lives on the checkout session, not the payment request.**
 *     Nothing in the payment-request family carries one -- `refresh-status`
 *     included -- and the session's is `open | completed | expired | cancelled`.
 *
 * Auth is `Authorization: <raw key>`. `Bearer` is refused, and a wrong key
 * answers `404 Invalid API key` rather than 401.
 *
 * ── Why a webhook can never grant credit ───────────────────────────────────
 * Moosyl documents no webhook signature scheme anywhere. So the webhook here is
 * only a *hint to go and look*: it reads the session status back from Moosyl
 * with our own key and credits from that. An unsigned POST from anyone on the
 * internet therefore cannot put money in a wallet, which is the property that
 * matters and is stronger than verifying a signature we would have had to guess.
 */

const restricted = require('./restricted');

/** Whole MRU. Moosyl quotes integers and the price is 30, so there is no unit
    below this to represent -- a second scale would only be somewhere for the
    two to disagree. */
const PRICE     = Number(process.env.WALLET_DAY_PRICE || 30);
/** The floor on a top-up. The client's number, and it is also PRICE: loading
    less than one day buys nothing and would only produce a confused driver. */
const MIN_TOPUP = Number(process.env.WALLET_MIN_TOPUP || 30);
/** How long one charge covers. 24h from the ride's own start. */
const DAY_HOURS = Number(process.env.WALLET_DAY_HOURS || 24);
const CURRENCY  = 'MRU';

const SECRET     = process.env.MOOSYL_SECRET_KEY || '';
const MOOSYL     = (process.env.MOOSYL_BASE || 'https://api.moosyl.com').replace(/\/$/, '');
/** Ours -- where Moosyl sends the driver back to. */
const PUBLIC_URL = (process.env.PUBLIC_URL || '').replace(/\/$/, '');
const DRIVER_URL = (process.env.DRIVER_URL || 'http://localhost:8016').replace(/\/$/, '');
/** A checkout the driver never finishes should not sit open all day. */
const EXPIRES_MIN = Number(process.env.WALLET_CHECKOUT_MINUTES || 30);

/**
 * ── Two countries since 2026-09-13 ─────────────────────────────────────────
 * Same model in both — credit loaded, one day taken at the first ride — with
 * each country's own price, currency and gateway:
 *
 *   Mauritania   30 MRU a day    Moosyl     (Bankily, Masrivi, Sedad…)
 *   Algeria     100 DA a day     Chargily   (Edahabia, CIB)
 *
 * A driver's country is his MERCHANT, read from the database for every call —
 * never taken from the phone. The constants above stay the Mauritanian row so
 * the one-country environment variables keep meaning what they meant.
 *
 * Chargily asks for the card type when the checkout is created (Moosyl lets the
 * driver pick on its own page), so an Algerian top-up carries `method`.
 * Credit is still only ever written after reading the checkout's status back
 * from the gateway with our own key — for Chargily as for Moosyl — so the
 * webhook stays a hint and an unsigned POST can never put money in a wallet.
 */
const DZ_MERCHANT = 'algeria0-0000-0000-0000-00000algeria';
const COUNTRIES = {
  MR: { price: PRICE, minTopup: MIN_TOPUP, currency: CURRENCY, gateway: 'moosyl' },
  DZ: {
    price: Number(process.env.WALLET_DAY_PRICE_DZ || 100),
    minTopup: Number(process.env.WALLET_MIN_TOPUP_DZ || 100),
    currency: 'DZD',
    gateway: 'chargily',
  },
};
const CHARGILY_SECRET = process.env.CHARGILY_SECRET_KEY || '';
const CHARGILY = (process.env.CHARGILY_BASE || 'https://pay.chargily.net/test/api/v2').replace(/\/$/, '');
const MAX_BODY = 64 * 1024;

function send(res, status, body) {
  const text = JSON.stringify(body);
  res.writeHead(status, {
    'content-type': 'application/json',
    'content-length': Buffer.byteLength(text),
    // Money. Never cached, anywhere, by anything.
    'cache-control': 'no-store',
  });
  res.end(text);
}

/**
 * Who is asking, proven against the driver backend.
 *
 * The id comes from that response and is never taken from the caller, so no
 * route here can be pointed at another driver's wallet. Same rule the
 * subscription had, and the reason it had it.
 */
async function driverFromToken(token) {
  if (!token) return null;
  try {
    const r = await fetch(`${DRIVER_URL}/ui/driver/profile`, { headers: { token } });
    if (!r.ok) return null;
    const body = await r.json();
    const id = typeof body.id === 'string' ? body.id.trim() : '';
    if (!id) return null;
    return { id };
  } catch (e) {
    console.error('[wallet] driver lookup failed:', e.message);
    return null;
  }
}

/* ── Moosyl ─────────────────────────────────────────────────────────────── */

async function moosyl(path, { method = 'GET', body, auth = true } = {}) {
  const headers = {};
  if (auth) headers.Authorization = SECRET;
  if (body !== undefined) headers['content-type'] = 'application/json';
  const r = await fetch(`${MOOSYL}${path}`, {
    method,
    headers,
    body: body === undefined ? undefined : JSON.stringify(body),
    signal: AbortSignal.timeout(20000),
  });
  const text = await r.text();
  let json = null;
  try { json = JSON.parse(text); } catch { /* keep the text for the log */ }
  return { ok: r.ok, status: r.status, json, text };
}

/**
 * Read a checkout session's state straight from Moosyl.
 *
 * Public on their side -- no key needed -- but this sends one anyway: the route
 * is public so their hosted page can poll it, and there is no reason for us to
 * be the anonymous caller.
 */
async function sessionStatus(sessionId) {
  const r = await moosyl(`/checkout-session/public/${encodeURIComponent(sessionId)}`);
  if (!r.ok || !r.json) return null;
  const s = r.json.data && r.json.data.status;
  return typeof s === 'string' ? s : null;
}

/* ── The wallet itself ──────────────────────────────────────────────────── */

/** Read, creating the row on first sight so every later query can assume it. */
async function walletOf(pool, driverId) {
  await pool.query(
    `INSERT INTO movin.wallet (driver_id) VALUES ($1) ON CONFLICT (driver_id) DO NOTHING`,
    [driverId],
  );
  const { rows } = await pool.query(
    `SELECT balance, day_until FROM movin.wallet WHERE driver_id = $1`,
    [driverId],
  );
  return rows[0] || { balance: 0, day_until: null };
}

/** His country's row, from his merchant. Anything not Algerian is Mauritanian. */
async function countryOf(pool, driverId) {
  const { rows } = await pool.query(
    `SELECT merchant_id FROM atlas_driver_offer_bpp.person WHERE id = $1`,
    [driverId],
  );
  const merchant = rows[0] ? String(rows[0].merchant_id).trim() : '';
  return merchant === DZ_MERCHANT ? COUNTRIES.DZ : COUNTRIES.MR;
}

/** Whether this country's gateway can be used at all. */
const configuredFor = (cfg) =>
  Boolean(PUBLIC_URL && (cfg.gateway === 'chargily' ? CHARGILY_SECRET : SECRET));

/* ── Chargily ───────────────────────────────────────────────────────────── */

async function chargily(path, { method = 'GET', body } = {}) {
  const headers = { authorization: `Bearer ${CHARGILY_SECRET}` };
  if (body !== undefined) headers['content-type'] = 'application/json';
  const r = await fetch(`${CHARGILY}${path}`, {
    method,
    headers,
    body: body === undefined ? undefined : JSON.stringify(body),
    signal: AbortSignal.timeout(20000),
  });
  const text = await r.text();
  let json = null;
  try { json = JSON.parse(text); } catch { /* keep the text for the log */ }
  return { ok: r.ok, status: r.status, json, text };
}

/** A Chargily checkout's state, in Moosyl's words: `paid` is `completed`. */
async function chargilyStatus(checkoutId) {
  const r = await chargily(`/checkouts/${encodeURIComponent(checkoutId)}`);
  if (!r.ok || !r.json) return null;
  const s = r.json.status;
  if (s === 'paid') return 'completed';
  return typeof s === 'string' ? s : null;
}

/**
 * What the driver sees.
 *
 * `canWork` is the same expression dispatch uses, computed in one place so the
 * screen and the pool cannot tell him different things. A driver told he is
 * fine while dispatch skips him is the failure the whole feature exists to
 * avoid, and it was how the old "vous travaillez normalement" line went wrong.
 */
async function status(pool, token, res) {
  const driver = await driverFromToken(token);
  if (!driver) return send(res, 401, { error: 'unauthorized' });

  try {
    const cfg = await countryOf(pool, driver.id);
    const w = await walletOf(pool, driver.id);
    const dayUntil = w.day_until ? new Date(w.day_until) : null;
    const dayActive = !!dayUntil && dayUntil.getTime() > Date.now();

    send(res, 200, {
      balance: w.balance,
      currency: cfg.currency,
      dayPrice: cfg.price,
      minTopup: cfg.minTopup,
      // Which page he will be sent to. The app shows the Edahabia / CIB choice
      // for Chargily only, and names the right gateway in its hand-off line.
      gateway: cfg.gateway,
      dayUntil: dayActive ? dayUntil.toISOString() : null,
      dayActive,
      /* Deliberately not `balance >= price` alone: an active day is worth as
         much as the credit to buy one, and a driver mid-day with an empty
         wallet is still working. */
      canWork: dayActive || w.balance >= cfg.price,
      configured: configuredFor(cfg),
    });
  } catch (e) {
    console.error('[wallet] status:', e.message);
    // Never a guessed state. A screen that says "lapsed" because Postgres
    // blinked would send a driver who has paid to argue at the office.
    send(res, 503, { error: 'unavailable' });
  }
}

/**
 * Open a top-up.
 *
 * Two calls to Moosyl -- a payment request, then a session over it -- because
 * that is what their API is. The row is written *before* the driver is sent
 * anywhere: a checkout that exists at Moosyl and not here is a payment we
 * cannot credit, which is the one failure that costs a driver money.
 */
async function topup(pool, token, amountRaw, method, res) {
  const driver = await driverFromToken(token);
  if (!driver) return send(res, 401, { error: 'unauthorized' });

  let cfg;
  try {
    cfg = await countryOf(pool, driver.id);
  } catch (e) {
    console.error('[wallet] country lookup:', e.message);
    return send(res, 503, { error: 'unavailable' });
  }
  if (!configuredFor(cfg)) return send(res, 503, { error: 'not_configured' });

  const amount = Math.floor(Number(amountRaw));
  if (!Number.isFinite(amount) || amount < cfg.minTopup) {
    return send(res, 400, { error: 'amount_too_small', minTopup: cfg.minTopup });
  }

  // Ours, and what the gateway echoes back on every read. Prefixed so a support
  // question carrying only this string is recognisable as ours at a glance.
  const transactionId = `movin-${driver.id.slice(0, 8)}-${Date.now()}`;

  try {
    await pool.query(
      `INSERT INTO movin.wallet_topup (transaction_id, driver_id, amount, currency)
       VALUES ($1, $2, $3, $4)`,
      [transactionId, driver.id, amount, cfg.currency],
    );

    if (cfg.gateway === 'chargily') {
      return await chargilyTopup(pool, transactionId, amount, method, cfg, res);
    }

    const pr = await moosyl('/payment-request', {
      method: 'POST',
      body: { transactionId, amount },
    });
    const prId = pr.json && pr.json.data && pr.json.data.id;
    if (!pr.ok || !prId) {
      console.error(`[wallet] payment-request refused: HTTP ${pr.status} ${pr.text.slice(0, 200)}`);
      await pool.query(
        `UPDATE movin.wallet_topup SET status = 'failed', updated_at = now()
          WHERE transaction_id = $1`,
        [transactionId],
      );
      return send(res, 502, { error: 'gateway' });
    }

    const cs = await moosyl('/checkout-session', {
      method: 'POST',
      body: {
        paymentRequestId: prId,
        successUrl: `${PUBLIC_URL}/wallet/done?ok=1`,
        cancelUrl: `${PUBLIC_URL}/wallet/done?ok=0`,
        expiresInMinutes: EXPIRES_MIN,
      },
    });
    // The URL is at the TOP level, outside `data`. Their schema documents only
    // `data`, which is why this looks like it should be `cs.json.data.url`.
    const url = cs.json && cs.json.checkoutUrl;
    const sessionId = cs.json && cs.json.data && cs.json.data.id;
    if (!cs.ok || !url || !sessionId) {
      console.error(`[wallet] checkout-session refused: HTTP ${cs.status} ${cs.text.slice(0, 200)}`);
      await pool.query(
        `UPDATE movin.wallet_topup SET status = 'failed', updated_at = now()
          WHERE transaction_id = $1`,
        [transactionId],
      );
      return send(res, 502, { error: 'gateway' });
    }

    await pool.query(
      `UPDATE movin.wallet_topup
          SET payment_ref = $2, checkout_url = $3, updated_at = now()
        WHERE transaction_id = $1`,
      [transactionId, sessionId, url],
    );

    send(res, 200, { transactionId, url, amount, currency: cfg.currency });
  } catch (e) {
    console.error('[wallet] topup:', e.message);
    send(res, 503, { error: 'unavailable' });
  }
}

/**
 * The Algerian half of `topup`: one Chargily checkout.
 *
 * The same order as Moosyl's — our row exists before the driver is sent
 * anywhere — and the same fee rule the subscription had: we pay Chargily's fee,
 * not the driver. `method` is his card, Edahabia unless he chose CIB.
 */
async function chargilyTopup(pool, transactionId, amount, method, cfg, res) {
  const pay = method === 'cib' ? 'cib' : 'edahabia';
  const cr = await chargily('/checkouts', {
    method: 'POST',
    body: {
      amount,
      currency: 'dzd',
      payment_method: pay,
      locale: 'fr',
      description: `Movin - rechargement ${amount} DA`,
      chargily_pay_fees_allocation: 'merchant',
      success_url: `${PUBLIC_URL}/wallet/done?ok=1`,
      failure_url: `${PUBLIC_URL}/wallet/done?ok=0`,
      // The same route Moosyl calls. Its body only names which top-up to go and
      // re-read (`data.id` is the checkout id, stored as payment_ref below).
      webhook_endpoint: `${PUBLIC_URL}/wallet/webhook`,
      metadata: [{ key: 'transaction_id', value: transactionId }],
    },
  });
  const checkoutId = cr.json && cr.json.id;
  // Chargily hands back an http:// URL that redirects once; ask for https.
  const url = cr.json && cr.json.checkout_url
    ? String(cr.json.checkout_url).replace(/^http:\/\//i, 'https://')
    : null;
  if (!cr.ok || !checkoutId || !url) {
    console.error(`[wallet] chargily refused: HTTP ${cr.status} ${cr.text.slice(0, 200)}`);
    await pool.query(
      `UPDATE movin.wallet_topup SET status = 'failed', updated_at = now()
        WHERE transaction_id = $1`,
      [transactionId],
    );
    return send(res, 502, { error: 'gateway' });
  }

  await pool.query(
    `UPDATE movin.wallet_topup
        SET payment_ref = $2, checkout_url = $3, updated_at = now()
      WHERE transaction_id = $1`,
    [transactionId, checkoutId, url],
  );
  return send(res, 200, { transactionId, url, amount, currency: cfg.currency });
}

/**
 * Credit a paid top-up, exactly once.
 *
 * `credited_at IS NULL` in the UPDATE is the whole guarantee: two callers -- the
 * webhook and the app polling from the success page -- race here constantly by
 * design, and the loser updates zero rows and credits nothing.
 */
async function creditIfPaid(pool, row) {
  // Read back from the gateway that took it. The currency says which: a DZD
  // top-up was a Chargily checkout, anything else a Moosyl session.
  const state = row.currency === 'DZD'
    ? await chargilyStatus(row.payment_ref)
    : await sessionStatus(row.payment_ref);
  if (state !== 'completed') return state;

  const client = await pool.connect();
  try {
    await client.query('BEGIN');
    const claim = await client.query(
      `UPDATE movin.wallet_topup
          SET status = 'paid', paid_at = COALESCE(paid_at, now()),
              credited_at = now(), invoice_no = nextval('movin.invoice_seq'),
              updated_at = now()
        WHERE transaction_id = $1 AND credited_at IS NULL
        RETURNING driver_id, amount`,
      [row.transaction_id],
    );
    if (claim.rowCount === 0) {
      await client.query('ROLLBACK');
      return 'completed';
    }
    const { driver_id: driverId, amount } = claim.rows[0];

    await client.query(
      `INSERT INTO movin.wallet (driver_id, balance) VALUES ($1, $2)
       ON CONFLICT (driver_id) DO UPDATE
         SET balance = movin.wallet.balance + EXCLUDED.balance, updated_at = now()`,
      [driverId, amount],
    );
    await client.query(
      `INSERT INTO movin.wallet_entry (driver_id, kind, amount, topup_id, note)
       VALUES ($1, 'topup', $2, $3, 'Rechargement')`,
      [driverId, amount, row.transaction_id],
    );
    await client.query('COMMIT');
    console.log(`[wallet] credited ${amount} ${row.currency || CURRENCY} to ${driverId.slice(0, 8)}`);
    return 'completed';
  } catch (e) {
    await client.query('ROLLBACK').catch(() => {});
    console.error('[wallet] credit failed:', e.message);
    return null;
  } finally {
    client.release();
  }
}

/** Where the app polls after sending the driver to the page. */
async function topupState(pool, token, transactionId, res) {
  const driver = await driverFromToken(token);
  if (!driver) return send(res, 401, { error: 'unauthorized' });

  try {
    const { rows } = await pool.query(
      `SELECT transaction_id, driver_id, amount, currency, status, payment_ref, credited_at
         FROM movin.wallet_topup WHERE transaction_id = $1`,
      [transactionId],
    );
    const row = rows[0];
    // Not 403. Whether a transaction id exists at all is not a driver's
    // business, and the two answers must look identical from outside.
    if (!row || row.driver_id !== driver.id) return send(res, 404, { error: 'not_found' });

    if (!row.credited_at && row.payment_ref) await creditIfPaid(pool, row);

    const w = await walletOf(pool, driver.id);
    const { rows: after } = await pool.query(
      `SELECT status, credited_at FROM movin.wallet_topup WHERE transaction_id = $1`,
      [transactionId],
    );
    send(res, 200, {
      status: after[0] ? after[0].status : row.status,
      credited: !!(after[0] && after[0].credited_at),
      balance: w.balance,
      currency: row.currency || CURRENCY,
    });
  } catch (e) {
    console.error('[wallet] topupState:', e.message);
    send(res, 503, { error: 'unavailable' });
  }
}

function rawBody(req) {
  return new Promise((resolve, reject) => {
    const chunks = [];
    let size = 0;
    req.on('data', (c) => {
      size += c.length;
      if (size > MAX_BODY) { reject(new Error('too large')); req.destroy(); return; }
      chunks.push(c);
    });
    req.on('end', () => resolve(Buffer.concat(chunks)));
    req.on('error', reject);
  });
}

/**
 * Moosyl telling us something happened.
 *
 * **Nothing here is trusted.** Moosyl documents no signature scheme, so the body
 * is used only to find *which* top-up to look at; the state itself is read back
 * from their API with our key. An unsigned POST from anyone on the internet can
 * therefore make us check a payment and never make us credit one, which is a
 * stronger property than verifying a signature we would have had to guess at.
 *
 * Always 200. A gateway that gets an error retries, and there is nothing here
 * for it to fix.
 */
async function webhook(pool, req, res) {
  let body = {};
  try {
    body = JSON.parse((await rawBody(req)).toString('utf8')) || {};
  } catch {
    return send(res, 200, { ok: true });
  }

  const ref =
    body.transactionId ||
    (body.data && (body.data.transactionId || body.data.id)) ||
    body.id ||
    null;

  try {
    if (ref) {
      const { rows } = await pool.query(
        `SELECT transaction_id, driver_id, amount, currency, status, payment_ref, credited_at
           FROM movin.wallet_topup
          WHERE (transaction_id = $1 OR payment_ref = $1) AND credited_at IS NULL`,
        [String(ref)],
      );
      if (rows[0]) await creditIfPaid(pool, rows[0]);
    }
  } catch (e) {
    console.error('[wallet] webhook:', e.message);
  }
  send(res, 200, { ok: true });
}

/**
 * Take the day off the first ride, for everyone who started one.
 *
 * ── Why this polls instead of being told ───────────────────────────────────
 * The shim is not in the ride flow -- the Haskell binary is -- and the app must
 * never be what triggers a charge. It shares a database with the driver
 * backend, so it can simply look at which rides have started. That needs no
 * rebuild and no app cooperation, and it cannot be skipped by a phone that is
 * switched off.
 *
 * ── Started, not accepted — and the marker is a timestamp, not a status ────
 * The obvious condition is `status = 'INPROGRESS'`, and it is wrong: a ride
 * that starts and finishes between two sweeps is never seen in that state, so
 * every short ride would be free. Invisible until somebody audited the takings.
 *
 * `trip_start_time IS NOT NULL` is the honest marker, and the live data says
 * exactly why. Measured 2026-09-06 over 88 rides:
 *
 *     COMPLETED   58 rides, 58 with a start time
 *     CANCELLED   30 rides,  1 with a start time
 *
 * So the 29 cancelled before pickup carry none and are correctly never charged
 * -- the client's rule is that an accepted-then-cancelled ride was not driven.
 * The one cancelled *after* starting does carry one, and should be charged:
 * that driver drove.
 *
 * ── Why it is safe to run twice ────────────────────────────────────────────
 * `wallet_entry_one_charge_per_ride` is a unique index, so a second pass over
 * the same ride violates it and that insert is skipped. The sweep is
 * at-least-once by nature; the exactly-once lives in the database.
 */
async function chargeStartedRides(pool) {
  if (!pool) return;
  let charged = 0;
  try {
    // Rides that began recently, whose driver has no charge for them yet. The
    // window is generous: it only bounds the scan, and the unique index is what
    // actually stops a double charge.
    const { rows } = await pool.query(
      `SELECT r.id AS ride_id, r.driver_id, r.trip_start_time AS started_at,
              p.merchant_id
         FROM atlas_driver_offer_bpp.ride r
         JOIN atlas_driver_offer_bpp.person p ON p.id = r.driver_id
        WHERE r.trip_start_time IS NOT NULL
          AND r.trip_start_time > now() - interval '2 days'
          AND NOT EXISTS (
            SELECT 1 FROM movin.wallet_entry e WHERE e.ride_id = r.id)
        ORDER BY r.trip_start_time ASC
        LIMIT 500`,
    );

    for (const r of rows) {
      const started = r.started_at ? new Date(r.started_at) : new Date();
      // The day costs what it costs in HIS country: his merchant decides.
      const price = String(r.merchant_id || '').trim() === DZ_MERCHANT
        ? COUNTRIES.DZ.price
        : COUNTRIES.MR.price;
      const client = await pool.connect();
      try {
        await client.query('BEGIN');
        await client.query(
          `INSERT INTO movin.wallet (driver_id) VALUES ($1) ON CONFLICT DO NOTHING`,
          [r.driver_id],
        );
        const { rows: cur } = await client.query(
          `SELECT day_until FROM movin.wallet WHERE driver_id = $1 FOR UPDATE`,
          [r.driver_id],
        );
        const dayUntil = cur[0] && cur[0].day_until ? new Date(cur[0].day_until) : null;

        // Already inside a paid day: the ride is free, and the entry is written
        // anyway with amount 0 so the index records that this ride was seen and
        // the sweep never looks at it again.
        const covered = !!dayUntil && dayUntil.getTime() > started.getTime();
        const amount = covered ? 0 : -price;
        const until = covered
          ? dayUntil
          : new Date(started.getTime() + DAY_HOURS * 3600 * 1000);

        await client.query(
          `INSERT INTO movin.wallet_entry
             (driver_id, kind, amount, ride_id, day_from, day_until, note)
           VALUES ($1, $2, $3, $4, $5, $6, $7)`,
          [
            r.driver_id,
            covered ? 'day' : 'day',
            amount,
            r.ride_id,
            covered ? null : started,
            covered ? null : until,
            covered ? 'Course incluse dans la journée' : 'Journée de 24 h',
          ],
        );

        if (!covered) {
          // Allowed to go negative, on the client's instruction: a driver with
          // a passenger in the car is never cut off, and he clears it on his
          // next top-up.
          await client.query(
            `UPDATE movin.wallet
                SET balance = balance - $2, day_until = $3, updated_at = now()
              WHERE driver_id = $1`,
            [r.driver_id, price, until],
          );
        }
        await client.query('COMMIT');
        if (!covered) charged += 1;
      } catch (e) {
        await client.query('ROLLBACK').catch(() => {});
        // A duplicate is the unique index doing its job on a re-run, not a
        // fault, and it must not be logged as one every five minutes.
        if (e.code !== '23505') {
          console.error(`[wallet] charge ${r.ride_id}: ${e.message}`);
        }
      } finally {
        client.release();
      }
    }
    if (charged) console.log(`[wallet] opened ${charged} day(s)`);
  } catch (e) {
    console.error('[wallet] sweep:', e.message);
  }
}

/** Every movement, newest first. What answers "where did my 30 go". */
async function history(pool, token, res) {
  const driver = await driverFromToken(token);
  if (!driver) return send(res, 401, { error: 'unauthorized' });

  try {
    const { rows } = await pool.query(
      `SELECT kind, amount, day_from, day_until, note, created_at
         FROM movin.wallet_entry
        WHERE driver_id = $1
        ORDER BY created_at DESC, id DESC
        LIMIT 100`,
      [driver.id],
    );
    const w = await walletOf(pool, driver.id);
    const cfg = await countryOf(pool, driver.id);
    send(res, 200, {
      balance: w.balance,
      currency: cfg.currency,
      entries: rows.map((r) => ({
        kind: r.kind,
        amount: r.amount,
        at: r.created_at,
        dayFrom: r.day_from,
        dayUntil: r.day_until,
        note: r.note,
      })),
    });
  } catch (e) {
    console.error('[wallet] history:', e.message);
    send(res, 503, { error: 'unavailable' });
  }
}

/**
 * Where Moosyl sends the driver's browser back to.
 *
 * Says nothing about whether he paid -- that is the app's job, by polling
 * `topupState`, which reads the truth from Moosyl. This page exists so the
 * browser lands somewhere that is not an error, and it deliberately does not
 * claim a result it has not checked.
 */
function done(query, res) {
  const ok = query.get('ok') === '1';
  const body = `<!doctype html><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Movin</title>
<style>body{font:16px system-ui;margin:0;display:grid;place-items:center;height:100vh;
background:#fff;color:#111;text-align:center;padding:24px}p{color:#666}</style>
<h1>${ok ? 'Merci' : 'Paiement annulé'}</h1>
<p>${ok ? 'Vous pouvez revenir à l’application.' : 'Rien n’a été débité.'}</p>`;
  res.writeHead(200, {
    'content-type': 'text/html; charset=utf-8',
    'content-length': Buffer.byteLength(body),
    'cache-control': 'no-store',
  });
  res.end(body);
}

const configured = () => Boolean(SECRET && PUBLIC_URL);

module.exports = {
  status, topup, topupState, webhook, history, done,
  chargeStartedRides, configured, PRICE, MIN_TOPUP, CURRENCY, DAY_HOURS, COUNTRIES,
};
