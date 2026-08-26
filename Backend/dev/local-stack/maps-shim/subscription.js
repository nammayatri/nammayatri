'use strict';
/**
 * The driver's 3 000 DA a month, taken through Chargily Pay.
 *
 * ── The model, settled with the client on 2026-08-25 ────────────────────────
 * Passengers pay drivers in cash and the app never touches that money. Drivers
 * pay *us* 3 000 DA a month, by CIB or Edahabia. No CCP ("cannot be automated,
 * no API"), no cash.
 *
 * Three defaults he approved on 2026-08-26, each of which is a line of config
 * below rather than a decision buried in the code:
 *
 *   1. We pay Chargily's fee, not the driver -- `chargily_pay_fees_allocation:
 *      'merchant'`. It is 0 % under 100 drivers anyway, and a driver asked for
 *      3 037,50 DA instead of 3 000 reads as a bug.
 *   2. No grace period. He is warned three days out (`warn` on the status
 *      route) and that is the whole of it.
 *   3. One month at a time. MONTHS is fixed at 1; the column exists so that
 *      changing our mind later is a config change and not a migration.
 *
 * ── Why there is no recurring billing, and never will be ────────────────────
 * Chargily's API has customers, products, prices, checkouts, payment links,
 * webhooks and balance -- and no subscriptions. Nor could it: CIB and Edahabia
 * have no card-on-file debit, so **no** Algerian gateway can charge a driver
 * automatically. "Monthly subscription" therefore means pay-then-extend: he
 * presses pay, the webhook writes `paid_until = +30 days`, and nobody is ever
 * charged without acting. Anyone reading this expecting to find the renewal job
 * should stop looking -- there is nothing to find.
 *
 * ── Why it lives in the shim ────────────────────────────────────────────────
 * probe-subscription.sql measured it: the deployed backend has nowhere to
 * record a payment, and upstream's driver-subscription subsystem is not in this
 * binary. The shim is already Node with a Postgres pool and already serves
 * routes of its own, so the gateway lands here. No rebuild.
 */

const crypto = require('crypto');
const restricted = require('./restricted');

/** Dinars, not centimes. Their own example is `"amount": 2000` = 2 000 DZD. */
const PRICE     = Number(process.env.SUBSCRIPTION_PRICE || 3000);
/* 30 days rather than a calendar month, so every driver buys the same thing.
   `+ interval '1 month'` would sell 28 days in February and 31 in March for
   the same 3 000 DA, which is the kind of detail a driver notices. */
const DAYS      = Number(process.env.SUBSCRIPTION_DAYS || 30);
const MONTHS    = 1;
const CURRENCY  = 'dzd';
/** Days before expiry that the status route starts saying `warn`. */
const WARN_DAYS = Number(process.env.SUBSCRIPTION_WARN_DAYS || 3);

const SECRET     = process.env.CHARGILY_SECRET_KEY || '';
const CHARGILY   = (process.env.CHARGILY_BASE || 'https://pay.chargily.net/test/api/v2').replace(/\/$/, '');
/** Our own public origin -- what Chargily redirects to and posts back to. */
const PUBLIC_URL = (process.env.PUBLIC_URL || '').replace(/\/$/, '');
const DRIVER_URL = (process.env.DRIVER_URL || 'http://localhost:8016').replace(/\/$/, '');

/** Largest webhook body we will read. Theirs are ~1 KB. */
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
 * Same trick as `riderIsSignedIn` in fleet.js -- the shim has no session of its
 * own, so it asks the app that does. One difference, and it is the important
 * one: this returns the **id from that response** rather than checking an id
 * the caller supplied.
 *
 * So there is no route here that takes a driver id from the client. A driver
 * cannot open a checkout against somebody else's account, cannot read whether a
 * rival has paid, and cannot enumerate the fleet by trying ids. The id is not
 * an input.
 */
async function driverFromToken(token) {
  if (!token) return null;
  try {
    const r = await fetch(`${DRIVER_URL}/ui/driver/profile`, { headers: { token } });
    if (!r.ok) return null;
    const body = await r.json();
    const id = typeof body.id === 'string' ? body.id.trim() : '';
    if (!id) return null;
    return {
      id,
      name: [body.firstName, body.lastName].filter((s) => typeof s === 'string' && s.trim()).join(' ').trim() || null,
    };
  } catch (e) {
    console.error('[subscription] driver lookup failed:', e.message);
    return null;
  }
}

/** The shape the app renders, from a `paid_until` that may be null. */
function stateOf(paidUntil) {
  if (!paidUntil) {
    // Never paid. NOT the same as lapsed, and the app must not say "expired" to
    // a driver who has never been asked for a dinar. Every driver in the pilot
    // is in this state today.
    return { state: 'never', paidUntil: null, daysLeft: null, warn: false };
  }
  const until = new Date(paidUntil);
  const daysLeft = Math.floor((until.getTime() - Date.now()) / 86400000);
  return {
    state: until.getTime() > Date.now() ? 'active' : 'lapsed',
    paidUntil: until.toISOString(),
    daysLeft,
    // The whole of "no grace period, but warn him three days out".
    warn: daysLeft >= 0 && daysLeft <= WARN_DAYS,
  };
}

/**
 * GET /subscription/status  (driver token)
 *
 * Everything the *Mon abonnement* screen needs in one call, including the price
 * -- so raising it one day is a config change on the server and not a new build
 * of the app.
 */
async function status(pool, token, res) {
  const driver = await driverFromToken(token);
  if (!driver) return send(res, 401, { error: 'sign in first' });
  if (!pool) return send(res, 503, { error: 'no database' });

  try {
    const q = await pool.query(
      'SELECT paid_until FROM movin.subscription WHERE driver_id = $1',
      [driver.id],
    );
    const last = await pool.query(
      `SELECT checkout_id, invoice_no, amount, currency, paid_at, covers_until
         FROM movin.subscription_payment
        WHERE driver_id = $1 AND applied_at IS NOT NULL
        ORDER BY paid_at DESC LIMIT 1`,
      [driver.id],
    );
    return send(res, 200, {
      ...stateOf(q.rows[0] ? q.rows[0].paid_until : null),
      price: PRICE,
      currency: CURRENCY,
      days: DAYS,
      lastPayment: last.rows[0]
        ? {
            checkoutId: last.rows[0].checkout_id,
            invoiceNo: Number(last.rows[0].invoice_no),
            amount: Number(last.rows[0].amount),
            currency: last.rows[0].currency,
            paidAt: last.rows[0].paid_at,
            coversUntil: last.rows[0].covers_until,
          }
        : null,
    });
  } catch (e) {
    console.error('[subscription] status', e.message);
    return send(res, 500, { error: 'query failed' });
  }
}

/**
 * POST /subscription/checkout?method=cib|edahabia  (driver token)
 *
 * Creates the payment page and hands back its URL. Nothing is extended here --
 * only the webhook writes `paid_until`, because only the webhook is Chargily
 * telling us money moved. A driver who opens the page and closes it leaves a
 * `pending` row and nothing else.
 */
async function checkout(pool, token, method, res) {
  const driver = await driverFromToken(token);
  if (!driver) return send(res, 401, { error: 'sign in first' });
  if (!pool) return send(res, 503, { error: 'no database' });
  if (!SECRET) {
    // Deliberately explicit. The alternative is a 401 from Chargily that looks
    // like the driver's problem, and an afternoon spent looking at the app.
    console.error('[subscription] CHARGILY_SECRET_KEY is not set');
    return send(res, 503, { error: 'payments not configured' });
  }
  if (!PUBLIC_URL) {
    console.error('[subscription] PUBLIC_URL is not set; Chargily could not call back');
    return send(res, 503, { error: 'payments not configured' });
  }

  const pay = method === 'cib' ? 'cib' : 'edahabia';
  let created;
  try {
    const r = await fetch(`${CHARGILY}/checkouts`, {
      method: 'POST',
      headers: {
        authorization: `Bearer ${SECRET}`,
        'content-type': 'application/json',
      },
      body: JSON.stringify({
        amount: PRICE,
        currency: CURRENCY,
        payment_method: pay,
        locale: 'fr',
        description: `Abonnement Movin DZ - ${DAYS} jours`,
        chargily_pay_fees_allocation: 'merchant',
        success_url: `${PUBLIC_URL}/subscription/done?state=success`,
        failure_url: `${PUBLIC_URL}/subscription/done?state=failure`,
        webhook_endpoint: `${PUBLIC_URL}/subscription/webhook`,
        /* Sent so a human reading their dashboard can tell whose payment this
           is. Deliberately never read back: their API reference calls metadata
           an array and their checkout object calls it a set of key-value pairs,
           and a subscription that depends on which one is right is a
           subscription that breaks on a documentation change. The mapping we
           actually trust is our own row, written immediately below. */
        metadata: [{ key: 'driver_id', value: driver.id }],
      }),
    });
    created = await r.json().catch(() => null);
    if (!r.ok || !created || !created.id || !created.checkout_url) {
      console.error('[subscription] chargily refused:', r.status, JSON.stringify(created));
      return send(res, 502, { error: 'gateway refused' });
    }
  } catch (e) {
    console.error('[subscription] chargily unreachable:', e.message);
    return send(res, 502, { error: 'gateway unreachable' });
  }

  try {
    await pool.query(
      `INSERT INTO movin.subscription_payment
         (checkout_id, driver_id, amount, currency, months, status)
       VALUES ($1, $2, $3, $4, $5, 'pending')
       ON CONFLICT (checkout_id) DO NOTHING`,
      [created.id, driver.id, PRICE, CURRENCY, MONTHS],
    );
  } catch (e) {
    /* The checkout exists at Chargily and we failed to record whose it is. Do
       not hand out the URL: a payment we cannot attribute is worse than a
       payment that never happened, and he can simply press the button again. */
    console.error('[subscription] could not record checkout', created.id, e.message);
    return send(res, 500, { error: 'could not record checkout' });
  }

  return send(res, 200, {
    checkoutId: created.id,
    checkoutUrl: created.checkout_url,
    amount: PRICE,
    currency: CURRENCY,
    method: pay,
  });
}

/** Read the body as bytes. The signature is over exactly these bytes. */
function rawBody(req) {
  return new Promise((resolve, reject) => {
    const chunks = [];
    let size = 0;
    req.on('data', (c) => {
      size += c.length;
      if (size > MAX_BODY) {
        reject(new Error('body too large'));
        req.destroy();
        return;
      }
      chunks.push(c);
    });
    req.on('end', () => resolve(Buffer.concat(chunks)));
    req.on('error', reject);
  });
}

/**
 * Is this really Chargily?
 *
 * HMAC-SHA256 of the **raw body**, keyed with the API secret, hex, in a header
 * called `signature`. The trap their own documentation walks people into: hash
 * a re-serialised `JSON.parse` of the body and it never matches, because key
 * order and whitespace are not preserved -- and the failure looks like a
 * Chargily bug rather than ours. Hence `rawBody` above, and hence this module
 * being wired in before anything can parse the request.
 */
function signatureOk(raw, signature) {
  if (!SECRET || !signature) return false;
  const mine = crypto.createHmac('sha256', SECRET).update(raw).digest('hex');
  const a = Buffer.from(mine, 'utf8');
  const b = Buffer.from(String(signature), 'utf8');
  // Length must match before timingSafeEqual, which throws otherwise.
  return a.length === b.length && crypto.timingSafeEqual(a, b);
}

/** Last-resort attribution if our own row is missing. Accepts either shape. */
function driverFromMetadata(meta) {
  if (!meta) return null;
  if (Array.isArray(meta)) {
    const hit = meta.find((m) => m && m.key === 'driver_id');
    return hit && typeof hit.value === 'string' ? hit.value : null;
  }
  if (typeof meta === 'object' && typeof meta.driver_id === 'string') return meta.driver_id;
  return null;
}

/**
 * POST /subscription/webhook  -- Chargily, not the app.
 *
 * The only thing in this system that extends a subscription.
 *
 * ── Idempotent, because their webhooks are retried ──────────────────────────
 * Without a replay guard one retry is a free month. The guard is the
 * `applied_at IS NULL` in the UPDATE ... RETURNING: the first delivery claims
 * the row and gets a driver back, every later delivery updates nothing and gets
 * no rows, and the extension below never runs. It is one statement, so two
 * deliveries arriving at once cannot both win -- the second blocks on the row
 * lock and then matches nothing.
 *
 * ── One transaction, because half of this is worse than none ────────────────
 * Marking the payment applied and extending the subscription are the same fact.
 * A crash between them either takes his money without giving him the month, or
 * leaves a payment that a retry would apply twice.
 *
 * ── Status codes are instructions to Chargily ───────────────────────────────
 *   403  bad signature      -- never retry, nothing will change
 *   200  applied, or already applied, or an event we ignore
 *   500  we could not write -- please retry, we want this one
 * A 500 on a bad signature would have them hammering us forever; a 200 on a
 * failed write would silently lose a month somebody paid for.
 */
async function webhook(pool, req, res) {
  let raw;
  try {
    raw = await rawBody(req);
  } catch (e) {
    return send(res, 400, { error: e.message });
  }

  if (!signatureOk(raw, req.headers.signature)) {
    console.error('[subscription] webhook with a bad signature, ignored');
    return send(res, 403, { error: 'bad signature' });
  }

  let event;
  try {
    event = JSON.parse(raw.toString('utf8'));
  } catch {
    return send(res, 400, { error: 'not json' });
  }

  const type = String(event.type || '');
  const data = event.data || {};
  const checkoutId = typeof data.id === 'string' ? data.id : '';
  if (!checkoutId) return send(res, 400, { error: 'no checkout id' });

  if (!pool) return send(res, 500, { error: 'no database' });

  // Anything that is not a payment: record the outcome and touch nothing else.
  if (type !== 'checkout.paid') {
    const ending = type.startsWith('checkout.') ? type.slice('checkout.'.length) : type;
    try {
      await pool.query(
        `UPDATE movin.subscription_payment
            SET status = $2, event = $3
          WHERE checkout_id = $1 AND applied_at IS NULL`,
        [checkoutId, ending || 'unknown', event],
      );
    } catch (e) {
      console.error('[subscription] could not record', type, e.message);
      return send(res, 500, { error: 'write failed' });
    }
    return send(res, 200, { ok: true, ignored: type });
  }

  const client = await pool.connect();
  try {
    await client.query('BEGIN');

    // If we never recorded the checkout -- a crash between creating it and
    // writing our row -- fall back to what Chargily echoed. Better a row
    // rebuilt from their copy than a month somebody paid for going missing.
    const known = await client.query(
      'SELECT driver_id FROM movin.subscription_payment WHERE checkout_id = $1',
      [checkoutId],
    );
    if (!known.rows[0]) {
      const guess = driverFromMetadata(data.metadata);
      if (!guess) {
        await client.query('ROLLBACK');
        // 200, not 500: a retry cannot help, and Chargily would retry for days.
        // The whole event is in the log so the office can reconcile it by hand.
        console.error('[subscription] paid checkout we cannot attribute:', JSON.stringify(event));
        return send(res, 200, { ok: true, unattributed: checkoutId });
      }
      await client.query(
        `INSERT INTO movin.subscription_payment
           (checkout_id, driver_id, amount, currency, months, status)
         VALUES ($1, $2, $3, $4, $5, 'pending')
         ON CONFLICT (checkout_id) DO NOTHING`,
        [checkoutId, guess, Number(data.amount) || PRICE, data.currency || CURRENCY, MONTHS],
      );
    }

    // The replay guard. No rows back means somebody already applied this.
    const claim = await client.query(
      `UPDATE movin.subscription_payment
          SET status = 'paid', paid_at = now(), applied_at = now(),
              invoice_no = nextval('movin.invoice_seq'), event = $2
        WHERE checkout_id = $1 AND applied_at IS NULL
      RETURNING driver_id, months`,
      [checkoutId, event],
    );
    if (!claim.rows[0]) {
      await client.query('ROLLBACK');
      return send(res, 200, { ok: true, alreadyApplied: checkoutId });
    }

    const { driver_id: driverId, months } = claim.rows[0];
    const days = DAYS * Number(months || 1);

    /* `greatest(paid_until, now())` is the whole of pay-then-extend: paying
       early stacks onto what is left, paying late starts from today. Without
       the `greatest`, a driver who lets three months lapse and then pays would
       buy a month that expired two months ago. */
    const extended = await client.query(
      `INSERT INTO movin.subscription AS s (driver_id, paid_until)
       VALUES ($1, now() + ($2 || ' days')::interval)
       ON CONFLICT (driver_id) DO UPDATE
          SET paid_until = greatest(s.paid_until, now()) + ($2 || ' days')::interval,
              updated_at = now()
       RETURNING paid_until`,
      [driverId, String(days)],
    );

    const paidUntil = extended.rows[0].paid_until;
    /* `covers_from` is derived by subtracting rather than read back from the
       statement above: inside ON CONFLICT ... RETURNING, the table alias is the
       *post-update* row, so `greatest(s.paid_until, now())` there would return
       the new expiry and every receipt would claim to cover zero days. The
       month always ends `days` after it starts, so this is exact either way --
       whether he stacked onto a live subscription or restarted a lapsed one. */
    await client.query(
      `UPDATE movin.subscription_payment
          SET covers_until = $2::timestamptz,
              covers_from  = $2::timestamptz - ($3 || ' days')::interval
        WHERE checkout_id = $1`,
      [checkoutId, paidUntil, String(days)],
    );

    await client.query('COMMIT');
    console.log(`[subscription] ${driverId} paid ${data.amount} ${data.currency}, until ${paidUntil}`);

    /* Republish the dispatch restriction list at once rather than waiting for
       its timer. A driver who has just paid 3 000 DA and then watches five
       more minutes of requests go past him has, from where he is sitting,
       paid for nothing. Deliberately not awaited: Chargily is owed a 200 for a
       payment that is already committed, and a slow Redis must not turn a
       successful webhook into a retry. */
    void restricted.refresh(pool, 'payment applied');

    return send(res, 200, { ok: true, driverId, paidUntil });
  } catch (e) {
    await client.query('ROLLBACK').catch(() => {});
    console.error('[subscription] apply failed for', checkoutId, e.message);
    // 500 on purpose: we want this one back.
    return send(res, 500, { error: 'apply failed' });
  } finally {
    client.release();
  }
}

/**
 * GET /subscription/receipt/{checkoutId}  (driver token)
 *
 * "No need for receipts. But we should be able to generate them." This is the
 * data one is generated from; the app draws it. Scoped to the caller's own
 * payments -- the id is Chargily's and long, but a receipt is not a thing to
 * hand out on the strength of a guessed identifier.
 */
async function receipt(pool, token, checkoutId, res) {
  const driver = await driverFromToken(token);
  if (!driver) return send(res, 401, { error: 'sign in first' });
  if (!pool) return send(res, 503, { error: 'no database' });

  try {
    const q = await pool.query(
      `SELECT invoice_no, checkout_id, amount, currency, months,
              paid_at, covers_from, covers_until
         FROM movin.subscription_payment
        WHERE checkout_id = $1 AND driver_id = $2 AND applied_at IS NOT NULL`,
      [String(checkoutId), driver.id],
    );
    const row = q.rows[0];
    if (!row) return send(res, 404, { error: 'no such receipt' });
    return send(res, 200, {
      invoiceNo: Number(row.invoice_no),
      checkoutId: row.checkout_id,
      driverId: driver.id,
      driverName: driver.name,
      amount: Number(row.amount),
      currency: row.currency,
      months: Number(row.months),
      paidAt: row.paid_at,
      coversFrom: row.covers_from,
      coversUntil: row.covers_until,
    });
  } catch (e) {
    console.error('[subscription] receipt', e.message);
    return send(res, 500, { error: 'query failed' });
  }
}

/**
 * GET /subscription/checkout/{id}  (driver token)
 *
 * The authoritative state of one payment.
 *
 * ── Why our own tables cannot answer this ──────────────────────────────────
 * A checkout the driver opened and closed, and one whose webhook is merely
 * late, are the *same row* here: `pending`, both of them. The app has to tell
 * them apart because the two messages are opposites, and both wrong ones cost
 * real money:
 *
 *   "Paiement enregistré, la confirmation suit"  to a man who abandoned the
 *   page is a lie about his money, and he waits for a subscription that is
 *   never coming.
 *
 *   "Paiement échoué"  to a man whose webhook is in flight makes him pay a
 *   second time.
 *
 * Only the gateway knows which it is, so this asks it.
 *
 * ── Four answers, not five ─────────────────────────────────────────────────
 *   paid        the webhook has landed and the month is his
 *   confirming  Chargily took the money; our webhook has not arrived yet
 *   pending     he has not paid. Past the app's wait, that means he left
 *   failed      refused, cancelled, or the checkout expired
 *
 * `confirming` is the whole reason this route exists. It is the only state in
 * which "wait a moment" is true rather than a guess.
 */
async function checkoutState(pool, token, checkoutId, res) {
  const driver = await driverFromToken(token);
  if (!driver) return send(res, 401, { error: 'sign in first' });
  if (!pool) return send(res, 503, { error: 'no database' });

  let row;
  try {
    const q = await pool.query(
      `SELECT driver_id, status, applied_at
         FROM movin.subscription_payment WHERE checkout_id = $1`,
      [String(checkoutId)],
    );
    row = q.rows[0];
  } catch (e) {
    console.error('[subscription] checkout lookup', e.message);
    return send(res, 500, { error: 'query failed' });
  }

  // Scoped to his own payments. The id is Chargily's and long, but one driver
  // has no business reading the state of another's payment.
  if (!row || row.driver_id !== driver.id) return send(res, 404, { error: 'no such checkout' });

  // Ours is final once the webhook has applied it; there is nothing the
  // gateway could add, and a network call would only add a way to fail.
  if (row.applied_at) return send(res, 200, { status: 'paid', source: 'webhook' });
  if (row.status === 'failed' || row.status === 'canceled') {
    return send(res, 200, { status: 'failed', source: 'webhook' });
  }

  if (!SECRET) return send(res, 200, { status: 'pending', source: 'local' });

  try {
    const r = await fetch(`${CHARGILY}/checkouts/${encodeURIComponent(checkoutId)}`, {
      headers: { authorization: `Bearer ${SECRET}` },
    });
    const body = await r.json().catch(() => null);
    if (!r.ok || !body) {
      // Not knowing is not the same as failing. `pending` keeps the app
      // waiting, which is the only harmless answer when we cannot tell.
      console.error('[subscription] chargily retrieve', r.status);
      return send(res, 200, { status: 'pending', source: 'unknown' });
    }
    const theirs = String(body.status || 'pending');
    const status =
      theirs === 'paid'
        ? 'confirming' // they have the money; our webhook has not landed yet
        : theirs === 'failed' || theirs === 'canceled' || theirs === 'expired'
          ? 'failed'
          : 'pending';
    return send(res, 200, { status, source: 'gateway', gateway: theirs });
  } catch (e) {
    console.error('[subscription] chargily unreachable:', e.message);
    return send(res, 200, { status: 'pending', source: 'unknown' });
  }
}

/** GET /subscription/history  (driver token) -- what the app lists under the state. */
async function history(pool, token, res) {
  const driver = await driverFromToken(token);
  if (!driver) return send(res, 401, { error: 'sign in first' });
  if (!pool) return send(res, 503, { error: 'no database' });

  try {
    const q = await pool.query(
      `SELECT checkout_id, invoice_no, amount, currency, status,
              paid_at, covers_from, covers_until, created_at
         FROM movin.subscription_payment
        WHERE driver_id = $1
        ORDER BY created_at DESC LIMIT 24`,
      [driver.id],
    );
    return send(res, 200, {
      payments: q.rows.map((r) => ({
        checkoutId: r.checkout_id,
        invoiceNo: r.invoice_no === null ? null : Number(r.invoice_no),
        amount: Number(r.amount),
        currency: r.currency,
        status: r.status,
        paidAt: r.paid_at,
        coversFrom: r.covers_from,
        coversUntil: r.covers_until,
        createdAt: r.created_at,
      })),
    });
  } catch (e) {
    console.error('[subscription] history', e.message);
    return send(res, 500, { error: 'query failed' });
  }
}

/**
 * GET /subscription/done?state=success|failure -- where Chargily sends him back.
 *
 * Not JSON, because a browser lands here. Without it the driver finishes paying
 * and is redirected to a 404, which is indistinguishable from having lost his
 * 3 000 DA. It tries the app's own scheme first and, when that fails -- an
 * in-app browser, a phone that has forgotten the association -- says the same
 * thing in French where he can read it.
 *
 * It deliberately does not report whether the *subscription* was extended: that
 * is the webhook's business and it may not have arrived yet. Claiming success
 * here and having the screen still say "expiré" would be worse than saying
 * nothing.
 */
function done(query, res) {
  const ok = query.get('state') !== 'failure';
  const title = ok ? 'Paiement reçu' : 'Paiement annulé';
  const line = ok
    ? 'Merci. Votre abonnement sera actif dans quelques instants.'
    : "Le paiement n'a pas abouti. Aucun montant n'a été débité.";
  const html = `<!doctype html><html lang="fr"><head>
<meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>${title}</title>
<style>
 body{margin:0;min-height:100vh;display:flex;align-items:center;justify-content:center;
      font:16px/1.5 system-ui,-apple-system,"Segoe UI",Roboto,sans-serif;
      background:#0f1115;color:#f2f4f8;padding:24px;text-align:center}
 .c{max-width:22rem}h1{font-size:1.25rem;margin:0 0 .5rem}
 p{margin:0 0 1.5rem;color:#aeb4bf}
 a{display:inline-block;padding:.75rem 1.5rem;border-radius:999px;
   background:#f2f4f8;color:#0f1115;text-decoration:none;font-weight:600}
</style></head><body><div class="c">
<h1>${title}</h1><p>${line}</p>
<a href="movin://driver/subscription">Revenir à Movin</a>
</div><script>setTimeout(function(){location.href='movin://driver/subscription';},400);</script>
</body></html>`;
  res.writeHead(ok ? 200 : 402, {
    'content-type': 'text/html;charset=utf-8',
    'content-length': Buffer.byteLength(html),
    'cache-control': 'no-store',
  });
  res.end(html);
}

/**
 * Is this switched on? Reported by /healthz so a deploy that forgot the secret
 * says so on the health check rather than the first time a driver tries to pay.
 * Never reports the key itself.
 */
const configured = () => Boolean(SECRET && PUBLIC_URL);

module.exports = { status, checkout, checkoutState, webhook, receipt, history, done, configured, PRICE, CURRENCY, DAYS };
