'use strict';
// The money path, against a real Postgres (PGlite) and our own migration.
// Phase 5 of the backend restructuring plan (2026-10-06): every line of
// maps-shim/wallet.js that decides whether a driver may work, or what he is
// charged, has a check here.
//
//   cd Backend/dev/local-stack/tests && npm ci && node wallet.test.js
//
//   1. canWork     -- the expression the screen and the guard both read
//   2. the top-up  -- bounds, the row before the gateway, both gateways
//   3. the credit  -- only after the gateway says paid, exactly once
//   4. the day     -- 30 MRU / 100 DA at a ride's START, 24 h, then free
const path = require('path');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');

const MR = 'favorit0-0000-0000-0000-00000favorit';
const DZ = 'algeria0-0000-0000-0000-00000algeria';
const HOUR = 3600 * 1000;

(async () => {
  const t = fakes.checker();
  const db = await open();
  const who = await fakes.backends();
  const gw = await fakes.gateways();
  const redis = await fakes.redis();

  Object.assign(process.env, {
    DRIVER_URL: who.url,
    MOOSYL_BASE: gw.url, MOOSYL_SECRET_KEY: 'moosyl-test',
    CHARGILY_BASE: gw.url, CHARGILY_SECRET_KEY: 'chargily-test',
    PUBLIC_URL: 'https://example.test',
    REDIS_HOST: '127.0.0.1', REDIS_PORT: String(redis.port),
  });
  const wallet = require(path.join(__dirname, '..', 'stack', 'maps-shim', 'wallet.js'));

  // A driver: a person row in his merchant, and a token that names him.
  let n = 0;
  const driver = async (merchant, { balance, dayUntil } = {}) => {
    n += 1;
    const id = `drv-${merchant === DZ ? 'dz' : 'mr'}-${String(n).padStart(4, '0')}`;
    await db.sql('INSERT INTO atlas_driver_offer_bpp.person (id, merchant_id) VALUES ($1, $2)', [id, merchant]);
    if (balance !== undefined || dayUntil !== undefined) {
      await db.sql('INSERT INTO movin.wallet (driver_id, balance, day_until) VALUES ($1, $2, $3)',
        [id, balance || 0, dayUntil || null]);
    }
    who.drivers.set(`tok-${id}`, id);
    return { id, token: `tok-${id}` };
  };
  const status = async (d) => { const r = fakes.res(); await wallet.status(db.pool, d.token, r); return r; };
  const canWork = async (d) => (await status(d)).json().canWork;
  const row = async (id) => (await db.sql('SELECT balance, day_until FROM movin.wallet WHERE driver_id = $1', [id]))[0];

  /* ── 1. canWork ───────────────────────────────────────────────────────── */
  t.section('1. canWork: an active day, or the credit to open one -- in HIS country\'s price');
  t.ok((await canWork(await driver(MR))) === false, 'MR, never topped up (no wallet row): may not work');
  t.ok((await canWork(await driver(MR, { balance: 29 }))) === false, 'MR, 29 MRU: may not work (a day is 30)');
  t.ok((await canWork(await driver(MR, { balance: 30 }))) === true, 'MR, 30 MRU: may work');
  t.ok((await canWork(await driver(DZ, { balance: 40 }))) === false,
    'DZ, 40 DA: may not work -- more than MR\'s 30, less than a 100 DA day');
  t.ok((await canWork(await driver(DZ, { balance: 100 }))) === true, 'DZ, 100 DA: may work');
  const midDay = await driver(MR, { balance: 0, dayUntil: new Date(Date.now() + 5 * HOUR) });
  t.ok((await canWork(midDay)) === true, 'MR, empty wallet but inside a paid day: may work');
  t.ok((await canWork(await driver(MR, { balance: -30, dayUntil: new Date(Date.now() + HOUR) }))) === true,
    'MR, negative balance (a ride started short) but inside the day: may work');
  t.ok((await canWork(await driver(MR, { balance: 0, dayUntil: new Date(Date.now() - HOUR) }))) === false,
    'MR, day ended an hour ago, empty wallet: may not work');
  {
    const s = (await status(await driver(DZ, { balance: 250 }))).json();
    t.ok(s.currency === 'DZD' && s.dayPrice === 100 && s.minTopup === 100 && s.gateway === 'chargily',
      'DZ status names DZD, 100 a day, Chargily', JSON.stringify(s));
    const m = (await status(await driver(MR, { balance: 0 }))).json();
    t.ok(m.currency === 'MRU' && m.dayPrice === 30 && m.gateway === 'moosyl', 'MR status names MRU, 30 a day, Moosyl', JSON.stringify(m));
    t.ok(m.maxTopup === 3000 && s.maxTopup === 10000, 'the top-up ceiling is 100 days in each country');
  }
  {
    const r = fakes.res();
    await wallet.status(db.pool, 'tok-nobody', r);
    t.ok(r.status === 401, 'a token no backend knows: 401, no wallet shown');
    const d = await driver(MR, { balance: 500 });
    db.pool.failNext = 1;
    const down = await status(d);
    t.ok(down.status === 503 && !('canWork' in down.json()),
      'database down: 503, never a guessed canWork', down.text);
    t.ok(down.headers['cache-control'] === 'no-store', 'money answers are never cached');
  }

  /* ── 2. the top-up ────────────────────────────────────────────────────── */
  t.section('2. the top-up: bounds, and our row exists before the driver is sent anywhere');
  const topup = async (d, amount, method) => { const r = fakes.res(); await wallet.topup(db.pool, d.token, amount, method, r); return r; };
  const payer = await driver(MR);
  let r = await topup(payer, 29);
  t.ok(r.status === 400 && r.json().error === 'amount_too_small' && r.json().minTopup === 30, 'MR 29: refused, too small');
  r = await topup(payer, 3001);
  t.ok(r.status === 400 && r.json().error === 'amount_too_large', 'MR 3001: refused, too large');
  r = await topup(payer, 'abc');
  t.ok(r.status === 400, 'MR "abc": refused');
  t.ok(gw.created.length === 0, 'no refused amount ever reached a gateway');
  r = await topup(payer, 30.9);
  const mrTx = r.json();
  t.ok(r.status === 200 && mrTx.amount === 30 && mrTx.currency === 'MRU' && /^https:\/\/pay\.example\//.test(mrTx.url),
    'MR 30.9: a Moosyl checkout for 30 MRU (whole MRU, rounded down)', r.text);
  t.ok(gw.created.map((c) => c.url).join(',') === '/payment-request,/checkout-session'
    && gw.created[0].auth === 'moosyl-test', 'Moosyl: payment request, then session, with our raw key');
  let tx = (await db.sql('SELECT * FROM movin.wallet_topup WHERE transaction_id = $1', [mrTx.transactionId]))[0];
  t.ok(tx && tx.status === 'pending' && tx.payment_ref === 'sess_2' && tx.driver_id === payer.id && !tx.credited_at,
    'the row: pending, the session id kept, nothing credited', JSON.stringify(tx));

  const dzPayer = await driver(DZ);
  r = await topup(dzPayer, 99, 'cib');
  t.ok(r.status === 400 && r.json().minTopup === 100, 'DZ 99 DA: refused (the floor is a 100 DA day)');
  r = await topup(dzPayer, 500, 'cib');
  const dzTx = r.json();
  const chk = gw.created[gw.created.length - 1];
  t.ok(r.status === 200 && dzTx.currency === 'DZD' && dzTx.url.startsWith('https://'),
    'DZ 500 DA: a Chargily checkout, its URL upgraded to https', r.text);
  t.ok(chk.url === '/checkouts' && chk.body.payment_method === 'cib' && chk.body.amount === 500
    && chk.body.chargily_pay_fees_allocation === 'merchant' && chk.auth === 'Bearer chargily-test',
    'Chargily: CIB as chosen, the fee is ours, Bearer key', JSON.stringify(chk.body));
  r = await topup(dzPayer, 500, 'anything');
  t.ok(gw.created[gw.created.length - 1].body.payment_method === 'edahabia', 'DZ, unknown card: Edahabia');

  gw.refuse = true;
  r = await topup(payer, 60);
  gw.refuse = false;
  tx = (await db.sql("SELECT status FROM movin.wallet_topup WHERE driver_id = $1 ORDER BY created_at DESC LIMIT 1", [payer.id]))[0];
  t.ok(r.status === 502 && tx.status === 'failed', 'gateway refuses: 502, and our row says failed (not pending forever)');

  /* ── 3. the credit ────────────────────────────────────────────────────── */
  t.section('3. the credit: only what the gateway says is paid, exactly once');
  const poll = async (d, id) => { const x = fakes.res(); await wallet.topupState(db.pool, d.token, id, x); return x; };
  r = await poll(payer, mrTx.transactionId);
  t.ok(r.json().credited === false && (await row(payer.id)).balance === 0, 'not yet paid: nothing credited');
  await wallet.webhook(db.pool, fakes.req({ data: { id: 'sess_2' } }), fakes.res());
  t.ok((await row(payer.id)).balance === 0, 'a webhook for an unpaid session: still nothing (the body is never trusted)');
  const askedBefore = gw.asked.length;
  const forged = fakes.res();
  await wallet.webhook(db.pool, fakes.req({ transactionId: 'movin-forged-1' }), forged);
  const garbage = fakes.res();
  await wallet.webhook(db.pool, fakes.req('not json'), garbage);
  t.ok(forged.status === 200 && garbage.status === 200 && gw.asked.length === askedBefore
    && (await row(payer.id)).balance === 0,
    'a forged reference and a body that is not JSON: 200, no gateway asked, nothing credited');

  gw.paid.add('sess_2');
  const before = redis.sets;
  await wallet.webhook(db.pool, fakes.req({ data: { id: 'sess_2' } }), fakes.res());
  t.ok((await row(payer.id)).balance === 30, 'paid at Moosyl, then the webhook: 30 MRU credited');
  r = await poll(payer, mrTx.transactionId);
  await wallet.webhook(db.pool, fakes.req({ data: { id: 'sess_2' } }), fakes.res());
  t.ok((await row(payer.id)).balance === 30 && r.json().credited === true,
    'the app\'s poll and a second webhook after it: still 30, never twice');
  const entries = await db.sql("SELECT kind, amount FROM movin.wallet_entry WHERE driver_id = $1", [payer.id]);
  tx = (await db.sql('SELECT status, invoice_no FROM movin.wallet_topup WHERE transaction_id = $1', [mrTx.transactionId]))[0];
  t.ok(entries.length === 1 && entries[0].kind === 'topup' && entries[0].amount === 30,
    'one ledger entry, kind topup, +30', JSON.stringify(entries));
  t.ok(tx.status === 'paid' && /^\d+$/.test(tx.invoice_no), 'the top-up is paid and numbered for the invoice', JSON.stringify(tx));
  for (let i = 0; i < 50 && redis.sets === before; i += 1) await new Promise((ok) => setTimeout(ok, 20));
  t.ok(redis.sets > before && !JSON.parse(redis.values.get('dynamic-offer-driver-app:movin:unpaid')).includes(payer.id),
    'and dispatch\'s unpaid list is republished at once, without him');
  t.ok((await canWork(payer)) === true, 'he may work now');

  gw.paid.add(dzTx.transactionId ? (await db.sql('SELECT payment_ref FROM movin.wallet_topup WHERE transaction_id = $1', [dzTx.transactionId]))[0].payment_ref : '');
  r = await poll(dzPayer, dzTx.transactionId);
  t.ok(r.json().credited === true && (await row(dzPayer.id)).balance === 500 && r.json().currency === 'DZD',
    'DZ: Chargily says paid, the app polls: 500 DA credited', r.text);
  // The race the `credited_at IS NULL` claim exists for: the gateway's webhook
  // and the app's poll from the success page land together, both read the
  // top-up as uncredited, and both go to credit it.
  r = await topup(payer, 90);
  const raceTx = r.json();
  const raceRef = (await db.sql('SELECT payment_ref FROM movin.wallet_topup WHERE transaction_id = $1', [raceTx.transactionId]))[0].payment_ref;
  gw.paid.add(raceRef);
  const balanceBefore = (await row(payer.id)).balance;
  await Promise.all([
    wallet.webhook(db.pool, fakes.req({ data: { id: raceRef } }), fakes.res()),
    poll(payer, raceTx.transactionId),
    wallet.webhook(db.pool, fakes.req({ transactionId: raceTx.transactionId }), fakes.res()),
    poll(payer, raceTx.transactionId),
  ]);
  t.ok((await row(payer.id)).balance === balanceBefore + 90,
    'webhook, poll, webhook and poll at the same moment: 90 credited once', `${balanceBefore} -> ${(await row(payer.id)).balance}`);

  r = await poll(payer, dzTx.transactionId);
  t.ok(r.status === 404, 'another driver\'s transaction: 404, the same answer as one that does not exist');

  /* ── 4. the day ───────────────────────────────────────────────────────── */
  t.section('4. the day: taken at a ride\'s START, covers 24 h, then rides are free');
  let rideN = 0;
  const ride = async (d, status, startedAgoH) => {
    rideN += 1;
    const started = startedAgoH === null ? null : new Date(Date.now() - startedAgoH * HOUR);
    await db.sql('INSERT INTO atlas_driver_offer_bpp.ride (id, driver_id, status, trip_start_time) VALUES ($1, $2, $3, $4)',
      [`ride-${rideN}`, d.id, status, started]);
    return started;
  };
  const sweep = () => wallet.chargeStartedRides(db.pool);

  const worker = await driver(MR, { balance: 90 });
  await ride(worker, 'CANCELLED', null);
  await sweep();
  t.ok((await row(worker.id)).balance === 90, 'accepted then cancelled before pickup (no start time): not charged');

  const firstStart = await ride(worker, 'COMPLETED', 3);
  await sweep();
  let w = await row(worker.id);
  t.ok(w.balance === 60, 'first ride started: 30 MRU taken (90 -> 60)', JSON.stringify(w));
  t.ok(Math.abs(new Date(w.day_until) - (firstStart.getTime() + 24 * HOUR)) < 1000,
    'the day runs 24 h from that ride\'s own start, not from the sweep');
  await ride(worker, 'COMPLETED', 2);
  await ride(worker, 'INPROGRESS', 1);
  await sweep();
  t.ok((await row(worker.id)).balance === 60, 'two more rides inside the day: free');
  await sweep();
  t.ok((await row(worker.id)).balance === 60, 'the sweep run again: nothing taken twice');
  const ledger = await db.sql("SELECT amount FROM movin.wallet_entry WHERE driver_id = $1 ORDER BY id", [worker.id]);
  t.ok(ledger.map((e) => e.amount).join(',') === '-30,0,0', 'the ledger: -30, then 0 per covered ride', JSON.stringify(ledger));

  const lateCancel = await driver(MR, { balance: 30 });
  await ride(lateCancel, 'CANCELLED', 1);
  await sweep();
  t.ok((await row(lateCancel.id)).balance === 0, 'cancelled AFTER the start: charged -- he drove');

  const shortOne = await driver(MR, { balance: 10 });
  await ride(shortOne, 'COMPLETED', 1);
  await sweep();
  t.ok((await row(shortOne.id)).balance === -20, 'started with 10 MRU: goes to -20, never cut off with a passenger aboard');

  const dzWorker = await driver(DZ, { balance: 250 });
  await ride(dzWorker, 'COMPLETED', 1);
  await sweep();
  t.ok((await row(dzWorker.id)).balance === 150, 'DZ: the day is 100 DA (250 -> 150)');

  const nextDay = await driver(MR, { balance: 100, dayUntil: new Date(Date.now() - 2 * HOUR) });
  await ride(nextDay, 'COMPLETED', 1);
  await sweep();
  t.ok((await row(nextDay.id)).balance === 70, 'yesterday\'s day has ended: today\'s first ride opens a new one');

  const check = await db.sql('SELECT driver_id, drift FROM movin.wallet_check WHERE drift <> 0 AND driver_id = ANY($1::text[])',
    [[payer.id, dzPayer.id]]);
  t.ok(check.length === 0, 'balance and ledger agree for the drivers who only topped up (movin.wallet_check)', JSON.stringify(check));

  who.close(); gw.close(); redis.close(); await db.close();
  t.finish();
})().catch((e) => { console.error(e); process.exit(2); });
