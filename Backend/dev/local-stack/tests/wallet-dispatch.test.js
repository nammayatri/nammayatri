'use strict';
// A driver who tops up is back in dispatch AT ONCE (2026-09-28) -- in BOTH
// countries: Chargily in Algeria (DA) and Moosyl in Mauritania (MRU).
//
// The bug: a top-up credited the wallet but nothing republished the unpaid
// list that dispatch reads, so the driver stayed skipped for up to five
// minutes -- long enough for the owner's test ride to go past him. This drives
// the real webhook -> creditIfPaid -> restricted.refresh chain against a fake
// gateway, a fake Postgres and a fake Redis, once per gateway, and checks what
// dispatch would read: the key, rewritten, without the driver.
//   node tests/wallet-dispatch.test.js
const http = require('http');
const net = require('net');
const path = require('path');
const { Readable } = require('stream');

let failed = 0;
const ok = (cond, label, detail = '') => {
  console.log(`${cond ? '  ok  ' : '  BAD '} ${label}${detail ? '  -- ' + detail : ''}`);
  if (!cond) failed = 1;
};

const KEY = 'dynamic-offer-driver-app:movin:unpaid';

// ── fake gateways, one server: every checkout is paid ───────────────────────
const asked = [];
const gateway = http.createServer((req, res) => {
  asked.push(req.url);
  res.writeHead(200, { 'content-type': 'application/json' });
  if (req.url.startsWith('/checkout-session/public/')) {
    // Moosyl: {data: {status: 'completed'}}
    return res.end(JSON.stringify({ data: { status: 'completed' } }));
  }
  // Chargily: {status: 'paid'}
  return res.end(JSON.stringify({ id: 'chk_1', status: 'paid' }));
});

// ── fake Redis: records every SET ───────────────────────────────────────────
let sets = [];
const redis = net.createServer((sock) => {
  sock.on('data', (buf) => {
    // RESP: *3 $3 SET $len key $len value -- keep the lines that are not headers.
    const words = buf.toString('utf8').split('\r\n').filter((l) => l !== '' && !/^[*$]/.test(l));
    if (words[0] === 'SET') sets.push({ key: words[1], value: words[2] });
    sock.write('+OK\r\n');
  });
});

/** A fake Postgres holding one unpaid driver and one pending top-up. */
function poolFor(driver, currency, ref) {
  const state = { credited: false };
  return {
    state,
    async query(sql) {
      if (/FROM movin\.wallet_topup/.test(sql)) {
        return { rows: [{ transaction_id: `tx-${currency}`, driver_id: driver, amount: 500, currency,
                          status: 'pending', payment_ref: ref, credited_at: null }] };
      }
      if (/FROM atlas_driver_offer_bpp\.person p/.test(sql)) {
        // restricted.js's policy query: after the credit he owes nothing.
        return { rows: state.credited ? [] : [{ id: driver }] };
      }
      return { rows: [] };
    },
    async connect() {
      return {
        async query(sql) {
          if (/UPDATE movin\.wallet_topup/.test(sql)) {
            state.credited = true;
            return { rowCount: 1, rows: [{ driver_id: driver, amount: 500 }] };
          }
          return { rowCount: 1, rows: [] };
        },
        release() {},
      };
    },
  };
}

const fakeReq = (body) => Readable.from([Buffer.from(JSON.stringify(body))]);
function fakeRes() {
  const r = { status: null };
  r.writeHead = (s) => { r.status = s; };
  r.end = () => {};
  return r;
}

async function scenario(wallet, name, driver, currency, ref, statusPath) {
  console.log(`\n${name}`);
  sets = [];
  asked.length = 0;
  const pool = poolFor(driver, currency, ref);
  const res = fakeRes();
  await wallet.webhook(pool, fakeReq({ data: { id: ref } }), res);
  // The refresh is fired, not awaited, by design; give it its round trip.
  for (let i = 0; i < 50 && !sets.some((s) => s.key === KEY); i += 1) {
    await new Promise((r) => setTimeout(r, 20));
  }
  ok(asked.some((u) => u.startsWith(statusPath)), `the payment was checked with ${name.split(' ')[0]}`, asked.join(', '));
  ok(res.status === 200 && pool.state.credited, 'the top-up was credited');
  const unpaid = sets.filter((s) => s.key === KEY);
  ok(unpaid.length === 1, 'the unpaid list dispatch reads was republished at once', `${unpaid.length} SET(s)`);
  ok(unpaid.length === 1 && !JSON.parse(unpaid[0].value).includes(driver),
     'and the driver who just paid is no longer on it', unpaid[0] && unpaid[0].value);
}

(async () => {
  await new Promise((r) => gateway.listen(0, '127.0.0.1', r));
  await new Promise((r) => redis.listen(0, '127.0.0.1', r));
  const base = `http://127.0.0.1:${gateway.address().port}`;
  process.env.CHARGILY_BASE = base;
  process.env.CHARGILY_SECRET_KEY = 'test';
  process.env.MOOSYL_BASE = base;
  process.env.MOOSYL_SECRET_KEY = 'test';
  process.env.REDIS_HOST = '127.0.0.1';
  process.env.REDIS_PORT = String(redis.address().port);

  const wallet = require(path.join(__dirname, '..', 'stack', 'maps-shim', 'wallet.js'));

  await scenario(wallet, 'Chargily (Algeria, DA)', 'drv-dz-0001', 'DZD', 'chk_1', '/checkouts/');
  await scenario(wallet, 'Moosyl (Mauritania, MRU)', 'drv-mr-0001', 'MRU', 'sess_1', '/checkout-session/public/');

  gateway.close();
  redis.close();
  console.log(failed ? '\nFAILED' : '\nALL PASSED');
  process.exit(failed);
})().catch((e) => {
  console.error(e);
  process.exit(2);
});
