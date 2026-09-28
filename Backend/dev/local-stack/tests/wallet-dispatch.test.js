'use strict';
// A driver who tops up is back in dispatch AT ONCE (2026-09-28).
//
// The bug: a Chargily top-up credited the wallet but nothing republished the
// unpaid list that dispatch reads, so the driver stayed skipped for up to five
// minutes -- long enough for the owner's test ride to go past him. This drives
// the real webhook -> creditIfPaid -> restricted.refresh chain against a fake
// Chargily, a fake Postgres and a fake Redis, and checks what dispatch would
// read: the key, rewritten, without the driver.
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

const DRIVER = 'drv-dz-0001';
const KEY = 'dynamic-offer-driver-app:movin:unpaid';

// ── fake Chargily: every checkout is paid ───────────────────────────────────
const chargily = http.createServer((req, res) => {
  res.writeHead(200, { 'content-type': 'application/json' });
  res.end(JSON.stringify({ id: 'chk_1', status: 'paid' }));
});

// ── fake Redis: records every SET ───────────────────────────────────────────
const sets = [];
const redis = net.createServer((sock) => {
  sock.on('data', (buf) => {
    // RESP: *3 $3 SET $len key $len value -- keep the lines that are not headers.
    const words = buf.toString('utf8').split('\r\n').filter((l) => l !== '' && !/^[*$]/.test(l));
    if (words[0] === 'SET') sets.push({ key: words[1], value: words[2] });
    sock.write('+OK\r\n');
  });
});

// ── fake Postgres: the wallet is credited, and the driver now has credit ────
let credited = false;
const pool = {
  async query(sql) {
    if (/FROM movin\.wallet_topup/.test(sql)) {
      return { rows: [{ transaction_id: 'tx-1', driver_id: DRIVER, amount: 500, currency: 'DZD',
                        status: 'pending', payment_ref: 'chk_1', credited_at: null }] };
    }
    if (/FROM atlas_driver_offer_bpp\.person p/.test(sql)) {
      // restricted.js's policy query: after the credit he owes nothing.
      return { rows: credited ? [] : [{ id: DRIVER }] };
    }
    return { rows: [] };
  },
  async connect() {
    return {
      async query(sql) {
        if (/UPDATE movin\.wallet_topup/.test(sql)) {
          credited = true;
          return { rowCount: 1, rows: [{ driver_id: DRIVER, amount: 500 }] };
        }
        return { rowCount: 1, rows: [] };
      },
      release() {},
    };
  },
};

function fakeReq(body) {
  return Readable.from([Buffer.from(JSON.stringify(body))]);
}
function fakeRes() {
  const r = { status: null, body: null };
  r.writeHead = (s) => { r.status = s; };
  r.end = (b) => { r.body = b; };
  return r;
}

(async () => {
  await new Promise((r) => chargily.listen(0, '127.0.0.1', r));
  await new Promise((r) => redis.listen(0, '127.0.0.1', r));
  process.env.CHARGILY_BASE = `http://127.0.0.1:${chargily.address().port}`;
  process.env.CHARGILY_SECRET_KEY = 'test';
  process.env.REDIS_HOST = '127.0.0.1';
  process.env.REDIS_PORT = String(redis.address().port);

  const wallet = require(path.join(__dirname, '..', 'maps-shim', 'wallet.js'));

  const res = fakeRes();
  await wallet.webhook(pool, fakeReq({ data: { id: 'chk_1' } }), res);
  // The refresh is fired, not awaited, by design; give it its round trip.
  for (let i = 0; i < 50 && !sets.some((s) => s.key === KEY); i += 1) {
    await new Promise((r) => setTimeout(r, 20));
  }

  ok(res.status === 200, 'the webhook answers 200');
  ok(credited, 'the top-up was credited');
  const unpaid = sets.filter((s) => s.key === KEY);
  ok(unpaid.length === 1, 'the unpaid list dispatch reads was republished at once', `${unpaid.length} SET(s)`);
  ok(unpaid.length === 1 && !JSON.parse(unpaid[0].value).includes(DRIVER),
     'and the driver who just paid is no longer on it', unpaid[0] && unpaid[0].value);

  chargily.close();
  redis.close();
  console.log(failed ? 'FAILED' : 'ALL PASSED');
  process.exit(failed);
})().catch((e) => {
  console.error(e);
  process.exit(2);
});
