'use strict';
// No top-up, no work -- the auth guard's half (403 WALLET_EMPTY). Phase 5
// (2026-10-06): the one decider of "may this driver work" that had no test.
//
//   node tests/auth-guard-wallet-gate.test.js
//
// The real guard runs between a fake wallet (maps-shim's /wallet/status) and a
// fake driver backend that counts what reaches it. What must hold:
//   * going online, or accepting a ride, is refused when the wallet says
//     canWork: false -- and the backend never sees the request;
//   * going OFFLINE, refusing a ride, and everything else pass, always;
//   * the wallet is asked with the driver's OWN token;
//   * a wallet that cannot be asked lets him through (fails open: dispatch
//     still skips an unpaid driver, and a paid one is never grounded by us).
const http = require('http');
const { spawn } = require('child_process');
const path = require('path');
const fakes = require('./lib/fakes');

const GUARD = path.join(__dirname, '..', 'stack', 'auth-guard', 'server.js');
const PORT = 8094;

const listen = (handler) => new Promise((r) => {
  const s = http.createServer(handler);
  s.listen(0, '127.0.0.1', () => r(s));
});

function call(method, pathname, { token, body } = {}) {
  return new Promise((resolve) => {
    const data = body === undefined ? '' : JSON.stringify(body);
    const req = http.request({
      host: '127.0.0.1', port: PORT, path: pathname, method,
      headers: { 'content-type': 'application/json', 'content-length': Buffer.byteLength(data), ...(token ? { token } : {}) },
    }, (res) => {
      let t = '';
      res.on('data', (c) => { t += c; });
      res.on('end', () => resolve({ status: res.statusCode, text: t }));
    });
    req.on('error', (e) => resolve({ status: 0, text: String(e) }));
    req.end(data);
  });
}

(async () => {
  const t = fakes.checker();

  // The wallet: `mode` decides its answer; it records whose token asked.
  const wallet = { mode: 'no', tokens: [] };
  const walletSrv = await listen((req, res) => {
    wallet.tokens.push(req.headers.token);
    if (wallet.mode === 'error') { res.writeHead(500); return res.end('{}'); }
    if (wallet.mode === 'odd') { res.writeHead(200, { 'content-type': 'application/json' }); return res.end('{"balance":0}'); }
    res.writeHead(200, { 'content-type': 'application/json' });
    res.end(JSON.stringify({ canWork: wallet.mode === 'yes' }));
  });
  // The driver backend: counts what got past the guard.
  const reached = [];
  const backend = await listen((req, res) => {
    reached.push(`${req.method} ${req.url}`);
    res.writeHead(200, { 'content-type': 'application/json' });
    res.end('{"result":"Success"}');
  });
  const walletUrl = `http://127.0.0.1:${walletSrv.address().port}`;
  const backendUrl = `http://127.0.0.1:${backend.address().port}`;

  const guard = spawn(process.execPath, [GUARD], {
    env: { ...process.env, PORT: String(PORT), UPSTREAM_URL: backendUrl, DRIVER_UPSTREAM_URL: backendUrl,
           WALLET_URL: walletUrl, MOORSYL_API_KEY: 'test-key', SMS_BYPASS: '' },
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  let log = '';
  guard.stdout.on('data', (c) => { log += c; });
  guard.stderr.on('data', (c) => { log += c; });
  for (let i = 0; i < 50; i += 1) {
    if ((await call('GET', '/healthz')).status === 200) break;
    await new Promise((r) => setTimeout(r, 100));
  }

  const ONLINE = '/ui/driver/setActivity?active=true';
  const OFFLINE = '/ui/driver/setActivity?active=false';
  const RESPOND = '/ui/driver/searchRequest/quote/respond';
  const passed = async (method, p, opts) => {
    const before = reached.length;
    const r = await call(method, p, opts);
    return { r, through: reached.length > before };
  };

  t.section('1. the wallet says he may not work');
  wallet.mode = 'no';
  let x = await passed('POST', ONLINE, { token: 'tok-driver-1' });
  t.ok(x.r.status === 403 && /WALLET_EMPTY/.test(x.r.text) && !x.through,
    'going online: 403 WALLET_EMPTY, and the backend never saw it', `${x.r.status} ${x.r.text}`);
  t.ok(wallet.tokens[wallet.tokens.length - 1] === 'tok-driver-1', 'the wallet was asked with HIS token');
  x = await passed('POST', RESPOND, { token: 'tok-driver-1', body: { searchRequestId: 's1', response: 'Accept' } });
  t.ok(x.r.status === 403 && !x.through, 'accepting a ride: 403');
  x = await passed('POST', RESPOND, { token: 'tok-driver-1', body: { searchRequestId: 's1', response: 'Reject' } });
  t.ok(x.r.status === 200 && x.through, 'refusing a ride: passes');
  x = await passed('POST', OFFLINE, { token: 'tok-driver-1' });
  t.ok(x.r.status === 200 && x.through, 'going OFFLINE: passes -- never trapped online');
  x = await passed('GET', '/ui/driver/profile', { token: 'tok-driver-1' });
  t.ok(x.r.status === 200 && x.through, 'anything else (his profile): passes');
  const asked = wallet.tokens.length;
  await passed('POST', OFFLINE, { token: 'tok-driver-1' });
  t.ok(wallet.tokens.length === asked, 'and those are not even asked about');

  t.section('2. the wallet says he may');
  wallet.mode = 'yes';
  x = await passed('POST', ONLINE, { token: 'tok-driver-2' });
  t.ok(x.r.status === 200 && x.through, 'going online: passes');
  x = await passed('POST', RESPOND, { token: 'tok-driver-2', body: { searchRequestId: 's2', response: 'Accept' } });
  t.ok(x.r.status === 200 && x.through, 'accepting: passes');

  t.section('3. the wallet cannot say: open, never grounded by us');
  wallet.mode = 'error';
  x = await passed('POST', ONLINE, { token: 'tok-driver-3' });
  t.ok(x.r.status === 200 && x.through, 'the wallet answers 500: passes');
  wallet.mode = 'odd';
  x = await passed('POST', ONLINE, { token: 'tok-driver-3' });
  t.ok(x.r.status === 200 && x.through, 'the wallet answers without canWork: passes');
  walletSrv.close();
  await new Promise((r) => setTimeout(r, 100));
  x = await passed('POST', ONLINE, { token: 'tok-driver-3' });
  t.ok(x.r.status === 200 && x.through, 'the wallet is down: passes');

  guard.kill();
  backend.close();
  if (t.failed) console.log('\n--- guard log ---\n' + log.slice(-2000));
  t.finish();
})().catch((e) => { console.error(e); process.exit(2); });
