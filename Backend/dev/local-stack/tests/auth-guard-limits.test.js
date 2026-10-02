/**
 * Does the new budget actually stop texts going out, and does the per-address
 * cap actually fire? Entirely local: a fake Moorsyl counts what it is asked to
 * send, a fake backend hands out authIds, and the real guard sits between them.
 */
const http = require('http');
const { spawn } = require('child_process');
const path = require('path');

const GUARD = process.argv[2] || path.join(__dirname, '..', 'auth-guard', 'server.js');
let smsAsked = 0;

function serve(port, handler) {
  return new Promise((r) => {
    const s = http.createServer(handler);
    s.listen(port, '127.0.0.1', () => r(s));
  });
}

function post(port, pathname, body) {
  return new Promise((resolve) => {
    const data = JSON.stringify(body);
    const req = http.request(
      { host: '127.0.0.1', port, path: pathname, method: 'POST',
        headers: { 'Content-Type': 'application/json',
                   'Content-Length': Buffer.byteLength(data) } },
      (res) => {
        let t = '';
        res.on('data', (c) => (t += c));
        res.on('end', () => resolve({ status: res.statusCode, text: t }));
      },
    );
    req.on('error', (e) => resolve({ status: 0, text: String(e) }));
    req.write(data);
    req.end();
  });
}

/** Resolves once nothing is listening on the port, so one run cannot answer
 *  for the next -- which is exactly how this test lied the first time. */
function portFree(port) {
  return new Promise((resolve, reject) => {
    const tryOnce = (left) => {
      const probe = http.request({ host: '127.0.0.1', port, path: '/healthz',
                                   method: 'GET', timeout: 300 }, (res) => {
        res.resume();
        if (left <= 0) return reject(new Error(`port ${port} still answering`));
        setTimeout(() => tryOnce(left - 1), 200);
      });
      probe.on('error', () => resolve());       // nothing there: what we want
      probe.on('timeout', () => { probe.destroy(); });
      probe.end();
    };
    tryOnce(25);
  });
}

async function startGuard(env) {
  await portFree(8099);
  const p = spawn(process.execPath, [GUARD], {
    env: { ...process.env, PORT: '8099',
           UPSTREAM_URL: 'http://127.0.0.1:8097',
           DRIVER_UPSTREAM_URL: 'http://127.0.0.1:8097',
           MOORSYL_API_KEY: 'test-key',
           VERIFY_SEND_URL: 'http://127.0.0.1:8098/verify/send',
           VERIFY_CHECK_URL: 'http://127.0.0.1:8098/verify/check',
           SMS_BYPASS: '', ...env },
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  await new Promise((r) => setTimeout(r, 900));
  return p;
}

(async () => {
  let n = 0;
  await serve(8098, (req, res) => {           // fake Moorsyl
    smsAsked += 1;
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ verificationId: `v${smsAsked}` }));
  });
  await serve(8097, (req, res) => {           // fake backend
    n += 1;
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ authId: `auth-${n}` }));
  });

  const fails = [];
  const ok = (name, cond, detail = '') => {
    console.log(`   ${cond ? 'PASS' : '**FAIL**'}  ${name}  ${detail}`);
    if (!cond) fails.push(name);
  };

  /* ── 1. the global budget ─────────────────────────────────────────────── */
  console.log('\n1. Global SMS budget: 3 an hour, six different numbers try');
  smsAsked = 0;
  let g = await startGuard({ MAX_SMS_PER_HOUR: '3', MAX_SMS_PER_DAY: '99',
                             MAX_STARTS_PER_IP: '999' });
  const codes = [];
  for (let i = 1; i <= 6; i++) {
    const r = await post(8099, '/v2/auth',
      { mobileCountryCode: '+222', mobileNumber: `2200000${i}` });
    codes.push(r.status);
  }
  g.kill();
  await portFree(8099);
  console.log(`   statuses: ${codes.join(' ')}`);
  ok('the gateway was asked exactly 3 times', smsAsked === 3, `asked ${smsAsked}`);
  ok('the first three were let through', codes.slice(0, 3).every((c) => c === 200));
  ok('the rest were refused, not texted', codes.slice(3).every((c) => c !== 200),
     `got ${codes.slice(3).join(' ')}`);

  /* ── 2. the per-address cap ───────────────────────────────────────────── */
  console.log('\n2. Per-address cap: 2 an hour, five different numbers, one host');
  smsAsked = 0;
  g = await startGuard({ MAX_SMS_PER_HOUR: '999', MAX_SMS_PER_DAY: '999',
                         MAX_STARTS_PER_IP: '2' });
  const c2 = [];
  for (let i = 1; i <= 5; i++) {
    const r = await post(8099, '/v2/auth',
      { mobileCountryCode: '+222', mobileNumber: `2211000${i}` });
    c2.push(r.status);
  }
  g.kill();
  await portFree(8099);
  console.log(`   statuses: ${c2.join(' ')}`);
  ok('rotating numbers from one host is capped', c2.filter((c) => c === 429).length >= 2,
     `429s: ${c2.filter((c) => c === 429).length}`);
  ok('and the gateway was spared the rest', smsAsked <= 2, `asked ${smsAsked}`);

  /* ── 3. a normal caller is untouched ──────────────────────────────────── */
  console.log('\n3. One ordinary sign-in still works');
  smsAsked = 0;
  g = await startGuard({});
  const r3 = await post(8099, '/v2/auth',
    { mobileCountryCode: '+222', mobileNumber: '22334455' });
  g.kill();
  await portFree(8099);
  ok('answered 200 and sent one text', r3.status === 200 && smsAsked === 1,
     `status ${r3.status}, texts ${smsAsked}`);

  console.log(`\n${fails.length ? 'FAILED: ' + fails.join(', ') : 'ALL PASSED'}`);
  process.exit(fails.length ? 1 : 0);
})();
