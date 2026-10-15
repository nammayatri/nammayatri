'use strict';
// The auth guard, request by request, against a golden file. Phase 5 step 4
// (2026-10-06): the net for splitting auth-guard/server.js into one module per
// subject with NO behaviour change -- the same method as maps-shim's.
//
//   node tests/auth-guard-routes.test.js            compare with the golden file
//   node tests/auth-guard-routes.test.js --record   (re)write it -- only from code
//                                                   whose behaviour is the reference
//
// One scripted day through the real server.js: sign-ins in both countries,
// wrong codes and the lock, a resend, an SMS the passenger sends, a trusted
// phone, a number change, a driver's sign-in, the wallet gate, a rating, the
// bounds, the limits, and /healthz before and after (its counters see every
// session, start and text). For each request the golden file holds the status,
// whether retry-after was set, the body, and everything the guard said to the
// backend, Moorsyl, maps-shim and admin-api. Random values -- codes, trust
// keys, change ids -- are replaced by placeholders numbered by first sight.
const fs = require('fs');
const http = require('http');
const os = require('os');
const path = require('path');
const { spawn } = require('child_process');

const GUARD = path.join(__dirname, '..', 'stack', 'auth-guard', 'server.js');
const GOLDEN = path.join(__dirname, 'fixtures', 'auth-guard-routes.golden.json');
const PORT = 8092;
const RECORD = process.argv.includes('--record');
const INBOX_TOKEN = 'inbox-test-token';

const said = [];
const listen = (name, answer) => new Promise((r) => {
  const s = http.createServer((req, res) => {
    let b = '';
    req.on('data', (c) => { b += c; });
    req.on('end', () => {
      let body = b;
      try { body = b ? JSON.parse(b) : null; } catch { /* keep text */ }
      said.push({ to: name, method: req.method, url: req.url, body, token: req.headers.token || null });
      const [status, out] = answer(req, body);
      res.writeHead(status, { 'content-type': 'application/json' });
      res.end(JSON.stringify(out));
    });
  });
  s.listen(0, '127.0.0.1', () => r(s));
});

(async () => {
  let authN = 0;
  let tokN = 0;
  const backend = await listen('backend', (req) => {
    if (req.method === 'POST' && /^\/(v2|ui)\/auth\/?$/.test(req.url)) { authN += 1; return [200, { authId: `auth-${authN}`, attempts: 3 }]; }
    if (req.method === 'POST' && /\/verify$/.test(req.url)) { tokN += 1; return [200, { token: `tok-${tokN}`, person: { firstName: 'Test' } }]; }
    if (/\/otp\/[^/]+\/resend/.test(req.url)) return [200, { authId: 'resent', attempts: 3 }];
    return [200, { result: 'Success' }];
  });
  let vN = 0;
  const moorsyl = await listen('moorsyl', (req, body) => {
    if (req.url.endsWith('/send')) { vN += 1; return [200, { verificationId: `ver-${vN}` }]; }
    return [200, { status: body && body.code === '123456' ? 'approved' : 'pending' }];
  });
  const shim = await listen('maps-shim', (req) => {
    if (req.url.startsWith('/wallet/status')) return [200, { canWork: req.headers.token === 'tok-rich' }];
    return [200, { ok: true }];
  });
  const admin = await listen('admin-api', () => [204, {}]);
  const url = (s) => `http://127.0.0.1:${s.address().port}`;
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'guard-'));

  const guard = spawn(process.execPath, [GUARD], {
    env: {
      PATH: process.env.PATH, PORT: String(PORT),
      UPSTREAM_URL: url(backend), DRIVER_UPSTREAM_URL: url(backend),
      MOORSYL_API_KEY: 'test-key', SMS_MODE: 'verify',
      VERIFY_SEND_URL: `${url(moorsyl)}/verify/send`, VERIFY_CHECK_URL: `${url(moorsyl)}/verify/check`,
      OPEN_COUNTRIES: '+222,+213', SMS_COUNTRIES: '+222',
      WALLET_URL: url(shim), RATINGS_URL: url(admin),
      SMS_INBOX_TOKEN: INBOX_TOKEN, SMS_INBOX_NUMBERS: '+213=+213783000000',
      TRUSTED_PHONES_FILE: path.join(dir, 'trusted.json'),
      DRIVER_CODES: path.join(dir, 'driver-codes.json'),
      MAX_BODY: '4096', MAX_STARTS: '5', SMS_BYPASS: '',
    },
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  let log = '';
  guard.stdout.on('data', (c) => { log += c; });
  guard.stderr.on('data', (c) => { log += c; });

  async function call(method, p, body, headers = {}) {
    const data = body === undefined ? undefined : (typeof body === 'string' ? body : JSON.stringify(body));
    try {
      const r = await fetch(`http://127.0.0.1:${PORT}${p}`, { method, headers: { 'content-type': 'application/json', ...headers }, body: data });
      const text = await r.text();
      let json = text;
      try { json = JSON.parse(text); } catch { /* text */ }
      return { status: r.status, retryAfter: r.headers.has('retry-after'), body: json };
    } catch (e) {
      return { status: 0, body: String(e) };
    }
  }
  for (let i = 0; i < 50; i += 1) {
    if ((await call('GET', '/healthz')).status === 200) break;
    await new Promise((r) => setTimeout(r, 100));
  }

  // Random values -> placeholders, numbered by first appearance.
  const seen = new Map();
  const ph = (kind, v) => {
    const k = `${kind}:${v}`;
    if (!seen.has(k)) seen.set(k, `<${kind}${[...seen.keys()].filter((x) => x.startsWith(`${kind}:`)).length + 1}>`);
    return seen.get(k);
  };
  const RANDOM_KEYS = { deviceTrust: 'trust', code: 'code', changeId: 'change', waCode: 'code', smsInCode: 'code' };
  const norm = (v) => {
    if (Array.isArray(v)) return v.map(norm);
    if (v && typeof v === 'object') {
      const o = {};
      for (const [k, x] of Object.entries(v)) o[k] = RANDOM_KEYS[k] && typeof x === 'string' ? ph(RANDOM_KEYS[k], x) : norm(x);
      return o;
    }
    if (typeof v === 'string') {
      let s = v;
      for (const [k, p] of seen) { const raw = k.slice(k.indexOf(':') + 1); if (raw.length >= 6) s = s.split(raw).join(p); }
      // Clock readings (healthz's lastAt) are not behaviour.
      s = s.replace(/\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d+)?Z/g, '<time>');
      return s.split(url(backend)).join('<backend>').split(url(moorsyl)).join('<moorsyl>');
    }
    return v;
  };

  const results = [];
  const step = async (label, method, p, body, headers) => {
    said.length = 0;
    const r = await call(method, p, body, headers);
    await new Promise((ok) => setTimeout(ok, 60)); // fire-and-forget calls (ratings)
    const out = { label, request: norm(`${method} ${p}`), status: r.status, retryAfter: r.retryAfter, body: norm(r.body),
      said: said.map((x) => norm(x)) };
    results.push(out);
    return r;
  };

  const rider = (n, extra = {}) => ({ mobileCountryCode: '+222', mobileNumber: n, merchantId: 'm', ...extra });
  await step('health at start', 'GET', '/healthz');
  await step('channels', 'GET', '/v2/auth/channels');
  await step('sms-in countries', 'GET', '/v2/auth/sms-in/countries');
  await step('not a route', 'GET', '/dashboard/anything');

  const a1 = await step('rider start +222', 'POST', '/v2/auth', rider('22778801'));
  for (let i = 1; i <= 3; i += 1) await step(`wrong code ${i}`, 'POST', `/v2/auth/${a1.body.authId}/verify`, { otp: '000000', deviceToken: 'd1' });
  await step('right code, but locked', 'POST', `/v2/auth/${a1.body.authId}/verify`, { otp: '123456', deviceToken: 'd1' });

  const a2 = await step('rider start, second number', 'POST', '/v2/auth', rider('22778802'));
  const v2 = await step('right code', 'POST', `/v2/auth/${a2.body.authId}/verify`, { otp: '123456', deviceToken: 'd1' });
  await step('verify again after success', 'POST', `/v2/auth/${a2.body.authId}/verify`, { otp: '123456', deviceToken: 'd1' });

  const a3 = await step('start to resend', 'POST', '/v2/auth', rider('22778803'));
  await step('resend', 'POST', `/v2/auth/otp/${a3.body.authId}/resend`);
  await step('unknown session', 'POST', '/v2/auth/no-such-session/verify', { otp: '123456' });

  await step('Algeria by SMS: refused', 'POST', '/v2/auth', { mobileCountryCode: '+213', mobileNumber: '0555000101', merchantId: 'm' });
  await step('a closed country', 'POST', '/v2/auth', { mobileCountryCode: '+216', mobileNumber: '20123456', merchantId: 'm' });
  await step('not JSON', 'POST', '/v2/auth', 'not json');
  await step('too large', 'POST', '/v2/auth', { pad: 'x'.repeat(5000) });

  const s1 = await step('Algeria by SMS-in', 'POST', '/v2/auth/sms-in', { mobileCountryCode: '+213', mobileNumber: '0555000102', merchantId: 'm' });
  await step('sms-in status, before', 'GET', `/v2/auth/${s1.body.authId}/sms-in`);
  const smsCode = s1.body.smsIn && s1.body.smsIn.code;
  await step('the office SIM forwards his SMS', 'POST', '/sms/inbox',
    { source: 'chatty-sms', count: 1, messages: [{ body: `MOVIN ${smsCode}`, direction: 'incoming', sender: '0555000102' }] },
    { authorization: `Bearer ${INBOX_TOKEN}` });
  await step('the inbox, wrong token', 'POST', '/sms/inbox', { messages: [] }, { authorization: 'Bearer nope' });
  await step('sms-in status, after', 'GET', `/v2/auth/${s1.body.authId}/sms-in`);
  await step('sms-in verify', 'POST', `/v2/auth/${s1.body.authId}/verify`, { otp: smsCode, deviceToken: 'd2' });

  const key = v2.body && v2.body.deviceTrust;
  await step('trusted phone', 'POST', '/v2/auth/trusted', rider('22778802', { deviceTrust: key, deviceToken: 'd1' }));
  await step('trusted, no key', 'POST', '/v2/auth/trusted', rider('22778802', { deviceToken: 'd1' }));
  await step('trusted, wrong number', 'POST', '/v2/auth/trusted', rider('22778899', { deviceTrust: key, deviceToken: 'd1' }));

  await step('number change, not signed in', 'POST', '/v2/number/change', { mobileCountryCode: '+222', mobileNumber: '22778855', channel: 'sms' });
  const n1 = await step('number change start', 'POST', '/v2/number/change', { mobileCountryCode: '+222', mobileNumber: '22778855', channel: 'sms' }, { token: 'tok-person' });
  await step('number change status', 'GET', `/v2/number/change/${n1.body.changeId}`, undefined, { token: 'tok-person' });
  await step('number change, wrong code', 'POST', `/v2/number/change/${n1.body.changeId}/confirm`, { otp: '000000' }, { token: 'tok-person' });
  await step('number change confirm', 'POST', `/v2/number/change/${n1.body.changeId}/confirm`, { otp: '123456' }, { token: 'tok-person' });

  const d1 = await step('driver start', 'POST', '/ui/auth', { mobileCountryCode: '+222', mobileNumber: '33000001', merchantId: 'd' });
  await step('driver verify', 'POST', `/ui/auth/${d1.body.authId}/verify`, { otp: '123456', deviceToken: 'dd' });
  await step('online, no credit', 'POST', '/ui/driver/setActivity?active=true', {}, { token: 'tok-poor' });
  await step('online, credit', 'POST', '/ui/driver/setActivity?active=true', {}, { token: 'tok-rich' });
  await step('offline, no credit', 'POST', '/ui/driver/setActivity?active=false', {}, { token: 'tok-poor' });
  await step('accept, no credit', 'POST', '/ui/driver/searchRequest/quote/respond', { searchRequestId: 's', response: 'Accept' }, { token: 'tok-poor' });
  await step('rate the passenger', 'POST', '/ui/driver/ride/r1/rateCustomer', { ratingValue: 4 }, { token: 'tok-rich' });
  await step('reply too long', 'PUT', '/ui/message/m1/response', { reply: 'x'.repeat(1001) }, { token: 'tok-rich' });
  await step('reply fine', 'PUT', '/ui/message/m1/response', { reply: 'merci' }, { token: 'tok-rich' });
  await step('anything else, forwarded', 'POST', '/v2/rideSearch', { contents: {} }, { token: 'tok-person' });

  for (let i = 1; i <= 6; i += 1) await step(`starts for one number, ${i}`, 'POST', '/v2/auth', rider('22778810'));
  await step('health at end', 'GET', '/healthz');

  guard.kill();
  for (const s of [backend, moorsyl, shim, admin]) s.close();

  if (RECORD) {
    fs.mkdirSync(path.dirname(GOLDEN), { recursive: true });
    fs.writeFileSync(GOLDEN, JSON.stringify(results, null, 1) + '\n');
    console.log(`recorded ${results.length} requests to ${path.relative(process.cwd(), GOLDEN)}`);
    process.exit(0);
  }
  const golden = JSON.parse(fs.readFileSync(GOLDEN, 'utf8'));
  let failed = golden.length === results.length ? 0 : 1;
  if (failed) console.log(`  BAD  ${results.length} requests, the golden file has ${golden.length}`);
  results.forEach((r, i) => {
    const want = JSON.stringify(golden[i]);
    const got = JSON.stringify(r);
    console.log(`${want === got ? '  ok  ' : '  BAD '} ${r.label}`);
    if (want !== got) {
      failed += 1;
      console.log(`         want ${want.slice(0, 500)}\n         got  ${got.slice(0, 500)}`);
    }
  });
  if (failed) console.log('\n--- guard log ---\n' + log.slice(-1500));
  console.log(failed ? `\nFAILED (${failed})` : `\nALL PASSED -- ${results.length} requests answered exactly as recorded`);
  process.exit(failed ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(2); });
