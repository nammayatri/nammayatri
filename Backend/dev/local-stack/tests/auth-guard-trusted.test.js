// Signing back in on a phone that already proved the number (2026-10-03), end
// to end against a fake backend. The property: the key a verify hands out
// opens that number -- passenger AND driver -- with no code, and nothing else
// does: not the number alone, not a made-up key, not the key for another number.
const http = require('http');
const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawn } = require('child_process');

const GUARD = path.join(__dirname, '..', 'auth-guard', 'server.js');
const UP = 18316, RIDER = 18313, PORT = 18343;
const TOKEN = 'test-inbox-token';
const FILE = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'trusted-')), 'trusted-phones.json');
let starts = 0;
const verifies = [];

const fake = http.createServer((req, res) => {
  let b = '';
  req.on('data', (c) => (b += c));
  req.on('end', () => {
    res.setHeader('content-type', 'application/json');
    if (req.method === 'POST' && /^\/(v2|ui)\/auth\/?$/.test(req.url)) {
      starts += 1;
      return res.end(JSON.stringify({ authId: `auth-${starts}`, attempts: 3 }));
    }
    if (req.method === 'POST' && /\/verify$/.test(req.url)) {
      verifies.push({ url: req.url, body: JSON.parse(b) });
      return res.end(JSON.stringify({ token: `tok-${verifies.length}`, person: { firstName: 'Sidi' } }));
    }
    res.statusCode = 404;
    res.end('{}');
  });
});

let failed = 0;
const check = (name, ok, detail) => {
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${name}${ok ? '' : `  ${JSON.stringify(detail)}`}`);
  if (!ok) failed += 1;
};
const wait = (ms) => new Promise((r) => setTimeout(r, ms));
async function call(method, url, body, headers = {}) {
  const r = await fetch(`http://127.0.0.1:${PORT}${url}`, {
    method,
    headers: { 'content-type': 'application/json', ...headers },
    body: body ? JSON.stringify(body) : undefined,
  });
  const text = await r.text();
  let json = null;
  try { json = JSON.parse(text); } catch { /* plain text */ }
  return { status: r.status, json, text };
}
const me = { mobileCountryCode: '+222', mobileNumber: '41234567' };

(async () => {
  await new Promise((r) => fake.listen(RIDER, '127.0.0.1', r));
  const fakeDriver = http.createServer(fake.listeners('request')[0]);
  await new Promise((r) => fakeDriver.listen(UP, '127.0.0.1', r));
  const guard = spawn(process.execPath, [GUARD], {
    env: {
      ...process.env,
      PORT: String(PORT),
      UPSTREAM_URL: `http://127.0.0.1:${RIDER}`,
      DRIVER_UPSTREAM_URL: `http://127.0.0.1:${UP}`,
      MOORSYL_API_KEY: '',
      OPEN_COUNTRIES: '+222,+213',
      SMS_COUNTRIES: '+222',
      WALLET_URL: 'http://127.0.0.1:1',
      SMS_INBOX_TOKEN: TOKEN,
      SMS_INBOX_NUMBERS: '+222=+22233000000',
      TRUSTED_PHONES_FILE: FILE,
    },
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  guard.stdout.on('data', () => {});
  guard.stderr.on('data', () => {});
  await wait(1500);

  // ── a first sign-in, the ordinary way: an SMS he sends us ─────────────────
  const start = await call('POST', '/v2/auth/sms-in', { ...me, merchantId: 'm' });
  const code = start.json.smsIn.code;
  await call('POST', '/sms/inbox', { source: 'chatty-sms', count: 1, messages: [
    { body: `MOVIN ${code}`, direction: 'incoming', sender: '41234567' },
  ] }, { authorization: `Bearer ${TOKEN}` });
  const v = await call('POST', `/v2/auth/${start.json.authId}/verify`, { otp: code, deviceToken: 'd1' });
  const key = v.json && v.json.deviceTrust;
  check('a verify hands this phone a key, beside the token', v.status === 200 && v.json.token === 'tok-1'
    && typeof key === 'string' && key.length >= 40, v.json);

  const onDisk = fs.readFileSync(FILE, 'utf8');
  check('the server keeps only its hash', !onDisk.includes(key) && onDisk.includes('+22241234567'), onDisk);

  // ── after « Se déconnecter »: the same phone, no code ─────────────────────
  const before = { starts, verifies: verifies.length };
  const back = await call('POST', '/v2/auth/trusted', { ...me, merchantId: 'm', deviceTrust: key, deviceToken: 'd1' });
  check('the key signs him back in, with no code', back.status === 200 && back.json.token === 'tok-2'
    && back.json.person.firstName === 'Sidi' && back.json.deviceTrust === key, back);
  check('one start and one verify upstream, with the backend\'s fixed code',
    starts === before.starts + 1 && verifies.length === before.verifies + 1
      && verifies[verifies.length - 1].body.otp === '7891' && verifies[verifies.length - 1].body.deviceToken === 'd1',
    verifies[verifies.length - 1]);

  // ── the boss's case: the same number, now as a driver ─────────────────────
  const drv = await call('POST', '/ui/auth/trusted', { ...me, merchantId: 'd', deviceTrust: key, deviceToken: 'd1' });
  check('the same key opens the driver side too', drv.status === 200 && /^tok-/.test(drv.json.token)
    && verifies[verifies.length - 1].url.startsWith('/ui/auth/'), drv);

  // ── and nothing else opens anything ───────────────────────────────────────
  const n = verifies.length;
  const none = await call('POST', '/v2/auth/trusted', { ...me, merchantId: 'm', deviceToken: 'd1' });
  check('the number alone: 401', none.status === 401 && none.text.includes('PHONE_NOT_TRUSTED'), none);
  const made = await call('POST', '/v2/auth/trusted', { ...me, merchantId: 'm', deviceTrust: 'x'.repeat(43), deviceToken: 'd1' });
  check('a made-up key: 401', made.status === 401, made);
  const other = await call('POST', '/v2/auth/trusted', { mobileCountryCode: '+222', mobileNumber: '41999999', merchantId: 'm', deviceTrust: key, deviceToken: 'd1' });
  check('his key for ANOTHER number: 401', other.status === 401, other);
  const closed = await call('POST', '/v2/auth/trusted', { mobileCountryCode: '+216', mobileNumber: '41234567', merchantId: 'm', deviceTrust: key });
  check('a closed country: 403, key or not', closed.status === 403 && closed.text.includes('COUNTRY_NOT_OPEN'), closed);
  check('none of those reached the backend\'s verify', verifies.length === n, verifies.length);

  const h = await call('GET', '/healthz');
  check('healthz counts trusted phones, never which', h.json.trustedPhones && h.json.trustedPhones.phones === 1
    && !h.text.includes('41234567'), h.json.trustedPhones);

  guard.kill();
  fake.close();
  fakeDriver.close();
  console.log(failed ? `\n${failed} FAILED` : '\nall passed');
  process.exit(failed ? 1 : 0);
})();
