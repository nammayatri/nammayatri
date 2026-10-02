// Local test of the auth-guard driver sign-up change, against a fake driver backend.
// No network beyond localhost, no SMS key (so a real send is impossible).
const http = require('http');
const fs = require('fs');
const os = require('os');
const path = require('path');
const crypto = require('crypto');
const { spawn } = require('child_process');

const GUARD = process.argv[2] || path.join(__dirname, '..', 'auth-guard', 'server.js');
const UP = 18016, RIDER = 18013;
let lastVerifyBody = null;
let n = 0;

const fake = http.createServer((req, res) => {
  let b = '';
  req.on('data', (c) => (b += c));
  req.on('end', () => {
    res.setHeader('content-type', 'application/json');
    if (req.method === 'POST' && /^\/ui\/auth\/?$/.test(req.url)) {
      n += 1;
      return res.end(JSON.stringify({ authId: `auth-${n}`, attempts: 3 }));
    }
    if (req.method === 'POST' && /\/verify$/.test(req.url)) {
      lastVerifyBody = JSON.parse(b);
      return res.end(JSON.stringify({ token: 'tok', person: {} }));
    }
    res.statusCode = 404;
    res.end('{}');
  });
});

const tmp = fs.mkdtempSync(path.join(os.tmpdir(), 'guard-'));
const codesFile = path.join(tmp, 'codes.json');
const enrolled = '+22222100001';
const salt = 'abc';
const hash = crypto.createHash('sha256').update(`${salt}:${enrolled}:654321`).digest('hex');
fs.writeFileSync(codesFile, JSON.stringify({ codes: { [enrolled]: { salt, hash } } }));

function startGuard(port, signup) {
  const env = {
    ...process.env,
    PORT: String(port),
    UPSTREAM_URL: `http://127.0.0.1:${RIDER}`,
    DRIVER_UPSTREAM_URL: `http://127.0.0.1:${UP}`,
    DRIVER_CODES: codesFile,
    MOORSYL_API_KEY: '',
    SMS_BYPASS: '+22222778899',
    OPEN_COUNTRIES: '+222',
    WALLET_URL: 'http://127.0.0.1:1',
  };
  if (signup) env.DRIVER_SIGNUP = signup;
  const p = spawn(process.execPath, [GUARD], { env, stdio: ['ignore', 'pipe', 'pipe'] });
  p.stdout.on('data', () => {});
  p.stderr.on('data', () => {});
  return p;
}

async function call(port, method, url, body) {
  const r = await fetch(`http://127.0.0.1:${port}${url}`, {
    method,
    headers: { 'content-type': 'application/json' },
    body: body ? JSON.stringify(body) : undefined,
  });
  return { status: r.status, json: await r.json().catch(() => null) };
}

const start = (port, nsn) =>
  call(port, 'POST', '/ui/auth', { mobileCountryCode: '+222', mobileNumber: nsn, merchantId: 'm' });

let failed = 0;
function check(name, ok, detail) {
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${name}${ok ? '' : `  ${JSON.stringify(detail)}`}`);
  if (!ok) failed += 1;
}

const wait = (ms) => new Promise((r) => setTimeout(r, ms));

(async () => {
  await new Promise((r) => fake.listen(UP, '127.0.0.1', r));
  const open = startGuard(18131, undefined);
  const closed = startGuard(18132, 'closed');
  await wait(1500);

  const h = await call(18131, 'GET', '/healthz');
  check('healthz reports signupOpen true', h.json.routes[1].signupOpen === true, h.json.routes);
  const hc = await call(18132, 'GET', '/healthz');
  check('closed guard reports signupOpen false', hc.json.routes[1].signupOpen === false, hc.json.routes);

  // 1. open: a new driver on the test list gets a session and signs in with the test code
  let s = await start(18131, '22778899');
  check('open: new driver (test number) start -> 200', s.status === 200 && s.json.authId, s);
  let v = await call(18131, 'POST', `/ui/auth/${s.json.authId}/verify`, { otp: '000000', deviceToken: 'd' });
  check('open: wrong code refused', v.status !== 200, v);
  v = await call(18131, 'POST', `/ui/auth/${s.json.authId}/verify`, { otp: '111111', deviceToken: 'd' });
  check('open: test code -> 200', v.status === 200, v);
  check('open: backend receives its fixed code, not the typed one', lastVerifyBody && lastVerifyBody.otp === '7891', lastVerifyBody);

  // 2. open: a new driver whose SMS cannot be sent is told so, like a passenger
  s = await start(18131, '33445566');
  check('open: new driver, SMS fails -> 502 SMS_SEND_FAILED', s.status === 502, s);

  // 3. open: an enrolled driver still signs in with his personal code even with no SMS
  s = await start(18131, '22100001');
  check('open: enrolled driver start -> 200 despite no SMS', s.status === 200 && s.json.authId, s);
  v = await call(18131, 'POST', `/ui/auth/${s.json.authId}/verify`, { otp: '654321', deviceToken: 'd' });
  check('open: enrolled driver personal code -> 200', v.status === 200, v);

  // 4. closed: the old refusal is back
  s = await start(18132, '33445566');
  check('closed: new driver -> 403 NOT_REGISTERED', s.status === 403, s);
  s = await start(18132, '22100001');
  check('closed: enrolled driver -> 200', s.status === 200, s);

  // 5. closed country still refused first
  const dz = await call(18131, 'POST', '/ui/auth', { mobileCountryCode: '+213', mobileNumber: '0666123456', merchantId: 'm' });
  check('open: closed country still 403', dz.status === 403, dz);

  open.kill();
  closed.kill();
  fake.close();
  console.log(failed ? `${failed} FAILED` : 'ALL PASSED');
  process.exitCode = failed ? 1 : 0;
})().catch((e) => {
  console.error(e);
  process.exit(2);
});
