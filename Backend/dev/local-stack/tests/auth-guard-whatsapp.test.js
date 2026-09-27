// Sign-in by WhatsApp, end to end against a fake driver backend (2026-09-27).
// The one property that matters: knowing the code opens nothing. Only a
// delivery Meta SIGNED, from the number signing in, does.
const http = require('http');
const path = require('path');
const crypto = require('crypto');
const { spawn } = require('child_process');

const GUARD = path.join(__dirname, '..', 'auth-guard', 'server.js');
const UP = 18116, RIDER = 18113, PORT = 18141;
const APP_SECRET = 'test-app-secret';
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

let failed = 0;
const check = (name, ok, detail) => {
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${name}${ok ? '' : `  ${JSON.stringify(detail)}`}`);
  if (!ok) failed += 1;
};
const wait = (ms) => new Promise((r) => setTimeout(r, ms));
async function call(method, url, body, headers = {}, raw) {
  const r = await fetch(`http://127.0.0.1:${PORT}${url}`, {
    method,
    headers: { 'content-type': 'application/json', ...headers },
    body: raw ?? (body ? JSON.stringify(body) : undefined),
  });
  const text = await r.text();
  let json = null;
  try { json = JSON.parse(text); } catch { /* plain text */ }
  return { status: r.status, json, text };
}
function delivery(from, text) {
  return JSON.stringify({
    object: 'whatsapp_business_account',
    entry: [{ changes: [{ field: 'messages', value: { messages: [
      { from, id: `wamid.${Math.random()}`, timestamp: String(Math.floor(Date.now() / 1000)), type: 'text', text: { body: text } },
    ] } }] }],
  });
}
const sign = (raw) => 'sha256=' + crypto.createHmac('sha256', APP_SECRET).update(raw).digest('hex');
const deliver = (raw, signed) =>
  call('POST', '/whatsapp/webhook', null, signed ? { 'x-hub-signature-256': sign(raw) } : {}, raw);

(async () => {
  await new Promise((r) => fake.listen(UP, '127.0.0.1', r));
  const guard = spawn(process.execPath, [GUARD], {
    env: {
      ...process.env,
      PORT: String(PORT),
      UPSTREAM_URL: `http://127.0.0.1:${RIDER}`,
      DRIVER_UPSTREAM_URL: `http://127.0.0.1:${UP}`,
      MOORSYL_API_KEY: '',
      OPEN_COUNTRIES: '+222,+213',
      WALLET_URL: 'http://127.0.0.1:1',
      WHATSAPP_VERIFY_TOKEN: 'vt',
      WHATSAPP_APP_SECRET: APP_SECRET,
      WHATSAPP_NUMBER: '213783079161',
      // No token: nobody is answered on WhatsApp in a test.
    },
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  guard.stdout.on('data', () => {});
  guard.stderr.on('data', () => {});
  await wait(1500);

  // An Algerian number, which the guard keeps WITH its trunk zero and
  // WhatsApp sends without -- the normalisation is part of what is tested.
  const start = await call('POST', '/ui/auth/whatsapp', { mobileCountryCode: '+213', mobileNumber: '0555123456', merchantId: 'm' });
  const wa = start.json && start.json.whatsapp;
  check('start -> 200 with an authId, a code and a link', start.status === 200 && start.json.authId && /^\d{6}$/.test(wa && wa.code) && /^https:\/\/wa\.me\/213783079161\?text=MOVIN%20\d{6}$/.test(wa.link), start);
  const id = start.json.authId;
  const code = wa.code;
  const status = () => call('GET', `/ui/auth/${id}/whatsapp`).then((r) => r.json && r.json.confirmed);

  check('not confirmed before any message', (await status()) === false);

  let v = await call('POST', `/ui/auth/${id}/verify`, { otp: code, deviceToken: 'd' });
  check('knowing the code alone opens nothing', v.status === 400, v);

  await deliver(delivery('213555123456', `MOVIN ${code}`), false);
  check('an UNSIGNED delivery confirms nothing', (await status()) === false);

  let r = await deliver(delivery('213555123456', `MOVIN ${code}`), false).then(() => null);
  const forged = await call('POST', '/whatsapp/webhook', null, { 'x-hub-signature-256': 'sha256=' + '0'.repeat(64) }, delivery('213555123456', `MOVIN ${code}`));
  check('a forged signature is refused', forged.status === 401, forged);

  await deliver(delivery('213555999999', `MOVIN ${code}`), true);
  check('the right code from ANOTHER number confirms nothing', (await status()) === false);

  await deliver(delivery('213555123456', 'MOVIN 000000'), true);
  check('a wrong code from the right number confirms nothing', (await status()) === false);

  await deliver(delivery('213555123456', `Bonjour, movin ${code} merci`), true);
  check('signed, right number, right code (any spacing, any case) -> confirmed', (await status()) === true);

  v = await call('POST', `/ui/auth/${id}/verify`, { otp: code, deviceToken: 'd' });
  check('verify -> 200', v.status === 200, v);
  check('the backend receives its fixed code, never the WhatsApp one', lastVerifyBody && lastVerifyBody.otp === '7891', lastVerifyBody);

  const again = await call('GET', `/ui/auth/${id}/whatsapp`);
  check('the session is spent after a sign-in', again.status === 404, again);

  const h = await call('GET', '/healthz');
  check('healthz says WhatsApp is ready', h.json.whatsapp.ready === true, h.json.whatsapp);

  guard.kill();
  // And a guard without the app secret refuses to start one at all.
  const bare = spawn(process.execPath, [GUARD], {
    env: { ...process.env, PORT: String(PORT), UPSTREAM_URL: `http://127.0.0.1:${RIDER}`, DRIVER_UPSTREAM_URL: `http://127.0.0.1:${UP}`, MOORSYL_API_KEY: '', OPEN_COUNTRIES: '+222', WALLET_URL: 'http://127.0.0.1:1', WHATSAPP_VERIFY_TOKEN: 'vt', WHATSAPP_NUMBER: '213783079161' },
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  bare.stdout.on('data', () => {});
  bare.stderr.on('data', () => {});
  await wait(1500);
  const noSecret = await call('POST', '/ui/auth/whatsapp', { mobileCountryCode: '+222', mobileNumber: '22123456', merchantId: 'm' });
  check('no app secret -> 503 WHATSAPP_UNAVAILABLE, no session opened', noSecret.status === 503 && n === 1, { noSecret, n });
  bare.kill();
  fake.close();
  console.log(failed ? `${failed} FAILED` : 'ALL PASSED');
  process.exitCode = failed ? 1 : 0;
})().catch((e) => {
  console.error(e);
  process.exit(2);
});
