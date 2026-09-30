// Sign-in by an SMS the passenger SENDS (2026-09-29), end to end against a
// fake backend. The WhatsApp test's property, over the office SIM: knowing
// the code opens nothing; only the office phone forwarding it FROM the number
// signing in does. And a country with no SIM has no such sign-in at all.
const http = require('http');
const path = require('path');
const { spawn } = require('child_process');

const GUARD = path.join(__dirname, '..', 'auth-guard', 'server.js');
const UP = 18216, RIDER = 18213, PORT = 18143;
const TOKEN = 'test-inbox-token';
let lastVerifyBody = null;
let n = 0;

const fake = http.createServer((req, res) => {
  let b = '';
  req.on('data', (c) => (b += c));
  req.on('end', () => {
    res.setHeader('content-type', 'application/json');
    if (req.method === 'POST' && /^\/(v2|ui)\/auth\/?$/.test(req.url)) {
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
// Exactly the client's forwarder: `send_to_webhook(messages)`.
const forward = (from, body, token = TOKEN) =>
  call('POST', '/sms/inbox', { source: 'chatty-sms', count: 1, messages: [{ from, body }] },
    { authorization: `Bearer ${token}` });

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
      // Mauritania has a SIM; Algeria does not (yet).
      SMS_INBOX_NUMBERS: '+222=+22233000000',
    },
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  guard.stdout.on('data', () => {});
  guard.stderr.on('data', () => {});
  await wait(1500);

  const c = await call('GET', '/v2/auth/sms-in/countries');
  check('countries: Mauritania only', c.status === 200 && JSON.stringify(c.json) === '{"countries":["+222"]}', c);
  const cd = await call('GET', '/ui/auth/sms-in/countries');
  check('the driver side answers the same', cd.status === 200 && JSON.stringify(cd.json) === '{"countries":["+222"]}', cd);

  // The phone screen's question (2026-09-30): which ways in, per country --
  // from the same settings the starts obey. Here: SMS for Mauritania only,
  // the SIM for Mauritania, and no WhatsApp configured in this test.
  const ch = await call('GET', '/v2/auth/channels');
  check('channels: SMS and SIM per country, WhatsApp as configured',
    ch.status === 200 && JSON.stringify(ch.json) === '{"sms":["+222"],"smsIn":["+222"],"whatsapp":false}', ch);
  const chd = await call('GET', '/ui/auth/channels');
  check('the driver side answers the same', chd.status === 200 && JSON.stringify(chd.json) === JSON.stringify(ch.json), chd);

  const before = n;
  const dz = await call('POST', '/v2/auth/sms-in', { mobileCountryCode: '+213', mobileNumber: '0555123456', merchantId: 'm' });
  check('no SIM for Algeria -> 503 SMS_IN_UNAVAILABLE, backend never asked',
    dz.status === 503 && dz.text.includes('SMS_IN_UNAVAILABLE') && n === before, dz);

  const start = await call('POST', '/v2/auth/sms-in', { mobileCountryCode: '+222', mobileNumber: '41234567', merchantId: 'm' });
  const si = start.json && start.json.smsIn;
  check('start -> 200 with an authId, a code, the SIM and the text',
    start.status === 200 && start.json.authId && /^\d{6}$/.test(si && si.code)
      && si.number === '+22233000000' && si.text === `MOVIN ${si.code}`, start);
  const id = start.json.authId;
  const code = si.code;
  const status = () => call('GET', `/v2/auth/${id}/sms-in`).then((r) => r.json && r.json.confirmed);

  check('not confirmed before any SMS', (await status()) === false);
  let v = await call('POST', `/v2/auth/${id}/verify`, { otp: code, deviceToken: 'd' });
  check('knowing the code alone opens nothing', v.status === 400, v);

  const bad = await forward('41234567', `MOVIN ${code}`, 'wrong-token');
  check('a forward with the wrong token is refused', bad.status === 401, bad);
  check('and confirms nothing', (await status()) === false);

  await forward('+22241999999', `MOVIN ${code}`);
  check('the right code from ANOTHER number confirms nothing', (await status()) === false);

  await forward('41234567', 'MOVIN 000000');
  check('a wrong code from the right number confirms nothing', (await status()) === false);

  // The client's forwarder posts what the office phone SENT too, with the
  // recipient in `sender` -- its real sample, 2026-09-29. Never a proof.
  await call('POST', '/sms/inbox', { source: 'chatty-sms', count: 1, messages: [
    { id: 13, uid: 'x', body: `Movin ${code}`, direction: 'outgoing', timestamp: 1790688560,
      status: null, subject: null, thread: '+22241234567', sender: '+22241234567' },
  ] }, { authorization: `Bearer ${TOKEN}` });
  check('a text the office phone SENT to him confirms nothing', (await status()) === false);

  // The phone writes a local sender the way the network gave it.
  // The client's exact shape, received this time.
  await call('POST', '/sms/inbox', { source: 'chatty-sms', count: 1, messages: [
    { id: 14, uid: 'y', body: `Movin ${code}`, direction: 'incoming', timestamp: 1790688600,
      status: null, subject: null, thread: '41234567', sender: '41234567' },
  ] }, { authorization: `Bearer ${TOKEN}` });
  check("the client's real shape, incoming, local sender -> confirmed", (await status()) === true);

  // A second text a moment later must not bury the code.
  await forward('41234567', 'Merci !');
  check('a later text without a code does not undo it', (await status()) === true);

  v = await call('POST', `/v2/auth/${id}/verify`, { otp: code, deviceToken: 'd' });
  check('verify -> 200', v.status === 200, v);
  check('the backend receives its fixed code, never the SMS one', lastVerifyBody && lastVerifyBody.otp === '7891', lastVerifyBody);
  const again = await call('GET', `/v2/auth/${id}/sms-in`);
  check('the session is spent after a sign-in', again.status === 404, again);

  // The driver side, same channel.
  const ds = await call('POST', '/ui/auth/sms-in', { mobileCountryCode: '+222', mobileNumber: '31234567', merchantId: 'm' });
  check('driver start -> 200 with the SIM', ds.status === 200 && ds.json.smsIn && ds.json.smsIn.number === '+22233000000', ds);
  await forward('+22231234567', `MOVIN ${ds.json.smsIn.code}`);
  v = await call('POST', `/ui/auth/${ds.json.authId}/verify`, { otp: ds.json.smsIn.code, deviceToken: 'd' });
  check('driver verify -> 200 once forwarded', v.status === 200, v);

  const h = await call('GET', '/healthz');
  check('healthz lists the countries with a SIM, never the number',
    JSON.stringify(h.json.smsInbox.countries) === '["+222"]' && !h.text.includes('33000000'), h.json.smsInbox);
  check('healthz counts the outgoing one it ignored', h.json.smsInbox.outgoing === 1, h.json.smsInbox);

  guard.kill();
  fake.close();
  fakeDriver.close();
  console.log(failed ? `\n${failed} FAILED` : '\nall passed');
  process.exit(failed ? 1 : 0);
})();
