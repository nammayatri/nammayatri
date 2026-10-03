// A signed-in person changing their own number (2026-10-03), end to end
// against a fake backend and a fake maps-shim. The properties: nothing is
// written until the NEW number is proved by the same means as a sign-in; only
// the caller who started it can finish it; the shim's refusals (taken, same,
// other country) reach the app by name; and afterwards this phone signs in with
// the new number without a code.
const http = require('http');
const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawn } = require('child_process');

const GUARD = path.join(__dirname, '..', 'auth-guard', 'server.js');
const UP = 18416, RIDER = 18413, SHIM = 18430, PORT = 18443;
const TOKEN = 'test-inbox-token';
const FILE = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'numchg-')), 'trusted-phones.json');

const shimCalls = [];
let shimAnswer = () => ({ ok: true });
const shim = http.createServer((req, res) => {
  let b = '';
  req.on('data', (c) => (b += c));
  req.on('end', () => {
    res.setHeader('content-type', 'application/json');
    if (req.url !== '/internal/number-change') { res.statusCode = 404; return res.end('{}'); }
    const body = JSON.parse(b);
    shimCalls.push(body);
    const a = shimAnswer(body);
    res.statusCode = a.ok ? 200 : 409;
    res.end(JSON.stringify(a));
  });
});

let n = 0;
const backend = http.createServer((req, res) => {
  let b = '';
  req.on('data', (c) => (b += c));
  req.on('end', () => {
    res.setHeader('content-type', 'application/json');
    if (req.method === 'POST' && /^\/(v2|ui)\/auth\/?$/.test(req.url)) {
      n += 1;
      return res.end(JSON.stringify({ authId: `auth-${n}` }));
    }
    if (req.method === 'POST' && /\/verify$/.test(req.url)) return res.end(JSON.stringify({ token: 'tok', person: {} }));
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
  try { json = JSON.parse(text); } catch { /* plain */ }
  return { status: r.status, json, text };
}
const inbox = (sender, body) => call('POST', '/sms/inbox', { source: 'chatty-sms', count: 1,
  messages: [{ body, direction: 'incoming', sender }] }, { authorization: `Bearer ${TOKEN}` });
const NEW = { mobileCountryCode: '+222', mobileNumber: '41999999' };
const me = { token: 'his-session' };

(async () => {
  await new Promise((r) => shim.listen(SHIM, '127.0.0.1', r));
  await new Promise((r) => backend.listen(RIDER, '127.0.0.1', r));
  const backendDriver = http.createServer(backend.listeners('request')[0]);
  await new Promise((r) => backendDriver.listen(UP, '127.0.0.1', r));
  const guard = spawn(process.execPath, [GUARD], {
    env: {
      ...process.env,
      PORT: String(PORT),
      UPSTREAM_URL: `http://127.0.0.1:${RIDER}`,
      DRIVER_UPSTREAM_URL: `http://127.0.0.1:${UP}`,
      WALLET_URL: `http://127.0.0.1:${SHIM}`,
      MOORSYL_API_KEY: '',
      OPEN_COUNTRIES: '+222,+213',
      SMS_COUNTRIES: '+222',
      SMS_INBOX_TOKEN: TOKEN,
      SMS_INBOX_NUMBERS: '+222=+22233000000',
      TRUSTED_PHONES_FILE: FILE,
    },
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  guard.stdout.on('data', () => {});
  guard.stderr.on('data', () => {});
  await wait(1500);

  const anon = await call('POST', '/v2/number/change', { ...NEW, channel: 'sms-in' });
  check('no session: 401, the shim never asked', anon.status === 401 && shimCalls.length === 0, anon);

  shimAnswer = () => ({ ok: false, error: 'NUMBER_TAKEN' });
  const taken = await call('POST', '/v2/number/change', { ...NEW, channel: 'sms-in' }, me);
  check('a number another account holds: 409 NUMBER_TAKEN, before any code', taken.status === 409
    && taken.text.includes('NUMBER_TAKEN') && !taken.json.smsIn, taken);
  shimAnswer = () => ({ ok: false, error: 'WRONG_COUNTRY' });
  const abroad = await call('POST', '/v2/number/change', { ...NEW, channel: 'sms-in' }, me);
  check('another country: 409 WRONG_COUNTRY', abroad.status === 409 && abroad.text.includes('WRONG_COUNTRY'), abroad);

  shimAnswer = () => ({ ok: true });
  shimCalls.length = 0;
  const start = await call('POST', '/v2/number/change', { ...NEW, channel: 'sms-in' }, me);
  const id = start.json && start.json.changeId;
  check('start: an id, and the code and SIM to text', start.status === 200 && /^chg/.test(id)
    && /^\d{6}$/.test(start.json.smsIn.code) && start.json.smsIn.number === '+22233000000', start);
  check('the shim was asked to CHECK, for this side, with his token and the new number',
    shimCalls.length === 1 && shimCalls[0].step === 'check' && shimCalls[0].side === 'rider'
      && shimCalls[0].token === 'his-session' && shimCalls[0].number === '41999999', shimCalls);
  const code = start.json.smsIn.code;

  let r = await call('POST', `/v2/number/change/${id}/confirm`, { otp: code }, me);
  check('knowing the code is not proof: nothing written', r.status === 400
    && !shimCalls.some((c) => c.step === 'apply'), r);
  await inbox('+22241000000', `MOVIN ${code}`);
  r = await call('GET', `/v2/number/change/${id}`, null, me);
  check('the code texted from ANOTHER number proves nothing', r.status === 200 && r.json.confirmed === false, r);

  await inbox('41999999', `MOVIN ${code}`);
  r = await call('GET', `/v2/number/change/${id}`, null, me);
  check('texted from the new number: confirmed', r.status === 200 && r.json.confirmed === true, r);
  r = await call('POST', `/v2/number/change/${id}/confirm`, { otp: code }, { token: 'someone-else' });
  check('someone else\'s session cannot finish it', r.status === 404, r);

  r = await call('POST', `/v2/number/change/${id}/confirm`, { otp: code }, me);
  const key = r.json && r.json.deviceTrust;
  check('confirm: changed, and this phone trusted for the new number', r.status === 200 && r.json.ok === true
    && typeof key === 'string', r);
  const apply = shimCalls.find((c) => c.step === 'apply');
  check('the shim was asked to APPLY, once, with the same token and number', apply
    && apply.token === 'his-session' && apply.number === '41999999' && apply.dialCode === '+222', shimCalls);
  r = await call('POST', `/v2/number/change/${id}/confirm`, { otp: code }, me);
  check('a change is spent once made', r.status === 404, r);

  const back = await call('POST', '/v2/auth/trusted', { ...NEW, merchantId: 'm', deviceTrust: key, deviceToken: 'd' });
  check('the new number now signs in on this phone with no code', back.status === 200 && back.json.token === 'tok', back);

  // Three wrong codes lock it, as at sign-in.
  const s2 = await call('POST', '/ui/number/change', { mobileCountryCode: '+222', mobileNumber: '41888888', channel: 'sms-in' }, me);
  check('the driver side starts the same way', s2.status === 200 && shimCalls[shimCalls.length - 1].side === 'driver', s2);
  let last;
  for (let i = 0; i < 3; i += 1) last = await call('POST', `/ui/number/change/${s2.json.changeId}/confirm`, { otp: '000000' }, me);
  check('three wrong codes: locked', last.status === 429 && last.text.includes('TOO_MANY_ATTEMPTS'), last);
  check('and nothing written for it', shimCalls.filter((c) => c.step === 'apply').length === 1, shimCalls);

  const sms = await call('POST', '/v2/number/change', { mobileCountryCode: '+213', mobileNumber: '0555123456', channel: 'sms' }, me);
  check('no SMS to a country we do not text', sms.status === 403 && sms.text.includes('SMS_NOT_AVAILABLE'), sms);

  guard.kill();
  shim.close();
  backend.close();
  backendDriver.close();
  console.log(failed ? `\n${failed} FAILED` : '\nall passed');
  process.exit(failed ? 1 : 0);
})();
