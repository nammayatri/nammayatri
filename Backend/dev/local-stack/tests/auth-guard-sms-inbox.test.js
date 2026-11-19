// The SMS inbox (2026-09-29): what the office phone's forwarder posts, in
// the exact shape of the client's `send_to_webhook`. Phase 1 -- the inbox
// only, so what is tested is the token, the parsing, and that a code is
// filed under the sender whichever way the phone wrote his number.
const path = require('path');
const { spawn } = require('child_process');

const GUARD = path.join(__dirname, '..', 'stack', 'auth-guard', 'server.js');
const PORT = 18142;
const TOKEN = 'test-inbox-token';

let failed = 0;
const check = (name, ok, detail) => {
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${name}${ok ? '' : `  ${JSON.stringify(detail)}`}`);
  if (!ok) failed += 1;
};
const wait = (ms) => new Promise((r) => setTimeout(r, ms));
async function post(messages, token = TOKEN, raw) {
  const r = await fetch(`http://127.0.0.1:${PORT}/sms/inbox`, {
    method: 'POST',
    headers: { 'content-type': 'application/json', ...(token ? { authorization: `Bearer ${token}` } : {}) },
    body: raw ?? JSON.stringify({ source: 'chatty-sms', count: messages.length, messages }),
  });
  return { status: r.status, json: await r.json().catch(() => null) };
}
const health = () => fetch(`http://127.0.0.1:${PORT}/healthz`).then((r) => r.json()).then((j) => j.smsInbox);

(async () => {
  const {
    codeFrom,
    international,
  } = require('../stack/auth-guard/sms-inbox');
  check('local Mauritanian sender', international('41234567') === '22241234567');
  check('local Algerian sender with the trunk zero', international('0555123456') === '213555123456');
  check('international with +', international('+222 41 23 45 67') === '22241234567');
  check('international with 00', international('00213555123456') === '213555123456');
  check('Algerian written with the trunk zero after +213', international('+2130555123456') === '213555123456');
  check('an operator short code is kept, and matches nobody', international('1234') === '1234');
  check('the module alone holds nothing', codeFrom('+22241234567') === null);

  const guard = spawn(process.execPath, [GUARD], {
    env: {
      ...process.env,
      PORT: String(PORT),
      UPSTREAM_URL: 'http://127.0.0.1:1',
      DRIVER_UPSTREAM_URL: 'http://127.0.0.1:1',
      MOORSYL_API_KEY: '',
      WALLET_URL: 'http://127.0.0.1:1',
      SMS_INBOX_TOKEN: TOKEN,
    },
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  guard.stdout.on('data', () => {});
  guard.stderr.on('data', () => {});
  await wait(1500);

  let h = await health();
  check('healthz: configured, nothing yet, no pulse', h.configured === true && h.received === 0 && h.lastAt === null, h);

  let r = await post([{ from: '41234567', body: 'MOVIN 483920' }], null);
  check('no token -> 401', r.status === 401, r);
  r = await post([{ from: '41234567', body: 'MOVIN 483920' }], 'wrong');
  check('wrong token -> 401', r.status === 401, r);
  r = await post(null, TOKEN, '{not json');
  check('not JSON -> 400', r.status === 400, r);
  r = await post(null, TOKEN, JSON.stringify({ source: 'chatty-sms', count: 0 }));
  check('no messages list -> 400', r.status === 400, r);

  r = await post([]);
  h = await health();
  check('an empty list is a heartbeat: 200, and the pulse moves', r.status === 200 && h.lastAt !== null, { r, h });

  r = await post([
    { from: '+22241234567', body: 'MOVIN 483920', timestamp: 1790000000 },
    { sender: '0555123456', message: 'movin-111222 merci' },
    { address: 'Mauritel', text: 'Votre solde est de 10 MRU' },
    { body: 'no sender at all' },
    'not an object',
  ]);
  // An operator's named sender ("Mauritel") has no digits: it can never sign
  // anybody in, so it is skipped like a message with no sender.
  check('a batch: every message from a number is accepted, both carry a code',
    r.status === 200 && r.json.accepted === 2 && r.json.withCode === 2, r);

  h = await health();
  check('healthz counts them and never shows a number or a message',
    h.received === 2 && h.withCode === 2 && h.rejected === 2 && !JSON.stringify(h).includes('4123'), h);

  const many = Array.from({ length: 201 }, (_, i) => ({ from: '41234567', body: `x${i}` }));
  r = await post(many);
  check('more than 200 in one delivery -> 413', r.status === 413, r);

  const get = await fetch(`http://127.0.0.1:${PORT}/sms/inbox`);
  check('GET -> 405', get.status === 405);

  guard.kill();

  // Without a token configured the inbox refuses everything, loudly.
  const bare = spawn(process.execPath, [GUARD], {
    env: { ...process.env, PORT: String(PORT), UPSTREAM_URL: 'http://127.0.0.1:1', DRIVER_UPSTREAM_URL: 'http://127.0.0.1:1', MOORSYL_API_KEY: '', SMS_INBOX_TOKEN: '' },
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  bare.stdout.on('data', () => {});
  bare.stderr.on('data', () => {});
  await wait(1500);
  r = await post([{ from: '41234567', body: 'MOVIN 483920' }], '');
  check('no token configured -> 503, even with an empty bearer', r.status === 503, r);
  bare.kill();

  console.log(failed ? `\n${failed} FAILED` : '\nall passed');
  process.exit(failed ? 1 : 0);
})();
