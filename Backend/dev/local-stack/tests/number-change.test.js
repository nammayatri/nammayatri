// The maps-shim half of a number change (2026-10-03): what it refuses, and
// exactly what it writes. A fake pool records every statement; a fake backend
// says whose token it is; a fake passetto encrypts.
const http = require('http');
const crypto = require('crypto');
const fs = require('fs');
const os = require('os');
const path = require('path');

const SALT = 'test-salt';
const PASSETTO = 18521, BACKEND = 18522;
process.env.NUMBER_HASH_SALT = SALT;
process.env.PASSETTO_URL = `http://127.0.0.1:${PASSETTO}`;
const AVATARS = fs.mkdtempSync(path.join(os.tmpdir(), 'avatars-'));
process.env.AVATAR_DIR = AVATARS;
const numberChange = require(path.join(__dirname, '..', 'maps-shim', 'number-change.js'));

let failed = 0;
const check = (name, ok, detail) => {
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${name}${ok ? '' : `  ${JSON.stringify(detail)}`}`);
  if (!ok) failed += 1;
};
const hash = (n) => crypto.createHash('sha256').update(SALT + n).digest();

// Whose token: "rider-tok" -> person r1, "driver-tok" -> d1; anything else, nobody.
const backend = http.createServer((req, res) => {
  res.setHeader('content-type', 'application/json');
  const t = req.headers.token;
  if (req.url === '/v2/profile' && t === 'rider-tok') return res.end(JSON.stringify({ id: 'r1' }));
  if (req.url === '/ui/driver/profile' && t === 'driver-tok') return res.end(JSON.stringify({ id: 'd1', firstName: 'Sidi' }));
  res.statusCode = 401;
  res.end('{}');
});
const encrypted = [];
const passetto = http.createServer((req, res) => {
  let b = '';
  req.on('data', (c) => (b += c));
  req.on('end', () => {
    encrypted.push(JSON.parse(b).value);
    res.setHeader('content-type', 'application/json');
    res.end(JSON.stringify({ value: '0.1.0|0|CIPHER' }));
  });
});

/** A pool over two people: r1 (+222 41234567) and, on the same side, x9 holding 41777777. */
function fakePool() {
  const calls = [];
  return {
    calls,
    async query(sql, params) {
      calls.push({ sql, params });
      if (/SELECT mobile_country_code AS cc/.test(sql)) {
        const id = params[0];
        if (id === 'r1') return { rows: [{ cc: '+222', num: '41234567', h: hash('41234567').toString('hex') }] };
        if (id === 'd1') return { rows: [{ cc: '+222', num: '22100099', h: hash('22100099').toString('hex') }] };
        return { rows: [] };
      }
      if (/SELECT 1 FROM/.test(sql)) {
        return { rows: Buffer.compare(params[1], hash('41777777')) === 0 ? [{ '?column?': 1 }] : [] };
      }
      if (/^\s*UPDATE/.test(sql)) return { rowCount: 1 };
      if (/INSERT INTO movin.admin_audit/.test(sql)) return { rowCount: 1 };
      throw new Error(`unexpected SQL: ${sql}`);
    },
  };
}
const urls = { riderUrl: `http://127.0.0.1:${BACKEND}`, driverUrl: `http://127.0.0.1:${BACKEND}` };
const ask = (pool, body) => numberChange.run(pool, urls, { dialCode: '+222', ...body });

(async () => {
  await new Promise((r) => backend.listen(BACKEND, '127.0.0.1', r));
  await new Promise((r) => passetto.listen(PASSETTO, '127.0.0.1', r));

  let pool = fakePool();
  let r = await ask(pool, { step: 'apply', side: 'rider', token: 'stolen', number: '41999999' });
  check('a token nobody owns: not_signed_in, nothing written', r.error === 'not_signed_in'
    && !pool.calls.some((c) => /UPDATE/.test(c.sql)), r);

  r = await ask(pool, { step: 'apply', side: 'rider', token: 'rider-tok', number: '41999999', dialCode: '+213' });
  check('another country: WRONG_COUNTRY', r.error === 'WRONG_COUNTRY', r);
  r = await ask(pool, { step: 'apply', side: 'rider', token: 'rider-tok', number: '41234567' });
  check('his own number again: SAME_NUMBER', r.error === 'SAME_NUMBER', r);
  r = await ask(pool, { step: 'apply', side: 'rider', token: 'rider-tok', number: '41777777' });
  check('a number someone else holds: NUMBER_TAKEN, nothing written', r.error === 'NUMBER_TAKEN'
    && !pool.calls.some((c) => /UPDATE/.test(c.sql)), r);
  r = await ask(pool, { step: 'apply', side: 'admin', token: 'rider-tok', number: '41999999' });
  check('an unknown side: refused', r.error === 'bad_request', r);

  pool = fakePool();
  r = await ask(pool, { step: 'check', side: 'rider', token: 'rider-tok', number: '41999999' });
  check('check: yes, and writes nothing', r.ok === true && !pool.calls.some((c) => /UPDATE|INSERT/.test(c.sql)), pool.calls);

  // Her photograph, under the key her OLD number gives (avatars.js: h_ + hash).
  const keyOf = (n) => 'h_' + hash(n).toString('hex').slice(0, 32);
  fs.writeFileSync(path.join(AVATARS, keyOf('41234567') + '.jpg'), 'face');
  fs.writeFileSync(path.join(AVATARS, 'd_d1.jpg'), 'his face');

  pool = fakePool();
  r = await ask(pool, { step: 'apply', side: 'rider', token: 'rider-tok', number: '41999999' });
  check('her photograph follows her to the new number, and leaves the old one',
    fs.existsSync(path.join(AVATARS, keyOf('41999999') + '.jpg'))
      && fs.readFileSync(path.join(AVATARS, keyOf('41999999') + '.jpg'), 'utf8') === 'face'
      && !fs.existsSync(path.join(AVATARS, keyOf('41234567') + '.jpg')), fs.readdirSync(AVATARS));
  const upd = pool.calls.find((c) => /UPDATE atlas_app\.person/.test(c.sql));
  check('apply: the rider row, all three places the number lives', r.ok === true && upd
    && upd.params[0] === '+222' && upd.params[1] === '0.1.0|0|CIPHER'
    && Buffer.compare(upd.params[2], hash('41999999')) === 0 && upd.params[3] === '41999999'
    && upd.params[4] === 'r1', upd);
  check('encrypted the way the backend does: Haskell show of the text', encrypted[encrypted.length - 1] === 'S"41999999"', encrypted);
  const audit = pool.calls.find((c) => /admin_audit/.test(c.sql));
  check('one audit row, naming the account and neither number', audit && audit.params[0] === 'rider r1'
    && !JSON.stringify(audit.params).includes('4199') && !JSON.stringify(audit.params).includes('4123'), audit);

  pool = fakePool();
  r = await ask(pool, { step: 'apply', side: 'driver', token: 'driver-tok', number: '41999999' });
  check('a driver\'s photograph is keyed by his id and does not move',
    fs.existsSync(path.join(AVATARS, 'd_d1.jpg')), fs.readdirSync(AVATARS));
  check('a driver: his own schema, his own id', r.ok === true
    && pool.calls.some((c) => /UPDATE atlas_driver_offer_bpp\.person/.test(c.sql) && c.params[4] === 'd1'), pool.calls);

  r = await ask(null, { step: 'apply', side: 'rider', token: 'rider-tok', number: '41999999' });
  check('no database: not_configured', r.error === 'not_configured', r);

  backend.close();
  passetto.close();
  console.log(failed ? `\n${failed} FAILED` : '\nall passed');
  process.exit(failed ? 1 : 0);
})();
