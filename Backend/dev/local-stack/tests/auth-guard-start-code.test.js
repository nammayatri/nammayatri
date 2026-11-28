'use strict';
// auth-guard/driver-rules.js `fillStartCode` (day 6, 2026-11-15): the edges
// the golden file does not walk. The main cases -- his ride gets its real
// code, another's or a failing maps-shim goes on as sent -- are requests in
// auth-guard-routes.test.js.
//
//   node tests/auth-guard-start-code.test.js
//
// Here: maps-shim ABSENT (nothing listening at all), what maps-shim answers
// that is not a code, and requests that are not a ride start.
const http = require('http');
const path = require('path');
const fakes = require('./lib/fakes');

(async () => {
  const t = fakes.checker();
  // A maps-shim whose answer each case sets.
  let answer = [200, { code: '4821' }];
  const asked = [];
  const shim = http.createServer((req, res) => {
    let b = '';
    req.on('data', (c) => { b += c; });
    req.on('end', () => {
      asked.push({ url: req.url, body: JSON.parse(b || 'null') });
      res.writeHead(answer[0], { 'content-type': 'application/json' });
      res.end(typeof answer[1] === 'string' ? answer[1] : JSON.stringify(answer[1]));
    });
  });
  await new Promise((r) => shim.listen(0, '127.0.0.1', r));
  process.env.WALLET_URL = `http://127.0.0.1:${shim.address().port}`;
  const { fillStartCode } = require(path.join(__dirname, '..', 'stack', 'auth-guard', 'driver-rules.js'));

  const START = '/ui/driver/ride/11111111-1111-4111-8111-111111111111/start';
  const sent = Buffer.from(JSON.stringify({ rideOtp: '0000', point: { lat: 18.08, lon: -15.97 } }));
  const fill = (pathname = START, method = 'POST', body = sent) => fillStartCode(pathname, method, body, 'tok-d1');
  const otpOf = (buf) => JSON.parse(buf.toString('utf8')).rideOtp;

  t.section('what maps-shim answers');
  let out = await fill();
  t.ok(otpOf(out) === '4821' && JSON.parse(out).point.lat === 18.08, 'a code: put in rideOtp, the point kept');
  t.ok(asked.at(-1).url === '/internal/ride-start-code' && asked.at(-1).body.token === 'tok-d1'
    && asked.at(-1).body.rideId === '11111111-1111-4111-8111-111111111111', 'asked with HIS token and the ride id from the path');
  for (const [label, a] of [
    ['404 (not his, or started)', [404, { error: 'no_such_ride' }]],
    ['a code of 3 digits', [200, { code: '482' }]],
    ['a code that is not digits', [200, { code: 'abcd' }]],
    ['200 with no code', [200, {}]],
    ['not JSON', [200, 'oops']],
  ]) {
    answer = a;
    out = await fill();
    t.ok(out === sent, `${label}: the request goes on exactly as sent (the same bytes)`);
  }

  t.section('maps-shim absent');
  answer = [200, { code: '4821' }];
  await new Promise((r) => shim.close(r));
  out = await fill();
  t.ok(out === sent, 'nothing listening: the request goes on as sent, the guard guesses nothing');

  t.section('not a ride start: not touched, maps-shim not asked');
  const before = asked.length;
  for (const [label, p, m, b] of [
    ['a GET on start', START, 'GET', sent],
    ['another driver route', '/ui/driver/ride/x/end', 'POST', sent],
    ['rateCustomer', '/ui/driver/ride/x/rateCustomer', 'POST', sent],
    ['a start whose body is not JSON', START, 'POST', Buffer.from('not json')],
    ['a start whose body is a list', START, 'POST', Buffer.from('[1,2]')],
  ]) {
    const b2 = await fill(p, m, b);
    t.ok(b2 === b, `${label}: unchanged`);
  }
  t.ok(asked.length === before, 'and maps-shim was asked nothing for any of them');

  t.finish();
})().catch((e) => { console.error(e); process.exit(1); });
