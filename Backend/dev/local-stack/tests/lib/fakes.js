'use strict';
// What surrounds the shims, faked on loopback: the Haskell apps (who owns a
// token), Moosyl and Chargily (is this payment paid?), and Redis (what
// dispatch reads). The database is NOT here -- see db.js, it is real.
const http = require('http');
const net = require('net');
const { Readable } = require('stream');

const listen = (server) =>
  new Promise((r) => server.listen(0, '127.0.0.1', () => r(`http://127.0.0.1:${server.address().port}`)));

/**
 * Both backends' "who is this token": the driver app's /ui/driver/profile
 * and the rider app's /v2/profile. `tokens` maps a token to a person id.
 */
async function backends() {
  const drivers = new Map();
  const riders = new Map();
  const server = http.createServer((req, res) => {
    const token = req.headers.token;
    const map = req.url.startsWith('/ui/driver/profile') ? drivers
      : req.url.startsWith('/v2/profile') ? riders : null;
    const id = map && map.get(token);
    if (!id) { res.writeHead(401); return res.end('{}'); }
    res.writeHead(200, { 'content-type': 'application/json' });
    res.end(JSON.stringify({ id, firstName: 'Test' }));
  });
  const url = await listen(server);
  return { url, drivers, riders, close: () => server.close() };
}

/**
 * Moosyl and Chargily, one server. Every checkout it creates is answered from
 * `paid` (a set of ids): paid -> completed/paid, else open/pending. `refuse`
 * makes creation fail, as a gateway down or a bad key would.
 */
async function gateways() {
  const g = { paid: new Set(), refuse: false, created: [], asked: [] };
  let n = 0;
  const server = http.createServer((req, res) => {
    let body = '';
    req.on('data', (c) => { body += c; });
    req.on('end', () => {
      const json = (status, obj) => { res.writeHead(status, { 'content-type': 'application/json' }); res.end(JSON.stringify(obj)); };
      if (req.method === 'POST') {
        if (g.refuse) return json(500, { error: 'refused (test)' });
        n += 1;
        const b = body ? JSON.parse(body) : {};
        g.created.push({ url: req.url, body: b, auth: req.headers.authorization });
        if (req.url === '/payment-request') return json(200, { data: { id: `pr_${n}` } });
        if (req.url === '/checkout-session') return json(200, { checkoutUrl: `https://pay.example/s_${n}`, data: { id: `sess_${n}` } });
        if (req.url === '/checkouts') return json(200, { id: `chk_${n}`, checkout_url: `http://pay.example/c_${n}` });
        return json(404, {});
      }
      g.asked.push(req.url);
      let m = /^\/checkout-session\/public\/(.+)$/.exec(req.url);
      if (m) return json(200, { data: { status: g.paid.has(decodeURIComponent(m[1])) ? 'completed' : 'open' } });
      m = /^\/checkouts\/(.+)$/.exec(req.url);
      if (m) return json(200, { id: m[1], status: g.paid.has(decodeURIComponent(m[1])) ? 'paid' : 'pending' });
      return json(404, {});
    });
  });
  g.url = await listen(server);
  g.close = () => server.close();
  return g;
}

/** Redis, as far as `SET key value` goes: keeps the last value of each key. */
async function redis() {
  const r = { values: new Map(), sets: 0, down: false };
  const server = net.createServer((sock) => {
    sock.on('data', (buf) => {
      if (r.down) return sock.destroy();
      const words = buf.toString('utf8').split('\r\n').filter((l) => l !== '' && !/^[*$]/.test(l));
      if (words[0] === 'SET') { r.values.set(words[1], words[2]); r.sets += 1; }
      sock.write('+OK\r\n');
    });
  });
  await new Promise((ok) => server.listen(0, '127.0.0.1', ok));
  r.port = server.address().port;
  r.close = () => server.close();
  return r;
}

/** A response object the shims' `send` can write to. */
function res() {
  const r = { status: null, headers: null, text: '' };
  r.writeHead = (s, h) => { r.status = s; r.headers = h || {}; };
  r.end = (t) => { r.text = t === undefined ? '' : String(t); };
  r.json = () => JSON.parse(r.text);
  return r;
}

const req = (body) => Readable.from([Buffer.from(typeof body === 'string' ? body : JSON.stringify(body))]);

/** The test runner the older tests use: a line per check, exit 1 on any miss. */
function checker() {
  const c = { failed: 0 };
  c.ok = (cond, label, detail = '') => {
    console.log(`${cond ? '  ok  ' : '  BAD '} ${label}${!cond && detail ? '  -- ' + detail : ''}`);
    if (!cond) c.failed += 1;
  };
  c.section = (t) => console.log(`\n${t}`);
  c.finish = () => {
    console.log(c.failed ? `\nFAILED (${c.failed})` : '\nALL PASSED');
    process.exit(c.failed ? 1 : 0);
  };
  return c;
}

module.exports = { backends, gateways, redis, res, req, checker };
