'use strict';
// Local test of maps-shim/push-relay.js with a fake Firebase and a fake APNs.
// No network, no real key, no server: proves the request shapes before deploy.
//   node ~/push-relay-test.js
const crypto = require('crypto');
const fs = require('fs');
const http = require('http');
const http2 = require('http2');
const os = require('os');
const path = require('path');
const { execFileSync } = require('child_process');

let failed = 0;
const ok = (cond, label, detail = '') => {
  console.log(`${cond ? '  ok  ' : '  BAD '} ${label}${detail ? '  -- ' + detail : ''}`);
  if (!cond) failed = 1;
};

const tmp = fs.mkdtempSync(path.join(os.tmpdir(), 'relay-'));
const apnsDir = path.join(tmp, 'apns');

// ── fake Firebase ───────────────────────────────────────────────────────────
let fcmSeen = null;
const fcm = http.createServer((req, res) => {
  const chunks = [];
  req.on('data', (c) => chunks.push(c));
  req.on('end', () => {
    fcmSeen = { url: req.url, auth: req.headers.authorization, body: Buffer.concat(chunks).toString() };
    res.writeHead(200, { 'content-type': 'application/json' });
    res.end('{"name":"projects/movin-dz/messages/0:fake"}');
  });
});

// ── fake APNs (TLS http2, self-signed) ──────────────────────────────────────
execFileSync('openssl', ['req', '-x509', '-newkey', 'rsa:2048', '-nodes', '-days', '1',
  '-subj', '/CN=localhost', '-keyout', path.join(tmp, 'tls.key'), '-out', path.join(tmp, 'tls.crt')],
  { stdio: 'ignore' });
let apnsSeen = null;
let apnsStatus = 200;
const apns = http2.createSecureServer({
  key: fs.readFileSync(path.join(tmp, 'tls.key')),
  cert: fs.readFileSync(path.join(tmp, 'tls.crt')),
});
apns.on('stream', (stream, headers) => {
  let body = '';
  stream.setEncoding('utf8');
  stream.on('data', (c) => (body += c));
  stream.on('end', () => {
    apnsSeen = { headers, body };
    stream.respond({ ':status': apnsStatus });
    stream.end(apnsStatus === 200 ? '' : '{"reason":"BadDeviceToken"}');
  });
});

function post(port, pathName, obj, auth) {
  return new Promise((resolve, reject) => {
    const body = JSON.stringify(obj);
    const req = http.request({ host: '127.0.0.1', port, path: pathName, method: 'POST',
      headers: { 'content-type': 'application/json', ...(auth ? { authorization: auth } : {}) } },
    (res) => {
      let data = '';
      res.on('data', (c) => (data += c));
      res.on('end', () => resolve({ status: res.statusCode, body: data }));
    });
    req.on('error', reject);
    req.end(body);
  });
}

const listen = (server) => new Promise((r) => server.listen(0, '127.0.0.1', () => r(server.address().port)));

(async () => {
  const fcmPort = await listen(fcm);
  const apnsPort = await listen(apns);

  process.env.FCM_ORIGIN = `http://127.0.0.1:${fcmPort}`;
  process.env.APNS_DIR = apnsDir;
  process.env.NODE_TLS_REJECT_UNAUTHORIZED = '0';
  // Relative to this file, so the test runs from a clone as well as from home.
  const relay = require(path.join(__dirname, '..', 'maps-shim', 'push-relay.js'));

  const server = http.createServer((req, res) => relay.handle(req, res, new URL(req.url, 'http://x')));
  const port = await listen(server);
  const P = '/v1/projects/movin-dz/messages:send';
  const hex = 'ab'.repeat(32);
  const android = (token, type) => ({ message: { token, android: { data: { notification_type: type } } } });

  console.log('== parsing');
  ok(relay.iosTarget(`apns:ar:${hex}`)?.lang === 'ar', 'tagged token -> ar');
  ok(relay.iosTarget(hex)?.lang === 'fr', 'bare 64-hex token -> fr (older builds)');
  ok(relay.iosTarget('fcmTok:APA91b' + 'x'.repeat(140)) === null, 'FCM token -> not iOS');
  ok(relay.readType({ a: { android: { data: { notification_type: 'TRIP_STARTED' } } } }) === 'TRIP_STARTED',
    'readType finds a nested notification_type');

  console.log('== Android token: forwarded to Firebase untouched');
  const fcmToken = 'dXk:APA91b' + 'y'.repeat(140);
  const sent = android(fcmToken, 'DRIVER_ASSIGNMENT');
  let r = await post(port, `/push/rider${P}`, sent, 'Bearer google-oauth');
  ok(r.status === 200, 'relay answers with Firebase status', `${r.status} ${r.body}`);
  ok(fcmSeen && fcmSeen.url === P, 'path forwarded', fcmSeen && fcmSeen.url);
  ok(fcmSeen && fcmSeen.auth === 'Bearer google-oauth', 'authorization forwarded');
  ok(fcmSeen && fcmSeen.body === JSON.stringify(sent), 'body forwarded byte for byte');

  console.log('== iPhone token, no key installed');
  fcmSeen = null;
  r = await post(port, `/push/rider${P}`, android(`apns:fr:${hex}`, 'DRIVER_ASSIGNMENT'), 'Bearer g');
  ok(r.status === 503, 'answers 503 when APNs is not configured', r.body);
  ok(fcmSeen === null, 'and never goes to Firebase');

  console.log('== iPhone token, silent type');
  r = await post(port, `/push/rider${P}`, android(`apns:fr:${hex}`, 'TRIP_STARTED'), 'Bearer g');
  ok(r.status === 200, 'silent type -> 200 and nothing sent', r.body);

  console.log('== iPhone token, key installed');
  fs.mkdirSync(apnsDir, { recursive: true });
  const { privateKey, publicKey } = crypto.generateKeyPairSync('ec', { namedCurve: 'P-256' });
  fs.writeFileSync(path.join(apnsDir, 'key.p8'), privateKey.export({ type: 'pkcs8', format: 'pem' }));
  fs.writeFileSync(path.join(apnsDir, 'config.json'), JSON.stringify({
    keyId: 'KEY1234567', teamId: '3T75H4J6T7', topic: 'net.movinapp.app',
    host: `https://127.0.0.1:${apnsPort}`,
  }));
  ok(relay.status().apns === true, 'status() reports apns: true');

  r = await post(port, `/push/driver${P}`, android(`apns:ar:${hex}`, 'DRIVER_ASSIGNMENT'), 'Bearer g');
  ok(r.status === 200, 'driver DRIVER_ASSIGNMENT delivered', `${r.status} ${r.body}`);
  const h = apnsSeen && apnsSeen.headers;
  ok(h && h[':path'] === `/3/device/${hex}`, 'APNs path is the bare device token', h && h[':path']);
  ok(h && h['apns-topic'] === 'net.movinapp.app', 'apns-topic');
  ok(h && h['apns-push-type'] === 'alert', 'apns-push-type alert');
  const payload = apnsSeen && JSON.parse(apnsSeen.body);
  ok(payload && payload.aps.alert.title === 'تم قبول عرضك', 'Arabic DRIVER title', payload && payload.aps.alert.title);
  ok(payload && payload.type === 'DRIVER_ASSIGNMENT', 'type carried for the app');

  // The provider token must verify against the public half of the key.
  const [head, claims, sig] = String(h && h.authorization).replace(/^bearer /, '').split('.');
  const verified = crypto.verify('sha256', Buffer.from(`${head}.${claims}`),
    { key: publicKey, dsaEncoding: 'ieee-p1363' }, Buffer.from(sig || '', 'base64url'));
  const decoded = JSON.parse(Buffer.from(head, 'base64url').toString());
  const claimSet = JSON.parse(Buffer.from(claims, 'base64url').toString());
  ok(verified, 'ES256 provider token verifies');
  ok(decoded.kid === 'KEY1234567' && claimSet.iss === '3T75H4J6T7', 'kid and iss', JSON.stringify([decoded, claimSet]));

  r = await post(port, `/push/rider${P}`, android(hex, 'DRIVER_HAS_REACHED'), 'Bearer g');
  ok(r.status === 200 && JSON.parse(apnsSeen.body).aps.alert.title === 'Votre chauffeur est arrivé',
    'bare token (older build) -> French rider text');

  apnsStatus = 400;
  r = await post(port, `/push/rider${P}`, android(`apns:en:${hex}`, 'DRIVER_ASSIGNMENT'), 'Bearer g');
  ok(r.status === 502 && /BadDeviceToken/.test(r.body), 'APNs rejection -> 502 with the reason', r.body);

  console.log('== routing guards');
  r = await post(port, `/push/nobody${P}`, sent, 'Bearer g');
  ok(r.status === 404, 'unknown side -> 404');

  console.log(failed ? '\nSOMETHING FAILED' : '\nALL RELAY TESTS PASSED');
  for (const s of [server, fcm, apns]) s.close();
  fs.rmSync(tmp, { recursive: true, force: true });
  process.exit(failed);
})().catch((e) => {
  console.error(e);
  process.exit(1);
});
