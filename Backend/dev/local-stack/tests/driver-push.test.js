'use strict';
// The office's own push to a driver (2026-09-28): maps-shim/driver-push.js
// against a fake Google token endpoint and a fake FCM, with a service account
// generated here. Proves the token request is a JWT Google would accept (signed
// by the account's key, the right scope and audience), that the FCM message is
// data-only with the type the app draws, that the token is reused, and that an
// unknown type is refused before anything is sent.
//   node tests/driver-push.test.js
const crypto = require('crypto');
const http = require('http');
const path = require('path');

let failed = 0;
const ok = (cond, label, detail = '') => {
  console.log(`${cond ? '  ok  ' : '  BAD '} ${label}${detail ? '  -- ' + detail : ''}`);
  if (!cond) failed = 1;
};

const { privateKey, publicKey } = crypto.generateKeyPairSync('rsa', { modulusLength: 2048 });
const tokenCalls = [];
const fcmCalls = [];

const google = http.createServer((req, res) => {
  let b = '';
  req.on('data', (c) => (b += c));
  req.on('end', () => {
    if (req.url === '/token') {
      tokenCalls.push(new URLSearchParams(b));
      res.writeHead(200, { 'content-type': 'application/json' });
      return res.end(JSON.stringify({ access_token: 'ya29.fake', expires_in: 3600 }));
    }
    fcmCalls.push({ url: req.url, auth: req.headers.authorization, body: JSON.parse(b) });
    res.writeHead(200, { 'content-type': 'application/json' });
    res.end('{"name":"projects/movin-dz/messages/1"}');
  });
});

(async () => {
  await new Promise((r) => google.listen(0, '127.0.0.1', r));
  const base = `http://127.0.0.1:${google.address().port}`;
  process.env.FCM_ORIGIN = base;
  const sa = {
    type: 'service_account',
    project_id: 'movin-dz',
    client_email: 'push@movin-dz.iam.gserviceaccount.com',
    private_key: privateKey.export({ type: 'pkcs8', format: 'pem' }),
    token_uri: `${base}/token`,
  };
  const pool = {
    async query(sql) {
      if (/transporter_config/.test(sql)) {
        return { rows: [{ fcm_service_account: Buffer.from(JSON.stringify(sa)).toString('base64') }] };
      }
      if (/device_token/.test(sql)) return { rows: [{ device_token: 'fcm-token:APA91b-fake' }] };
      return { rows: [] };
    },
  };

  const push = require(path.join(__dirname, '..', 'maps-shim', 'driver-push.js'));
  const DRIVER = 'ccc203ea-1615-4d8f-960d-fcc2ca7941e6';

  const r1 = await push.notify(pool, DRIVER, 'REGISTRATION_APPROVED');
  ok(r1.ok && r1.via === 'fcm', 'approved -> sent through FCM', JSON.stringify(r1));

  const form = tokenCalls[0];
  const [h, c, sig] = (form && form.get('assertion') || '..').split('.');
  const claims = JSON.parse(Buffer.from(c || '', 'base64url').toString() || '{}');
  ok(form && form.get('grant_type') === 'urn:ietf:params:oauth:grant-type:jwt-bearer', 'the token request is a JWT bearer grant');
  ok(crypto.verify('RSA-SHA256', Buffer.from(`${h}.${c}`), publicKey, Buffer.from(sig || '', 'base64url')),
     'signed by the service account\'s own key');
  ok(claims.scope === 'https://www.googleapis.com/auth/firebase.messaging' && claims.aud === sa.token_uri
     && claims.iss === sa.client_email, 'with the FCM scope, Google as audience, the account as issuer');

  const m = fcmCalls[0];
  ok(m && m.url === '/v1/projects/movin-dz/messages:send' && m.auth === 'Bearer ya29.fake',
     'to the project\'s FCM v1 endpoint, with that token');
  ok(m && m.body.message.token === 'fcm-token:APA91b-fake' && m.body.message.data.notification_type === 'REGISTRATION_APPROVED'
     && !m.body.message.notification, 'data-only, the type the app draws in its own words');

  await push.notify(pool, DRIVER, 'REGISTRATION_REFUSED');
  ok(fcmCalls.length === 2 && tokenCalls.length === 1, 'the refusal too, and the Google token is reused',
     `${tokenCalls.length} token call(s)`);

  const bad = await push.notify(pool, DRIVER, 'NEW_RIDE_AVAILABLE');
  ok(!bad.ok && fcmCalls.length === 2, 'a type that is not ours is refused before anything is sent');

  google.close();
  console.log(failed ? 'FAILED' : 'ALL PASSED');
  process.exit(failed);
})().catch((e) => { console.error(e); process.exit(2); });
