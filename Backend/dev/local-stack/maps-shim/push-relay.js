'use strict';
//
// Push relay: Firebase for Android, Apple directly for iPhones. 2026-09-16.
//
// ── Why this exists ──────────────────────────────────────────────────────────
// Both backends send push through FCM, to whatever `fcm_url` their config row
// names (atlas_app.merchant and atlas_driver_offer_bpp.transporter_config). The
// payload is data-only for Android -- the app draws its own French/Arabic words
// from `notification_type` -- and that is why Android works.
//
// iPhones get nothing, for two reasons that stack:
//   1. The app registers the token iOS gives it, which is an APNs token, not an
//      FCM one. Firebase rejects it with INVALID_ARGUMENT.
//   2. Even with a real FCM token, iOS would draw the backend's own `apns.alert`
//      -- English text compiled into the binary ("Driver assigned!") -- for
//      every type, including the ones the client asked to keep silent. There
//      is no `mutable-content` in the payload, so the app cannot rewrite it.
//
// Fixing either in the backend is a rebuild. Instead `fcm_url` points here,
// the same trick as googleMapsUrl -> this shim:
//
//   POST /push/rider/v1/projects/{p}/messages:send    atlas_app.merchant
//   POST /push/driver/v1/projects/{p}/messages:send   transporter_config
//
// and:
//   • an Android (FCM) token is forwarded to Google byte for byte, with the
//     backend's own bearer token. Nothing about Android changes.
//   • an iPhone token is sent straight to APNs with the app's own words, in
//     the phone's language, and only for the types the app shows.
//
// ── How an iPhone token is recognised ────────────────────────────────────────
// The app registers `apns:{fr|ar|en}:{hex}` on iOS (lib/notifications.ts). The
// backend stores the string as it is and hands it back to us, so the language
// travels with the token and needs no table here. A bare 64-hex token is an
// iPhone on a build older than that change: French, the app's default. An FCM
// token is long and contains a colon, so the two cannot be confused.
//
// ── The words ────────────────────────────────────────────────────────────────
// A COPY of the shown entries in the app's lib/notifications.ts. The app stays
// the source: change a word or a `shown` flag there, then here. Only entries
// with `shown: true` are listed; anything absent is silent, exactly like the
// app's own fallback.
//
// ── The key ──────────────────────────────────────────────────────────────────
// APNS_DIR (default /data/avatars/.apns, the shim's one persistent volume):
//   key.p8        the APNs auth key from developer.apple.com, as downloaded
//   config.json   {"keyId": "...", "teamId": "3T75H4J6T7", "topic": "net.movinapp.app"}
// Never in git. avatars.js cannot serve it: it only resolves hash-named .jpg and
// .png files. Missing key = iPhone sends answer 503 and log why; Android is
// unaffected.
//
// Loopback callers only: the backends are on this host, and nothing else has
// any business asking this shim to push to a phone.

const crypto = require('crypto');
const fs = require('fs');
const http2 = require('http2');
const path = require('path');

const FCM_ORIGIN = (process.env.FCM_ORIGIN || 'https://fcm.googleapis.com').replace(/\/$/, '');
const APNS_DIR = process.env.APNS_DIR || '/data/avatars/.apns';
const MAX_BODY = 64 * 1024;

/* ── the words (copy of lib/notifications.ts, shown entries only) ─────────── */

const RIDER = {
  fr: {
    DRIVER_ASSIGNMENT: ['Course confirmée', 'Votre chauffeur arrive.'],
    DRIVER_HAS_REACHED: ['Votre chauffeur est arrivé', 'Il vous attend au point de départ.'],
  },
  ar: {
    DRIVER_ASSIGNMENT: ['تم تأكيد الرحلة', 'سائقك في الطريق.'],
    DRIVER_HAS_REACHED: ['وصل سائقك', 'ينتظرك في نقطة الانطلاق.'],
  },
  en: {
    DRIVER_ASSIGNMENT: ['Ride confirmed', 'Your driver is on the way.'],
    DRIVER_HAS_REACHED: ['Your driver has arrived', 'They are waiting at the pickup point.'],
  },
};

const DRIVER = {
  fr: {
    DRIVER_ASSIGNMENT: ['Votre offre a été acceptée', 'Rejoignez le passager au point de départ.'],
    REGISTRATION_APPROVED: ['Dossier accepté', 'Vous pouvez commencer à travailler.'],
  },
  ar: {
    DRIVER_ASSIGNMENT: ['تم قبول عرضك', 'التحق بالراكب في نقطة الانطلاق.'],
    REGISTRATION_APPROVED: ['تمت الموافقة على الملف', 'يمكنك البدء في العمل.'],
  },
  en: {
    DRIVER_ASSIGNMENT: ['Your offer was accepted', 'Meet the passenger at the pickup point.'],
    REGISTRATION_APPROVED: ['Application approved', 'You can start working.'],
  },
};

/* ── small helpers ─────────────────────────────────────────────────────────── */

function sendJson(res, code, obj) {
  const body = JSON.stringify(obj);
  res.writeHead(code, {
    'content-type': 'application/json;charset=utf-8',
    'content-length': Buffer.byteLength(body),
  });
  res.end(body);
}

/** An answer in FCM's own error shape, so the backend logs it like any other. */
const fcmError = (code, message, status) => ({ error: { code, message, status } });

const isLoopback = (addr) =>
  addr === '127.0.0.1' || addr === '::1' || addr === '::ffff:127.0.0.1';

/** `apns:ar:abcd…` -> {lang: 'ar', device: 'abcd…'}; an FCM token -> null. */
function iosTarget(token) {
  const t = String(token || '');
  const tagged = /^apns:(fr|ar|en):([0-9a-fA-F]{64,200})$/.exec(t);
  if (tagged) return { lang: tagged[1], device: tagged[2].toLowerCase() };
  if (/^[0-9a-fA-F]{64}$/.test(t)) return { lang: 'fr', device: t.toLowerCase() };
  return null;
}

/**
 * `notification_type`, wherever it sits. Searched rather than read from one
 * path, for the reason the app's readType gives: the nesting is not ours.
 */
function readType(value, depth = 5) {
  if (depth < 0 || typeof value !== 'object' || value === null) return null;
  const raw = value.notification_type ?? value.notificationType;
  if (typeof raw === 'string' && raw) return raw;
  for (const inner of Object.values(value)) {
    const found = readType(inner, depth - 1);
    if (found) return found;
  }
  return null;
}

/* ── APNs ──────────────────────────────────────────────────────────────────── */

let lastConfigError = 'not read yet';

/** Read every time it is needed: dropping the key in place needs no restart. */
function apnsConfig() {
  try {
    const cfg = JSON.parse(fs.readFileSync(path.join(APNS_DIR, 'config.json'), 'utf8'));
    if (!cfg.keyId || !cfg.teamId || !cfg.topic) {
      throw new Error('config.json needs keyId, teamId and topic');
    }
    const key = crypto.createPrivateKey(fs.readFileSync(path.join(APNS_DIR, 'key.p8')));
    lastConfigError = '';
    return {
      keyId: String(cfg.keyId),
      teamId: String(cfg.teamId),
      topic: String(cfg.topic),
      // TestFlight and App Store builds are production APNs. The sandbox host
      // is only for builds signed for development.
      host: String(cfg.host || 'https://api.push.apple.com'),
      key,
    };
  } catch (err) {
    lastConfigError = err.message;
    return null;
  }
}

const b64url = (input) => Buffer.from(input).toString('base64url');

/**
 * The provider token. Apple rejects one older than an hour and throttles one
 * minted more often than every twenty minutes, so it is kept for forty.
 */
let jwt = null;
function providerToken(cfg) {
  const now = Math.floor(Date.now() / 1000);
  if (jwt && jwt.keyId === cfg.keyId && now - jwt.at < 40 * 60) return jwt.value;
  const head = b64url(JSON.stringify({ alg: 'ES256', kid: cfg.keyId }));
  const claims = b64url(JSON.stringify({ iss: cfg.teamId, iat: now }));
  const signature = crypto.sign('sha256', Buffer.from(`${head}.${claims}`), {
    key: cfg.key,
    dsaEncoding: 'ieee-p1363',
  });
  jwt = { keyId: cfg.keyId, at: now, value: `${head}.${claims}.${b64url(signature)}` };
  return jwt.value;
}

/** One HTTP/2 connection, reused: Apple asks providers not to open one per push. */
let session = null;
function apnsSession(host) {
  if (session && !session.closed && !session.destroyed) return session;
  session = http2.connect(host);
  session.on('error', (err) => console.error(`[push] apns connection: ${err.message}`));
  session.on('close', () => {
    session = null;
  });
  // Closed when idle so a quiet night does not hold a socket Apple will drop.
  session.setTimeout(10 * 60 * 1000, () => session && session.close());
  return session;
}

function apnsSend(cfg, device, payload) {
  return new Promise((resolve) => {
    let done = false;
    const finish = (result) => {
      if (done) return;
      done = true;
      clearTimeout(timer);
      resolve(result);
    };
    const timer = setTimeout(() => finish({ status: 0, reason: 'timeout' }), 15000);

    let req;
    try {
      req = apnsSession(cfg.host).request({
        ':method': 'POST',
        ':path': `/3/device/${device}`,
        authorization: `bearer ${providerToken(cfg)}`,
        'apns-topic': cfg.topic,
        'apns-push-type': 'alert',
        'apns-priority': '10',
        // Ten minutes. "Your driver has arrived" delivered an hour late, when
        // the phone comes back into coverage, is worse than never.
        'apns-expiration': String(Math.floor(Date.now() / 1000) + 600),
        'content-type': 'application/json',
      });
    } catch (err) {
      return finish({ status: 0, reason: err.message });
    }

    let status = 0;
    let body = '';
    req.setEncoding('utf8');
    req.on('response', (headers) => {
      status = headers[':status'];
    });
    req.on('data', (chunk) => {
      body += chunk;
    });
    req.on('end', () => {
      let reason = '';
      try {
        reason = body ? JSON.parse(body).reason || '' : '';
      } catch {
        reason = body.slice(0, 80);
      }
      finish({ status, reason });
    });
    req.on('error', (err) => finish({ status: 0, reason: err.message }));
    req.end(JSON.stringify(payload));
  });
}

/* ── the two paths ─────────────────────────────────────────────────────────── */

async function toFirebase(upstreamPath, req, raw, res) {
  const headers = { 'content-type': req.headers['content-type'] || 'application/json' };
  if (req.headers.authorization) headers.authorization = req.headers.authorization;
  try {
    const upstream = await fetch(`${FCM_ORIGIN}${upstreamPath}`, {
      method: 'POST',
      headers,
      body: raw,
      signal: AbortSignal.timeout(30000),
    });
    const body = Buffer.from(await upstream.arrayBuffer());
    console.log(
      `[push] fcm ${upstream.status}` +
        (upstream.status === 200 ? '' : ` ${body.toString('utf8').slice(0, 160)}`),
    );
    res.writeHead(upstream.status, {
      'content-type': upstream.headers.get('content-type') || 'application/json',
    });
    res.end(body);
  } catch (err) {
    console.error(`[push] fcm unreachable: ${err.message}`);
    sendJson(res, 502, fcmError(502, 'FCM unreachable from the relay', 'UNAVAILABLE'));
  }
}

async function toApple(side, ios, message, res) {
  const type = readType(message);
  const words = (side === 'driver' ? DRIVER : RIDER)[ios.lang] || RIDER.fr;
  const text = type ? words[type] : null;
  const tail = `…${ios.device.slice(-6)}`;

  // Silent is a success: the app would have drawn nothing either.
  if (!text) {
    console.log(`[push] ios ${side} ${type || 'unknown'} ${ios.lang} ${tail}: silent`);
    return sendJson(res, 200, { name: `relay/silent/${Date.now()}` });
  }

  const cfg = apnsConfig();
  if (!cfg) {
    console.error(`[push] ios ${side} ${type} ${tail}: APNs not configured (${lastConfigError})`);
    return sendJson(res, 503, fcmError(503, 'APNs is not configured on the relay', 'UNAVAILABLE'));
  }

  const result = await apnsSend(cfg, ios.device, {
    aps: { alert: { title: text[0], body: text[1] }, sound: 'default' },
    // What the app's handler reads when the notification is opened.
    type,
  });
  console.log(
    `[push] ios ${side} ${type} ${ios.lang} ${tail} -> apns ${result.status} ${result.reason}`.trim(),
  );
  if (result.status === 200) return sendJson(res, 200, { name: `relay/apns/${Date.now()}` });

  // Always a 502, never a 404: FCM's "unregistered" can lead a backend to drop
  // the token, and a sandbox/production mismatch also reads as BadDeviceToken.
  // The app would never resend a token it believes the server already has.
  return sendJson(
    res,
    502,
    fcmError(502, `APNs answered ${result.status} ${result.reason}`, 'UNAVAILABLE'),
  );
}

/* ── the route ─────────────────────────────────────────────────────────────── */

function handle(req, res, url) {
  if (!isLoopback(req.socket.remoteAddress)) {
    return sendJson(res, 403, fcmError(403, 'local callers only', 'PERMISSION_DENIED'));
  }
  const match = /^\/push\/(rider|driver)(\/v1\/projects\/[^/]+\/messages:send)$/.exec(url.pathname);
  if (!match) return sendJson(res, 404, fcmError(404, 'no such push route', 'NOT_FOUND'));
  if (req.method !== 'POST') return sendJson(res, 405, fcmError(405, 'POST only', 'INVALID_ARGUMENT'));

  const [, side, upstreamPath] = match;
  const chunks = [];
  let size = 0;
  req.on('data', (chunk) => {
    size += chunk.length;
    if (size > MAX_BODY) {
      req.destroy();
      return;
    }
    chunks.push(chunk);
  });
  req.on('end', () => {
    const raw = Buffer.concat(chunks);
    let message = null;
    try {
      message = JSON.parse(raw.toString('utf8')).message || null;
    } catch {
      // Not ours to judge: Google answers a malformed body better than we can.
    }
    const ios = message && iosTarget(message.token);
    if (ios) return void toApple(side, ios, message, res);
    return void toFirebase(upstreamPath, req, raw, res);
  });
  return undefined;
}

/** For /healthz. Never the key. */
function status() {
  const cfg = apnsConfig();
  return cfg ? { apns: true, topic: cfg.topic } : { apns: false, why: lastConfigError };
}

module.exports = { handle, status, iosTarget, readType };
