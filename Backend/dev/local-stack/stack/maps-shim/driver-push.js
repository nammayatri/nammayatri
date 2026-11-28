'use strict';
//
// One notification of OUR OWN to one driver's phone. 2026-09-28.
//
// ── Why this exists ──────────────────────────────────────────────────────────
// Every push until now was the backend's: it builds the FCM message, signs its
// own Google token, and push-relay.js only forwards it (Android) or rewrites it
// for Apple (iPhone). Nothing the office does reaches a phone that way --
// upstream's `POST /message/send` writes a row and pushes to nobody (measured
// 2026-09-24), and enabling a driver through the dashboard sends nothing either.
//
// So a driver accepted in the console heard nothing until he opened the app,
// and the owner found out by being that driver. The app has always had the
// words for `REGISTRATION_APPROVED` ("Dossier accepté"); nobody sent it.
//
// ── How ─────────────────────────────────────────────────────────────────────
//   iPhone  (`apns:{lang}:{hex}` token)  push-relay's own Apple path, same key,
//                                        same words as a backend-sent push.
//   Android (an FCM token)               FCM v1, data-only like the backend's,
//                                        so the app draws its own words in the
//                                        phone's language. Signed with the
//                                        service account the backend itself
//                                        uses (transporter_config, base64 JSON
//                                        -- see apply-fcm.sh); the access token
//                                        is cached for its hour.
//
// Called by admin-api over POST /internal/driver-push (server.js), which the
// edge never routes here. Best effort by design: the decision is already made
// and recorded, and a phone that cannot be reached must not undo it.

const crypto = require('crypto');
const relay = require('./push-relay');

/** The only types this sends. Both are ours to send; neither is a ride event. */
const TYPES = new Set(['REGISTRATION_APPROVED', 'REGISTRATION_REFUSED']);

const FCM_ORIGIN = (process.env.FCM_ORIGIN || 'https://fcm.googleapis.com').replace(/\/$/, '');
const SCOPE = 'https://www.googleapis.com/auth/firebase.messaging';

let cached = null; // { token, expiresAt, project }

async function serviceAccount(pool) {
  const { rows } = await pool.query(
    `SELECT fcm_service_account FROM atlas_driver_offer_bpp.transporter_config
      WHERE coalesce(fcm_service_account, '') <> '' LIMIT 1`,
  );
  if (!rows[0]) throw new Error('no fcm_service_account in transporter_config');
  const raw = String(rows[0].fcm_service_account).trim();
  const text = raw.startsWith('{') ? raw : Buffer.from(raw, 'base64').toString('utf8');
  const sa = JSON.parse(text);
  if (!sa.private_key || !sa.client_email || !sa.project_id) throw new Error('service account incomplete');
  return sa;
}

const b64url = (buf) => Buffer.from(buf).toString('base64url');

/** A Google access token for FCM, from the service account's own key. */
async function accessToken(pool) {
  if (cached && cached.expiresAt - 60_000 > Date.now()) return cached;
  const sa = await serviceAccount(pool);
  const aud = sa.token_uri || 'https://oauth2.googleapis.com/token';
  const now = Math.floor(Date.now() / 1000);
  const head = b64url(JSON.stringify({ alg: 'RS256', typ: 'JWT' }));
  const claims = b64url(JSON.stringify({ iss: sa.client_email, scope: SCOPE, aud, iat: now, exp: now + 3600 }));
  const signature = crypto.createSign('RSA-SHA256').update(`${head}.${claims}`).sign(sa.private_key);
  const assertion = `${head}.${claims}.${b64url(signature)}`;

  const r = await fetch(aud, {
    method: 'POST',
    headers: { 'content-type': 'application/x-www-form-urlencoded' },
    body: new URLSearchParams({ grant_type: 'urn:ietf:params:oauth:grant-type:jwt-bearer', assertion }),
    signal: AbortSignal.timeout(15000),
  });
  const j = await r.json().catch(() => ({}));
  if (!r.ok || !j.access_token) throw new Error(`google token ${r.status} ${j.error || ''}`.trim());
  cached = { token: j.access_token, expiresAt: Date.now() + (Number(j.expires_in) || 3600) * 1000, project: sa.project_id };
  return cached;
}

/**
 * Send `type` to the driver's phone. Resolves to {ok, via, detail}; never
 * throws, because the caller has already made the decision this reports.
 */
async function notify(pool, driverId, type) {
  if (!TYPES.has(type)) return { ok: false, via: null, detail: 'type not allowed' };
  try {
    const { rows } = await pool.query(
      `SELECT device_token FROM atlas_driver_offer_bpp.person WHERE id = $1`,
      [String(driverId)],
    );
    const token = rows[0] && rows[0].device_token;
    if (!token) return { ok: false, via: null, detail: 'no device token' };

    const ios = relay.iosTarget(token);
    if (ios) {
      const r = await relay.appleNotify('driver', ios, type);
      return { ok: r.status === 200, via: 'apns', detail: `${r.status} ${r.reason || ''}`.trim() };
    }

    const auth = await accessToken(pool);
    const r = await fetch(`${FCM_ORIGIN}/v1/projects/${auth.project}/messages:send`, {
      method: 'POST',
      headers: { authorization: `Bearer ${auth.token}`, 'content-type': 'application/json' },
      body: JSON.stringify({
        message: { token, data: { notification_type: type }, android: { priority: 'high' } },
      }),
      signal: AbortSignal.timeout(15000),
    });
    const detail = r.ok ? '200' : `${r.status} ${(await r.text()).slice(0, 160)}`;
    console.log(`[driver-push] ${type} -> ${String(driverId).slice(0, 8)} fcm ${detail}`);
    return { ok: r.ok, via: 'fcm', detail };
  } catch (err) {
    console.error(`[driver-push] ${type} -> ${String(driverId).slice(0, 8)}: ${err.message}`);
    return { ok: false, via: null, detail: err.message };
  }
}

module.exports = { notify, TYPES };
