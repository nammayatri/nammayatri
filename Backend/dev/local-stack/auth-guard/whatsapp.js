'use strict';
/**
 * The WhatsApp Cloud API webhook -- where Meta delivers the messages people
 * send to Movin's WhatsApp number (+213 783 07 91 61, "MovinApp").
 *
 * ── Why here, in the guard ──────────────────────────────────────────────────
 * The plan (2026-09-27) is sign-in by WhatsApp with no template: the app opens
 * WhatsApp to our number with `MOVIN 483920` already typed, the passenger
 * presses Send, and the message proves he holds that number -- the sender is
 * WhatsApp's word, not something he typed. Messages people send us are free
 * and need no approval from Meta. The guard owns sign-in sessions, so the
 * message lands next to the session waiting for it.
 *
 * This file is the receiving half only. What it keeps is read by nothing yet;
 * the sign-in path that consumes it is the next piece of work.
 *
 * ── The two halves of Meta's contract ───────────────────────────────────────
 *   GET  ?hub.mode=subscribe&hub.verify_token=…&hub.challenge=…
 *        Meta checking the URL when somebody presses "Verify and save". The
 *        token is ours (WHATSAPP_VERIFY_TOKEN); the answer is the challenge,
 *        as plain text, and nothing else.
 *   POST the messages, signed: X-Hub-Signature-256 is an HMAC-SHA256 of the
 *        raw body with the APP SECRET (not the access token). Checked whenever
 *        WHATSAPP_APP_SECRET is set. Until it is, messages are kept marked
 *        `signed: false`, and the sign-in path must refuse those -- anybody
 *        could post an unsigned one.
 *
 * Always 200 for a POST we could read, fast: Meta retries a slow or failed
 * delivery for days, and a retry storm is not a thing to invite.
 *
 * ── What is logged ──────────────────────────────────────────────────────────
 * Counts and the last three digits of a sender, never a message: this is
 * people's WhatsApp, and a log line is the one place nobody protects.
 */
const crypto = require('crypto');

const VERIFY_TOKEN = process.env.WHATSAPP_VERIFY_TOKEN || '';
const APP_SECRET = process.env.WHATSAPP_APP_SECRET || '';

/** How long a received message waits for the sign-in that asked for it. */
const KEEP_MS = 10 * 60 * 1000;
/** Enough for a busy morning; a flood cannot grow it past this. */
const KEEP_MAX = 5000;

/** Sender (digits, no +) → the latest message from him. */
const inbox = new Map();
let received = 0;
let rejected = 0;

/** `MOVIN 483920`, however it was spaced or cased. */
const CODE = /\bMOVIN\s*[-:]?\s*(\d{6})\b/i;

function prune(now) {
  for (const [from, m] of inbox) if (now - m.at > KEEP_MS) inbox.delete(from);
  while (inbox.size > KEEP_MAX) inbox.delete(inbox.keys().next().value);
}

/** The text messages in one delivery. Anything else (status, image) is counted and skipped. */
function messagesIn(payload) {
  const out = [];
  for (const entry of payload?.entry ?? []) {
    for (const change of entry?.changes ?? []) {
      if (change?.field !== 'messages') continue;
      for (const m of change?.value?.messages ?? []) {
        if (typeof m?.from !== 'string') continue;
        out.push({
          from: m.from.replace(/\D/g, ''),
          id: m.id,
          type: m.type,
          text: m.type === 'text' ? String(m.text?.body ?? '') : '',
          at: Number(m.timestamp) * 1000 || Date.now(),
        });
      }
    }
  }
  return out;
}

function signatureOk(raw, header) {
  if (typeof header !== 'string' || !header.startsWith('sha256=')) return false;
  const want = crypto.createHmac('sha256', APP_SECRET).update(raw).digest();
  const got = Buffer.from(header.slice(7), 'hex');
  return got.length === want.length && crypto.timingSafeEqual(got, want);
}

const mask = (from) => `…${from.slice(-3)}`;

/** GET: Meta's check of the URL. Returns [status, text]. */
function handshake(url) {
  const q = new URLSearchParams(url.split('?')[1] || '');
  if (!VERIFY_TOKEN) return [503, 'webhook not configured'];
  if (q.get('hub.mode') === 'subscribe' && q.get('hub.verify_token') === VERIFY_TOKEN) {
    console.log('[whatsapp] Meta verified the webhook URL');
    return [200, q.get('hub.challenge') || ''];
  }
  console.warn('[whatsapp] webhook check refused: wrong verify token');
  return [403, 'forbidden'];
}

/** POST: a delivery. Returns [status, text]. */
function deliver(raw, headers) {
  const signed = APP_SECRET !== '';
  if (signed && !signatureOk(raw, headers['x-hub-signature-256'])) {
    rejected += 1;
    console.warn('[whatsapp] delivery refused: bad signature');
    return [401, 'bad signature'];
  }
  let payload;
  try {
    payload = JSON.parse(raw.toString('utf8'));
  } catch {
    return [400, 'not json'];
  }
  const now = Date.now();
  prune(now);
  for (const m of messagesIn(payload)) {
    received += 1;
    const code = CODE.exec(m.text)?.[1] ?? null;
    inbox.set(m.from, { code, id: m.id, at: now, signed });
    console.log(
      `[whatsapp] message from ${mask(m.from)} (${m.type}${code ? ', with a sign-in code' : ''}` +
        `${signed ? '' : ', UNSIGNED'})`,
    );
  }
  return [200, 'ok'];
}

/**
 * For the sign-in path, when it exists: the code this number sent in the last
 * ten minutes, or null. Unsigned messages are never an answer.
 */
function codeFrom(number) {
  const m = inbox.get(String(number).replace(/\D/g, ''));
  if (!m || !m.signed || Date.now() - m.at > KEEP_MS) return null;
  return m.code;
}

/** For /healthz: whether it is set up, and how much has come in. Never a secret. */
function health() {
  return {
    verifyToken: VERIFY_TOKEN !== '',
    signatureChecked: APP_SECRET !== '',
    received,
    rejected,
    waiting: inbox.size,
  };
}

module.exports = { handshake, deliver, codeFrom, health };
