'use strict';
/**
 * The SMS inbox -- where the office's own phone forwards the texts people send
 * to Movin's SIM (2026-09-29).
 *
 * ── Why ─────────────────────────────────────────────────────────────────────
 * The boss's plan (2026-09-28): stop paying Moorsyl to text a code OUT, and
 * have the passenger text `MOVIN 483920` IN, to a SIM in an Android phone in
 * the office -- the WhatsApp sign-in, over SMS. The phone runs the client's
 * forwarder ("chatty-sms"), which POSTs what it received here. Same shape as
 * whatsapp.js on purpose: an inbox by sender, and `codeFrom()` for the
 * sign-in path.
 *
 * ── Phase 2: the sign-in (built 2026-09-29, dormant until a SIM is set) ─────
 * server.js starts a sign-in by SMS with `expect()`, which hands the app the
 * code and the SIM to text it to; the status route and verify read
 * `codeFrom()`. Which SIM serves which country is `SMS_INBOX_NUMBERS`
 * (`+222=+22233000000,+213=+213…`) -- one per country, or every text is an
 * international one the passenger pays for. A country with no SIM there has
 * no such sign-in: the start answers 503 and the app hides the button (it asks
 * `countries()` first). Moorsyl stays until this is proved on the real phone
 * (owner's decision, 2026-09-28).
 *
 * ── The contract with the forwarder ─────────────────────────────────────────
 *   POST /sms/inbox
 *   Authorization: Bearer <SMS_INBOX_TOKEN>
 *   { "source": "chatty-sms", "count": 1,
 *     "messages": [ { "from": "+22241234567", "body": "MOVIN 483920",
 *                     "timestamp": 1790000000, "sim": "+22233000000" } ] }
 * The field names inside a message are read leniently -- `from`, `sender`,
 * `address`, `number`, `phone` for the sender; `body`, `text`, `message`,
 * `content` for the words -- because Android apps disagree and the forwarder
 * is not ours. The sender may be local (`41234567`, `0555123456`) or
 * international; see `international()`. An empty list is a heartbeat: it
 * moves `lastAt`, which is how a dead phone becomes visible.
 *
 * ── Trust ───────────────────────────────────────────────────────────────────
 * The bearer token proves the POST came from our phone. It does NOT prove the
 * sender: an SMS sender number can be forged on some international routes,
 * which Meta's signature rules out for WhatsApp. That is the known cost of
 * this channel, told to the client on 2026-09-28.
 *
 * Logged: counts and the last three digits of a sender, never a message.
 */
const crypto = require('crypto');
const { normal } = require('./whatsapp');

const TOKEN = process.env.SMS_INBOX_TOKEN || '';

/**
 * Dialling code → the SIM a passenger of that country texts, `+` and digits.
 * Malformed pairs are dropped rather than trusted: a wrong number here sends
 * people's texts to a stranger.
 */
const SIMS = new Map(
  (process.env.SMS_INBOX_NUMBERS || '')
    .split(',')
    .map((pair) => pair.split('=').map((x) => x.trim()))
    .filter(([cc, n]) => /^\+\d{1,4}$/.test(cc || '') && /^\+\d{8,15}$/.test(n || '')),
);

/** Same window and ceiling as the WhatsApp inbox. */
const KEEP_MS = 10 * 60 * 1000;
const KEEP_MAX = 5000;
/** One delivery; the forwarder batches, but not a whole phone's history. */
const MAX_PER_DELIVERY = 200;

const CODE = /\bMOVIN\s*[-:]?\s*(\d{6})\b/i;

/** Sender (international digits, no trunk zero) → the latest message from him. */
const inbox = new Map();
let received = 0;
let withCode = 0;
let rejected = 0;
let outgoing = 0;
let lastAt = null;

/**
 * A text the office phone SENT, not one it received. The client's forwarder
 * reads the whole SMS database and posts both, with `direction` telling them
 * apart and `sender` holding the OTHER party for an outgoing one (its first
 * real sample, 2026-09-29: `direction: 'outgoing'`, `sender` = the number it
 * was sent to). Believing one would let anything the office phone sends sign
 * in the person it was sent to -- so only what came IN is proof. A message
 * with no `direction` at all is taken as received: that is what a forwarder
 * that only reads the inbox sends.
 */
const OUTGOING = /^(outgoing|outbox|sent|out)$/i;

function prune(now) {
  for (const [from, m] of inbox) if (now - m.at > KEEP_MS) inbox.delete(from);
  while (inbox.size > KEEP_MAX) inbox.delete(inbox.keys().next().value);
}

/**
 * The sender as international digits. A phone shows a local sender the way
 * the network gave it, and the two countries cannot be confused: a
 * Mauritanian mobile is eight digits starting 2/3/4, an Algerian one nine
 * starting 5/6/7, or ten with the trunk zero. Anything else is kept as it
 * came, and simply matches no sign-in.
 */
function international(raw) {
  const s = String(raw || '').trim();
  let d = s.replace(/\D/g, '');
  if (s.startsWith('+')) return normal(d);
  if (d.startsWith('00')) return normal(d.slice(2));
  if (/^[234]\d{7}$/.test(d)) return `222${d}`;
  if (/^0[567]\d{8}$/.test(d)) return `213${d.slice(1)}`;
  if (/^[567]\d{8}$/.test(d)) return `213${d}`;
  return normal(d);
}

const pick = (m, keys) => {
  for (const k of keys) if (m[k] != null && m[k] !== '') return String(m[k]);
  return '';
};

function tokenOk(header) {
  if (!TOKEN || typeof header !== 'string' || !header.startsWith('Bearer ')) return false;
  // Hashed first so the comparison is constant-time whatever the lengths.
  const h = (v) => crypto.createHash('sha256').update(v).digest();
  return crypto.timingSafeEqual(h(header.slice(7).trim()), h(TOKEN));
}

const mask = (from) => `…${from.slice(-3)}`;

/** POST: a delivery from the phone. Returns [status, body]. */
function deliver(raw, headers) {
  if (!TOKEN) return [503, { ok: false, error: 'inbox not configured' }];
  if (!tokenOk(headers.authorization)) {
    rejected += 1;
    console.warn('[sms-inbox] delivery refused: bad token');
    return [401, { ok: false, error: 'bad token' }];
  }
  let payload;
  try {
    payload = JSON.parse(raw.toString('utf8'));
  } catch {
    return [400, { ok: false, error: 'not json' }];
  }
  const list = payload?.messages;
  if (!Array.isArray(list)) return [400, { ok: false, error: 'messages must be a list' }];
  if (list.length > MAX_PER_DELIVERY) {
    return [413, { ok: false, error: `at most ${MAX_PER_DELIVERY} messages per delivery` }];
  }

  const now = Date.now();
  lastAt = now;
  prune(now);
  let accepted = 0;
  let codes = 0;
  for (const m of list) {
    if (m == null || typeof m !== 'object') continue;
    if (OUTGOING.test(String(m.direction ?? m.type ?? ''))) {
      outgoing += 1;
      console.log('[sms-inbox] outgoing message ignored');
      continue;
    }
    const from = international(pick(m, ['from', 'sender', 'address', 'number', 'phone']));
    if (from === '') continue;
    const text = pick(m, ['body', 'text', 'message', 'content']);
    const code = CODE.exec(text)?.[1] ?? null;
    // Only a text with a code is filed: an operator's balance message or a
    // "c'est fait ?" sent a second later must not bury the code before it
    // is read.
    if (code) inbox.set(from, { code, at: now });
    accepted += 1;
    received += 1;
    if (code) { codes += 1; withCode += 1; }
    // Field NAMES only when no text was found -- the forwarder is not ours,
    // and its first real messages (2026-09-29) arrived with no code; this
    // tells a naming mismatch from a message that simply had none.
    const why = code ? ' (with a sign-in code)'
      : text ? ' (text, no code)'
      : ` (no text; fields: ${Object.keys(m).join(',').slice(0, 120)})`;
    console.log(`[sms-inbox] message from ${mask(from)}${why}`);
  }
  if (list.length === 0) console.log('[sms-inbox] heartbeat');
  return [200, { ok: true, accepted, withCode: codes }];
}

/** The code this number texted in the last ten minutes, or null. */
function codeFrom(number) {
  const m = inbox.get(normal(number));
  if (!m || Date.now() - m.at > KEEP_MS) return null;
  return m.code;
}

/** The SIM this country's passengers text, or null: no sign-in by SMS there. */
const simFor = (dialCode) => SIMS.get(String(dialCode || '')) ?? null;

/** The countries that have a SIM -- what the app asks before offering the button. */
const countries = () => [...SIMS.keys()];

/** A sign-in is waiting for this code: what the app shows and writes for him. */
function expect(dialCode, code) {
  return { number: simFor(dialCode), text: `MOVIN ${code}` };
}

/** For /healthz. `lastAt` is the phone's pulse; never a secret, never a number. */
function health() {
  return {
    configured: TOKEN !== '',
    // Dialling codes only. The SIM numbers are not secret -- the app shows
    // them -- but a health page is no place to publish them.
    countries: countries(),
    received,
    withCode,
    rejected,
    outgoing,
    waiting: inbox.size,
    lastAt: lastAt && new Date(lastAt).toISOString(),
  };
}

module.exports = { deliver, codeFrom, health, international, simFor, countries, expect };
