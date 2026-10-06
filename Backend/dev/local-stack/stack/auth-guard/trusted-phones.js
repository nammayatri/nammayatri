'use strict';
/**
 * Phones that have already proved a number -- so signing back in on them needs
 * no code (the boss, 2026-10-03).
 *
 * ── Why ─────────────────────────────────────────────────────────────────────
 * A session lasts a year, so a code was only ever asked again after « Se
 * déconnecter », a reinstall or a new phone. The boss: on the SAME phone that
 * already confirmed the number, signing back in should skip it -- "specially
 * for someone who wants to use it as a driver and passenger". The passenger
 * and driver accounts are separate sign-ins, so switching role was a second
 * code on a phone that had just proved the number.
 *
 * ── How ─────────────────────────────────────────────────────────────────────
 * Every successful verify (SMS, WhatsApp, the SMS sent to us) hands the phone a
 * random key, tied here to the number it just proved. The phone keeps it and
 * presents it with that number to `POST {prefix}auth/trusted`; a match signs it
 * in with no code, on either side. The key is the proof that this handset
 * proved this number -- the number alone proves nothing, which is why the
 * code exists at all.
 *
 * ── What is kept ────────────────────────────────────────────────────────────
 * Only sha256 of each key, so this file leaking signs nobody in. One file next
 * to driver-codes.json (same bind-mounted directory, not in git), rewritten
 * through a temporary file and a rename so a crash mid-write cannot truncate
 * it. Entries end after `TTL_DAYS`, the same year a session lasts, and a
 * number keeps at most `PER_NUMBER` phones -- the oldest goes first.
 *
 * Fails closed: a missing or unreadable file trusts nobody, and everybody
 * simply gets a code as before.
 */
const fs = require('fs');
const path = require('path');
const crypto = require('crypto');

const FILE = process.env.TRUSTED_PHONES_FILE || path.join(__dirname, 'trusted-phones.json');
const TTL_DAYS = Number(process.env.TRUSTED_PHONES_TTL_DAYS || 365);
const PER_NUMBER = Number(process.env.TRUSTED_PHONES_PER_NUMBER || 5);
const DAY_MS = 24 * 60 * 60 * 1000;

/** hash -> { number, at, used } */
let table = null;

const hash = (key) => crypto.createHash('sha256').update(String(key)).digest('hex');

function load() {
  if (table) return table;
  try {
    const parsed = JSON.parse(fs.readFileSync(FILE, 'utf8'));
    table = parsed && typeof parsed === 'object' && parsed.phones && typeof parsed.phones === 'object'
      ? parsed.phones
      : {};
  } catch (err) {
    if (err.code !== 'ENOENT') console.error(`[guard] trusted phones unreadable: ${err.message} -- trusting none`);
    table = {};
  }
  return table;
}

function save() {
  const tmp = `${FILE}.tmp-${process.pid}`;
  try {
    fs.writeFileSync(tmp, JSON.stringify({ phones: table }), { mode: 0o600 });
    fs.renameSync(tmp, FILE);
  } catch (err) {
    // Losing a write costs a code at the next sign-in, never a sign-in.
    console.error(`[guard] trusted phones not saved: ${err.message}`);
    try { fs.unlinkSync(tmp); } catch { /* nothing to clean */ }
  }
}

function prune(now = Date.now()) {
  const t = load();
  for (const [h, e] of Object.entries(t)) {
    if (!e || typeof e.number !== 'string' || now - (e.used || e.at || 0) > TTL_DAYS * DAY_MS) delete t[h];
  }
}

/**
 * Trust this phone for `number`, and return the key it must keep.
 * Called after a verify the backend accepted.
 */
function issue(number, now = Date.now()) {
  if (!number) return null;
  prune(now);
  const t = load();
  const key = crypto.randomBytes(32).toString('base64url');
  t[hash(key)] = { number, at: now, used: now };
  // At most PER_NUMBER phones per number: the least recently used goes.
  const mine = Object.entries(t)
    .filter(([, e]) => e.number === number)
    .sort((a, b) => (b[1].used || 0) - (a[1].used || 0));
  for (const [h] of mine.slice(PER_NUMBER)) delete t[h];
  save();
  return key;
}

/** Does `key` prove this phone already confirmed `number`? Refreshes it if so. */
function check(number, key, now = Date.now()) {
  if (!number || typeof key !== 'string' || key.length < 20 || key.length > 200) return false;
  const t = load();
  const h = hash(key);
  const e = t[h];
  if (!e || now - (e.used || e.at || 0) > TTL_DAYS * DAY_MS) return false;
  // Lookup by hash already compared the key; the number must be the same one.
  const a = Buffer.from(e.number);
  const b = Buffer.from(number);
  if (a.length !== b.length || !crypto.timingSafeEqual(a, b)) return false;
  e.used = now;
  save();
  return true;
}

/** Forget every phone trusted for `number` -- an account erased, a number given up. */
function revoke(number) {
  const t = load();
  let n = 0;
  for (const [h, e] of Object.entries(t)) if (e.number === number) { delete t[h]; n += 1; }
  if (n) save();
  return n;
}

/** For /healthz: how many, never which. */
function health() {
  prune();
  return { phones: Object.keys(load()).length, ttlDays: TTL_DAYS };
}

/** Tests only: forget the in-memory copy so the file is read again. */
function _reset() { table = null; }

module.exports = { issue, check, revoke, health, _reset, FILE };
