'use strict';
// Drivers' personal sign-in codes, from the enrolment file (phase 5 split,
// 2026-10-06 -- moved out of server.js unchanged). Fails closed: a missing or
// unreadable file refuses everyone. tests/auth-guard-signup.test.js signs an
// enrolled driver in with his code; auth-guard-routes.test.js holds the rest.

const fs = require('fs');
const crypto = require('crypto');

/* ────────────────────────────────────────────────────────────────────────────
   Personal codes

   File shape:
     { "codes": { "+2130551234567": { "salt": hex, "hash": hex, "note": str } } }

   hash = sha256(`${salt}:${number}:${code}`). Salted per number so the file is
   not a rainbow-table lookup, and so two drivers who pick the same code do not
   share a hash. No pepper: one more secret to lose, for a pilot whose whole
   list is ten numbers, and the file is already handled as a secret.

   Re-read when its mtime changes, so enrolling a driver needs no restart. The
   directory is bind-mounted, not the file, so replacing the file inside it is
   safe -- the trap where `tar -x` unlinks a bind-mounted inode and the
   container keeps serving the old one does not apply here.
   ──────────────────────────────────────────────────────────────────────────── */

const codeCache = new Map(); // path -> { mtimeMs, codes }

function loadCodes(path) {
  if (!path) return null;
  let stat;
  try {
    stat = fs.statSync(path);
  } catch {
    // Absent means nobody is enrolled, which must read as "refuse everyone",
    // never as "let everyone through". A typo in the mount path has to fail
    // closed.
    return {};
  }
  const seen = codeCache.get(path);
  if (seen && seen.mtimeMs === stat.mtimeMs) return seen.codes;
  let codes = {};
  try {
    const parsed = JSON.parse(fs.readFileSync(path, 'utf8'));
    codes = parsed && typeof parsed.codes === 'object' ? parsed.codes : {};
    console.log(`[guard] loaded ${Object.keys(codes).length} personal codes from ${path}`);
  } catch (err) {
    // Same reasoning: a malformed file refuses everyone rather than admitting
    // everyone. Loud, because it means an enrolment did not take effect.
    console.error(`[guard] cannot read ${path}: ${err.message} -- refusing all sign-ins`);
  }
  codeCache.set(path, { mtimeMs: stat.mtimeMs, codes });
  return codes;
}

function codeMatches(entry, number, code) {
  if (!entry || !entry.salt || !entry.hash) return false;
  const want = Buffer.from(String(entry.hash), 'hex');
  const got = crypto.createHash('sha256')
    .update(`${entry.salt}:${number}:${code}`)
    .digest();
  // Equal length is a precondition of timingSafeEqual, and a truncated hash in
  // the file would otherwise throw rather than refuse.
  return want.length === got.length && crypto.timingSafeEqual(want, got);
}

module.exports = { loadCodes, codeMatches };
