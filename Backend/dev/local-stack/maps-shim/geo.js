'use strict';
/**
 * Which of our countries a phone's IP address belongs to — for the sign-in
 * screen (2026-09-14).
 *
 * The boss asked that a user never see the other country: the app works out
 * the country and shows only its number format. The phone asks its own GPS
 * first (on the device — the position never leaves it); this route is the
 * second opinion, for a phone whose location is off or refused.
 *
 *   GET /geo/country   ->  { "country": "DZ" | "MR" | null }
 *
 * ── The address is the caller's, and it is never stored ──────────────────────
 * nginx sets X-Real-IP to the connecting address (edge/proxy-common.inc); a
 * client cannot choose it. Nothing here writes it anywhere, and the log line
 * carries only the answer.
 *
 * ── The table is AfriNIC's, held in memory ───────────────────────────────────
 * ~70 blocks, rebuilt by ../geo-ip-refresh.sh into ip-countries.json. Two
 * countries do not need a database table. null means "not one of ours", and
 * the app then falls back to the last country used on the phone.
 */

const fs = require('fs');
const path = require('path');

const FILE = path.join(__dirname, 'ip-countries.json');

/** [country, first, last] — numbers for IPv4, BigInts for IPv6. */
let v4 = [];
let v6 = [];

function ipv4ToInt(ip) {
  const p = ip.split('.').map(Number);
  if (p.length !== 4 || p.some((n) => !Number.isInteger(n) || n < 0 || n > 255)) return null;
  return ((p[0] * 256 + p[1]) * 256 + p[2]) * 256 + p[3];
}

function ipv6ToBig(ip) {
  let [head, tail] = ip.split('::');
  const h = head ? head.split(':') : [];
  const t = tail !== undefined && tail ? tail.split(':') : [];
  if (tail === undefined && h.length !== 8) return null;
  const groups = tail === undefined ? h : [...h, ...Array(8 - h.length - t.length).fill('0'), ...t];
  if (groups.length !== 8) return null;
  let n = 0n;
  for (const g of groups) {
    if (!/^[0-9a-f]{1,4}$/i.test(g)) return null;
    n = (n << 16n) + BigInt(parseInt(g, 16));
  }
  return n;
}

function load() {
  try {
    const j = JSON.parse(fs.readFileSync(FILE, 'utf8'));
    v4 = j.v4
      .map(([c, from, count]) => {
        const first = ipv4ToInt(from);
        return first === null ? null : [c, first, first + count - 1];
      })
      .filter(Boolean);
    v6 = j.v6
      .map(([c, prefix, len]) => {
        const base = ipv6ToBig(prefix);
        return base === null ? null : [c, base, base + (1n << BigInt(128 - len)) - 1n];
      })
      .filter(Boolean);
    console.log(`[geo] ${v4.length} IPv4 and ${v6.length} IPv6 blocks (${j.source}, ${j.built})`);
  } catch (e) {
    // No table: every answer is null and the app falls back. Never a crash.
    console.error('[geo] ip-countries.json not loaded:', e.message);
  }
}

/** The connecting address, as nginx saw it. */
function clientIp(req) {
  const real = String(req.headers['x-real-ip'] || '').trim();
  const forwarded = String(req.headers['x-forwarded-for'] || '').split(',')[0].trim();
  const raw = real || forwarded || req.socket.remoteAddress || '';
  return raw.replace(/^::ffff:/i, '');
}

function countryOfIp(ip) {
  if (ip.includes(':')) {
    const n = ipv6ToBig(ip);
    if (n === null) return null;
    const hit = v6.find(([, a, b]) => n >= a && n <= b);
    return hit ? hit[0] : null;
  }
  const n = ipv4ToInt(ip);
  if (n === null) return null;
  const hit = v4.find(([, a, b]) => n >= a && n <= b);
  return hit ? hit[0] : null;
}

function serve(req, res) {
  const country = countryOfIp(clientIp(req));
  console.log(`[geo] country -> ${country || 'none'}`);
  const body = JSON.stringify({ country });
  res.writeHead(200, {
    'content-type': 'application/json;charset=utf-8',
    'content-length': Buffer.byteLength(body),
    // Per caller, and a phone moves: never cached anywhere.
    'cache-control': 'no-store',
  });
  res.end(body);
}

load();

module.exports = { serve, countryOfIp, load };
