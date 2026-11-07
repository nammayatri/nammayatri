'use strict';
// How much sign-in the guard allows (phase 5 split, 2026-10-06 -- moved out of
// server.js unchanged): sign-ins started per number and per address, and the
// SMS budget, the hourly and daily cap on texts that cost money.
// tests/auth-guard-routes.test.js and auth-guard-limits.test.js hold the
// answers it must keep. server.js's minute sweep still clears `starts` and
// `ipStarts` beside its sessions.

/**
 * How many sign-ins one phone number may start per window.
 *
 * Locking a *session* is worthless on its own: an attacker just asks for a new
 * authId, spends three guesses on it, and repeats. Three in ten thousand per
 * session means about 3,300 sessions for an even chance -- minutes, if starting
 * one is free. This is the control that makes the session lock mean something.
 *
 * It is also what stops someone burning our SMS credit the day a real gateway
 * exists, which is the more expensive version of the same request.
 */
const MAX_STARTS = Number(process.env.MAX_STARTS || 5);
const START_WINDOW_MS = Number(process.env.START_WINDOW_MS || 60 * 60 * 1000);

/**
 * How many sign-ins one *address* may start per window.
 *
 * ── The hole MAX_STARTS does not close ──────────────────────────────────────
 * That counter is keyed on the number, and an attacker does not reuse a
 * number, he rotates them. Measured 2026-09-23: nothing else stood between one
 * host and the gateway except nginx's 20 requests a minute on the auth zone,
 * which is **1,200 texts an hour from a single address**, every one of them
 * billed to us. This is the counter that makes rotating numbers pointless.
 *
 * ── Why it is this generous, and must stay generous ─────────────────────────
 * Mauritanian mobile networks are behind carrier-grade NAT: thousands of real
 * handsets share a handful of public addresses, so a tight per-IP cap does not
 * hit an attacker, it hits a whole city. Thirty an hour is far above what any
 * one person does and far below what a flood needs, and the global budget
 * below is the control that actually bounds the bill.
 *
 * If it ever does bite a real launch surge the fix is `MAX_STARTS_PER_IP` and
 * a restart -- no build, no APK.
 */
const MAX_STARTS_PER_IP = Number(process.env.MAX_STARTS_PER_IP || 30);

/**
 * The whole fleet's SMS budget, and the reason this file has one at all.
 *
 * Per-number and per-address counters both answer "is this one caller abusive".
 * Neither answers "are we about to spend a month's credit tonight", and that is
 * the question that costs money: the counters above are ceilings per key, and
 * an attacker's supply of keys is not ours to limit.
 *
 * So this is an absolute floor under the bill. Past it the guard stops texting
 * and says so, loudly, in the log and on /healthz.
 *
 * ── Why refusing is the right failure ───────────────────────────────────────
 * The alternative is spending until Moorsyl's credit is gone, and Moorsyl
 * publishes no balance route (measured 2026-09-23: /api/balance, /api/account
 * and /api/me all 404). So an exhausted account cannot be detected from here --
 * it would show up as every registration failing, silently, on launch week.
 * A budget we enforce ourselves is the only warning we get.
 *
 * Enrolled drivers keep their personal codes throughout, exactly as during a
 * gateway outage: this must never be the thing that grounds the fleet.
 *
 * Sized for the pilot -- a few dozen drivers a day -- with room to spare.
 * Raise with `MAX_SMS_PER_HOUR` / `MAX_SMS_PER_DAY` and a restart.
 */
const MAX_SMS_PER_HOUR = Number(process.env.MAX_SMS_PER_HOUR || 60);
const MAX_SMS_PER_DAY = Number(process.env.MAX_SMS_PER_DAY || 400);

/**
 * When each accepted text went out, newest last — the budget's whole memory.
 *
 * Timestamps and not two counters, because a counter reset on the hour lets an
 * attacker spend the next hour's allowance the second it rolls over. A rolling
 * window has no such edge.
 *
 * Only *accepted* sends are recorded: Moorsyl bills for those, a refusal costs
 * nothing, and a budget that counted failures would let a broken gateway lock
 * out a fleet that had spent nothing at all.
 *
 * Bounded by MAX_SMS_PER_DAY, so it cannot grow: entries older than a day are
 * dropped every time it is read.
 */
const smsTimes = [];

const HOUR_MS = 60 * 60 * 1000;
const DAY_MS = 24 * HOUR_MS;

/** Drops anything older than a day and reports what the window holds now. */
function smsSpend() {
  const now = Date.now();
  while (smsTimes.length && now - smsTimes[0] > DAY_MS) smsTimes.shift();
  let hour = 0;
  for (let i = smsTimes.length - 1; i >= 0; i--) {
    if (now - smsTimes[i] > HOUR_MS) break;
    hour += 1;
  }
  return { hour, day: smsTimes.length };
}

/** Is there room in the budget for one more text? */
function smsBudgetLeft() {
  const { hour, day } = smsSpend();
  if (hour >= MAX_SMS_PER_HOUR) return { ok: false, reason: `${hour}/${MAX_SMS_PER_HOUR} this hour` };
  if (day >= MAX_SMS_PER_DAY) return { ok: false, reason: `${day}/${MAX_SMS_PER_DAY} today` };
  return { ok: true };
}

/** Called where the gateway has said yes, and nowhere else. */
function recordSms() {
  smsTimes.push(Date.now());
  const { hour, day } = smsSpend();
  // Warned at four fifths, so the day it matters there is something in the log
  // from before everything started failing rather than only after.
  if (hour * 5 >= MAX_SMS_PER_HOUR * 4 || day * 5 >= MAX_SMS_PER_DAY * 4) {
    console.warn(`[guard] SMS budget running low: ${hour}/${MAX_SMS_PER_HOUR} this hour, ` +
      `${day}/${MAX_SMS_PER_DAY} today`);
  }
}

/** `${route}:${number}` -> [timestamps of sign-ins started] */
const starts = new Map();

/** `address` -> [timestamps of sign-ins started]. See MAX_STARTS_PER_IP. */
const ipStarts = new Map();

/** Records a start against `key` in `map` and says whether it is over `max`. */
function overStartLimit(map, key, max) {
  const now = Date.now();
  const times = (map.get(key) || []).filter((t) => now - t < START_WINDOW_MS);
  times.push(now);
  map.set(key, times);
  return times.length > max;
}

/** Records a sign-in start and says whether this number has had too many. */
function tooManyStarts(key) {
  return overStartLimit(starts, key, MAX_STARTS);
}

/** The same question for the address the request came from. */
function tooManyStartsFromIp(ip) {
  return overStartLimit(ipStarts, ip, MAX_STARTS_PER_IP);
}

/**
 * Who is asking, as nginx saw them.
 *
 * `X-Real-IP` is set by the edge from `$remote_addr` (edge/proxy-common.inc)
 * and a client cannot choose it. `X-Forwarded-For` deliberately is NOT read:
 * it is caller-supplied, so keying a limit on it would let an attacker mint a
 * fresh allowance per request by changing one header.
 *
 * Falls back to the socket, which is only ever reached when something talks to
 * this process directly -- and only the box itself can, since the port is not
 * published.
 */
function callerIp(req) {
  const real = req.headers['x-real-ip'];
  if (typeof real === 'string' && real.trim()) return real.trim();
  return req.socket.remoteAddress || 'unknown';
}

module.exports = {
  MAX_STARTS, START_WINDOW_MS, MAX_STARTS_PER_IP, MAX_SMS_PER_HOUR, MAX_SMS_PER_DAY,
  smsSpend, smsBudgetLeft, recordSms, starts, ipStarts, tooManyStarts, tooManyStartsFromIp, callerIp,
};
