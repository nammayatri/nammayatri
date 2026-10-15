'use strict';
// The sign-in code and the gateway that carries it (phase 5 split,
// 2026-10-06 -- moved out of server.js unchanged): making a code, sending it
// through Moorsyl (Verify, or our own SMS), checking it, and the counters
// /healthz shows. Every paid text goes through issueCode(), which asks the
// budget in limits.js first. tests/auth-guard-routes.test.js holds the answers
// it must keep.

const crypto = require('crypto');
const { smsBudgetLeft, recordSms } = require('./limits');

/* ────────────────────────────────────────────────────────────────────────────
   The gateway

   Moorsyl, Mauritanian. `POST /api/sms`, an `x-api-key` header, a body of
   { to, from, body }. Measured against the live API on 2026-09-06:

     • Validation runs BEFORE authentication, so a 400 says nothing about
       whether the key is good. Only a well-formed request ever reaches the key
       check -- which is also why the probes that mapped this contract cost
       nothing to run.
     • `to` must match /^\+222[234]\d{7}$/: the country code, a mobile prefix,
       then seven digits. That is the same rule the app enforces before it calls
       us, so a number which got this far already fits.
     • Success is `{ accepted: true, messageId }`, and `accepted` means queued,
       not delivered. The only delivery signal is a webhook we do not consume,
       so "sent" here always means "the gateway took it".

   The key is never in git, never in the database and never in a log line. It
   arrives as an environment variable from /opt/ny/secrets/moorsyl.env.
   ──────────────────────────────────────────────────────────────────────────── */

/**
 * Which of Moorsyl's two products delivers the code.
 *
 *   'verify'  Moorsyl makes the code, sends it under its own sender and
 *             template, and checks it for us. Codes are exactly 6 characters.
 *   'sms'     We make the code and send it as plain SMS under our own sender
 *             name, with our own French wording.
 *
 * `sms` is the one we want and it is written and tested, but on 2026-09-06 this
 * account could not use it: POST /api/sms answers 403 COMPLIANCE_REQUIRED for
 * every sender -- "Movin", "moorsyl", and none at all -- so it is the account
 * that is not cleared, not the name. Measured, not inferred. /verify/send on
 * the same key at the same moment answered 200.
 *
 * The likely reason is the one that applies to branded SMS everywhere: a sender
 * ID has to be registered with the operators before it may be used, whereas
 * Verify sends under Moorsyl's own already-registered sender. So this is a mode
 * and not a rewrite -- when the client clears compliance, SMS_MODE=sms is the
 * whole switch, and riders get the Movin-branded French message instead.
 *
 * Both modes use a 6-character code so that the app is built once and the
 * switch is invisible to it.
 */
const SMS_MODE = (process.env.SMS_MODE || 'verify').toLowerCase();

const SMS_URL = process.env.SMS_URL || 'https://api.moorsyl.com/api/sms';
const VERIFY_SEND_URL = process.env.VERIFY_SEND_URL || 'https://api.moorsyl.com/api/verify/send';
const VERIFY_CHECK_URL = process.env.VERIFY_CHECK_URL || 'https://api.moorsyl.com/api/verify/check';
const SMS_KEY = process.env.MOORSYL_API_KEY || '';
const SMS_SENDER = process.env.SMS_SENDER || 'Movin';
const SMS_TIMEOUT_MS = Number(process.env.SMS_TIMEOUT_MS || 15000);

/** For /healthz. Never holds the key or a code. */
let lastSmsError = null;
let smsSent = 0;

/**
 * One GSM-7 segment, so one message and one charge. Accented characters are in
 * the GSM alphabet and would be safe; it is emoji and the like that silently
 * force UCS-2 and halve the room. There are none here.
 */
const smsText = (code) =>
  `Movin : votre code est ${code}. Ne le communiquez a personne. ` +
  'Il expire dans 10 minutes.';

/**
 * A code, uniformly at random.
 *
 * `Math.random()` is neither uniform after a modulo nor unpredictable, and this
 * string is now the whole secret -- it is what 7891 used to be, except that it
 * differs per session and nobody but the holder of the phone is told it.
 * Leading zeros are kept: the app compares strings of a fixed length, and
 * dropping them would quietly shrink the keyspace.
 */
function mkCode(digits) {
  let out = '';
  for (let i = 0; i < digits; i += 1) out += String(crypto.randomInt(0, 10));
  return out;
}

/** Constant-time, so a wrong code leaks nothing through how long it took. */
function sameCode(given, want) {
  const a = Buffer.from(String(given));
  const b = Buffer.from(String(want));
  return a.length === b.length && crypto.timingSafeEqual(a, b);
}

async function sendSms(number, code) {
  if (!SMS_KEY) {
    lastSmsError = 'no API key configured';
    console.error('[guard] sms: MOORSYL_API_KEY is empty -- nothing can be sent');
    return { ok: false };
  }

  let res;
  let text;
  try {
    res = await fetch(SMS_URL, {
      method: 'POST',
      headers: { 'content-type': 'application/json', 'x-api-key': SMS_KEY },
      body: JSON.stringify({ to: number, from: SMS_SENDER, body: smsText(code) }),
      signal: AbortSignal.timeout(SMS_TIMEOUT_MS),
    });
    text = await res.text();
  } catch (err) {
    // A failure here is a rider watching a screen, so record which failure it
    // was rather than a bare "could not send".
    lastSmsError = `${err.name}: ${err.message}`;
    console.error(`[guard] sms to ${number} failed -- ${lastSmsError}`);
    return { ok: false };
  }

  // The gateway quotes parts of a request it rejects. Nothing that echoes our
  // body reaches the log with the code still legible in it.
  const safe = text.split(code).join('****').slice(0, 300);

  let accepted = false;
  try {
    accepted = JSON.parse(text).accepted === true;
  } catch { /* not JSON: not a success either, and `safe` says what it was */ }

  if (!res.ok || !accepted) {
    lastSmsError = `HTTP ${res.status} ${safe}`;
    console.error(`[guard] sms to ${number} refused -- ${lastSmsError}`);
    return { ok: false };
  }

  smsSent += 1;
  recordSms();
  console.log(`[guard] sms to ${number} accepted by the gateway`);
  return { ok: true };
}

/** POST to Moorsyl and say what came back. Never logs the key. */
async function moorsyl(url, payload) {
  if (!SMS_KEY) {
    lastSmsError = 'no API key configured';
    console.error('[guard] MOORSYL_API_KEY is empty -- nothing can be sent');
    return { reachable: false };
  }
  try {
    const res = await fetch(url, {
      method: 'POST',
      headers: { 'content-type': 'application/json', 'x-api-key': SMS_KEY },
      body: JSON.stringify(payload),
      signal: AbortSignal.timeout(SMS_TIMEOUT_MS),
    });
    const text = await res.text();
    let json = null;
    try { json = JSON.parse(text); } catch { /* keep the text for the log */ }
    return { reachable: true, status: res.status, ok: res.ok, json, text };
  } catch (err) {
    lastSmsError = `${err.name}: ${err.message}`;
    return { reachable: false };
  }
}

/**
 * Ask Moorsyl to send a code, in Verify mode.
 *
 * We never learn the code -- checking it is a second call. That is the trade
 * for not needing a registered sender ID.
 */
async function verifySend(number) {
  const r = await moorsyl(VERIFY_SEND_URL, { to: number });
  if (!r.reachable) {
    console.error(`[guard] verify to ${number} failed -- ${lastSmsError}`);
    return { ok: false };
  }
  const id = r.json && r.json.verificationId;
  if (!r.ok || !id) {
    lastSmsError = `HTTP ${r.status} ${String(r.text).slice(0, 300)}`;
    console.error(`[guard] verify to ${number} refused -- ${lastSmsError}`);
    return { ok: false };
  }
  smsSent += 1;
  recordSms();
  console.log(`[guard] verify to ${number} accepted by the gateway`);
  return { ok: true, verificationId: id };
}

/**
 * Check a typed code against a verification.
 *
 * `reachable` is separated from `approved` on purpose. A wrong code and an
 * outage at Moorsyl must not look the same: the first spends an attempt, the
 * second must not, or a bad afternoon at the gateway silently locks people out
 * of their accounts. A 400 here is the API rejecting the code's *shape*, which
 * is a wrong code and nothing more.
 */
async function verifyCheck(verificationId, code) {
  const r = await moorsyl(VERIFY_CHECK_URL, { verificationId, code });
  if (!r.reachable || (r.status >= 500)) {
    console.error(`[guard] verify check unavailable -- ${lastSmsError || `HTTP ${r.status}`}`);
    return { reachable: false, approved: false };
  }
  return { reachable: true, approved: !!(r.json && r.json.status === 'approved') };
}

/**
 * A new code for this session, sent.
 *
 * In `sms` mode the code lives in this process and nowhere else; in `verify`
 * mode we hold only the id and Moorsyl holds the code. Either way the session
 * is only marked as having one if the gateway actually took it -- a session
 * carrying a code nobody received would also refuse the personal code, and lock
 * a driver out of his own account.
 */
async function issueCode(route, s, number) {
  /* The budget, checked here rather than at each call site: this is the one
     door every text goes through, in both modes, so nothing can be added later
     that spends credit without passing it. Refused the same shape a gateway
     failure is refused, so the caller's existing handling applies unchanged --
     an enrolled driver keeps his personal code, a rider is told plainly. */
  const budget = smsBudgetLeft();
  if (!budget.ok) {
    lastSmsError = `budget reached (${budget.reason})`;
    console.error(`[guard] REFUSING to text ${number}: SMS budget reached -- ${budget.reason}. ` +
      'Raise MAX_SMS_PER_HOUR / MAX_SMS_PER_DAY and restart if this is real traffic.');
    return { ok: false };
  }
  if (SMS_MODE === 'verify') {
    const sent = await verifySend(number);
    if (sent.ok) {
      s.verificationId = sent.verificationId;
      s.smsCode = null;
    }
    return sent;
  }
  const code = mkCode(route.codeDigits);
  const sent = await sendSms(number, code);
  if (sent.ok) {
    s.smsCode = code;
    s.verificationId = null;
  }
  return sent;
}

/** What /healthz shows of the gateway: how many texts went, and the last
    error. Read through here because both change inside this module. */
const smsStats = () => ({ sent: smsSent, lastError: lastSmsError });

module.exports = {
  SMS_MODE, SMS_URL, VERIFY_SEND_URL, SMS_KEY, SMS_SENDER,
  mkCode, sameCode, verifyCheck, issueCode, smsStats,
};
