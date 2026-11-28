'use strict';
// A signed-in person changing their own number (the boss, 2026-10-03), the
// guard's half: proving the NEW number as a sign-in would, then asking
// maps-shim to write it to his account (maps-shim/number-change.js). Moved out
// of server.js unchanged in the phase 5 split (2026-10-06) -- the route's body
// one indentation level less, nothing else. tests/auth-guard-routes.test.js
// and auth-guard-number-change.test.js hold the answers it must keep.

const crypto = require('crypto');
const whatsapp = require('./whatsapp');
const smsInbox = require('./sms-inbox');
const trusted = require('./trusted-phones');
const { WALLET_URL } = require('./driver-rules');
const { START_WINDOW_MS, tooManyStarts, tooManyStartsFromIp, callerIp } = require('./limits');
const { mkCode, sameCode, verifyCheck, issueCode } = require('./gateway');

/**
 * The maps-shim's half of a number change: it reads and writes the person
 * row, which this process cannot. `check` before a code is sent, `apply`
 * once it has been proved. Never told the code. See maps-shim/number-change.js.
 */
async function numberChange(step, side, token, dialCode, number) {
  try {
    const r = await fetch(`${WALLET_URL}/internal/number-change`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ step, side, token, dialCode, number }),
      signal: AbortSignal.timeout(8000),
    });
    return await r.json();
  } catch (err) {
    console.error(`[guard] number change ${step}: ${err.message}`);
    return { ok: false, error: 'unreachable' };
  }
}

/** server.js's own pieces this route uses, handed over once at start-up. */
let ctx = null;
const use = (c) => { ctx = c; };

/** The route itself; server.js has matched the path. `key` is per request. */
async function handleNumberChange(req, res, route, body, key, numberStart, numberConfirm, numberStatus) {
  const { send, refusal, sessions, OPEN_COUNTRIES, textable, AUTH_TTL_MS, LOCK_MS, MAX_ATTEMPTS } = ctx;
  const token = typeof req.headers.token === 'string' ? req.headers.token : '';
  if (!token) return send(res, 401, refusal('NOT_SIGNED_IN'));
  const reasons = { not_signed_in: [401, 'NOT_SIGNED_IN'], WRONG_COUNTRY: [409, 'WRONG_COUNTRY'],
    SAME_NUMBER: [409, 'SAME_NUMBER'], NUMBER_TAKEN: [409, 'NUMBER_TAKEN'] };
  const refuse = (r) => {
    const [status, code] = reasons[r.error] || [502, 'NUMBER_CHANGE_UNAVAILABLE'];
    return send(res, status, refusal(code));
  };

  if (numberStart) {
    let parsed = null;
    try { parsed = JSON.parse(body.toString('utf8')); } catch { /* below */ }
    const dialCode = parsed && typeof parsed.mobileCountryCode === 'string' ? parsed.mobileCountryCode : '';
    const mobileNumber = parsed && typeof parsed.mobileNumber === 'string' ? parsed.mobileNumber : '';
    const channel = parsed && parsed.channel;
    if (!dialCode || !mobileNumber || !['sms', 'whatsapp', 'sms-in'].includes(channel)) {
      return send(res, 400, refusal('INVALID_REQUEST'));
    }
    const number = `${dialCode}${mobileNumber}`;
    if (!OPEN_COUNTRIES.has(dialCode)) return send(res, 403, refusal('COUNTRY_NOT_OPEN'));
    if (channel === 'sms' && (!route.sms || !textable(dialCode))) return send(res, 403, refusal('SMS_NOT_AVAILABLE'));
    if (channel === 'whatsapp' && !whatsapp.ready()) return send(res, 503, refusal('WHATSAPP_UNAVAILABLE'));
    if (channel === 'sms-in' && !smsInbox.simFor(dialCode)) return send(res, 503, refusal('SMS_IN_UNAVAILABLE'));
    if (tooManyStarts(key(number)) || tooManyStartsFromIp(callerIp(req))) {
      return send(res, 429, refusal('TOO_MANY_REQUESTS'),
        { 'retry-after': String(Math.ceil(START_WINDOW_MS / 1000)) });
    }

    // Refused before a code is spent on a change that could never be made.
    const checked = await numberChange('check', route.name, token, dialCode, mobileNumber);
    if (!checked.ok) return refuse(checked);

    const id = `chg${crypto.randomBytes(12).toString('hex')}`;
    const s = { born: Date.now(), attempts: 0, resends: 0, lockedUntil: 0, number, dialCode,
      mobileNumber, token, change: true, smsCode: null, verificationId: null };
    sessions.set(key(id), s);

    if (channel === 'sms') {
      const sent = await issueCode(route, s, number);
      if (!sent.ok) {
        sessions.delete(key(id));
        return send(res, 502, refusal('SMS_SEND_FAILED'));
      }
      console.log(`[guard] ${route.name}: number change started (sms)`);
      return send(res, 200, { changeId: id });
    }
    if (channel === 'whatsapp') {
      s.waCode = mkCode(6);
      const wa = whatsapp.expect(number, s.waCode);
      console.log(`[guard] ${route.name}: number change started (whatsapp)`);
      return send(res, 200, { changeId: id, whatsapp: { code: s.waCode, ...wa } });
    }
    s.smsInCode = mkCode(6);
    const sms = smsInbox.expect(dialCode, s.smsInCode);
    console.log(`[guard] ${route.name}: number change started (sms-in)`);
    return send(res, 200, { changeId: id, smsIn: { code: s.smsInCode, ...sms } });
  }

  const id = decodeURIComponent((numberConfirm || numberStatus)[1]);
  const s = sessions.get(key(id));
  const now = Date.now();
  // Only the person who started it may finish it: his token, not just the id.
  if (!s || !s.change || s.token !== token) return send(res, 404, refusal('INVALID_AUTH_DATA'));
  if (now - s.born > AUTH_TTL_MS) {
    sessions.delete(key(id));
    return send(res, 400, refusal('INVALID_AUTH_DATA'));
  }

  if (numberStatus) {
    const confirmed = s.waCode ? whatsapp.codeFrom(s.number) === s.waCode
      : s.smsInCode ? smsInbox.codeFrom(s.number) === s.smsInCode : false;
    return send(res, 200, { confirmed });
  }

  if (now < s.lockedUntil) {
    return send(res, 429, refusal('TOO_MANY_ATTEMPTS'),
      { 'retry-after': String(Math.ceil((s.lockedUntil - now) / 1000)) });
  }
  let given = null;
  try { given = String(JSON.parse(body.toString('utf8')).otp ?? ''); } catch { /* below */ }
  if (!given) return send(res, 400, refusal('INVALID_REQUEST'));

  const bySms = s.smsCode ? sameCode(given, s.smsCode) : false;
  const byWhatsapp = s.waCode ? sameCode(given, s.waCode) && whatsapp.codeFrom(s.number) === s.waCode : false;
  const bySmsIn = s.smsInCode ? sameCode(given, s.smsInCode) && smsInbox.codeFrom(s.number) === s.smsInCode : false;
  let byVerify = false;
  if (s.verificationId && !bySms) {
    const checked = await verifyCheck(s.verificationId, given);
    if (!checked.reachable) return send(res, 502, refusal('SMS_CHECK_FAILED'));
    byVerify = checked.approved;
  }
  if (!bySms && !byWhatsapp && !bySmsIn && !byVerify) {
    s.attempts += 1;
    if (s.attempts >= MAX_ATTEMPTS) {
      s.lockedUntil = now + LOCK_MS;
      return send(res, 429, refusal('TOO_MANY_ATTEMPTS'),
        { 'retry-after': String(Math.ceil(LOCK_MS / 1000)) });
    }
    return send(res, 400, refusal('INVALID_AUTH_DATA'));
  }

  const applied = await numberChange('apply', route.name, token, s.dialCode, s.mobileNumber);
  sessions.delete(key(id));
  if (!applied.ok) return refuse(applied);
  console.log(`[guard] ${route.name}: number changed`);
  // This phone has just proved the new number: it signs in with it, no code.
  return send(res, 200, { ok: true, deviceTrust: trusted.issue(s.number) });
}

module.exports = { use, handleNumberChange };
