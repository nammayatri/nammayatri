'use strict';
/**
 * Who is calling — proven against the backend that issued the token.
 *
 * ── Why this exists as its own file ────────────────────────────────────────
 * Three routes needed it and two had grown their own copy: `fleet.js` asks the
 * rider app whether a token is real, `subscription.js` asks the driver app the
 * same question and keeps the id it gets back. The third, `/avatar/`, had none
 * at all — and that was the hole: **PUT and DELETE were open to the internet**,
 * so anyone who knew a driver's id could replace the face a passenger sees when
 * choosing him, or delete it, with no credential of any kind. Measured against
 * the live edge on 2026-08-27: `DELETE /avatar/driver/{id}` answered 200.
 *
 * Duplicated authentication is how one copy quietly stops matching the others.
 *
 * ── The rule these functions exist to enforce ──────────────────────────────
 * **The identity is never an input.** Each returns the id *the backend says the
 * token belongs to*, and callers use that to build the storage key rather than
 * trusting anything from the URL. A caller cannot act on somebody else's
 * account even by naming them correctly, because the name in the path is not
 * read.
 *
 * ── Failure is always "nobody" ─────────────────────────────────────────────
 * A bad token, an unreachable backend, a reply we cannot parse: all return
 * null, and every caller refuses. The alternative — treating an unreachable
 * backend as "probably fine" — is an outage that opens the door.
 */

/** One call, and it is cheap: these routes are hit on a profile screen. */
async function ask(baseUrl, path, token) {
  if (!token) return null;
  try {
    const r = await fetch(`${baseUrl}${path}`, { headers: { token } });
    if (!r.ok) return null;
    return await r.json();
  } catch (e) {
    console.error(`[identity] ${path}: ${e.message}`);
    return null;
  }
}

/**
 * The driver this token belongs to.
 *
 * `id` on the profile response is his `person.id` — the same id dispatch uses,
 * the same one a photograph is keyed by, and the same one the passenger's app
 * receives on an offer. Confirmed present on the live response 2026-08-24.
 */
async function driverFromToken(driverUrl, token) {
  const body = await ask(driverUrl, '/ui/driver/profile', token);
  const id = body && typeof body.id === 'string' ? body.id.trim() : '';
  if (!id) return null;
  return {
    id,
    name:
      [body.firstName, body.lastName]
        .filter((s) => typeof s === 'string' && s.trim())
        .join(' ')
        .trim() || null,
  };
}

/**
 * The passenger this token belongs to.
 *
 * `GET /v2/profile` returns eight fields; the only one usable here is `id`.
 * The number comes back **masked** (`055...188`), so it cannot be used to build
 * an avatar key — which is why the caller looks the hash up from the person id
 * instead of from a number it was given.
 */
async function riderFromToken(riderUrl, token) {
  const body = await ask(riderUrl, '/v2/profile', token);
  const id = body && typeof body.id === 'string' ? body.id.trim() : '';
  return id ? { id } : null;
}

module.exports = { driverFromToken, riderFromToken };
