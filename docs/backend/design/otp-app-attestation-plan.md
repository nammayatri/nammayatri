# OTP Protection with Direct App Attestation (Play Integrity + App Attest)

**Status:** Backend implemented for the rider app and the driver app (see §11). Every city starts at `mode = OFF`.
**Scope:** Rider `/v2/auth` (send OTP) and `/v2/auth/otp/:id/resend`, and the same pair on the driver app (`/ui`).
**Approach:** Use Google Play Integrity (Android) and Apple App Attest (iOS) directly, without Firebase App Check.

Detailed FE and BE task breakdowns will be written separately. This document defines **what** must be built and **why**, so both sides can plan against one contract.

---

## 1. Background: why we need this

An investigation of rider OTP traffic in October–November 2026 found the following. Data comes from ClickHouse `registration_token` / `person`, AWS ALB access logs, and istio logs.

Today, every `/v2/auth` call creates a `person` row (if new), creates a `registration_token` row, and sends one SMS/WhatsApp OTP. Nothing checks that the caller is our real app.

### OTP split for the non-Bharat-Taxi rider apps

These figures cover NY, JS, Anna and Meghalaya One, for 20 Oct–12 Nov scaled to 30 days.

| | OTPs / month | Share |
|---|---|---|
| Genuine (new + returning users who verified) | ~11.5L | 65% |
| Fraud: "POMO" fake app (Namma Yatri) | ~5.3L | 30% |
| Fraud: no device headers (scripted) | ~0.27L | 2% |
| Unclear (app user, never verified) | ~0.67L | 4% |

### Bharat Taxi

Bharat Taxi has a separate bot that sends no device headers, about 6.2L OTPs in October. BT's verify rate fell from about 85% in early September to 35–45%.

### Bot fingerprints observed

- **POMO bot.**
  - Headers: `x-device` contains `POMO M5 Pro` / Android 11, `x-client-version: 3.1.6`, one bundle, no device IDs.
  - Verification: 4.7L OTPs from 1 Oct to 13 Nov, only 101 verified.
  - Timing: bursts every 5 minutes.
  - IPs: 470–780 rotating VPN/hosting IPs a day (Tzulo, Datacamp, Mullvad, M247, Cogent, …).
  - User agents: fake, truncated browser UAs.
  - Entry point: all traffic arrives via the AWS ALB, which has no WAF.
- **Header spoofing defeats header checks.** Header checks, user-agent checks and IP rate limits can all be copied or rotated by an attacker. The bot already does this.

### Conclusion

We need proof that the request comes from **our genuine, unmodified app on a real device**. Only the platform (Google Play / Apple) can provide that proof. An attacker cannot forge it from a server farm.

---

## 2. Goals and non-goals

### Goals

1. Send an OTP only when the request carries a valid platform attestation from our genuine app. Gate this per merchant and city, with a rollout mode.
2. Bind each attestation to a server-issued, single-use challenge and to the specific auth request (phone number + merchant). This blocks token replay and token reuse across numbers.
3. Provide a fallback for real users whose device cannot attest (rooted, custom ROM, no Google Play services, old iOS, attestation outage). No hard lockout of genuine users.
4. Ship log-only (shadow) mode first, so we can measure the false-reject rate before enforcing.
5. Provide observability: verdict distribution per merchant, app version and platform, plus alerting.

### Non-goals (this phase)

- Attesting every API call. Only OTP send and resend are protected for now; other endpoints remain token-authenticated.
- Web or partner login flows. These are exempt and are covered by the existing signature auth and dashboard flows.
- Replacing edge protection. WAF rules and per-number limits remain complementary layers.
- Detecting fraud by real humans on real phones (device farms). This is partially addressed by optional signals in §6.4 and is a later phase.

---

## 3. Threat model

| Attacker capability | Stopped by this design? |
|---|---|
| Script or headless HTTP client sending requests (POMO, BT bot) | **Yes.** It has no valid platform token. |
| Copies real headers, user agents, device names | **Yes.** Headers are no longer trusted. |
| Rotates IPs / residential proxies | **Yes.** Attestation is IP-independent. |
| Replays a captured valid token | **Yes.** Tokens are tied to a single-use challenge and a request hash. |
| Uses one valid token for many phone numbers | **Yes.** The request hash includes the phone number. |
| Repackaged or modified APK/IPA | **Yes.** It fails app integrity (`UNRECOGNIZED_VERSION`, cert mismatch, or App Attest app ID mismatch). |
| Rooted phone or emulator running our real app | Mostly. Android fails device integrity; emulators fail App Attest. Such users go to the fallback. |
| Real unmodified phones in a device farm, automated | **Partially.** This needs device-activity signals and per-key velocity limits. Later phase. |
| Platform outage (Google/Apple attestation down) | Not an attack. We fail open to the fallback path and alert (§8). |

---

## 4. How the platform services work

### 4.1 Google Play Integrity: Standard API request

```
App start (once, warm-up):
  IntegrityManager.prepareIntegrityToken(cloudProjectNumber) → StandardIntegrityTokenProvider

Per protected action:
  requestHash = base64url(SHA-256(canonical request incl. server challenge))
  tokenProvider.request(requestHash) → integrityToken (opaque, encrypted)
  app → our backend: integrityToken + challengeId + request

Backend:
  POST https://playintegrity.googleapis.com/v1/{packageName}:decodeIntegrityToken
       (OAuth2 service account, scope https://www.googleapis.com/auth/playintegrity)
  → tokenPayloadExternal (decoded verdict JSON)
```

**Main verdict fields.** Treat the exact field list as "verify against current reference". Optional signals must be enabled in the Play Console.

- `requestDetails`: `requestPackageName`, `requestHash`, `timestampMillis`
- `appIntegrity`: `appRecognitionVerdict` (`PLAY_RECOGNIZED` | `UNRECOGNIZED_VERSION` | `UNEVALUATED`), `packageName`, `certificateSha256Digest[]`, `versionCode`
- `deviceIntegrity`: `deviceRecognitionVerdict[]` (`MEETS_BASIC_INTEGRITY`, `MEETS_DEVICE_INTEGRITY`, `MEETS_STRONG_INTEGRITY`, `MEETS_VIRTUAL_INTEGRITY`); optional `recentDeviceActivity` level
- `accountDetails`: `appLicensingVerdict` (`LICENSED` | `UNLICENSED` | `UNEVALUATED`)
- `environmentDetails` (optional): `playProtectVerdict`, `appAccessRiskVerdict`

**Facts that drive the design:**

- **Decoding is server-side.** Standard-request tokens can only be decoded by Google's server. We need a Google Cloud service account and an outbound call per verification. Latency is typically tens to low hundreds of ms.
- **Quota.** The default is 10,000 requests/day per Cloud project, counting token requests and decodes. An increase is free but requires:
  - linking the Cloud project to the app in the Play Console
  - the app being on Play
  - correct retry logic in the app

  Approval takes up to about a week. Unlinked projects are hard-capped at 10k/day. **We must raise the quota before enforcing.** Real OTP traffic is about 40–60k/day across apps.
- **`requestHash` is echoed back inside the signed payload.** It can be up to 500 bytes; we send a SHA-256. This is how we bind the token to *this* request.
- **Each app needs its own entries.** Every app (NY, JS, Anna, BT, …) has its own package name and signing certificate. Verification config is per app.

### 4.2 Apple App Attest (iOS 14+)

Two phases. **The verification logic runs entirely on our server.** Apple offers no "verify" API for attestations.

**Phase 1: key attestation (once per install).**

```
app:  DCAppAttestService.isSupported?  (false on simulator, some devices/OS)
app:  keyId = generateKey()                       // key lives in Secure Enclave
app:  GET challenge from backend
app:  attestation = attestKey(keyId, clientDataHash = SHA-256(challenge))
app → backend: keyId, attestation (CBOR), challengeId
backend: verify attestation (steps below); store {keyId, publicKey, counter=0, receipt}
```

Server verification of the attestation object (Apple "Validating apps that connect to your server"):

1. CBOR-decode `{fmt: "apple-appattest", attStmt: {x5c, receipt}, authData}`.
2. Verify the `x5c` certificate chain up to the **Apple App Attestation Root CA**.
3. Compute `nonce = SHA-256(authData ‖ clientDataHash)`. It must equal the value in credential certificate extension OID `1.2.840.113635.100.8.2`.
4. `SHA-256(credential cert public key)` must equal `keyId`.
5. `authData.rpIdHash` must equal `SHA-256("<TeamID>.<BundleID>")`.
6. `authData.signCount` must equal 0.
7. `authData.aaguid` must be `appattest` (production) or `appattestdevelop` (development), as expected for the build.
8. `authData.credentialId` must equal `keyId`.
9. Store the public key, counter and receipt. If verification fails, the app must discard the key.

**Phase 2: assertion (per protected request).**

```
app:  assertion = generateAssertion(keyId, clientDataHash = SHA-256(canonical request incl. challenge))
app → backend: keyId, assertion (CBOR {signature, authenticatorData}), request
backend: look up keyId → publicKey, lastCounter
         verify ECDSA P-256 signature over SHA-256(authenticatorData ‖ clientDataHash)
         rpIdHash matches; counter > lastCounter; store new counter
```

**Known pitfalls** (from community reports; verify with a real-device test vector before relying on them):

- The hashing of the signed data (single vs double SHA-256 at the verify step).
- Whether `clientDataHash` is the raw challenge or its SHA-256. **We define it explicitly in our contract (§5.3).**

**Other constraints:**

- No published daily quota. `attestKey` contacts Apple and can be throttled, so the app must attest **once per install** and reuse the key, with retry/backoff. Apple recommends a gradual rollout.
- The entitlement `com.apple.developer.devicecheck.appattest-environment` must be `production` for App Store builds.
- Unsupported devices need the fallback path.
- **Optional, later:** use the receipt to query Apple's fraud-metric endpoint (attestations per device) for device-farm detection.

---

## 5. Proposed design

### 5.1 Components

```
                    ┌──────────────────────────── rider-app backend ─────────────────────────────┐
 App ──GET challenge──► AttestationChallenge  (Redis, single-use, TTL ~5 min)                   │
 App ──POST ios key ──► AppAttestKeyRegistry  (verify attestation, store key)                   │
 App ──POST /v2/auth──► AttestationGate ──► PlayIntegrityVerifier ──► Google decode API          │
                    │                  └─► AppAttestVerifier (local crypto, key lookup)          │
                    │                  └─► AttestationPolicy (per merchant/city mode + rules)   │
                    │                  └─► Decision: ALLOW | FALLBACK | SILENT_DROP | REJECT     │
                    │   ALLOW → existing auth flow (person, registration_token, send OTP)       │
                    │   + AttestationLog (ClickHouse/Kafka event) for every decision             │
                    └─────────────────────────────────────────────────────────────────────────────┘
```

**Placement in the existing flow** (`Domain/Action/UI/Registration.hs` `auth`):

- The gate runs **after request validation and IP-block checks**, and **before** `createPerson`, the `registration_token` insert and `sendOTP`.
- A rejected request leaves no DB rows and sends no SMS. This also stops bots inflating the `person` / "new users" count.

### 5.2 New / changed API surface (contract level)

| Endpoint | Purpose |
|---|---|
| `GET /v2/auth/attestation/challenge` | Returns `{challengeId, challenge (base64, 32 random bytes), expiresAt}`. No auth. IP-rate-limited. Stored in Redis, single use. |
| `POST /v2/auth/attestation/ios/key` | iOS only. Body `{challengeId, keyId, attestation}`. Verifies and registers the App Attest key. Returns `{status}`. Idempotent per `keyId`. |
| `POST /v2/auth` (existing) | New optional headers (below). Body unchanged. |
| `POST /v2/auth/otp/:id/resend` (existing) | Same new headers. A resend needs a fresh challenge. |

**New request headers on protected endpoints:**

```
x-attestation-platform:      ANDROID_PLAY_INTEGRITY | IOS_APP_ATTEST
x-attestation-challenge-id:  <challengeId>
x-attestation-token:         <Play Integrity token>            (Android)
x-app-attest-key-id:         <keyId>                           (iOS)
x-app-attest-assertion:      <base64 CBOR assertion>           (iOS)
```

**Response for a fallback decision.** The response must stay backward compatible, so additions are new optional fields:

- A distinct error code (e.g. `ATTESTATION_REQUIRED`) or a field in `AuthRes` telling the app to show the fallback (§5.6).
- **Silent drop.** The response looks like a normal success, but no OTP is sent.

### 5.3 Request binding (canonical request)

App and server compute the same hash:

```
canonical = "v1|" + merchantShortId + "|" + mobileCountryCode + "|" + mobileNumber + "|" + challenge
          (for email identifier: email instead of country code + number)
requestHash (Android) = base64url( SHA-256(canonical) )
clientDataHash (iOS)  = SHA-256(canonical)            // raw 32 bytes passed to generateAssertion
```

This guarantees four things:

- **One token, one number.** A token minted for number A cannot be used to send an OTP to number B.
- **No reuse.** The challenge is single-use (deleted on first verification attempt, success or failure).
- **Freshness.** The challenge has a TTL. Android `timestampMillis` must also be within N minutes.

### 5.4 Server verification checklist

**Android (Play Integrity):**

1. The challenge exists in Redis, is unexpired, and is consumed atomically (`GETDEL`).
2. Decode the token via Google, using a per-app package name.
3. `requestDetails.requestPackageName` equals the configured package for this merchant/app.
4. `requestDetails.requestHash` equals the recomputed hash (§5.3).
5. `requestDetails.timestampMillis` is within the allowed skew (e.g. 10 min).
6. `appIntegrity.appRecognitionVerdict == PLAY_RECOGNIZED`, `packageName` matches, and `certificateSha256Digest` contains our release signing cert digest.
7. `deviceIntegrity.deviceRecognitionVerdict` meets the configured minimum (default `MEETS_DEVICE_INTEGRITY`; configurable to `MEETS_BASIC_INTEGRITY`).
8. Optional, config-driven: `appLicensingVerdict`, `appAccessRiskVerdict`, `recentDeviceActivity` thresholds.

**iOS (App Attest assertion):**

1. The challenge exists and is consumed atomically.
2. `keyId` exists in the key registry, is not revoked, and belongs to the expected app (bundle/team) for this merchant.
3. Assertion signature verifies with the stored public key over the agreed data.
4. `rpIdHash == SHA-256(TeamID.BundleID)`.
5. `counter > storedCounter`. Store the new counter atomically (compare-and-set).
6. Optional: per-key velocity limits (e.g. max OTP requests per key per day). This catches a single real device being used for many numbers.

Each verification yields a structured result:

```
AttestationResult { platform, outcome: VALID | INVALID | MISSING | UNSUPPORTED | ERROR,
                    reasons: [..], verdictSummary, appVersion, latencyMs }
```

### 5.5 Policy (per merchant + city, config-driven)

```
attestationMode:         OFF | SHADOW | ENFORCE_NEW_USERS | ENFORCE_ALL
onInvalid:               SILENT_DROP | REJECT | FALLBACK
onMissing:               ALLOW (during app rollout) | FALLBACK | SILENT_DROP
onUnsupportedOrError:    FALLBACK | ALLOW
minAndroidDeviceVerdict: MEETS_BASIC_INTEGRITY | MEETS_DEVICE_INTEGRITY | MEETS_STRONG_INTEGRITY
minAppVersionForAttestation: per platform
androidApps:             [{packageName, certSha256[]}]
iosApps:                 [{teamId, bundleId, environment}]
exemptions:              dashboard requests, /auth/signature partner flow, fake-OTP test numbers
```

**Decision table:**

| Result | SHADOW | ENFORCE_NEW_USERS | ENFORCE_ALL |
|---|---|---|---|
| VALID | allow | allow | allow |
| INVALID | allow + log | new person → `onInvalid`; existing → allow | `onInvalid` |
| MISSING (old app) | allow + log | new person → `onMissing`; existing → allow | `onMissing` |
| UNSUPPORTED / ERROR / platform outage | allow + log | `onUnsupportedOrError` | `onUnsupportedOrError` |

**What "new person" means here.** The phone number has no existing `person` for the merchant. A read-only lookup happens before the gate; we must not create the person first.

**Recommended action for bot-like failures: `SILENT_DROP`.** It returns a normal-looking success without sending an SMS, so the attacker gets no signal to adapt to.

### 5.6 Fallback for genuine users who cannot attest

Choose one or more per merchant:

- **CAPTCHA challenge** before the OTP is sent. Provider to be decided (open question).
- **WhatsApp OTP** (already supported by `otpChannel`). Cheaper, and harder to pump.
- **Stricter limits** on the fallback path: 1–2 OTPs per number per hour, per-IP caps, and no OTP for numbers with high prior unverified counts.

### 5.7 Data and storage

| Store | Data | Notes |
|---|---|---|
| Redis | `attestation:challenge:<id>` → `{challenge, createdAt, ip}` | TTL ~5 min, `GETDEL` on use |
| Postgres (new table via NammaDSL) | `app_attest_key`: `key_id` (PK), `merchant_id`, `bundle_id`, `team_id`, `environment`, `public_key`, `sign_count`, `receipt`, `status` (ACTIVE/REVOKED), `created_at`, `last_used_at`, `use_count` | iOS only. `sign_count` updated with compare-and-set. Keys are per install, not per person (pre-login). |
| Kafka → ClickHouse | `attestation_event`: ts, merchant, city, platform, endpoint, outcome, reasons, verdict summary, app version, ip, mode, decision, latency | Hash the phone number; never store it raw. Used for dashboards and alerts. |
| Config | Policy fields in §5.5 | JSON column `app_attestation_config` on `rider_config` / `transporter_config`, one per city |
| Secrets | Google service account for the Play Integrity decode API, CAPTCHA secret | `merchant_service_config` services `AppAttestation_PlayIntegrity` and `Captcha_SiteVerify`, encrypted like other provider credentials |

### 5.8 Backend building blocks (high level)

- **Play Integrity client.** Get an OAuth2 access token from the service account (reuse the FCM-style JWT flow, cached until expiry), then call `decodeIntegrityToken`, with timeouts and retries.
- **App Attest verifier.** Needs:
  - CBOR decoding (`cborg` / `serialise`, not currently a dependency)
  - X.509 chain verification against the pinned Apple App Attestation Root CA (`x509`, `x509-validation`)
  - ECDSA P-256 verify and SHA-256 (`cryptonite` / `crypton`)
- **Test vectors.** Build a test suite from real-device attestation/assertion samples, including the pitfalls in §4.2.
- **Challenge service.** Redis, single-use, rate-limited.
- **Attestation gate and policy.** Pure decision function plus side effects (log, metrics). Unit-test it against the decision table.
- **Metrics.** Prometheus counters `attestation_result_total{merchant,platform,outcome,decision}` and a latency histogram for the Google decode call.

### 5.9 App building blocks (high level)

- **Android.** Integrate Play Integrity (Standard request).
  - Call `prepareIntegrityToken` at app start and after expiry/errors.
  - Fetch a challenge, compute `requestHash`, request a token, attach the headers.
  - Handle the remediation dialog codes (optional).
- **iOS.** Integrate App Attest.
  - Generate and attest a key once per install (Keychain-stored `keyId`).
  - Generate an assertion per protected request.
  - Re-attest on key invalidation (e.g. app reinstall or restore to a new device invalidates the key).
- **React Native bridge** exposing `getAttestationHeaders(canonicalRequest)` to the login screen.
- **Fallback UI** (CAPTCHA / WhatsApp option) when the backend asks for it.
- **Both platforms:** attest only on the login/OTP screen. Never block the UI on attestation for more than a few seconds; time out and send the request without headers. The backend policy then decides.

---

## 6. Rollout plan

| Phase | What | Exit criteria |
|---|---|---|
| **0. Prerequisites** | Link the Cloud project in the Play Console for each Android app; request a Play Integrity quota increase (target ≥ 2× peak daily OTP requests across apps, e.g. 150k/day); create the service account; enable the App Attest capability + production entitlement for each iOS app; collect package names, signing cert digests, Team IDs, bundle IDs per merchant | Quota approved; credentials in secrets |
| **1. Backend, shadow** | Challenge endpoint, iOS key registration, verifiers, gate in `SHADOW` mode, events + dashboard | Deployed; zero behaviour change |
| **2. App release** | Android + iOS send attestation headers on `/auth` and resend | Release live; adoption tracked by app version |
| **3. Measure** | 1–2 weeks of shadow data | On new app versions, genuine (later-verified) users show ≥ 99% VALID; known bot fingerprints (POMO, no-header) show ~0% VALID; Google decode p95 latency is acceptable |
| **4. Enforce for new users** | `ENFORCE_NEW_USERS` with `SILENT_DROP` on Namma Yatri, then Bharat Taxi, then the others | OTP volume drops by roughly the fraud share; new-user verify rate recovers to ≥ 80%; no rise in support tickets about login |
| **5. Enforce for everyone** | `ENFORCE_ALL`, and `onMissing = FALLBACK` once old versions are a small share; force-update the minimum version | Stable for 2 weeks |
| **6. Extend** | Driver app `/ui/auth`; device-farm signals (Android `recentDeviceActivity`, Apple fraud metric, per-key velocity) | — |

**Kill switch.** Setting `attestationMode = OFF` per merchant/city must take effect without a deploy.

---

## 7. Observability and alerting

**Dashboard (Metabase over `attestation_event`).** By merchant and platform:

- outcome mix (VALID / INVALID / MISSING / UNSUPPORTED / ERROR)
- decisions (allow / silent drop / fallback / reject)
- top INVALID reasons
- app-version adoption
- Google decode latency and errors
- OTPs sent vs verified

**Alerts:**

- The OTP verify rate (sent → verified within 30 min) per merchant drops below a threshold (e.g. 60% over 2 h). This would have caught both the POMO and BT attacks within a day.
- The attestation ERROR rate spikes (Google/Apple outage, quota exhaustion, expired credentials).
- Play Integrity quota usage passes 70% of the daily quota.
- VALID share among later-verified users falls below 98% (false-reject regression).

---

## 8. Risks and edge cases

| Risk | Mitigation |
|---|---|
| Quota exhaustion (Play Integrity 10k/day default) | Raise the quota before enforcing; alert at 70%; on quota errors treat as ERROR → `onUnsupportedOrError` (fallback, not hard block) |
| Google/Apple outage or latency | Timeouts (e.g. 2 s), fail to fallback, alert. Never hard-block genuine users on a platform error. |
| Low-end / older Android devices failing `MEETS_DEVICE_INTEGRITY` | Measure in shadow; allow `MEETS_BASIC_INTEGRITY` per merchant if needed |
| Devices without Google Play Services (some Huawei, custom ROMs) | `UNSUPPORTED` → fallback |
| APKs distributed outside Play (sideloaded, other stores) | They return `UNRECOGNIZED_VERSION` / `UNEVALUATED`. Confirm distribution channels per app (open question). |
| iOS simulator / unsupported devices | `isSupported == false` → fallback; debug environment for QA builds |
| App reinstall / device restore invalidates the App Attest key | The app re-attests; old keys are left inactive and cleaned up after N days |
| Counter race on concurrent iOS requests | Compare-and-set on `sign_count`; reject non-increasing counters |
| Multi-cloud (AWS + GCP) | Challenge must be readable wherever `/auth` lands. Use the shared Redis or cloud-aware routing. Verify against `docs/backend/12-multi-cloud.md`. |
| Bot shifts to device farms with real phones | Phase 6 signals + per-key/per-device velocity + per-number limits |
| PII | Never log raw phone numbers in attestation events; hash them |

---

## 9. Open questions

1. **Apps in scope.** List every rider (and later driver) app per merchant, with package name / bundle ID. Are any distributed outside Play or the App Store (APK downloads, partner builds)?
2. **Fallback choice.** Which fallback per merchant: CAPTCHA (which provider?), WhatsApp OTP, or both?
3. **Old app versions.** What is the minimum-version policy? How long do we accept MISSING before forcing an update?
4. **Config home.** Decided: the policy in `rider_config` / `transporter_config`, the credentials in `merchant_service_config` (§5.7).
5. **Shared Cloud project.** Do we use one Google Cloud project for all Android apps (shared quota), or one per app?
6. **Driver app timing.** When does it get the same protection, and does it share the infrastructure?
7. **Edge protection.** Do we also want WAF managed rules (anonymous/hosting IPs) on the AWS ALB `k8s-ekskurukshetraist-4e612188d3`, which currently has no WAF, as an immediate complementary layer?

---

## 10. References

- Play Integrity, standard requests: https://developer.android.com/google/play/integrity/standard
- Play Integrity, setup and quota: https://developer.android.com/google/play/integrity/setup, https://developer.android.com/google/play/integrity/quota-limits
- Apple, establishing app integrity: https://developer.apple.com/documentation/devicecheck/establishing-your-app-s-integrity
- Apple, validating apps that connect to your server: https://developer.apple.com/documentation/devicecheck/validating-apps-that-connect-to-your-server
- Apple `attestKey`: https://developer.apple.com/documentation/devicecheck/dcappattestservice/attestkey(_:clientdatahash:completionhandler:)
- Existing code: rider auth `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs` (`auth`, `resend`); API `.../API/UI/Registration.hs`; device header parsing `Kernel.Utils.Version.getDeviceFromText` (shared-kernel); Google service-account JWT `Kernel.Utils.JWT` (shared-kernel)

---

## 11. Backend as built (contract for the apps)

Status: implemented for the rider app and the driver app. Each city starts at `mode = OFF`, so nothing changes until a city is configured.

### Endpoints

Same paths on rider-app (`/v2`) and the driver app (`/ui`):

| Endpoint | Notes |
|---|---|
| `GET /auth/attestation/challenge` | `{challengeId, challenge, expiresAt}`. `challenge` is 32 random bytes, unpadded base64url. Valid 5 minutes, single use. |
| `POST /auth/attestation/ios/key` | `{merchantId, city?, challengeId, keyId, attestation}`. `merchantId` is the value the app sends to `/auth` (short id on rider, merchant id on driver). `keyId` and `attestation` are base64 as `DCAppAttestService` returns them. Answers `{status: "REGISTERED"}`; idempotent per `keyId`. |
| `POST /auth`, `POST /auth/otp/:authId/resend` | Optional headers below. Bodies unchanged. |

Headers: `x-attestation-platform` (`ANDROID_PLAY_INTEGRITY` / `IOS_APP_ATTEST`), `x-attestation-challenge-id`, `x-attestation-token` (Android), `x-app-attest-key-id` and `x-app-attest-assertion` (iOS, base64), `x-attestation-error` (the app could not attest; any short reason), `x-captcha-token` (CAPTCHA fallback).

### What is hashed

| Request | Canonical string |
|---|---|
| `/auth` with a phone number | `v1|<merchantId>|<mobileCountryCode>|<mobileNumber>|<challenge>` |
| `/auth` with an email | `v1|<merchantId>|<email>|<challenge>` |
| resend | `v1|resend|<authId>|<challenge>` |

`merchantId`, number and email are exactly what the app puts in the body. Android: `requestHash = base64url(SHA-256(canonical))`, unpadded. iOS assertion: `clientDataHash = SHA-256(canonical)` (raw 32 bytes). iOS key attestation: `clientDataHash = SHA-256(challenge)`, the challenge string as UTF-8. Each challenge works once, so every `/auth` and resend needs a fresh one; fetch it when the user submits, not when the screen opens.

### Answers the app must handle

| Case | Answer |
|---|---|
| Allowed | Unchanged. |
| Silent drop | Normal-looking `200` with an `authId`. No OTP is sent; verify returns `INVALID_AUTH_DATA`; resend counts attempts down like a real one. |
| `ATTESTATION_REQUIRED` (403) | `errorPayload.fallbackMethods` lists what is accepted (`WHATSAPP_OTP`, `CAPTCHA`). Retry with `otpChannel: WHATSAPP`, or with `x-captcha-token`. |
| `ATTESTATION_FAILED` (403) | Rejected (`onInvalid` / `onMissing` set to `REJECT`). |
| `ATTESTATION_CHALLENGE_INVALID` (400) | The challenge is unknown or expired. Fetch a new one and retry once. A replayed challenge is not reported; it is treated as an invalid attestation. |
| `ATTESTATION_KEY_REJECTED` (400) | iOS key registration failed. Discard the key, generate a new one. |
| `ATTESTATION_NOT_CONFIGURED` (400) | Key registration while the merchant has no iOS app configured. Keep the key and retry on a later launch. |
| `HITS_LIMIT_EXCEED` (429) | Fallback limit per number or IP reached. |

### Where it lives

- Logic: `Backend/lib/shared-services/src/AppAttestation/` (gate, policy, Play Integrity client and checks, App Attest verifier, challenge store, key registry, dashboard config). Glue in each app: `SharedLogic/AppAttestation.hs`, `Domain/Action/UI/Registration.hs`.
- Policy: the JSON column `app_attestation_config` on `rider_config` (rider) and `transporter_config` (driver), one per city. No value means `OFF`; missing fields take their defaults.
- Credentials: `merchant_service_config` services `AppAttestation_PlayIntegrity` (service account JSON and decode URL) and `Captcha_SiteVerify` (secret and verify URL), encrypted.
- Table (both schemas): `app_attest_key`, the registered iOS keys.
- Dashboard: rider `RIDER_MANAGEMENT/APP_ATTESTATION`, driver `PROVIDER_MANAGEMENT/DRIVER_APP_ATTESTATION`. `GET/POST .../config` and `POST .../iosKey/revoke`. One update writes both the policy and the credentials. Secrets are write-only. An update clears the config caches on every pod, so setting `mode` to `OFF` (the kill switch) takes effect within a minute without a deploy. New or rotated credentials are written through Redis and reach Postgres when the drainer flushes; the update clears the caches again once the row is visible there (checked every 2 s for up to 5 minutes), so a request that arrives in between does not leave the old or missing credentials cached for hours (unless the drainer is more than 5 minutes behind). Editing the rows directly in the database skips that and waits for the caches to expire.
- Config checks on update: an enforcing mode is refused until the city has at least one Android or iOS app, and Play Integrity credentials if it has Android apps. `minAndroidAttestingAppVersion` / `minIosAttestingAppVersion` must parse as versions. Optional settings (`maxAndroidRecentDeviceActivityLevel`, both minimum attesting versions, `maxOtpPerIosKeyPerDay`, `captchaMinScore`, `captchaAllowedHostnames`) are cleared by naming them in `unset`, e.g. `{"unset": ["MIN_IOS_ATTESTING_APP_VERSION"]}`.
- Rider city: a rider's city is not known before login. A new rider is checked under the merchant's default city and an existing rider under their own city. So for riders `ENFORCE_NEW_USERS` works per merchant: set it on the default city, where it covers every new rider of the merchant; on any other city it does nothing for new riders. Drivers send their city, so for them it is per city.
- A platform with no apps configured (`androidApps` or `iosApps` empty) counts as not configured: the request is an ERROR and follows `onUnsupportedOrError`, it is not treated as invalid.
- Requests the mode does not enforce on (`SHADOW`, and existing users in `ENFORCE_NEW_USERS`) are checked in the background, so they do not wait for Google.
- Several Android apps in one city: the token is decoded under each package in turn until one succeeds. If none does and any attempt failed on Google's side, the result is an ERROR, otherwise the token is invalid. What Google returns for a token decoded under the wrong package is still to be confirmed with a real token on master.
- `requireAndroidPlayRecognizedApp` (default `true`): set it to `false` only where the apps are not installed from Play, such as master debug builds. Google then reports `UNRECOGNIZED_VERSION`, which is accepted when every other check passes. `UNEVALUATED` is never accepted.
- Exempt: dashboard-initiated auth, signature auth, social-login email auth, fake-OTP test numbers.
- Observability: Prometheus `app_attestation_result_total{service, merchant_operating_city_id, platform, endpoint, outcome, decision}`; Kafka topic `app-attestation-events` (local ClickHouse: `atlas_kafka.AppAttestationEvents`). Each event carries both the decision taken and `enforcedDecision`, the one `ENFORCE_ALL` would take, so shadow data shows the impact of enforcing.
- CAPTCHA: any provider with a reCAPTCHA-style `siteverify` (form `secret`, `response`, `remoteip`; answer `success`, optional `score`, `hostname`, `apk_package_name`), e.g. reCAPTCHA, hCaptcha, Turnstile. Optional `captchaMinScore` and `captchaAllowedHostnames` (hostnames or Android package names) are checked when set.
- A device's own `x-attestation-error` is never simply allowed: if `onUnsupportedOrError` is `ALLOW`, it still gets the fallback. `ALLOW` applies only to failures on our or Google's side. An unknown or expired challenge and a busy iOS key are client-triggerable too, so while enforcing they always return `ATTESTATION_CHALLENGE_INVALID` and the app retries with a new challenge.

### Tests

- Unit: `cabal test shared-services` covers the decision table, the canonical hashes (fixed vectors), header parsing, every Play Integrity check, and App Attest verification on objects signed by a local test CA.
- Integration: Google and the phone are mocked for the local stack (`dev/mock-servers/services/playintegrity.py`, `appattest.py`). The rider and driver flow collections that use them are not part of this change yet.

### Still needed outside the backend

Phase 0 (Play Console linking, quota, service account, App Attest entitlement), the per-app package names, certificate digests and team/bundle ids in each city's config, and one real-device App Attest sample to add to the unit fixtures before enforcing on iOS.
