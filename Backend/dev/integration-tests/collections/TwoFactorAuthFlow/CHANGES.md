# TwoFactorAuthFlow — Fix Summary

## What this flow tests

End-to-end 2FA setup for a dashboard user:
1. Login
2. Switch to merchant → server says "2FA mandatory, not yet enabled"
3. Initiate 2FA setup → OTP sent to phone
4. Verify OTP → server generates TOTP secret, returns QR code
5. Switch to merchant again with live TOTP → server returns auth token
6. Logout

---

## Issues found and fixed

### Issue 1 — Wrong environment selected → `MERCHANT_DOES_NOT_EXIST`

**Error**: `{"errorCode": "MERCHANT_DOES_NOT_EXIST", "errorMessage": "No merchant matches passed data \"\""}`

**Root cause**: The BAP flow uses `{{bap_short_id}}` in the Switch Merchant request body.
The environment "Delhi (BTP_Delhi)" is the **BPP/provider-dashboard** environment — it only
has `bpp_short_id`, not `bap_short_id`. So `{{bap_short_id}}` resolved to an empty string `""`.

**Fix**: Select **"Delhi (BT_Delhi)"** in the environment dropdown when running the BAP 2FA Flow.
(`Local_BT_Delhi.postman_environment.json` has `bap_short_id = "BHARAT_TAXI"`)

---

### Issue 2 — `is2fa_mandatory = false` → flow aborts at first switch

**Error**: Test assertion failed — `is2faMandatory` was `false`, test said
"Enable 2FA mandatory on merchant before running this flow".

**Root cause**: The BHARAT_TAXI merchant in the local DB had `is2fa_mandatory = false`.
The `juspay_admin` user has role `JUSPAY_ADMIN` → `dashboard_access_type = DASHBOARD_ADMIN`.
The merchant's `two_factor_mandatory_for_roles = [DASHBOARD_USER]` — admin role is excluded.
So `is2FARequiredForPerson` returned `false` for `juspay_admin`.

**Fix**: Feature migration `0023-enable-2fa-mandatory-for-bharat-taxi.sql`
```sql
UPDATE atlas_bap_dashboard.merchant SET is2fa_mandatory = true WHERE short_id = 'BHARAT_TAXI';
```
This makes 2FA mandatory for all users of this merchant regardless of role.

---

### Issue 3 — `HITS_LIMIT_EXCEED` on login after repeated runs

**Error**: `{"errorCode": "HITS_LIMIT_EXCEED", "errorMessage": "Hits limit reached. Try again in 600 sec."}`

**Root cause**: The login endpoint uses a sliding-window rate limiter keyed on
`dashboard:Email:<email>:hitsCount` in Redis. Running the test several times
(each run calls login + initiate 2FA which also checks the same key) exhausts the limit.

**Fix**: Clear the Redis key manually between runs:
```bash
redis-cli -p 30002 DEL "dashboard:Email:juspay_admin@dashboard.com:hitsCount"
```

---

### Issue 4 — `Authenticator OTP does not match` at final switch step

**Error**: Step 5 "Switch Merchant And City Copy" returned 200 but TOTP validation failed.
`authToken` was empty, test assertion "expected true to equal false" triggered.

**Root cause**: The environment has `switch_otp = "526925"` hardcoded. TOTP codes are
time-based (change every 30 seconds) so a static value can never work. The pre-request
script added in an earlier attempt used `CryptoJS.HmacSHA1(...)` which is a Postman
Desktop global — it does **not** exist in this test runner's sandboxed `new Function()`
context. The script threw `ReferenceError: CryptoJS is not defined` silently, leaving
`switch_otp` as the wrong hardcoded value.

**Fix**: Replaced `CryptoJS` usage with a pure-JS SHA1 + HMAC-SHA1 + TOTP implementation
in the `prerequest` script of "Switch Merchant And City Copy" (both BAP and BPP flows).
The script:
1. Reads `bap_2fa_secret` (set in step 4 from the QR code URI)
2. Base32-decodes it
3. Computes HMAC-SHA1(secret, floor(now/30)) — the TOTP algorithm
4. Truncates to 6 digits and writes to `switch_otp`

No external libraries needed — only JS arithmetic available in every sandbox.

---

## Files changed

| File | Change |
|------|--------|
| `BAP_2FA_Flow.json` | Added `prerequest` script on "Switch Merchant And City Copy" to compute TOTP dynamically |
| `BPP_2FA_Flow.json` | Same fix for the BPP flow |
| `Local/Local_BT_Delhi.postman_environment.json` | *(use this env, not BTP_Delhi)* |
| `dev/feature-migrations/0023-enable-2fa-mandatory-for-bharat-taxi.sql` | Enables `is2fa_mandatory` for BHARAT_TAXI merchant |

## Re-run checklist

Before each test run:
1. Ensure environment is **"Delhi (BT_Delhi)"** (not BTP_Delhi)
2. Reset 2FA state so setup steps run fresh:
   ```sql
   UPDATE atlas_bap_dashboard.merchant_access
   SET is2fa_enabled = false, secret_key = NULL
   WHERE person_id = '3680f4b5-dce4-4d03-aa8c-5405690e87bd'
     AND merchant_short_id = 'BHARAT_TAXI';
   ```
3. If rate-limited: `redis-cli -p 30002 DEL "dashboard:Email:juspay_admin@dashboard.com:hitsCount"`
