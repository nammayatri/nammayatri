# Plan: Integrate SVP into Pass Module

## Context

SVP (Season Value Pass) is currently a standalone module (`/svp/*`). Goal: fold SVP into the
existing pass module so it appears as a regular pass category and reuses existing APIs.
No new endpoints — only the already-existing `/svp/publicKey` remains.

The existing `handleExit` in `SVP.hs` calculates fare but **never deducts it** — the Juspay
`/txns` call is the missing piece this plan adds.

---

## End-to-End Flow

```
First time rider:
  GET /multimodal/pass/availablePasses  →  SVP pass visible (PassCategory entry)
  UI shows "Create SVP Account" / "Get SVP Pass" button
  → user taps → POST /multimodal/pass/{svpPassId}/select (first call)

Subsequent calls (every ~15s for QR refresh):
  POST /multimodal/pass/{svpPassId}/select  →  checks existing purchased_pass, checks balance,
                                               returns fresh signed QR

AFC gate ENTRY scan:
  Scanner reads QR → decodes mobileNumber, tktSlNo
  POST /svp/gate { mobileNumber, stationCode, scanType: ENTRY }
  →  creates pass_verify_transaction (journeyStatus = ENTERED, sourceStopCode)

AFC gate EXIT scan:
  POST /svp/gate { mobileNumber, stationCode, scanType: EXIT }
  →  finds ENTERED journey → calls getFares → calls loyaltyInfo → calls Juspay /txns
  →  creates purchased_pass_payment (orderId = Juspay order_id)
  →  updates pass_verify_transaction (EXITED, destinationStopCode, fareCharged)
  →  deletes tktSlNo from Redis (next entry gets fresh serial)
```

---

## Phase 1: Storage Schema Changes

### 1a. Add `SVP` to `PassEnum`
**File**: `spec/Storage/PassType.yaml`
```yaml
PassEnum:
  enum: "TouristPass, RegularPass, StudentPass, SVP"
```

### 1b. Extend `PassVerifyTransaction` for SVP journey tracking
**File**: `spec/Storage/PurchasedPass.yaml` — `PassVerifyTransaction` section

Add fields:
```yaml
journeyStatus: Maybe PassVerifyJourneyStatus   # ENTERED | EXITED | TIMED_OUT
exitTime: Maybe UTCTime
fareCharged: Maybe HighPrecMoney
currency: Maybe Currency
tktSlNo: Maybe Text                            # ticket serial from QR
```

Add enum type:
```yaml
PassVerifyJourneyStatus:
  enum: "ENTERED,EXITED,TIMED_OUT"
  derive': "Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema"
```

Add queries:
- `findActiveJourneyByMobile` — look up ENTERED journey by riderId for EXIT processing
- `updateJourneyExitById` — set exitTime, destinationStopCode, fareCharged, currency, journeyStatus=EXITED

For SVP ENTRY scans, populate existing fields as:
- `fleetId = "AFC:" <> stationCode`
- `sourceStopCode = Just stationCode`
- `vehicleNumber = Nothing` (not applicable for metro)

```sql
ALTER TABLE pass_verify_transaction ADD COLUMN journey_status TEXT;
ALTER TABLE pass_verify_transaction ADD COLUMN exit_time TIMESTAMP WITH TIME ZONE;
ALTER TABLE pass_verify_transaction ADD COLUMN fare_charged NUMERIC(30,10);
ALTER TABLE pass_verify_transaction ADD COLUMN currency TEXT;
ALTER TABLE pass_verify_transaction ADD COLUMN tkt_sl_no TEXT;
```

### 1c. Make `PurchasedPassPayment.orderId` nullable + add wallet field
**File**: `spec/Storage/PurchasedPass.yaml` — `PurchasedPassPayment` section

Changes:
- `orderId: Id PaymentOrder` → `orderId: Maybe (Id PaymentOrder)`
  - For SVP: `orderId = Just <UUID we generate and pass to Juspay /txns as order_id>`
  - `purchased_pass_payment` record is created **only at EXIT** (after Juspay /txns succeeds)
- Add `walletTransactionRef: Maybe Text` — Juspay `/txns` transaction reference from response

Remove `orderId: SecondaryKey` constraint (nullable fields can't be reliable secondary keys).

```sql
ALTER TABLE purchased_pass_payment ALTER COLUMN order_id DROP NOT NULL;
ALTER TABLE purchased_pass_payment ADD COLUMN wallet_transaction_ref TEXT;
```

### 1d. Add `svpMinBalance` to `IntegratedBPPConfig`
**File**: `spec/Storage/BecknConfig.yaml` — `IntegratedBPPConfig` section

Add as top-level field (not inside any provider sub-config):
```yaml
svpMinBalance: Maybe HighPrecMoney   # minimum wallet balance in rupees required for SVP entry
```

Fare calculation at exit uses `CallAPI.getFares integratedBppConfig ...` — no provider config
inspection needed here; `getFares` dispatches to CMRL or CMRLV2 internally.

```sql
ALTER TABLE integrated_bpp_config ADD COLUMN svp_min_balance NUMERIC(30,10);

UPDATE integrated_bpp_config
SET svp_min_balance = 50.0
WHERE platform_type = 'APPLICATION' AND vehicle_category = 'METRO'
  AND merchant_operating_city_id = '<mocid>';
```

---

## Phase 2: API Type Changes (`spec/API/Pass.yaml`)

### 2a. Extend `PassSelectionAPIEntity`
```yaml
qrData: Maybe Text                   # RSA-signed QR payload (SVP only)
tktSlNo: Maybe Text                  # ticket serial number (SVP only)
validTill: Maybe UTCTime             # QR expiry (SVP only, now + 1-2h for offline tolerance)
rechargeRequired: Maybe Bool         # true if wallet balance < svpMinBalance
currentBalance: Maybe HighPrecMoney  # live wallet balance
requiredBalance: Maybe HighPrecMoney # svpMinBalance from config
```

### 2b. Extend `PassVerifyReq` (bus pass verify — unchanged for SVP gate)
No changes needed. The SVP gate uses `/svp/gate` (existing endpoint), not this.
`vehicleNumber` remains required for bus passes.

### 2c. Extend `PurchasedPassAPIEntity`
```yaml
journeyStatus: Maybe PassVerifyJourneyStatus  # from active PassVerifyTransaction
walletBalance: Maybe HighPrecMoney            # live balance (SVP only, fetched at list time)
```

---

## Phase 3: Business Logic Changes

### 3a. Extract SVP QR helpers to `SharedLogic/SVP.hs`

Move from `Domain.Action.UI.SVP` so both `Pass.hs` and `SVP.hs` can import without circular deps:
- `buildSqdsrPlaintext`, `signPlaintext`, `loadOrBootstrapKey`, `generateAndStoreKeyPair`
- `getOrCreateTktSlNo`, `buildTktSlNo`
- Redis key helpers: `rsaPrivateKeyRedisKey`, `tktSlNoRedisKey`, `tktSlNoCounterKey`
- Encoding helpers: `toHexN`, `encodeCoord`, `encodeMobile`, `nowEpochSec`

### 3b. Modify `postMultimodalPassSelectUtil` in `Pass.hs`

Detect SVP via passEnum and route:
```haskell
passType <- CQPassType.findById pass.passTypeId
case passType >>= (.passEnum) of
  Just DPassType.SVP -> purchaseSvpPass person pass merchantId personId lat lon
  _                  -> purchasePassWithPayment ...   -- existing flow unchanged
```

### 3c. New `purchaseSvpPass` in `Pass.hs`

```
1. Fetch integratedBppConfig for METRO/APPLICATION
   → svpMinBalance = fromMaybe 50.0 integratedBppConfig.svpMinBalance

2. Call LoyaltyWallet.svpWalletBalance → walletResp
   → balance = walletResp.walletData.usableCashAmount  (HighPrecMoney, rupees)

3. If balance < svpMinBalance:
   Return PassSelectionAPIEntity
     { rechargeRequired = Just True
     , currentBalance   = Just balance
     , requiredBalance  = Just svpMinBalance
     , qrData           = Nothing
     , paymentOrder     = Nothing
     }

4. Else:
   a. Find or create PurchasedPass for this rider + pass:
      - QPurchasedPass.findActiveSvpByPersonId personId
      - If Nothing:  create PurchasedPass { status=Active, amount=0, ... }
      - If Just pp:  reuse it (handles the 15s refresh — no new DB record)

   b. getOrCreateTktSlNo (from SharedLogic.SVP)

   c. buildSqdsrPlaintext + signPlaintext with:
        { tktSlNo, now, balancePaisa, txnRef=riderId, mobile, lat, lon }

   d. Return PassSelectionAPIEntity
        { qrData           = Just qrPayload
        , tktSlNo          = Just slNo
        , validTill        = Just (now + 2h)   -- long enough for offline QR cache
        , rechargeRequired = Just False
        , currentBalance   = Just balance
        , paymentOrder     = Nothing
        }
```

**Note**: `purchased_pass_payment` is NOT created here. It is created at EXIT (after actual
deduction). SELECT is only a balance check + QR generation.

### 3d. Enhance `handleEntry` in `SVP.hs`

After creating `SvpJourney` (existing), additionally create `PassVerifyTransaction`:
```haskell
pvtId <- generateGUID
QPassVerifyTransaction.create PassVerifyTransaction
  { id              = Id pvtId
  , purchasedPassId = activePurchasedPass.id
  , riderId         = person.id
  , fleetId         = "AFC:" <> stationCode
  , sourceStopCode  = Just stationCode
  , journeyStatus   = Just ENTERED
  , tktSlNo         = Just tktSlNo'
  , verifiedAt      = entryTime
  , validTill       = addUTCTime (8 * 3600) entryTime
  , vehicleNumber   = Nothing
  , ...
  }
```

Look up active SVP `PurchasedPass` by riderId to populate `purchasedPassId`.

### 3e. Enhance `handleExit` in `SVP.hs`

Current `handleExit` calculates fare but never deducts. Fix:

```
1. Find ENTERED PassVerifyTransaction for this rider (by riderId + journeyStatus=ENTERED)

2. Fetch integratedBppConfig → CallAPI.getFares (same as existing code)
   → fareAmount :: HighPrecMoney

3. Get Juspay program details via loyaltyInfo:
   → loyaltyResp = LoyaltyWallet.loyaltyInfo (getId person.id) merchantId merchantOperatingCityId
   → programId    = loyaltyResp.programs[0].id_
   → burnOptionId = loyaltyResp.programs[0].burnOptions[0].id
   → paymentMethod = loyaltyResp.programs[0].paymentMethod   -- e.g. "LOYALTYOS1"
   → gatewayId    = loyaltyResp.programs[0].gatewayId        -- e.g. "1207"

4. Generate orderId = generateGUID  (this is the order_id we pass to Juspay)

5. Call Juspay /txns:
   POST https://api.juspay.in/txns
   {
     order_id: orderId,
     merchant_id: <from config>,
     payment_method_type: "REWARD",
     payment_method: paymentMethod,
     gateway_id: gatewayId,
     loyalty_os_details: {
       program_id: programId,
       burn_options_selected: [{ id: burnOptionId, points: show fareAmount }]
     },
     format: "json"
   }
   → txnRef = juspayResp.transactionRef (or similar field)

6. Create PurchasedPassPayment:
   Qpurchased_pass_payment.create PurchasedPassPayment
     { orderId             = Just (Id orderId)
     , walletTransactionRef = Just txnRef
     , purchasedPassId     = pvt.purchasedPassId
     , status              = Active
     , amount              = fareAmount
     , currency            = INR
     , ...
     }

7. Update PassVerifyTransaction:
   QPassVerifyTransaction.updateJourneyExitById
     { journeyStatus       = EXITED
     , destinationStopCode = Just stationCode
     , exitTime            = Just now
     , fareCharged         = Just fareAmount
     , currency            = Just INR
     } pvt.id

8. Update SvpJourney (existing):
   QSvpJourney.updateStatusAndExitDetailsById EXITED ...

9. Delete tktSlNo from Redis:
   Hedis.del (tktSlNoRedisKey person.id)
   -- next ENTRY gets a fresh serial number

10. Return GateCallbackResp { allowed=True, fareCharged=Just fareAmount }
```

### 3f. Modify `getMultimodalPassList` in `Pass.hs`

For SVP passes (`passEnum == SVP`):
- Call `LoyaltyWallet.svpWalletBalance` once per rider
- Find active `PassVerifyTransaction` with `journeyStatus = ENTERED` for the pass
- Populate `walletBalance` and `journeyStatus` in `PurchasedPassAPIEntity`

---

## Phase 4: Data Seed Migrations

```sql
-- 1. PassCategory
INSERT INTO pass_category (id, name, description, merchant_id, merchant_operating_city_id, created_at, updated_at)
VALUES (gen_random_uuid(), 'SVP', 'Season Value Pass', '<merchant_id>', '<mocid>', now(), now());

-- 2. PassType (pass_enum = 'SVP')
INSERT INTO pass_type (id, pass_category_id, name, title, "order", pass_enum,
                       merchant_id, merchant_operating_city_id, created_at, updated_at)
VALUES (gen_random_uuid(), '<category_id>', 'SVP', 'Smart Value Pass', 1, 'SVP',
        '<merchant_id>', '<mocid>', now(), now());

-- 3. Pass (amount=0 — wallet-funded, no upfront payment)
INSERT INTO pass (id, pass_type_id, code, amount, benefit_description,
                  applicable_vehicle_service_tiers, purchase_eligibility_json_logic,
                  redeem_eligibility_json_logic, "order", enable, auto_apply,
                  verification_validity, documents_required,
                  merchant_id, merchant_operating_city_id, created_at, updated_at)
VALUES (gen_random_uuid(), '<pass_type_id>', 'SVP_WALLET', 0,
        'Wallet-funded metro travel', '{METRO}', '[]', '[]', 1,
        true, false, 9000, '{}',
        '<merchant_id>', '<mocid>', now(), now());
```

---

## Phase 5: File Summary

| File | Change | Description |
|------|--------|-------------|
| `spec/Storage/PassType.yaml` | Modify | Add `SVP` to `PassEnum` |
| `spec/Storage/PurchasedPass.yaml` | Modify | `orderId` nullable + `walletTransactionRef` on `PurchasedPassPayment`; SVP journey fields + `PassVerifyJourneyStatus` on `PassVerifyTransaction` |
| `spec/Storage/BecknConfig.yaml` | Modify | Add `svpMinBalance: Maybe HighPrecMoney` to `IntegratedBPPConfig` |
| `spec/API/Pass.yaml` | Modify | Extend `PassSelectionAPIEntity` (QR fields); extend `PurchasedPassAPIEntity` (walletBalance, journeyStatus) |
| `src/SharedLogic/SVP.hs` | **New** | QR crypto helpers extracted from `SVP.hs` |
| `src/Domain/Action/UI/Pass.hs` | Modify | SVP branch in select (`purchaseSvpPass`); list enrichment |
| `src/Domain/Action/UI/SVP.hs` | Modify | `handleEntry`: create `PassVerifyTransaction`; `handleExit`: add Juspay `/txns` deduction + create `PurchasedPassPayment` |
| `dev/migrations/rider-app/<date>_svp_pass_integration.sql` | **New** | All schema + seed migrations |

All paths relative to `app/rider-platform/rider-app/Main/`.

---

## Phase 6: Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| Gate webhook stays at `/svp/gate` | AFC already integrated to this URL; no migration needed |
| Gate identified by `mobileNumber` from QR | Gate scanner decodes mobile from QR payload; doesn't know purchasedPassId |
| `purchased_pass_payment` created at EXIT only | No payment happens at SELECT — only balance check. Deduction triggers payment record creation |
| `orderId` = UUID we generate, passed to Juspay as `order_id` | Juspay `/txns` takes order_id as input; we own the ID, Juspay returns transaction ref |
| Balance in QR is informational only | Gate always calls backend (always online); balance in QR is for offline display, not trust anchor |
| QR `validTill = now + 2h` | Handles offline scenario — cached QR works even with no signal; gate still does live balance check |
| `svpMinBalance` from `IntegratedBPPConfig` (top-level) | Not inside any provider sub-config — accessible without pattern-matching on providerConfig |

---

## Phase 7: Verification Checklist

1. `, run-generator` after YAML changes → no errors
2. `cabal build all` → zero warnings (`-Werror`)
3. End-to-end test:
   - `GET /multimodal/pass/availablePasses` → SVP category appears
   - `POST /multimodal/pass/{svpPassId}/select` (balance ≥ svpMinBalance) → `qrData`, `tktSlNo`, `validTill` in response, no `purchased_pass_payment` created
   - `POST /multimodal/pass/{svpPassId}/select` (balance < svpMinBalance) → `rechargeRequired: true`, balances populated
   - `POST /multimodal/pass/{svpPassId}/select` again (15s later) → fresh QR, no new `purchased_pass` record
   - `GET /multimodal/pass/list` → SVP pass with `walletBalance` and `journeyStatus`
   - `POST /svp/gate` with `scanType=ENTRY` → `PassVerifyTransaction` created (journeyStatus=ENTERED), no payment record
   - `POST /svp/gate` with `scanType=EXIT` → Juspay `/txns` called, `PurchasedPassPayment` created, `PassVerifyTransaction` updated (EXITED, fareCharged set), tktSlNo deleted from Redis
   - Second `POST /svp/gate` ENTRY after exit → fresh tktSlNo issued
   - `GET /svp/publicKey` → unchanged, no regression
