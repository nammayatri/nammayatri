# Ride Invoice Parity (BPP CUSTOMER ↔ BAP RIDER) — In-place Bug Fixes

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the driver side's (BPP) CUSTOMER Ride invoice and the customer side's (BAP) RIDER Ride invoice identical in labels, order, amounts and metadata.

**Architecture:** Fix each divergence where it lives. No new modules, no new shared functions. Per item, change whichever side needs the smaller edit.

**Tech Stack:** Haskell (`-Werror`), NammaDSL, finance-kernel, beckn-spec.

## Global Constraints

- Never edit `src-read-only/`. Run `, run-generator --apply-hint` after YAML changes.
- The user runs all builds. Hand back edits unverified.
- No code comments.
- Minimal diff: no new modules or helpers. Inline at the existing sites.

## Files

- BPP invoice: `app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Ride/EndRide/Internal.hs:654-760` (`mkRideLineItems`, `customerInvoiceConfig`)
- BPP breakup: `app/provider-platform/dynamic-offer-driver-app/Main/src/Beckn/OnDemand/Utils/OnUpdate.hs:54-70`
- BPP tags: `app/provider-platform/dynamic-offer-driver-app/Main/src/Beckn/ACL/OnConfirm.hs:60-115`
- BAP invoice: `app/rider-platform/rider-app/Main/src/SharedLogic/Finance/RidePayment.hs` (`buildRidePaymentInvoiceConfig` 893-943, `createRidePaymentLedger` 435-500, `upsertCoreRidePaymentLedger` 545-660, `applyBookingProviderFieldsToCtx` 413)
- BAP ledger info: `app/rider-platform/rider-app/Main/src/SharedLogic/Payment.hs:830-920`
- BAP ctx and call sites: `app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/Common.hs:640-700, 1085-1140`

## Target (both sides end up like this)

| # | Group | Label / `descriptionType` |
|---|---|---|
| 1 | g-ride Fare | "Ride Fare" / `RideFare` |
| 2 | g-ride Tax | "Ride Tax" / `RideTax` |
| 3 | g-toll Fare (ext) | "Toll Charges" / `TollCharges` |
| 4 | g-toll Tax (ext) | "Toll Charges Tax" / `TollChargesTax` |
| 5 | g-parking Fare (ext) | "Parking Charges" / `ParkingCharges` |
| 6 | g-parking Tax (ext) | "Parking Charges Tax" / `ParkingChargesTax` |
| 7 | g-airport Fare (ext) | "Airport Entry Fee" / `AirportEntryFeeCharge` |
| 8 | g-airport Tax (ext) | "Airport Entry Fee Tax" / `AirportEntryFeeChargeTax` |
| 9 | g-cancel Fare | "Customer Cancellation Fee" / `CustomerCancellationFee` |
| 10 | g-cancel Tax | "Customer Cancellation Fee Tax" / `CancellationFeeVat` (VAT) or `GstOnCancellationFee` (GST) |
| 11 | g-payment Fare | "Payment Charge" / `PaymentCharge` |
| 12 | g-payment Tax | "Payment Charge VAT" / `PaymentChargeTax` |
| 13 | Adjustment | "Discount" / `OfferDiscount` (negative, clamped) |
| 14 | Adjustment | "Tip" / `Tip` |

Rows 1–2 and 9–14 already use the BPP's labels or match on both sides, so the BPP changes only rows 1–2. The BAP changes rows 3–6 and adds rows 9–10.

---

### Task 1: BPP — labels, order, payment charge source, discount clamp

**File:** `EndRide/Internal.hs`

- [ ] **Step 1: Labels (rows 1–2)** in `rideAndTollLines`:

```haskell
                [ mkPair "g-ride" Fare False "Ride Fare" RideFare rawBaseFare,
                  mkPair "g-ride" Tax False "Ride Tax" RideTax taxAmount,
```

This also changes the driver/fleet copy, which shares `mkRideLineItems`. That is intended.

- [ ] **Step 2: Order.** In `commonLines`, move the two `issuedToType == CUSTOMER && customerBearsPayment` payment pair lines to the top of the list, before the Discount line. The CUSTOMER order is then cancel → payment → discount → tip. The supplier copy is unaffected because those lines are `Nothing` for it.

- [ ] **Step 3: Payment charge amount from fare params** (this is what the BPP sends in the breakup and what the BAP itemises). For the customer pair only:

```haskell
                  if issuedToType == CUSTOMER && customerBearsPayment then mkPair "g-payment" Fare False "Payment Charge" PaymentCharge (fromMaybe 0 fareParams.paymentProcessingFee) else Nothing,
                  if issuedToType == CUSTOMER && customerBearsPayment then mkPair "g-payment" Tax False "Payment Charge VAT" PaymentChargeTax (fromMaybe 0 fareParams.paymentProcessingFeeTax) else Nothing,
```

`rawBaseFare` already subtracts exactly these two fields, so the totals stay consistent. The supplier deduction keeps `paymentChargeAmt`/`paymentChargeVatAmt`.

- [ ] **Step 4: Discount clamp.** The customer Discount line uses the clamped amount, the same one the ledger already uses:

```haskell
                  if issuedToType == CUSTOMER then mkAdjustment "Discount" OfferDiscount (negate discountSubsidyAmount) else Nothing,
```

- [ ] **Step 5: Hand off the build** (`cabal build dynamic-offer-driver-app`), then commit:

```bash
git commit -am "backend/fix: align BPP customer ride invoice labels, order and amounts with BAP"
```

---

### Task 2: BPP — EDC rides must not send parking/airport in the RIDE_COMPLETED breakup

The BPP `fare` excludes parking and airport when EDC collects them (`FareCalculator.fareSum`), but `mkFareParamsBreakups` still emits those lines. The BAP then subtracts them from Ride Fare (understating it) and itemises them. The BPP invoice already zeroes them. Fix at the source, for the ride-completed payload only. Estimate and on_init breakups keep parking, because `Domain/Action/UI/Payment.hs:219` on the BAP reads it from there.

**File:** `Beckn/OnDemand/Utils/OnUpdate.hs:66-67`

- [ ] **Step 1:**

```haskell
      fareBreakup =
        Fare.mkFareParamsBreakups isValueAddNP (mkPrice' currency) mkBreakupItem $
          if SL.edcCollectsParking fareParams.fareSettlementType
            then fareParams {parkingCharge = Nothing, parkingChargeTax = Nothing, airportEntryFee = Nothing, airportEntryFeeTax = Nothing, parkingTaxCgst = Nothing, parkingTaxSgst = Nothing, parkingTaxIgst = Nothing, airportEntryFeeTaxCgst = Nothing, airportEntryFeeTaxSgst = Nothing, airportEntryFeeTaxIgst = Nothing, customerGateFeeItems = Nothing}
            else fareParams
```

Import `SL` the same way `EndRide/Internal.hs` does. Check that `mkFareParamsBreakups` builds the airport line from `airportEntryFee` + `customerGateFeeItems` only (`FareCalculator.hs:~150-155`), so nothing else leaks.

- [ ] **Step 2: Hand off the build**, then commit:

```bash
git commit -am "backend/fix: drop EDC-collected parking and airport from ride-completed breakup"
```

---

### Task 3: BAP — labels, order, no cashback, regime-based isVat

**File:** `SharedLogic/Finance/RidePayment.hs`

- [ ] **Step 1:** Add a `Tax.Regime` parameter to `buildRidePaymentInvoiceConfig` and remove its `cashbackPayoutAmount` parameter. Replace the `lineItems` list and `isVat`:

```haskell
          lineItems =
            catMaybes
              [ mkLineItem "Ride Fare" RideFare preDiscountFareLine False Fare (Just "g-ride"),
                mkLineItem "Ride Tax" RideTax preDiscountTax False Tax (Just "g-ride"),
                mkLineItem "Toll Charges" TollCharges tollFare True Fare (Just "g-toll"),
                mkLineItem "Toll Charges Tax" TollChargesTax tollTaxAmount True Tax (Just "g-toll"),
                mkLineItem "Parking Charges" ParkingCharges parkingCharge True Fare (Just "g-parking"),
                mkLineItem "Parking Charges Tax" ParkingChargesTax parkingChargeTax True Tax (Just "g-parking"),
                mkLineItem "Airport Entry Fee" AirportEntryFeeCharge airportEntryFee True Fare (Just "g-airport"),
                mkLineItem "Airport Entry Fee Tax" AirportEntryFeeChargeTax airportEntryFeeTax True Tax (Just "g-airport"),
                mkLineItem "Customer Cancellation Fee" CustomerCancellationFee cancellationCharge False Fare (Just "g-cancel"),
                mkLineItem "Customer Cancellation Fee Tax" (if taxRegime == Tax.VAT then CancellationFeeVat else GstOnCancellationFee) cancellationTax False Tax (Just "g-cancel"),
                mkLineItem "Payment Charge" PaymentCharge paymentChargeAmount False Fare (Just "g-payment"),
                mkLineItem "Payment Charge VAT" PaymentChargeTax paymentChargeTax False Tax (Just "g-payment"),
                mkDeductionLineItem "Discount" OfferDiscount offerDiscountAmount False
              ],
          gstBreakdown = gstBreakdown,
          isVat = taxRegime == Tax.VAT,
```

The new parameters are `taxRegime`, `(cancellationCharge, cancellationTax)` and `gstBreakdown :: Maybe GstAmountBreakdown` (Task 4 and Task 5 supply the values). Update the call in `createRidePaymentLedger` to pass `taxRegime` (already in scope) and drop `cashbackPayoutAmount` from that call. The cashback ledger leg stays.

- [ ] **Step 2:** If `CashbackOffer` and `TollFare`/`TollTax` imports become unused, remove them (`-Werror`).

---

### Task 4: BAP — carried cancellation dues inside the Ride invoice

**File:** `SharedLogic/Finance/RidePayment.hs`

- [ ] **Step 1:** Add a `(HighPrecMoney, HighPrecMoney)` `(cancellationCharge, cancellationTax)` parameter to `createRidePaymentLedger`, after the payment-charge tuple. Post the legs in the same `runFinance` block, before `invoice`, so they link to the Ride invoice:

```haskell
    postRiderObligation cancellationCharge ridePaymentRefCancellationFee
    postRiderObligation cancellationTax (ridePaymentRefCancellationGST taxRegime)
```

Pass the tuple to `buildRidePaymentInvoiceConfig`.

- [ ] **Step 2:** In `upsertCoreRidePaymentLedger`:
  - add `+ cancellationCharge + cancellationTax` to `newTotal`;
  - add `ridePaymentRefCancellationFee : map ridePaymentRefCancellationGST taxRegimes` to `coreRidePaymentRefTypes`;
  - pass `(cancellationCharge, cancellationTax)` in `doCreate`;
  - replace the `cancelIds` block (which called `createPendingCancellationFeeLedger` and minted a separate `RideCancellation` invoice) with:

```haskell
  pure coreResult {cancellationEntryIds = []}
```

The cancellation entries are now part of `coreEntryIds`, so `Common.hs:1124`'s `coreEntryIds <> cancellationEntryIds` still settles them exactly once.

- [ ] **Step 3:** `createFullyDiscountedRidePaymentLedger` and `Common.hs:697` pass `(ledgerInfo.cancellationCharge, ledgerInfo.cancellationTax)` to `createRidePaymentLedger`. `createFullyDiscountedRidePaymentLedger` gets the same extra tuple parameter, and its caller `SharedLogic/Payment.hs:1728` passes it from its ledger info.

---

### Task 5: BAP — GST split on the invoice

The BPP sends `RIDE_FARE_CGST` / `RIDE_FARE_SGST` / `RIDE_FARE_IGST` breakup lines (`FareCalculator.fareTaxesOf`). The BAP currently passes `gstBreakdown = Nothing`.

**File:** `SharedLogic/Payment.hs` (`RidePaymentLedgerInfo`, `buildLedgerInfoFromBreakups`)

- [ ] **Step 1:** Add a field `gstBreakdown :: Maybe GstAmountBreakdown` to `RidePaymentLedgerInfo` and a matching last argument to `mkRidePaymentLedgerInfo`. In `buildLedgerInfoFromBreakups` pass:

```haskell
    (let amountOf tag = sum [l.amount | l <- breakupLines, l.title `elem` FB.tagTitles (tag ((.rideFare) FB.taxTags))]
         positive x = if x > 0 then Just x else Nothing
         (c, s, i) = (amountOf (.cgst), amountOf (.sgst), amountOf (.igst))
      in if c + s + i > 0 then Just GstAmountBreakdown {cgstAmount = positive c, sgstAmount = positive s, igstAmount = positive i} else Nothing)
```

- [ ] **Step 2:** Thread `gstBreakdown` from the ledger info through `upsertCoreRidePaymentLedger` / `createFullyDiscountedRidePaymentLedger` / `createRidePaymentLedger` into `buildRidePaymentInvoiceConfig`, next to the cancellation tuple. Update the 4 call sites (`Common.hs:697, 1103`, `Payment.hs:1133, 1728`).

- [ ] **Step 3: Check the BPP source (no code yet).** The BPP invoice uses `computeGstBreakdownForRideOwner` (driver/fleet residence state vs pickup state). The breakup carries the fare-time split from `Tax.taxOn ... rideGst`. On a ride where the driver's residence state differs from the pickup state, compare the BPP invoice's `igst_amount` with the `RIDE_FARE_IGST` line. If they differ, the fix is in `EndRide/Internal.hs`: write `rideGstBreakdown` into `fareParams.rideTax{Cgst,Sgst,Igst}` with a `QFare` update before the RIDE_COMPLETED on_update is built. Confirm with the user before doing that.

- [ ] **Step 4: Hand off the build** (`cabal build rider-app`), then commit Tasks 3–5:

```bash
git commit -am "backend/fix: align BAP ride invoice lines, cancellation dues, isVat and GST split with BPP"
```

---

### Task 6: Metadata — issuer tax numbers and supplier on BAP

The BAP ctx never sets `merchantGstin`/`merchantVatNumber`, so `issuedByTaxNo` is empty (`FinanceM.hs:908`). For non-fleet rides, `on_confirm` sends the merchant as supplier while the BPP invoice uses the driver.

- [ ] **Step 1: Tags.** In `lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs`, add `ISSUED_BY_GSTIN` and `ISSUED_BY_TAX_NO` next to `ISSUED_BY_ADDRESS`, and map both to `BPP_INVOICE_INFO`.

- [ ] **Step 2: BPP `OnConfirm.hs`.** Add `issuedByGstin = (mbMerchantOpCity >>= (.gstin)) <|> merchant.gstin` and `issuedByTaxNo = merchant.vatNumber` to `BPPInvoiceInfo` (both branches), and emit them in `mkBPPInvoiceInfoTagGroup`. In the non-fleet branch, when `res.rideInfo` is `Just`, set the supplier to what `buildFinanceCtx` uses:

```haskell
          supplierName = res.rideInfo <&> \ri -> ri.driver.firstName <> maybe "" (" " <>) ri.driver.lastName,
          supplierAddress = Nothing,
          supplierGSTIN = Nothing,
          supplierTaxNo = Nothing,
          supplierId = res.rideInfo <&> (.driver.id.getId)
```

Keep today's merchant values when `res.rideInfo` is `Nothing`. Check that the RIDE_ASSIGNED on_update also emits this tag group (`grep -rn "mkBPPInvoiceInfoTagGroup\|resolveBPPInvoiceInfo" app/provider-platform`).

- [ ] **Step 3: BAP storage.** In `app/rider-platform/rider-app/Main/spec/Storage/Booking.yaml`, add `issuedByGstin: Maybe Text` and `issuedByTaxNo: Maybe Text` next to `supplierTaxNo`. Run `, run-generator --apply-hint`.

- [ ] **Step 4: BAP parse.** In `Beckn/ACL/OnConfirm.hs:71-80`, `Domain/Action/Beckn/OnConfirm.hs:75-95` and `Common.hs:528-540`, add the two fields with the same `getTag` / `<|>` pattern as `issuedByAddress`.

- [ ] **Step 5: BAP ctx.** In `applyBookingProviderFieldsToCtx`:

```haskell
      merchantGstin = booking.issuedByGstin <|> ctx.merchantGstin,
      merchantVatNumber = booking.issuedByTaxNo <|> ctx.merchantVatNumber,
```

---

### Task 7: Metadata — name, address, place of supply on BAP

**Files:** `Domain/Action/Beckn/Common.hs` (cash ctx ~1092, online ctx ~1128, ride-assigned ctx ~650), `SharedLogic/Finance/RidePayment.hs`

- [ ] **Step 1: Name.** The BPP stores `riderName = person.firstName` (BAP `OnInit.hs:171`). At each `buildRiderFinanceCtx` site, replace `listToMaybe $ catMaybes [person.firstName, person.middleName, person.lastName]` with `person.firstName`.

- [ ] **Step 2: Address.** The BPP `fullAddress` is a `", "` join of the non-empty fields in the order from `provider-platform/.../Beckn/OnDemand/Utils/Common.hs:~372-387` (copy the exact list and order). At each `buildRiderFinanceCtx` site, replace `listToMaybe $ catMaybes [area, street, city]` with that same join over `booking.fromLocation.address`.

- [ ] **Step 3: Place of supply.** The BPP stamps `updatePlaceOfSupply (pickup state)` on the customer invoice. On the BAP, in `createRidePaymentLedger`'s `Right (mbInvoiceId, entryIds)` branch:

```haskell
    Right (mbInvoiceId, entryIds) -> do
      whenJust mbInvoiceId $ QInvoice.updatePlaceOfSupply placeOfSupply Nothing Nothing
      pure $ Right RidePaymentLedgerResult {invoiceId = mbInvoiceId, entryIds}
```

`placeOfSupply :: Maybe Text` is a new argument to `createRidePaymentLedger`. Callers pass `booking.fromLocation.address.state` with whitespace stripped and empty treated as `Nothing`, the same way the BPP's `gstPlaceOfSupply` does. Check that `QInvoice.updatePlaceOfSupply` exists in finance-kernel's generated queries (the BPP uses `QFInvoice.updatePlaceOfSupply`).

- [ ] **Step 4: Hand off the build**, then commit Tasks 6–7:

```bash
git commit -am "backend/fix: identical issuer, supplier and party metadata on BAP ride invoice"
```

---

### Task 8: Verify end to end

- [ ] For each of these rides, compare `line_items`, `is_vat`, CGST/SGST/IGST, `payment_mode`, `issued_to_name`, `issued_to_address`, `issued_by_address`, `supplier_*`, `merchant_gstin`/issuer tax no, and `place_of_supply` between `atlas_driver_offer_bpp` (CUSTOMER, Ride, not Voided) and `atlas_app` (RIDER, Ride, not Voided):
  - cash GST ride
  - online VAT ride with an offer discount
  - ride with carried cancellation dues
  - EDC ride with parking and an airport fee
  - ride with a tip
  - cashback ride
- [ ] Allowed differences: invoice id/number, `issued_to_type`, `issued_to_id`, `reference_id`, B2C QR.

## Not covered (call out to the user)

- **`paymentMode`:** the BPP forces ONLINE when `driverWalletConfig.forceOnlineLedger` is set, which the BAP cannot see. Mention this only if any merchant enables it.
- **Cancellation dues on the cancelled ride:** both sides also mint a `RideCancellation` invoice when the cancellation happens (BPP `CancelRide/Internal.hs:424`, BAP `RidePayment.hs:1244`). That behaviour is unchanged.
