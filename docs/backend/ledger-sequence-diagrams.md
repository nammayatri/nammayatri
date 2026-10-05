# Ledger Entry Sequence Diagrams

## 1. Normal Ride (India/GST) — Online Payment — Ride Completion

```mermaid
sequenceDiagram
    participant Customer
    participant BAP as BAP (Rider App)
    participant BPP as BPP (Driver App)
    participant FK as Finance Kernel
    participant Passetto as Passetto

    Customer->>BAP: Complete ride
    BAP->>BPP: on_update (ride end)
    BPP->>BPP: calculateFareParameters
    Note over BPP: baseFare=100, GST=18,<br/>toll=20, parking=5,<br/>commission=10

    BPP->>FK: createDriverWalletTransaction (Online)
    Note over FK: --- BPP Ledger Entries ---

    FK->>FK: transfer BuyerAsset → BuyerExternal (BaseRide: 100)
    FK->>FK: transfer BuyerExternal → OwnerLiability (BaseRide: 100)
    Note right of FK: Dr BuyerAsset, Cr OwnerLiability<br/>Driver gets ride fare

    FK->>FK: transfer BuyerAsset → BuyerExternal (GSTOnline: 18)
    FK->>FK: transfer BuyerExternal → GovtIndirect (GSTOnline: 18)
    Note right of FK: Dr BuyerAsset, Cr GovtIndirect<br/>GST to government

    FK->>FK: transfer BuyerAsset → BuyerExternal (TollCharges: 20)
    FK->>FK: transfer BuyerExternal → OwnerLiability (TollCharges: 20)
    Note right of FK: Toll pass-through to driver

    FK->>FK: transfer BuyerAsset → BuyerExternal (ParkingCharges: 5)
    FK->>FK: transfer BuyerExternal → OwnerLiability (ParkingCharges: 5)

    FK->>FK: transfer OwnerLiability → SellerRevenue (Commission: 10)
    Note right of FK: Platform commission deducted from driver

    FK->>FK: mint Driver Invoice
    Note over FK: Line items: Base Fare, Tax,<br/>Toll Charges, Parking,<br/>Platform Commission

    BPP-->>BAP: fare breakups (on_confirm/on_update)

    BAP->>FK: createRidePaymentLedger (Online)
    Note over FK: --- BAP Ledger Entries ---

    FK->>FK: transferPending OwnerLiability → BuyerAsset (RideFare: 100)
    FK->>FK: transferPending OwnerLiability → BuyerAsset (RideGST: 18)
    FK->>FK: transferPending OwnerLiability → BuyerAsset (PlatformFee: 0)
    FK->>FK: mint Customer Invoice
    Note over FK: Line items: Ride Fare, GST,<br/>Toll, Parking
```

## 2. International Ride (Finland/VAT) — Online Payment — Ride Completion with Discount

```mermaid
sequenceDiagram
    participant Customer
    participant BAP as BAP (Rider App)
    participant BPP as BPP (Driver App)
    participant FK as Finance Kernel

    Customer->>BAP: Complete ride
    BAP->>BPP: on_update (ride end)

    BPP->>BPP: calculateFareParameters
    Note over BPP: Partition via vatChargeConfig.appliesOn:<br/>rideFareNoTaxApplicable=30 (exempt)<br/>rideFareTaxExclusive=50 (taxable)<br/>rideTax=6.75 (13.5%)<br/>tollFareTaxExclusive=15<br/>tollTax=2.03 (13.5%)<br/>discount=8.68 (from BAP)

    BPP->>BPP: Emit wire breakups
    Note over BPP: RIDE_FARE_NO_TAX_APPLICABLE=30<br/>RIDE_FARE_TAX_EXCLUSIVE=50<br/>RIDE_TAX=6.75<br/>TOLL_FARE_TAX_EXCLUSIVE=15<br/>TOLL_TAX=2.03<br/>+ RIDE_TAX_MULTIPLIER=1.135<br/>+ TOLL_TAX_MULTIPLIER=1.135

    BPP->>FK: createDriverWalletTransaction (Online, isVat=true)
    Note over FK: --- BPP Ledger Entries (VAT) ---

    rect rgb(40, 60, 80)
        Note over FK: baseFarePostDiscount = baseFare - discount
        FK->>FK: transfer BuyerAsset → BuyerExternal (BaseRide: postDiscountAmt)
        FK->>FK: transfer BuyerExternal → OwnerLiability (BaseRide)
        Note right of FK: Post-discount ride fare to driver

        FK->>FK: transfer BuyerAsset → BuyerExternal (VATOnline: taxAmt)
        FK->>FK: transfer BuyerExternal → OwnerLiability (VATOnline)
        Note right of FK: VAT stays with driver (not govt)<br/>because isVat=true

        FK->>FK: transfer BuyerAsset → BuyerExternal (TollCharges: tollInclVat)
        FK->>FK: transfer BuyerExternal → OwnerLiability (TollCharges)
    end

    FK->>FK: transfer OwnerLiability → SellerRevenue (Commission)
    Note right of FK: Commission on rideFareTaxExcl +<br/>rideTax + rideFareNoTaxApplicable

    FK->>FK: transfer GovtIndirect → OwnerLiability (VATInput: serviceVat)
    Note right of FK: ServiceVAT 25.5% input credit<br/>on pre-discount ride inclusive

    FK->>FK: transfer OwnerExpense → OwnerLiability (DiscountsOnline: discount)
    Note right of FK: Platform absorbs discount<br/>Driver gets paid full pre-discount

    FK->>FK: mint Driver Invoice
    Note over FK: driverInvoiceLineItemsVatInclusive=true:<br/>Ride Fare (Incl. VAT)<br/>Toll Fare (Incl. VAT)<br/>Parking Charges<br/>Platform Commission

    BPP-->>BAP: breakups + MULTIPLIER tags

    BAP->>BAP: calculateFareParameters (pure)
    Note over BAP: Proportional discount allocation:<br/>ratio = postDiscountIncl / rideTotalIncl<br/>newRideTax = taxableIncl × (m-1)/m<br/>where m = rideTaxMultiplier (1.135)

    BAP->>FK: Customer ledger entries
    FK->>FK: transfer BuyerAsset → SellerLiability (CustomerRideFare)
    FK->>FK: transfer BuyerAsset → GovtIndirect (CustomerRideVAT)
    FK->>FK: transfer BuyerAsset → SellerLiability (CustomerTollCharges)
    FK->>FK: transfer SellerLiability → BuyerAsset (CustomerDiscount)
    FK->>FK: mint Customer Invoice
    Note over FK: Ride Fare (Excl. VAT)<br/>Discount: −8.68<br/>Ride VAT (13.5% post-recompute)<br/>Toll Fare (Incl. VAT)
```

## 3. Normal Ride (India/GST) — Cash Payment — Ride Completion with Discount

```mermaid
sequenceDiagram
    participant Customer
    participant BPP as BPP (Driver App)
    participant FK as Finance Kernel

    Customer->>BPP: Ride ends (cash)
    BPP->>BPP: calculateFareParameters

    BPP->>FK: createDriverWalletTransaction (Cash)
    Note over FK: --- BPP Cash Ledger Entries ---

    Note over FK: No BuyerAsset → BuyerExternal<br/>(cash collected by driver directly)

    FK->>FK: transfer OwnerLiability → GovtIndirect (GSTCash: gstAmt)
    Note right of FK: GST deducted from driver wallet<br/>to government (only for GST, not VAT)

    FK->>FK: transfer OwnerLiability → SellerRevenue (CashCommission)
    Note right of FK: Commission deducted from wallet

    FK->>FK: transfer OwnerExpense → OwnerLiability (DiscountsCash: discount)
    Note right of FK: Platform settles discount portion<br/>Driver collected less cash from customer

    FK->>FK: mint Driver Invoice
    Note over FK: Line items: Base Fare, Tax,<br/>Toll Charges, Platform Commission

    Note over Customer,BPP: Customer collects in hand:<br/>amountToCollectInCash = ride.fare − discount<br/>amountToBeSettledOnline = discount (via platform wallet)
```

## 4. International Ride (Finland/VAT) — Cash Payment — Ride Completion

```mermaid
sequenceDiagram
    participant Customer
    participant BPP as BPP (Driver App)
    participant FK as Finance Kernel

    Customer->>BPP: Ride ends (cash)
    BPP->>BPP: calculateFareParameters (isVat=true)

    BPP->>FK: createDriverWalletTransaction (Cash, isVat=true)
    Note over FK: --- BPP Cash VAT Ledger ---

    Note over FK: Cash: NO GST deduction to govt<br/>(isVat=true → skip GSTCash transfer)

    FK->>FK: transfer OwnerLiability → SellerRevenue (CashCommission)

    FK->>FK: transfer GovtIndirect → OwnerLiability (VATInput: serviceVat)
    Note right of FK: ServiceVAT 25.5% input credit

    FK->>FK: transfer OwnerExpense → OwnerLiability (DiscountsCash)
    Note right of FK: Only if discount applied

    FK->>FK: mint Driver Invoice (VAT-inclusive lines)
    Note over FK: Ride Fare (Incl. VAT)<br/>Toll Fare (Incl. VAT)<br/>Platform Commission

    Note over Customer,BPP: Customer pays cash:<br/>amountToCollectInCash = fare − discount<br/>amountToBeSettledOnline = discount
```

## 5. Customer Cancellation with No-Show Charges — Online Payment

```mermaid
sequenceDiagram
    participant Customer
    participant BAP as BAP (Rider App)
    participant BPP as BPP (Driver App)
    participant FK as Finance Kernel

    Customer->>BAP: Cancel ride (after no-show window)
    BAP->>BPP: cancel (with cancellation reason)

    BPP->>BPP: Calculate cancellation fee
    Note over BPP: fee.amount from cancellationFarePolicy<br/>(min/max/per-metre/per-minute/% of fare)<br/>Fee is VAT/GST inclusive

    alt Normal (GST regime)
        BPP->>BPP: Extract GST from inclusive fee
        Note over BPP: gstPct = cgst + sgst + igst<br/>gstOnCancel = fee × gstPct/(1+gstPct)<br/>baseCancel = fee − gstOnCancel
    else International (VAT regime)
        BPP->>BPP: Extract VAT from inclusive fee
        Note over BPP: vatPct = ride's effective VAT rate (tax/taxExcl from<br/>projectFareParamsBreakup)<br/>vatOnCancel = fee × vatPct/(1+vatPct)<br/>baseCancel = fee − vatOnCancel
    end

    BPP->>FK: cancelRideTransaction
    Note over FK: --- BPP Cancellation Ledger ---

    FK->>FK: transfer BuyerAsset → BuyerExternal (CustomerCancellationCharges: baseCancel)
    FK->>FK: transfer BuyerExternal → OwnerLiability (CustomerCancellationCharges)
    Note right of FK: Cancellation fee base to driver

    FK->>FK: transfer BuyerAsset → BuyerExternal (CustomerCancellationGST: taxOnCancel)
    FK->>FK: transfer BuyerExternal → GovtIndirect (CustomerCancellationGST)
    Note right of FK: Tax portion to government (GST)<br/>or to OwnerLiability (VAT)

    opt TDS applicable
        FK->>FK: transfer OwnerLiability → GovtDirect (TDSDeductionCancellation)
    end

    opt VAT regime — ServiceVAT on cancellation
        FK->>FK: transfer GovtIndirect → OwnerLiability (CancellationVATInput: serviceVat)
        Note right of FK: ServiceVAT 25.5% input credit<br/>on cancellation inclusive
    end

    alt driverInvoiceLineItemsVatInclusive = true
        FK->>FK: mint Driver Invoice
        Note over FK: Cancellation Fee (Incl. VAT): single line
    else
        FK->>FK: mint Driver Invoice
        Note over FK: Customer Cancellation Fee<br/>GST on Cancellation Fee
    end

    BPP-->>BAP: on_cancel with breakups
    Note over BPP: CANCELLATION_FEE_TAX_EXCLUSIVE<br/>CANCELLATION_TAX<br/>CANCELLATION_TAX_MULTIPLIER

    BAP->>FK: Customer cancellation ledger
    FK->>FK: transfer BuyerAsset → SellerLiability (CustomerCancellationFee: baseCancel)
    FK->>FK: transfer BuyerAsset → GovtIndirect (CustomerCancellationVAT: taxOnCancel)
    FK->>FK: mint Customer Cancel Invoice
    Note over FK: Cancellation Fee<br/>Cancellation Fee VAT
```

## 6. Customer Cancellation — No-Show Charges Failed to Collect (Card declined)

```mermaid
sequenceDiagram
    participant Customer
    participant BPP as BPP (Driver App)
    participant FK as Finance Kernel

    Customer->>BPP: Cancel ride (no-show, card failed)

    BPP->>FK: cancelRideTransaction (FailedToCollect)
    Note over FK: --- Platform absorbs cancellation ---

    FK->>FK: transfer OwnerExpense → OwnerLiability (NoShowCancellationExpense: feeIncl)
    Note right of FK: Platform expense → driver still gets paid<br/>Customer's card failed but driver<br/>shouldn't lose the no-show fee

    opt VAT regime
        FK->>FK: transfer GovtIndirect → OwnerLiability (CancellationVATInput: serviceVat)
    end

    FK->>FK: mint Driver Invoice
    Note over FK: Cancellation Fee (platform-absorbed)

    Note over Customer,BPP: Customer owes this amount —<br/>collected later via CancellationRevenueDeferred<br/>when card is retried successfully
```

## 7. Post-Ride Tip Flow — Invoice Regeneration

```mermaid
sequenceDiagram
    participant Customer
    participant BAP as BAP (Rider App)
    participant BPP as BPP (Driver App)
    participant FK as Finance Kernel

    Note over BPP: Ride completed. Original invoice<br/>minted at ride-end (no tip line).

    Customer->>BAP: Add tip (€5)
    BAP->>BAP: postPaymentAddTip (ride must be COMPLETED)
    BAP->>BPP: populateTipAmount (internal API)

    BPP->>BPP: Update ride.tipAmount = 5
    BPP->>BPP: Update dailyStats

    BPP->>FK: voidPriorRideInvoice
    FK->>FK: Find BaseRide entry → linked invoice
    FK->>FK: Mark invoice as Voided
    Note right of FK: Original ledger entries preserved<br/>(immutable historical facts)

    opt Prior Tips entries exist (multi-tip update)
        FK->>FK: createReversal for each old Tips entry
    end

    BPP->>FK: transfer BuyerAsset → OwnerLiability (Tips: 5)
    Note right of FK: New supplemental tip entry

    BPP->>FK: createInvoice (priorEntryIds + newTipEntryId)
    Note over FK: New invoice minted with:<br/>• Original line items (Base Fare, Tax, Toll, Commission)<br/>• + Tip: €5<br/>Links to original entries + new Tips entry

    FK-->>BPP: New invoice ID
    Note over BPP: Old invoice: Voided (audit trail)<br/>New invoice: Active with tip line
```

## Ledger Account Summary

| Account Role     | Counterparty        | Type      | Used For                                                |
| ---------------- | ------------------- | --------- | ------------------------------------------------------- |
| `BuyerAsset`     | BUYER (merchant)    | Asset     | Customer payment pool (online)                          |
| `BuyerExternal`  | BUYER (merchant)    | External  | Settlement intermediary                                 |
| `OwnerLiability` | Driver/Fleet        | Liability | Driver wallet / payable                                 |
| `OwnerExpense`   | Driver/Fleet        | Expense   | Platform-absorbed costs (discounts, failed collections) |
| `GovtIndirect`   | GOVERNMENT_INDIRECT | Liability | GST/VAT payable to government                           |
| `GovtDirect`     | GOVERNMENT_DIRECT   | Liability | TDS payable to government                               |
| `SellerRevenue`  | SELLER (platform)   | Revenue   | Platform commission income                              |
| `PlatformAsset`  | SELLER (platform)   | Asset     | Wallet topup source                                     |

## Reference Type Summary

| Reference Type                | When Used                    | Direction                                               |
| ----------------------------- | ---------------------------- | ------------------------------------------------------- |
| `BaseRide`                    | Ride-end                     | BuyerAsset → OwnerLiability                             |
| `GSTOnline` / `VATOnline`     | Ride-end (online)            | BuyerAsset → GovtIndirect (GST) or OwnerLiability (VAT) |
| `GSTCash` / `VATCash`         | Ride-end (cash)              | OwnerLiability → GovtIndirect (GST only)                |
| `TollCharges`                 | Ride-end                     | BuyerAsset → OwnerLiability                             |
| `ParkingCharges`              | Ride-end                     | BuyerAsset → OwnerLiability                             |
| `Commission`                  | Ride-end                     | OwnerLiability → SellerRevenue                          |
| `VATInput`                    | Ride-end (VAT regime)        | GovtIndirect → OwnerLiability                           |
| `DiscountsOnline`/`DiscountsCash` | Ride-end (+ discount)    | OwnerExpense → OwnerLiability                           |
| `VATAbsorbedOnDiscount`       | Ride-end (VAT + discount)    | OwnerExpense → OwnerLiability                           |
| `Tips`                        | Post-ride tip                | BuyerAsset → OwnerLiability                             |
| `CustomerCancellationCharges` | Cancellation                 | BuyerAsset → OwnerLiability                             |
| `CustomerCancellationGST`     | Cancellation                 | BuyerAsset → GovtIndirect                               |
| `CancellationVATInput`        | Cancellation (VAT)           | GovtIndirect → OwnerLiability                           |
| `NoShowCancellation`          | Cancel (card collected)      | BuyerAsset → OwnerLiability                             |
| `NoShowCancellationExpense`   | Cancel (card failed)         | OwnerExpense → OwnerLiability                           |
| `TDSDeductionOnline`          | Ride-end (TDS)               | OwnerLiability → GovtDirect                             |
| `TDSDeductionCash`            | Ride-end (TDS)               | OwnerLiability → GovtDirect                             |
| `TDSDeductionCancellation`    | Cancellation (TDS)           | OwnerLiability → GovtDirect                             |
