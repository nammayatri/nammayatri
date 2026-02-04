# No Show Cancellation Charges - Approach Review

## Executive Summary

This document reviews the proposed No Show cancellation charges flow against the current implementation. It identifies gaps, potential breaking changes, and missing components.

---

## Current Implementation Overview

### 1. **Cancellation Charge Calculation**
- Uses **dynamic logic** (`USER_CANCELLATION_DUES`) to calculate charges
- Charges stored in `riderDetails.cancellationDues` (cumulative field)
- No separate "pending_dues" table exists
- Calculation happens in:
  - `Domain.Action.UI.Ride.CancelRide.Internal.customerCancellationChargesCalculation`
  - `Domain.Action.Beckn.Cancel.cancel`

### 2. **Payment Collection Flow**

#### Card Rides:
- **Scheduled job** (`CancelExecutePaymentIntent`) runs after `riderConfig.cancellationPaymentDelay`
- Attempts capture via `SPayment.makeCxCancellationPayment`
- On success: Updates invoice to `CAPTURED`, calls `customerCancellationDuesSync` with `paymentMadeToDriver = True`
- On failure: Updates invoice to `FAILED`, calls `customerCancellationDuesSync` with `paymentMadeToDriver = False` and `cancellationCharges = Nothing`
- Sends notification to customer about pending dues

#### Cash Rides:
- Creates payment invoice with status `FAILED` (immediately)
- Calls `customerCancellationDuesSync` with `paymentMadeToDriver = False`
- Sends notification to driver about pending charges

### 3. **Zero Dues Check**
- Currently implemented in `API.UI.Search.search'` (line 195-199)
- **Only checks for merchants with `onlinePayment = True`**
- Blocks booking if `cancellationDues > 0`
- **Missing**: Backend validation in other entry points

### 4. **Data Storage**
- `riderDetails.cancellationDues`: Cumulative total
- `payment_invoice` table: Tracks individual cancellation charges with status
- `cancellation_charges` table: Created when `paymentMadeToDriver = True`

---

## Proposed Approach Analysis

### ✅ **What Aligns with Current Implementation**

1. **Zero dues check** - Already exists (but only for onlinePayment merchants)
2. **Dynamic logic for No Show** - Already implemented via `USER_CANCELLATION_DUES`
3. **Payment invoice creation** - Already happens via `createCancellationPaymentInvoice`
4. **BAP sync API** - `customerCancellationDuesSync` already exists
5. **FCM notifications** - Already implemented for pending dues

### ❌ **Critical Gaps & Missing Components**

#### 1. **"Pending Dues Table" Concept**
**Issue**: Flowchart mentions "pending_dues table on BAP side" but:
- No such table exists currently
- Dues are tracked in `riderDetails.cancellationDues` (cumulative)
- Payment invoices track individual charges but not as a "pending_dues" table

**Impact**: 
- Cannot track individual pending dues per ride
- Cannot implement "resolve before any ride" per specific due
- Hard to reconcile which dues are paid vs pending

**Recommendation**: 
- Either create a `pending_dues` table with `ride_id` as entity
- OR clarify that `payment_invoice` table with `FAILED`/`PENDING` status serves this purpose

#### 2. **Immediate Capture for Card Rides**
**Issue**: Flowchart shows "kapture immediately with all cancellation due" but:
- Current implementation uses **scheduled job** with delay (`riderConfig.cancellationPaymentDelay`)
- No immediate capture attempt

**Impact**:
- Delay in payment collection
- Customer might use card before capture happens
- Different behavior than proposed

**Recommendation**:
- Add immediate capture attempt in cancellation flow
- Keep scheduled job as fallback for failed immediate capture
- Or clarify if delay is intentional

#### 3. **Reallocation Time Check**
**Issue**: Flowchart mentions "customer has been NO SHOW for X minutes > reallocation_time" but:
- No explicit `reallocation_time` field found in codebase
- Dynamic logic uses various time-based checks (driver waiting time, time since creation)
- Reallocation logic exists but doesn't directly relate to cancellation charges

**Impact**:
- Unclear what "reallocation_time" refers to
- May need to clarify or add this specific check

**Recommendation**:
- Clarify what "reallocation_time" means in context
- If it's `transporterConfig.searchRepeatLimit` or similar, document it
- Or add explicit `reallocation_time` config if needed

#### 4. **Zero Dues Check Coverage**
**Issue**: Currently only checked in Search API for `onlinePayment` merchants:
- Missing in other booking entry points (if any)
- Not checked for cash-only merchants
- No backend validation in BPP side

**Impact**:
- Customers might bypass check via other flows
- Cash merchants don't enforce zero dues

**Recommendation**:
- Add zero dues check in all booking entry points
- Consider enforcing for all merchants (not just onlinePayment)
- Add backend validation in BPP as well

#### 5. **Card Ride Failure Handling**
**Issue**: Flowchart shows "failed → pending_Dues → dues api on BAP" but:
- Current flow calls `customerCancellationDuesSync` which updates `riderDetails.cancellationDues`
- No explicit "pending_dues" table update
- Invoice status is set to `FAILED` but not explicitly tracked as "pending"

**Impact**:
- Unclear how to query "all pending dues" for a customer
- Hard to show customer which specific rides have pending charges

**Recommendation**:
- Clarify if `payment_invoice` with `FAILED` status = pending dues
- Or create explicit pending_dues tracking

#### 6. **Cash Ride Notification to BAP**
**Issue**: Flowchart shows "Could Send FCM here to BAP to indicate the pending Dues" (optional):
- Currently sends notification to customer about pending dues
- But doesn't explicitly notify BAP about pending dues for cash rides
- Driver notification is sent, but BAP notification is unclear

**Impact**:
- BAP might not know about pending cash ride dues immediately

**Recommendation**:
- Add explicit BAP notification for cash ride pending dues
- Or clarify that customer notification serves this purpose

#### 7. **Payment Order Creation for Pending Dues**
**Issue**: Flowchart mentions "create a payment order here based on pending_dues table" but:
- Current implementation doesn't create payment orders for pending dues resolution
- Payment orders are created during ride booking, not for dues payment

**Impact**:
- No clear flow for customer to pay pending dues
- Customer can't "add card and pay" for pending dues

**Recommendation**:
- Design payment order creation flow for pending dues
- Create API endpoint for customer to pay pending dues
- Link payment order to specific pending dues entries

#### 8. **"All Cancellation Due" Capture**
**Issue**: Flowchart shows capturing "all cancellation due" but:
- Current implementation captures only the current ride's cancellation charge
- `riderDetails.cancellationDues` is cumulative, but capture is per-ride

**Impact**:
- If customer has multiple pending dues, only current ride charge is captured
- Customer might still have other pending dues after capture

**Recommendation**:
- Clarify if "all cancellation due" means:
  - All pending dues for this customer, OR
  - Just this ride's cancellation charge
- If "all", implement aggregation logic

---

## Potential Breaking Changes

### 1. **Scheduled Job Timing**
- **Current**: Payment capture happens after delay
- **Proposed**: Immediate capture (or unclear)
- **Risk**: If changed to immediate, might break existing flows that rely on delay

### 2. **Zero Dues Check Scope**
- **Current**: Only for `onlinePayment` merchants
- **Proposed**: Appears to be for all merchants
- **Risk**: Cash merchants might start blocking customers unexpectedly

### 3. **Pending Dues Data Model**
- **Current**: Cumulative `cancellationDues` field
- **Proposed**: Individual `pending_dues` table
- **Risk**: Migration needed, data consistency issues

### 4. **Payment Invoice Status**
- **Current**: `FAILED` status for cash rides, `PENDING`/`FAILED` for card rides
- **Proposed**: Unclear if status mapping changes
- **Risk**: Existing queries/filters might break

---

## Missing Implementation Details

### 1. **No Show Detection Logic**
- Flowchart mentions "customer has been NO SHOW for X minutes > reallocation_time"
- Need to clarify:
  - How is "NO SHOW" detected? (tag-based? time-based?)
  - What is the exact condition?
  - Is this part of dynamic logic or separate check?

### 2. **Ride Status Check**
- Flowchart mentions "ride must in CANCELLED"
- Current: Ride is cancelled before charge calculation
- Need to verify: Is this check redundant or necessary?

### 3. **Merchant Online Payments Check**
- Flowchart mentions `merchant.onlinePayments enable`
- Current: Uses `merchant.onlinePayment` (singular)
- Need to verify: Is this the same field?

### 4. **FCM Notification Content**
- Flowchart mentions specific notification texts
- Current: Uses merchant push notification templates
- Need to verify: Are templates configured correctly?

### 5. **BPP Payment Notification**
- Flowchart mentions "send FCM to BPP we will pay you X for No show charges"
- Current: Sends notification via `sendDriverCancellationNotification`
- Need to verify: Does this cover the requirement?

---

## Recommendations

### High Priority

1. **Clarify Data Model**
   - Decide: Use `payment_invoice` table OR create `pending_dues` table
   - Document the relationship between tables

2. **Implement Immediate Capture**
   - Add immediate capture attempt in cancellation flow
   - Keep scheduled job as retry mechanism
   - Handle race conditions

3. **Expand Zero Dues Check**
   - Add check in all booking entry points
   - Consider enforcing for all merchants
   - Add backend validation

4. **Design Pending Dues Payment Flow**
   - Create API for customer to view pending dues
   - Create payment order creation for dues payment
   - Link payment to specific dues entries

### Medium Priority

5. **Clarify Reallocation Time**
   - Document what "reallocation_time" means
   - Add explicit check if needed

6. **Improve Notification Flow**
   - Ensure BAP gets notified about pending dues
   - Verify notification content matches requirements

7. **Handle "All Cancellation Due"**
   - Clarify if capture should be per-ride or cumulative
   - Implement aggregation if needed

### Low Priority

8. **Add Monitoring & Logging**
   - Track pending dues resolution rate
   - Monitor capture success/failure rates
   - Alert on stuck pending dues

9. **Add Reconciliation**
   - Periodic job to reconcile pending dues
   - Handle edge cases (partial payments, disputes)

---

## Questions to Resolve

1. **What is "reallocation_time"?** Is it a config field or derived value?
2. **Should pending_dues be a separate table or use payment_invoice?**
3. **Should capture be immediate or delayed?** What's the business requirement?
4. **Should zero dues check apply to all merchants or just onlinePayment?**
5. **How should "all cancellation due" be interpreted?** Per-ride or cumulative?
6. **What's the exact No Show detection logic?** Time-based? Tag-based?
7. **How should customers pay pending dues?** New flow needed?

---

## Conclusion

The proposed approach aligns well with current implementation but has several gaps that need clarification:

1. **Data model** - Need to decide on pending_dues table vs payment_invoice
2. **Timing** - Immediate vs delayed capture needs clarification
3. **Scope** - Zero dues check needs to be expanded
4. **Payment flow** - Need to design customer payment flow for pending dues

**Recommendation**: Review and clarify these gaps before implementation to avoid breaking changes and ensure smooth integration.
