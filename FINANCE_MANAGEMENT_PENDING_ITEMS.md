# Finance Management APIs - Pending Items

## Overview
This document tracks pending items and TODOs for the Finance Management Dashboard APIs.

---

## 1. Subscription Purchase List API (`/subscriptionPurchase/list`)

### Current Status: ✅ Basic Implementation Complete

### Pending Items:

#### 1.1 Calculate Utilized Value
**Location**: `FinanceManagement.hs` - `calculateUtilizedValue` function
**Description**: Calculate the current used balance for the subscription
**Logic Needed**:
- Find Ride Credit account for owner (finance_account where account_type = 'RideCredit' and counterparty_id = ownerId)
- Sum all debit amounts from ledger entries where reference_type = 'RideSubscriptionDebit'
**SQL Reference**:
```sql
-- From subscription balance report query
WITH ride_credit_accounts AS (
  SELECT id AS account_id, counterparty_id AS driver_id
  FROM atlas_driver_offer_bpp.finance_account
  WHERE account_type IN ('Ride Credit', 'RideCredit')
  AND counterparty_type IN ('DRIVER', 'FLEET_OWNER')
)
-- Get utilized amount from ledger entries
```

#### 1.2 Get Linked Ride IDs
**Location**: `FinanceManagement.hs` - `getLinkedRideIds` function
**Description**: Get ride IDs from which this subscription was used
**Logic Needed**:
- Find ledger entries where reference_type = 'SubscriptionPurchase' and reference_id = subscription.id
- Extract ride_id from metadata or related entries
- Or use invoice_ledger_link to find related rides

#### 1.3 Query to Fetch ALL Subscriptions
**Location**: `FinanceManagement.hs` - fallback case when no filters
**Description**: Need query to fetch all subscriptions for merchant with pagination
**Files to Modify**: 
- `SubscriptionPurchase.yaml` - Add new query
- `SubscriptionPurchaseExtra.hs` - Implement query

#### 1.4 Status Filter Implementation
**Location**: `FinanceManagement.hs` - `mbStatus` parameter
**Description**: Currently status filter is not implemented
**Note**: Need to convert Text to SubscriptionPurchaseStatus type

---

## 2. Fleet Operator List API (`/fleetOperator/list`)

### Current Status: ✅ Basic Implementation Complete

### Pending Items:

#### 2.1 Calculate Total Earnings
**Location**: `FinanceManagement.hs` - `calculateFleetFinancials` function
**Description**: Calculate total earnings from ledger entries
**Logic Needed**:
```sql
-- From earnings ledger query
-- Earnings: Sum of ledger entries where to_account is DRIVER_PAYABLE
SELECT SUM(le.amount) 
FROM finance_ledger_entry le
INNER JOIN finance_account fa_to ON fa_to.id = le.to_account_id
WHERE fa_to.description = 'DRIVER_PAYABLE'
AND fa_to.counterparty_id = fleetOwnerId
AND le.reference_type IN ('BaseRide', 'UserCancellationCharges', 'Discount', 'TDSReimbursement', 'TollCharges', 'ParkingCharges')
```

#### 2.2 Calculate Total Deductions
**Location**: `FinanceManagement.hs` - `calculateFleetFinancials` function
**Description**: Calculate total deductions from ledger entries
**Logic Needed**:
```sql
-- From earnings ledger query
-- Deductions: Sum of ledger entries where from_account is DRIVER_PAYABLE
SELECT SUM(le.amount)
FROM finance_ledger_entry le
INNER JOIN finance_account fa_from ON fa_from.id = le.from_account_id
WHERE fa_from.description = 'DRIVER_PAYABLE'
AND fa_from.counterparty_id = fleetOwnerId
AND le.reference_type IN ('RiderRefunds', 'DriverCancellationCharges', 'GSTCash', 'TDSDeductionCash', 'TDSDeductionOnline', 'GSTOnline')
```

#### 2.3 Get Outstanding Balance
**Location**: `FinanceManagement.hs` - `getOutstandingBalance` function
**Description**: Get amount from finance_account table for that account_id
**Logic Needed**:
- Find finance_account where counterparty_id = fleetOwnerId and account_type = 'Liability'
- Get the current balance from the latest ledger entry
**SQL Reference**:
```sql
SELECT current_balance 
FROM finance_account fa
WHERE fa.counterparty_id = fleetOwnerId 
AND fa.account_type = 'Liability'
```

#### 2.4 Get Payout Information
**Location**: `FinanceManagement.hs` - `payoutReference`, `payoutAmount`, `payoutStatus` fields
**Description**: Get latest payout from payout table for fleet_owner_id
**Logic Needed**:
- Query `payout_request` table where beneficiary_id = fleetOwnerId
- Get latest payout (by created_at)
- Fields needed: id, amount, status
**SQL Reference**:
```sql
SELECT id, amount, status
FROM payout_request
WHERE beneficiary_id = fleetOwnerId
ORDER BY created_at DESC
LIMIT 1
```

#### 2.5 Get Fleet Owner Details (PAN, GSTIN)
**Location**: `FinanceManagement.hs` - `pan`, `gstin` fields
**Description**: Get actual PAN and GSTIN from fleet_owner_info table
**Files to Check**:
- `FleetOwnerInformation.yaml` - Check if pan/gstin fields exist
- May need to add fields to schema

#### 2.6 Query to Fetch ALL Fleet Owners
**Location**: `FinanceManagement.hs` - fallback case
**Description**: Need query to fetch all FLEET_OWNERs with pagination
**Files to Modify**:
- May need to add query to `Person.yaml` or create custom query

---

## 3. Invoice List API (`/invoice/list`)

### Current Status: ✅ Basic Implementation Complete

### Pending Items:

#### 3.1 Get Counterparty Type
**Location**: `FinanceManagement.hs` - `counterpartyType` field
**Description**: Get counterparty type from finance account
**Logic Needed**:
- Join with finance_account table using issuedToId
- Get counterparty_type field
**Note**: May need to add issuedToType field to invoice or fetch from account

#### 3.2 Query to Fetch ALL Invoices
**Location**: `FinanceManagement.hs` - fallback case
**Description**: Need query to fetch all invoices with filters (date range, status, type)
**Files to Modify**:
- `Invoice.yaml` - Add new queries for date range, status filters
- `InvoiceExtra.hs` - Implement complex queries

#### 3.3 Invoice Type Filter
**Location**: `FinanceManagement.hs` - `mbInvoiceType` parameter
**Description**: Currently not used in filtering
**Implementation**: Add filter when fetching invoices

#### 3.4 Status Filter
**Location**: `FinanceManagement.hs` - `mbStatus` parameter
**Description**: Currently not used in filtering
**Implementation**: Add filter when fetching invoices

---

## 4. Additional Queries/Files Needed

### 4.1 IndirectTaxTransactionExtra.hs
**Status**: ✅ Created with `findByInvoiceNumber`
**Location**: `Backend/lib/finance-kernel/src/Lib/Finance/Storage/Queries/IndirectTaxTransactionExtra.hs`

### 4.2 FleetDriverAssociationExtra.hs
**Status**: ✅ Added `findAllActiveByFleetOwnerId`
**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Queries/FleetDriverAssociationExtra.hs`

### 4.3 FinanceAccount Queries
**Needed For**: Outstanding balance calculations
**Location**: May need `FinanceAccountExtra.hs`
**Functions Needed**:
- `findByCounterpartyId` - Get account by counterparty
- `findRideCreditAccount` - Get Ride Credit account for driver/fleet

### 4.4 LedgerEntry Queries
**Needed For**: Earnings/deductions calculations
**Location**: May need `LedgerEntryExtra.hs`
**Functions Needed**:
- `findEarningsByCounterparty` - Sum earnings for a driver/fleet
- `findDeductionsByCounterparty` - Sum deductions for a driver/fleet
- `findByReferenceTypeAndId` - Find entries by reference

### 4.5 PayoutRequest Queries
**Needed For**: Payout information
**Location**: May need `PayoutRequestExtra.hs`
**Functions Needed**:
- `findLatestByBeneficiaryId` - Get latest payout for fleet owner

---

## 5. Testing

### 5.1 Curl Commands Needed
Create curl commands for testing each endpoint:
- Subscription Purchase List with various filters
- Fleet Operator List with various filters
- Invoice List with various filters

### 5.2 Test Data Needed
- Active subscriptions with different statuses
- Fleet owners with linked drivers
- Invoices with GST details
- Ledger entries for earnings/deductions calculations

---

## 6. Schema Updates (If Needed)

### 6.1 FleetOwnerInformation
**Potential Fields to Add**:
- `pan :: Maybe Text`
- `gstin :: Maybe Text`
- `legalName :: Maybe Text`

### 6.2 Invoice
**Potential Fields to Add**:
- `issuedToType :: Maybe Text` (counterparty type)

---

## 7. Implementation Priority

### High Priority (Core Functionality)
1. Calculate utilized value for subscriptions
2. Get linked ride IDs for subscriptions
3. Calculate total earnings/deductions for fleet operators
4. Get outstanding balance for fleet operators

### Medium Priority (Enhanced Filtering)
1. Query to fetch ALL subscriptions (no filters)
2. Query to fetch ALL fleet operators (no filters)
3. Query to fetch ALL invoices with filters
4. Status and type filters

### Low Priority (Additional Details)
1. Payout information for fleet operators
2. PAN/GSTIN from fleet_owner_info
3. Counterparty type for invoices
4. TDS references for invoices

---

## 8. Files Modified

### Created Files:
1. `Backend/lib/finance-kernel/src/Lib/Finance/Storage/Queries/IndirectTaxTransactionExtra.hs`

### Modified Files:
1. `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Queries/FleetDriverAssociationExtra.hs`
2. `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Dashboard/Management/FinanceManagement.hs`

### Auto-Generated Files (from YAML):
1. `Backend/app/dashboard/CommonAPIs/src-read-only/API/Types/ProviderPlatform/Management/Endpoints/FinanceManagement.hs`
2. `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/API/Action/Dashboard/Management.hs`
3. `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/API/Types/ProviderPlatform/Management/FinanceManagement.hs`

---

## 9. Notes

- All response fields are `Maybe` types to handle missing data gracefully
- The APIs follow the existing dashboard API patterns
- Authentication is via `ApiAuthV2` (dashboard token auth)
- The YAML spec is in `Backend/app/dashboard/CommonAPIs/spec/ProviderPlatform/Management/API/FinanceManagement.yaml`

---

Last Updated: 2026-02-23