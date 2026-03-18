# Test Plan for usePublicTransportData Hook Fix

## Overview
This test plan verifies that the singleton pattern fix for the `usePublicTransportData` hook correctly prevents duplicate API calls while maintaining all existing functionality.

## Test Scenarios

### 1. Single API Call Per Session

#### Test 1.1: Simultaneous Component Mounts
**Objective**: Verify only one API call is made when multiple components mount at the same time.

**Steps**:
1. Clear all caches (MMKV and globalCache)
2. Mount 3 components simultaneously using `usePublicTransportData(true)`
3. Wait for all components to initialize

**Expected Result**:
- Only 1 API call to `/publicTransport/data`
- All 3 components report `cacheInitialized: true`
- All 3 components share the same cached data

#### Test 1.2: Sequential Component Mounts
**Objective**: Verify only one API call is made when components mount one after another.

**Steps**:
1. Clear all caches
2. Mount first component
3. Wait for initialization
4. Mount second component
5. Mount third component

**Expected Result**:
- Only 1 API call total
- Subsequent components use cached data from first component

#### Test 1.3: Disabled Hook
**Objective**: Verify no API calls when hook is disabled.

**Steps**:
1. Clear all caches
2. Mount component with `usePublicTransportData(false)`

**Expected Result**:
- 0 API calls
- `cacheInitialized: true` (initialization still completes)

#### Test 1.4: Offline Mode
**Objective**: Verify no API calls when offline.

**Steps**:
1. Clear all caches
2. Set NetInfo to `{ isConnected: false }`
3. Mount component with `usePublicTransportData(true)`

**Expected Result**:
- 0 API calls
- Hook waits for connection

---

### 2. Cache Hit on Subsequent Mounts

#### Test 2.1: Cache Reuse
**Objective**: Verify cached data is used without API calls on subsequent mounts.

**Steps**:
1. Pre-populate MMKV with cached data (version: v1.0.0)
2. Pre-populate globalCache with same data
3. Mount multiple components

**Expected Result**:
- 0 API calls
- All components report `isDataAvailable: true`
- All components report `isDataLoaded: true`

#### Test 2.2: Global Cache Availability
**Objective**: Verify global cache is correctly checked.

**Steps**:
1. Pre-populate only globalCache (no MMKV data)
2. Mount component

**Expected Result**:
- Component uses globalCache data
- `isDataAvailable: true`

---

### 3. Version Mismatch Detection

#### Test 3.1: Version Mismatch Triggers Refetch
**Objective**: Verify refetch occurs when cached version differs from profile version.

**Steps**:
1. Pre-populate cache with version v1.0.0
2. Set profile version to v2.0.0
3. Mount component

**Expected Result**:
- 1 API call (refetch triggered)
- `hasVersionMismatch: true`
- `isDataLoaded: false` until new data arrives

#### Test 3.2: Matching Versions Use Cache
**Objective**: Verify no refetch when versions match.

**Steps**:
1. Pre-populate cache with version v1.0.0
2. Set profile version to v1.0.0
3. Mount component

**Expected Result**:
- 0 API calls
- `hasVersionMismatch: false`
- `isDataLoaded: true`

#### Test 3.3: Multiple Components with Version Mismatch
**Objective**: Verify only one refetch despite multiple components detecting mismatch.

**Steps**:
1. Pre-populate cache with version v1.0.0
2. Set profile version to v2.0.0
3. Mount 3 components simultaneously

**Expected Result**:
- Only 1 API call (not 3)
- All components share the refetched data

---

### 4. Manual Refresh

#### Test 4.1: Refresh Triggers New API Call
**Objective**: Verify manual refresh triggers a new API call.

**Steps**:
1. Pre-populate cache
2. Mount component
3. Wait for initialization
4. Reset API call counter
5. Call `refreshData()`

**Expected Result**:
- 1 API call after refresh
- Cache is cleared before fetch

#### Test 4.2: Refresh Clears Cache
**Objective**: Verify refresh clears both MMKV and globalCache.

**Steps**:
1. Pre-populate both caches
2. Mount component
3. Call `refreshData()`

**Expected Result**:
- MMKV data removed
- globalCache data deleted
- New fetch triggered

---

### 5. Premium Bus Refetch Logic

#### Test 5.1: Missing Premium Bus Triggers Refetch
**Objective**: Verify refetch when premium bus data is missing.

**Steps**:
1. Pre-populate cache with only REGULAR routes
2. Enable `enablePremiumBusDataRefetch`
3. Mount component

**Expected Result**:
- 1 API call (premium bus refetch)
- Query params include `newServiceTiers: ['PREMIUM']`

#### Test 5.2: Existing Premium Bus No Refetch
**Objective**: Verify no refetch when premium bus data exists.

**Steps**:
1. Pre-populate cache with PREMIUM routes
2. Enable `enablePremiumBusDataRefetch`
3. Mount component

**Expected Result**:
- 0 API calls
- `isDataAvailable: true`

#### Test 5.3: Premium Refetch Only Once
**Objective**: Verify premium bus refetch happens only once.

**Steps**:
1. Pre-populate cache without premium bus
2. Enable `enablePremiumBusDataRefetch`
3. Mount component (triggers refetch)
4. Unmount and remount component

**Expected Result**:
- Only 1 API call total
- `premiumRefetchAttempted` ref prevents duplicate calls

---

### 6. Loading States

#### Test 6.1: Initial Loading State
**Objective**: Verify correct loading state during initial fetch.

**Steps**:
1. Clear all caches
2. Set query state to `isLoading: true`
3. Mount component

**Expected Result**:
- `isLoading: true`
- `isDataLoaded: false`

#### Test 6.2: Loaded State
**Objective**: Verify correct state when data is loaded.

**Steps**:
1. Pre-populate cache
2. Mount component

**Expected Result**:
- `isLoading: false`
- `isDataLoaded: true`

---

### 7. Error Handling

#### Test 7.1: API Error Handling
**Objective**: Verify graceful handling of API errors.

**Steps**:
1. Clear all caches
2. Set query state to return error
3. Mount component

**Expected Result**:
- Error is exposed via `error` property
- Hook remains functional
- `cacheInitialized: true`

#### Test 7.2: Storage Error Handling
**Objective**: Verify graceful handling of storage read errors.

**Steps**:
1. Make MMKV throw error on read
2. Mount component

**Expected Result**:
- Hook doesn't crash
- `cachedVersion: null`
- Falls back to API fetch

---

### 8. Integration Scenarios

#### Test 8.1: Complete Lifecycle
**Objective**: Verify complete data lifecycle works correctly.

**Steps**:
1. Initial mount with no cache → triggers fetch
2. Simulate API response → data cached
3. Mount new component → uses cache (no fetch)
4. Simulate version mismatch → triggers refetch

**Expected Result**:
- Total API calls: 2 (initial + refetch)
- Cache correctly updated at each stage

#### Test 8.2: Rapid Mount/Unmount
**Objective**: Verify no duplicate calls during rapid mount/unmount cycles.

**Steps**:
1. Perform 5 rapid mount/unmount cycles
2. Final mount

**Expected Result**:
- Only 1 API call total
- No memory leaks
- No race conditions

---

## Test Execution

### Prerequisites
```bash
# Install dependencies
cd Frontend/ny-react-native-patches
npm install

# Run tests
npm test
```

### Running Specific Tests
```bash
# Run all tests
npm test

# Run with coverage
npm test -- --coverage

# Run specific test file
npm test -- usePublicTransportData.test.ts

# Run in watch mode
npm test -- --watch
```

## Success Criteria

All tests must pass with the following criteria:

1. **API Call Count**: No scenario should result in more than 1 API call per unique data fetch requirement
2. **Cache Consistency**: All components should see the same cached data
3. **State Correctness**: Loading states, error states, and data states must be accurate
4. **No Regressions**: All existing functionality (version check, manual refresh, premium bus) must work

## Performance Benchmarks

- Initial mount with cache: < 50ms
- Initial mount without cache: < 100ms + API latency
- Subsequent mounts: < 10ms (cache hit)
- Memory usage: No significant increase from singleton pattern
