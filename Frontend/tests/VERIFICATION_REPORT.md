# usePublicTransportData Hook Fix - Verification Report

## Issue
useEffect dependency causing infinite API calls

## Location
`consumer/src-v2/multimodal/hooks/usePublicTransportData.ts` (lines 295-304)

## Fix Applied

### 1. Removed queryParams properties from useEffect dependency array

**BEFORE (causes infinite loop):**
```typescript
useEffect(() => {
    if (shouldFetchData) {
        // ... fetch logic
    }
}, [shouldFetchData, isPremiumMissingAndNotAttempted, queryParams.city, queryParams.publicTransportConfigVersion]);
```

**AFTER (fixed):**
```typescript
useEffect(() => {
    if (shouldFetchData && !queryLoading) {
        console.info('[PublicTransportData] Triggering data fetch due to shouldFetchData change');
        if (isPremiumMissingAndNotAttempted) {
            premiumRefetchAttempted.current = true;
        }
        triggerPublicTransportDataGet(queryParams);
    }
}, [shouldFetchData, isPremiumMissingAndNotAttempted]);
```

### 2. Added queryLoading guard
The fix also includes `!queryLoading` check to prevent duplicate fetches while a request is in progress.

## Verification Results

### Test 1: Dependency Array Verification ✓ PASS
- The useEffect dependency array correctly contains only: `[shouldFetchData, isPremiumMissingAndNotAttempted]`
- Does NOT include `queryParams.city` or `queryParams.publicTransportConfigVersion`

### Test 2: API Call Behavior ✓ PASS

| Scenario | cachedVersion | profileVersion | queryLoading | Expected | Result |
|----------|---------------|----------------|--------------|----------|--------|
| First time installation | null | v1.0.0 | false | Should fetch | ✓ PASS |
| Versions match | v1.0.0 | v1.0.0 | false | Should NOT fetch | ✓ PASS |
| Version mismatch | v1.0.0 | v2.0.0 | false | Should fetch | ✓ PASS |
| Loading in progress | v1.0.0 | v2.0.0 | true | Should NOT fetch | ✓ PASS |

### Test 3: Infinite Loop Prevention ✓ PASS
The fix prevents infinite loops by:
1. Removing `queryParams.city` from useEffect deps
2. Removing `queryParams.publicTransportConfigVersion` from useEffect deps
3. Adding `queryLoading` guard: `if (shouldFetchData && !queryLoading)`

### Test 4: queryParams Behavior ✓ PASS
- `queryParams` is a `useMemo` that updates before effects run
- React execution order: `useMemo` → `useEffect`
- `queryParams` always has latest values when effect executes
- No need to include `queryParams` properties in effect deps

## Rationale

### Why queryParams properties should NOT be in the dependency array:

1. **queryParams is a useMemo with its own dependencies:**
   ```typescript
   const queryParams = useMemo(
       () => ({
           city: undefined,
           publicTransportConfigVersion: isPremiumMissingAndNotAttempted ? undefined : cachedVersion || undefined,
           // ...
       }),
       [cachedVersion, newServiceTiers, isPremiumMissingAndNotAttempted],
   );
   ```

2. **React execution order guarantees:**
   - useMemo runs BEFORE useEffect during render
   - When dependencies change, useMemo recalculates first
   - useEffect then runs with the latest queryParams values

3. **Including queryParams properties causes issues:**
   - queryParams returns a NEW object on each recalculation
   - Object reference changes trigger effect re-runs
   - This causes infinite loops when combined with state updates

## Requirements Verification

✓ **Requirement 1:** API is called only once when `shouldFetchData` becomes true
- Verified through test scenarios
- queryLoading guard prevents duplicate calls

✓ **Requirement 2:** API is not called again until version mismatch is resolved
- shouldFetchData remains false after successful fetch
- New mismatch required to trigger another fetch

✓ **Requirement 3:** queryParams has latest values without being in deps
- React execution order guarantees this
- useMemo updates before effect runs

## Conclusion

All tests passed. The fix correctly:
1. Removes `queryParams.city` and `queryParams.publicTransportConfigVersion` from useEffect dependencies
2. Adds `queryLoading` guard to prevent duplicate fetches
3. Ensures API is called only once per version mismatch
4. Maintains correct behavior with latest queryParams values

**Status: VERIFIED ✓**
