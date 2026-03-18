# Fix for Excessive /publicTransport/data API Calls

## Problem Summary

The `/publicTransport/data` API was being called millions of times per day instead of a few times per user per day. The issue was in the `usePublicTransportData` hook which is used by multiple components:

- `consumer/src-v2/screens/HomeScreen/Flow.tsx` (line 398)
- `consumer/src-v2/screens/MockCity/UI.tsx` (line 37)
- `consumer/src-v2/multimodal/screens/Home/UI.tsx` (line 41)
- `consumer/src-v2/multimodal/screens/MetroSubwayBooking/Flow.tsx` (line 33)

Each component instance independently triggered API calls without coordination, causing the same data to be fetched multiple times simultaneously.

## Root Cause

The hook's `shouldFetchData` logic and the fetch effect could trigger independently in each component instance. When `cachedVersion` was null (first load) or when there was a version mismatch, every component instance triggered its own fetch.

The original implementation used a `useRef` to track fetch state, but this was local to each hook instance:
```typescript
const fetchInProgressRef = useRef(false);  // This is per-instance, not global!
```

## Solution

Implemented a **singleton pattern** using module-level variables to coordinate fetches across all hook instances:

### Key Changes

1. **Module-level state variables** (outside the hook):
   ```typescript
   let isFetchInProgress = false;
   let ongoingFetchPromise: Promise<publicTransportData | null> | null = null;
   ```

2. **Separation of concerns**:
   - `performFetch()`: Internal function that actually performs the API call
   - `fetchData()`: Public function that coordinates with other instances

3. **Singleton coordination logic**:
   - If a fetch is already in progress, wait for it instead of starting a new one
   - Multiple callers share the same promise and await the same result
   - Only one API call is made regardless of how many components use the hook

### How It Works

```
Component A calls fetchData()
  ↓
  isFetchInProgress = false, so start new fetch
  ongoingFetchPromise = performFetch()
  ↓
Component B calls fetchData() (while A's fetch is still running)
  ↓
  isFetchInProgress = true, so wait for existing promise
  await ongoingFetchPromise  ← Same promise as Component A!
  ↓
Both components receive the same data from a single API call
```

## Files Changed

### Primary Fix
- `consumer/src-v2/multimodal/hooks/usePublicTransportData.ts` - Refactored to use singleton pattern

### No Changes Required
The following files use the hook but don't need modifications:
- `consumer/src-v2/screens/HomeScreen/Flow.tsx`
- `consumer/src-v2/screens/MockCity/UI.tsx`
- `consumer/src-v2/multimodal/screens/Home/UI.tsx`
- `consumer/src-v2/multimodal/screens/MetroSubwayBooking/Flow.tsx`

## How to Apply the Fix

### Option 1: Apply the Patch File

```bash
cd ../deps/ny-react-native
git apply ../../Frontend/ny-react-native-patches/fix-publicTransport-data-singleton.patch
```

### Option 2: Copy the Fixed File

```bash
cp Frontend/ny-react-native-patches/usePublicTransportData.ts \
   ../deps/ny-react-native/consumer/src-v2/multimodal/hooks/usePublicTransportData.ts
```

### Option 3: Manual Edit

Apply the changes from the patch file manually to:
`consumer/src-v2/multimodal/hooks/usePublicTransportData.ts`

## Testing

After applying the fix:

1. **Verify single API call**: Open the app and check network logs - only one `/publicTransport/data` call should be made regardless of how many components mount

2. **Verify data sharing**: All components should receive the same data without additional API calls

3. **Verify error handling**: If the API fails, all components should handle the error appropriately

4. **Verify refresh**: Calling `refreshData()` should still work and trigger a new fetch

## Expected Behavior After Fix

| Scenario | Before Fix | After Fix |
|----------|-----------|-----------|
| 4 components mount simultaneously | 4 API calls | 1 API call |
| Component mounts while fetch in progress | New API call | Waits for existing fetch |
| Version mismatch detected | Each component fetches | Single coordinated fetch |
| Network reconnection | Multiple refresh calls | Debounced single refresh |

## Rollback

If issues occur, revert to the original implementation:

```bash
cd ../deps/ny-react-native
git checkout -- consumer/src-v2/multimodal/hooks/usePublicTransportData.ts
```

## Additional Notes

- The fix maintains backward compatibility - the hook's API (return values) remains unchanged
- Components using the hook don't need any modifications
- The global cache (`globalCache`) was already working correctly and is still used
- Error handling and loading states work the same way for all component instances
