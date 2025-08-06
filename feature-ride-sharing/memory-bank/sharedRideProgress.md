# Shared Ride Feature - Progress Tracker

This document tracks the development progress of the Shared Ride feature.

---

## Implementation Status

**Current Branch:** `backend/feat/rider-pooling-impl`

**Recent Commits:**
- `b45f38ed99` - backend/fix: removed recovery files
- `73905a61e0` - backend/fix: added extra fields to shared-entity  
- `7184a193e6` - backend/fix: added shared entity table and added db references
- `d7a0de733c` - backend/fix: removed old share ride database design references from code
- `01c85268ea` - backend/fix: removed old share ride databse design files

---

## Received Flowchart Chunks

- **Chunk 1:** ✅ **IMPLEMENTED** - Initial Search and Pre-Pooling Checks
- **Chunk 2:** ✅ **COMPLETED** - Pre-Pooling Validation and Rider Pooling Logic
- **Chunk 3:** ⏳ **PENDING** - Batch Creation and Driver Pooling
- **Chunk 4:** ⏳ **PENDING** - Booking, Fulfillment, and Exception Handling
- **Chunk 5:** ✅ **COMPLETED** - Rider Pooling Logic (Detailed Drill-Down)
- **Chunk 6:** ⏳ **PENDING** - Rider Pooling Logic (Advanced Filtering)
- **Chunk 7:** ⏳ **PENDING** - Rider Pooling Logic (Final Filters & Grouping)
- **Chunk 8:** ⏳ **PENDING** - Driver Pooling Logic (Detailed Drill-Down)
- **Chunk 9:** ⏳ **PENDING** - Asynchronous Rider Pooling (Cron Job)

---

## Architecture Decisions Made

### Database Design
- ✅ **Adopted New SharedEntity Design**: Single table approach replacing 8-table multi-table design
- ✅ **TrackedEntity Implementation**: Individual entity status tracking (active/cancelled)
- ✅ **Cross-App Consistency**: Separate SharedEntity tables for rider-app and driver-app

### Core Components
- ✅ **SharedEntity Table**: Primary state management with waypoints, status tracking
- ✅ **SharedRideConfigs Table**: Configuration parameters for pooling logic
- ✅ **Status-Driven Flow**: State machine approach for ride lifecycle

---

## Current Implementation Tasks

### Chunk 2 - Rider Pooling Logic (✅ COMPLETED)
- [x] **Estimate Filtering Enhancement** ✅ **APPROACH CHANGED**
  - **Decision**: Use Chunk 1 OnSearch.hs filtering (already implemented) 
  - **Result**: Progressive disclosure UX - show estimates, handle KYC at selection time
  - **Outcome**: Better conversion rates and debugging capabilities
- [x] **Seat Selection & Validation Logic** ✅ 
  - Enhanced `DSelectReq` with `numSeats` and `sharedEntityId` fields in `Domain/Action/UI/Select.hs:121-135`
  - Added validation with range check (1-8 seats) and future review comment
  - Implemented comprehensive error handling with clear messages
- [x] **Shared Ride Selection Validation** ✅
  - Enhanced `select2` function with shared ride-specific logic in `Domain/Action/UI/Select.hs:277-314`
  - KYC verification with user-friendly error messages
  - Config-based sync/async flow routing
- [x] **Smart Sync Pooling Handler** ✅
  - Implemented `handleSyncRiderPooling` interface function in `Domain/Action/UI/Select.hs:584-602`
  - Conditional GSI management: only add to waiting pool when no immediate match
  - Returns SharedEntity ID for successful matches
- [x] **Enhanced DSelectRes for Driver Communication** ✅
  - Added `sharedEntityId` and `numSeats` fields to `DSelectRes` for Beckn protocol
  - SharedEntity context available for driver-app communication
  - Proper state capture from pooling results
- [x] **Waiting Pool Management** ✅
  - Implemented `addToSharedRideGSI` function in `Domain/Action/UI/Select.hs:550-583`
  - Config-driven TTL: `searchRequestExpirySeconds - searchExpiryBufferSeconds`
  - Updated member format: `estimateId:validTill:numSeats` for better tracking
- [x] **Smart Resource Management** ✅
  - Only use GSI when sync pooling finds no immediate match
  - Efficient Redis usage with automatic cleanup
  - Clear logging for debugging and monitoring

### Supporting Infrastructure 
- [x] **Database Schema**: SharedEntity and SharedRideConfigs tables implemented ✅
- [x] **Configuration Structure**: SharedRideConfigs YAML with core parameters ✅
- [x] **Redis Integration** ✅
  - Geospatial Index setup for `ShareRideCustomerLoc` ✅
  - Config-driven TTL management for waiting customers ✅
  - EstimateId-based tracking for better debugging ✅
- [x] **Configuration Management** ✅
  - Added `enableSharedRide` and `enableSyncPooling` flags ✅
  - CachedQueries implementation for SharedRideConfigs ✅
  - Parameter validation and defaults ✅
- [x] **Error Handling** ✅
  - Clear validation error responses ✅
  - Graceful degradation with meaningful messages ✅
  - KYC and configuration validation ✅

---

### Chunk 5 - Rider Pooling Logic (✅ COMPLETED)
- [x] **Complete `handleSyncRiderPooling` Implementation** ✅
  - GSI queries for nearby riders within pickup radius (`geoSearchDecoded`)
  - 4-step modular filtering cascade replacing nested if-else statements:
    - **Lock Check**: Using `Redis.safeGet` for thread safety
    - **Expiry Check**: Validate estimate hasn't expired
    - **Seat Availability**: Check vehicle capacity constraints (AUTO=1, CAR=2)
    - **Dropoff Compatibility**: Staged distance validation with optimization
  - SharedEntity creation for matched riders with OVERLAPPING type
  - Optimized distance calculations with staged validation to reduce API calls
- [x] **Enhanced Distance Calculation Strategy** ✅
  - Straight-line distance filtering first (using `calculateDistanceFromCoords`)
  - Actual route distance using `Tools.Maps.getDistance` for precision
  - Pickup distance compatibility check before expensive route calls
  - Only calculate drop-to-drop distance when pickup distances are compatible
  - Concrete `Location` types instead of generic parameters for better performance
- [x] **Robust Error Handling & Logging** ✅
  - Proper `fromMaybeM` usage with specific error types (`EstimateDoesNotExist`, `SearchRequestDoesNotExist`)
  - Detailed validation failure logging with specific reasons
  - Clean separation of concerns with `ValidationStep` data type
  - Performance-aware staged validation to minimize expensive operations

## Next Phase: Pending Tasks for Future Chunks

### Chunk 6-7 - Advanced Rider Pooling Logic
- [ ] **Route Overlap Analysis**
  - Implement geo-hashing with precision 9 for route matching
  - Advanced filtering for route compatibility beyond distance thresholds
  - Multi-waypoint route optimization algorithms

### Beckn Protocol Integration (Cross-Chunk Task)
- [ ] **Modify Beckn Select Request ACL**
  - Update rider-app select ACL to include `sharedEntityId` in payload
  - Update rider-app select ACL to include `numSeats` in payload  
  - Ensure driver-app receives SharedEntity context via Beckn protocol
  - Test SharedEntity ID propagation through Beckn network

### Chunk 9 - Async Pooling System
- [ ] **Cron-based Async Pooling**
  - Process Redis GSI for queued customers
  - Implement notification system for successful matches
  - Handle queue management and cleanup

### Codebase Integration Points Identified
- **Quote.hs**: `getEstimates` function needs shared ride filtering logic
- **Select.hs**: `select2'` function needs shared ride pre-processing
- **Redis Pattern**: Follow existing geospatial patterns from DriverPool.hs
- **Empty Stubs**: SharedEntityExtra.hs modules need pooling implementations

---

## Open Questions & Follow-ups

### Technical Implementation Questions
1. **Redis GSI Performance**: Need to validate performance of geospatial queries under high load
2. **Route Caching Strategy**: Confirm Google Maps API response caching implementation details
3. **Locking Mechanism**: Design distributed locking strategy for preventing race conditions in rider pairing
4. **Configuration Hot-Reload**: Should SharedRideConfigs support runtime updates without restart?

### Business Logic Clarifications Needed
1. **KYC Verification Flow**: What specific KYC fields are required for shared rides?
2. **Cancellation Penalties**: Define penalty structure for customer no-shows mentioned in Chunk 4
3. **Vehicle Capacity Rules**: Confirm capacity validation rules for different vehicle types
4. **Search Expiry Buffer**: Determine optimal buffer time calculation for `Y` seconds (marked as "Under Review")

### Integration Concerns
1. **ONDC Compatibility**: Verify batched booking flow works with ONDC `init`/`confirm` models
2. **FCM Notification Format**: Define shared ride notification payload structure
3. **Driver App Integration**: Confirm driver app can handle multi-pickup/drop-off UI
4. **Fare Calculation**: Implement fare splitting logic for overlapping vs individual portions

---

## Next Steps

**Immediate Priority (Chunk 2 Implementation):**
1. Examine current codebase structure for rider pooling components
2. Implement seat selection validation logic
3. Set up Redis GSI for ShareRideCustomerLoc
4. Build core filtering cascade for rider matching
5. Implement waiting pool management with TTL

**Follow-up Priorities:**
1. Complete Chunks 3-4 for batch creation and driver pooling
2. Implement Chunks 5-7 for advanced filtering algorithms  
3. Build Chunk 8 driver pooling logic
4. Set up Chunk 9 asynchronous cron job system

---

**Last Updated:** 2025-08-05  
**Status:** Active Development - Chunks 1,2,5 Completed - Focus on Chunks 6-7  
**Current Developer:** Claude Code Assistant
