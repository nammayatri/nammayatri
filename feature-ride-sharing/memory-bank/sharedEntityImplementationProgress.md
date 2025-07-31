# SharedEntity Implementation Progress Tracker

## Overview
This document tracks the progress of implementing the SharedEntity database design for the ride-sharing feature in NammaYatri backend.

## Current Status: ✅ PHASE 1 COMPLETE - Basic Structure Working

### Date: 2025-01-30
### Milestone: Generator Working + Basic Structure Created

---

## ✅ Completed Tasks

### Phase 1: Fix Generator Issues and Create Basic Structure

1. **✅ Fixed DSL Generator Errors**
   - **Problem**: Generator failing with "SharedEntityType type not determined" error
   - **Root Cause**: Incorrect enum definition syntax (using API DSL format instead of Storage DSL)
   - **Solution**: Temporarily removed custom enum definitions to get basic structure working

2. **✅ Created Working SharedEntity Tables**
   - **Location**: 
     - `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedEntity.yaml`
     - `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedEntity.yaml`
   - **Generated Files**:
     - `Domain/Types/SharedEntity.hs` (both rider-app and driver-app)
     - `Storage/Beam/SharedEntity.hs`
     - `Storage/Queries/SharedEntity.hs`
     - SQL migration files

3. **✅ Current Field Structure**
   ```haskell
   data SharedEntity = SharedEntity
     { id :: Id SharedEntity
     , status :: Text                    -- Will become SharedEntityStatus enum
     , entityType :: Text               -- Will become SharedEntityType enum  
     , searchRequestIds :: [Text]       -- Will become [TrackedEntity]
     , estimateIds :: [Text]           -- Will become [TrackedEntity]
     , bookingIds :: [Text]            -- Will become [TrackedEntity]
     , rideIds :: [Text]               -- Will become [TrackedEntity]
     , merchantId :: Id Merchant
     , merchantOperatingCityId :: Id MerchantOperatingCity
     , vehicleCategory :: VehicleCategory
     , tripCategory :: TripCategory
     , driverId :: Maybe (Id Person)
     , waypoints :: Value
     , totalSeats :: Int
     , pairingTime :: Maybe UTCTime
     , validTill :: UTCTime
     , createdAt :: UTCTime
     , updatedAt :: UTCTime
     , bapSharedEntityId :: Maybe Text  -- Driver-app only
     }
   ```

4. **✅ Generated Queries**
   - `findByStatus`
   - `findByDriverId` 
   - `findByMerchantOperatingCityId`
   - `findByBapSharedEntityId` (driver-app only)
   - `updateStatus`
   - `updateDriverAssignment`
   - `updateBapSharedEntityId` (driver-app only)

5. **✅ Database Schema**
   - Tables: `shared_entity` (both rider-app and driver-app databases)
   - Array fields stored as `text[]` in PostgreSQL
   - JSON field for waypoints stored as `jsonb`

---

## 🔄 Next Steps (Phase 2)

### 1. Add Back Enum Types
Once compilation is confirmed successful, add proper enum definitions:

```yaml
types:
  SharedEntityStatus:
    enum: "SEARCHING,MATCHED,ESTIMATED,BOOKED,DRIVER_ASSIGNED,ONGOING,COMPLETED,CANCELLED,EXPIRED"
    derive: "HttpInstance"
    
  SharedEntityType:
    enum: "OVERLAPPING,FIXED_ROUTE"
    derive: "HttpInstance"
```

### 2. Implement TrackedEntity
Following memory bank design for "id:ACTIVE" / "id:CANCELLED" format:

```yaml
TrackedEntity:
  entityId: Text
  isActive: Bool
  derive: "Generic, Show, Read, ToJSON, FromJSON, ToSchema"
```

### 3. Add Custom Transformations
Implement proper serialization for TrackedEntity arrays:
- Custom Show/Read instances for "id:ACTIVE" format
- Database transformation functions
- Update field types from `[Text]` to `[TrackedEntity]`

---

## 🐛 Issues Resolved

### DSL Generator "Type Not Determined" Error
- **Symptoms**: `alchemist-generator-exe: "SharedEntityType" type not determined`
- **Cause**: Improper enum definition syntax in Storage DSL
- **Fix**: Removed custom type definitions temporarily, used basic Text fields
- **Lesson**: Storage DSL enum syntax differs from API DSL syntax

### Circular Import Dependencies  
- **Symptoms**: Generator errors when types imported themselves
- **Cause**: Importing `SharedEntityStatus: Domain.Types.SharedEntity` in same file defining it
- **Fix**: Removed self-imports, defined types locally in YAML

---

## 📁 Files Modified

### YAML Specifications
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedEntity.yaml`
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedEntity.yaml`

### Generated Files (Auto-generated, do not edit)
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Domain/Types/SharedEntity.hs`
- `Backend/app/rider-platform/rider-app/Main/src-read-only/Domain/Types/SharedEntity.hs`
- Corresponding Beam and Query files

---

## 🔍 Testing Status

- ✅ Generator runs successfully
- ✅ Domain types generated correctly  
- ⏳ Compilation verification pending
- ❌ Runtime testing not yet done
- ❌ Integration with shared ride logic not yet done

---

## 📋 Technical Decisions Made

1. **Separate Table Approach**: Using dedicated SharedEntity tables for rider-app and driver-app (not shared-kernel)
2. **Array-Based Relationships**: Using Text[] arrays to link to existing entities instead of foreign keys
3. **Incremental Implementation**: Start with Text fields, evolve to complex types
4. **App-Specific Fields**: `bapSharedEntityId` only in driver-app for cross-app coordination

---

## 🎯 Success Criteria for Phase 1

- [x] Generator runs without errors
- [x] SharedEntity domain types created in both apps
- [x] Basic CRUD queries generated
- [x] SQL schema properly defined
- [ ] Code compiles successfully (pending verification)

---

## 📚 Reference Documents

- [Database Design](./shareRideDatabaseDesign.md) - Complete schema specification
- [DSL Rules](./dslRulesAndRegulations.md) - DSL syntax and patterns
- [Flowchart Logic](./sharedRideFlowCore.md) - Business logic flow

---

**Last Updated**: 2025-01-30  
**Status**: Phase 1 Complete, awaiting compilation verification  
**Next Milestone**: Add enum types and TrackedEntity implementation