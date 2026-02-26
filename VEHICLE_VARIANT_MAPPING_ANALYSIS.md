# Vehicle Variant Mapping Analysis

## Current Configuration Analysis

Based on the CSV data for operating city `beabba6a-c817-43d2-93b2-a916f5cf2ceb`:

### 1. **ECO (Electric)** Service Tier
- **Service Tier Type**: `ECO`
- **Current Allowed Vehicle Variant**: `["HATCHBACK", "SEDAN", "SUV"]`
- **Auto Selected Vehicle Variant**: `["SEDAN", "HATCHBACK"]`
- **Default For Vehicle Variant**: `["HATCHBACK"]`

### 2. **BLACK (Aura)** Service Tier
- **Service Tier Type**: `BLACK`
- **Current Allowed Vehicle Variant**: `["BLACK", "SUV_PLUS"]`
- **Auto Selected Vehicle Variant**: `["BLACK", "SUV_PLUS"]`

### 3. **SUV_PLUS (XL)** Service Tier
- **Service Tier Type**: `SUV_PLUS`
- **Current Allowed Vehicle Variant**: `["SUV_PLUS"]`

---

## Required Changes

### ✅ Change 1: Add BLACK_XL to ECO's allowedVehicleVariant

**Current State:**
```sql
ECO.allowedVehicleVariant = ["HATCHBACK", "SEDAN", "SUV"]
```

**Target State:**
```sql
ECO.allowedVehicleVariant = ["HATCHBACK", "SEDAN", "SUV", "BLACK_XL"]
```

**SQL Migration:**
```sql
UPDATE atlas_driver_offer_bpp.vehicle_service_tier 
SET allowed_vehicle_variant = array_append(allowed_vehicle_variant, 'BLACK_XL')
WHERE service_tier_type = 'ECO' 
  AND merchant_operating_city_id = 'beabba6a-c817-43d2-93b2-a916f5cf2ceb'
  AND 'BLACK_XL' != ALL(allowed_vehicle_variant); -- Prevent duplicates
```

**Note:** `BLACK_XL` already exists in the `VehicleVariant` enum, so no code changes needed.

---

### ❓ Change 2: Premium Electric Variant/Service Tier

**Clarification Needed:**

**Option A: Create New Vehicle Variant `PREMIUM_ELECTRIC`**
- Add to `VehicleVariant` enum
- Add to `ServiceTierType` enum (if needed)
- Update cast functions
- Add to RCVerificationConfig

**Option B: Create New Service Tier `PREMIUM_ELECTRIC`**
- Add to `ServiceTierType` enum
- Map to existing variant (e.g., `PREMIUM_SEDAN` or `BLACK_XL`)
- Configure in `VehicleServiceTier` table
- Add to RCVerificationConfig

**Option C: Add Premium Electric as Allowed Variant to Existing Service Tier**
- Add `PREMIUM_SEDAN` or another existing variant to ECO's `allowedVehicleVariant`
- Configure in RCVerificationConfig

---

## Recommended Approach

Based on your description, I recommend:

### Step 1: Add BLACK_XL to ECO ✅
```sql
-- Migration file: Backend/dev/migrations/dynamic-offer-driver-app/XXXX-add-black-xl-to-eco.sql
UPDATE atlas_driver_offer_bpp.vehicle_service_tier 
SET 
  allowed_vehicle_variant = array_append(allowed_vehicle_variant, 'BLACK_XL'),
  updated_at = NOW()
WHERE service_tier_type = 'ECO' 
  AND merchant_operating_city_id = 'beabba6a-c817-43d2-93b2-a916f5cf2ceb'
  AND 'BLACK_XL' != ALL(allowed_vehicle_variant);
```

### Step 2: Premium Electric Configuration

**If Premium Electric = New Service Tier:**
1. Add `PREMIUM_ELECTRIC` to `ServiceTierType` enum
2. Create new `VehicleServiceTier` entry
3. Map to appropriate variant (e.g., `PREMIUM_SEDAN` or `BLACK_XL`)
4. Configure in RCVerificationConfig

**If Premium Electric = Variant Mapping:**
1. Use existing variant (e.g., `PREMIUM_SEDAN`)
2. Add to ECO's or new service tier's `allowedVehicleVariant`
3. Configure in RCVerificationConfig

---

## RCVerificationConfig Configuration

For RCVerificationConfig, you'll need to add the variant to `supportedVehicleClasses`:

```json
{
  "RCValidClasses": [
    {
      "vehicleClass": "LMV",
      "vehicleVariant": "BLACK_XL",
      "vehicleCapacity": null,
      "manufacturer": null,
      "manufacturerModel": null,
      "vehicleModel": null,
      "reviewRequired": false,
      "bodyType": null,
      "priority": null
    }
  ]
}
```

Or if creating PREMIUM_ELECTRIC:
```json
{
  "RCValidClasses": [
    {
      "vehicleClass": "LMV",
      "vehicleVariant": "PREMIUM_ELECTRIC",
      ...
    }
  ]
}
```

---

## Questions to Clarify

1. **Premium Electric**: Is this a new `VehicleVariant` enum value or a new `ServiceTierType`?
2. **Mapping**: Which existing variant should Premium Electric map to? (PREMIUM_SEDAN, BLACK_XL, or new variant?)
3. **Service Tier**: Should Premium Electric be a separate service tier or part of ECO?
4. **RC Config**: What vehicle class should Premium Electric use in RCVerificationConfig?

---

## Next Steps

1. ✅ Confirm BLACK_XL addition to ECO is correct
2. ❓ Clarify Premium Electric requirements
3. 📝 Create migration for BLACK_XL
4. 📝 Create code changes for Premium Electric (if new variant/tier)
5. 📝 Update RCVerificationConfig
