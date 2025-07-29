# Shared Ride Database Design

This document defines the comprehensive schema for the shared ride feature tables, mapped from actual YAML specifications to facilitate DSL-based development. All schemas are synchronized with the current implementation.

---

## Table of Contents
1. [rider-app Schema](#rider-app-schema)
2. [dynamic-offer-driver-app Schema](#dynamic-offer-driver-app-schema)
3. [YAML Creation Reference](#yaml-creation-reference)
4. [Type Definitions](#type-definitions)

---

## rider-app Schema

### Table: `SharedSearchRequest`

Wraps multiple `SearchRequest` entities to represent a pool of customers waiting to be matched.

**YAML Reference:** `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedSearchRequest.yaml`

**Domain Fields:**
- `id`: `Id SharedSearchRequest` (Primary Key)
- `status`: `SharedSearchRequestStatus`
- `searchRequestIds`: `[Id SearchRequest]`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `vehicleCategory`: `VehicleCategory`
- `waypoints`: `Value`
- `maxDistance`: `Maybe Distance`
- `totalCustomerExtraFee`: `Maybe Price`
- `validTill`: `UTCTime`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `status`: `character varying(255)`
- `search_request_ids`: `uuid[]`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `vehicle_category`: `character varying(255)`
- `waypoints`: `jsonb`
- `max_distance`: `double precision` (via beam transformation)
- `max_distance_value`: `double precision` (via beam transformation)
- `distance_unit`: `character varying(255)` (via beam transformation)
- `total_customer_extra_fee`: `numeric(30,2)` (via beam transformation)
- `total_customer_extra_fee_amount`: `numeric(30,10)` (via beam transformation)
- `currency`: `character varying(255)` (via beam transformation)
- `valid_till`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Beam Transformations:**
- `maxDistance` splits into: `maxDistance` (Centesimal), `maxDistanceValue` (HighPrecDistance), `distanceUnit` (DistanceUnit)
- `totalCustomerExtraFee` splits into: `totalCustomerExtraFee` (Money), `totalCustomerExtraFeeAmount` (HighPrecMoney), `currency` (Currency)

**Queries:**
- `updateStatus(status, updatedAt)` WHERE `id`
- `findActiveRequests()` WHERE `status`

**Sample `waypoints` JSON:**
```json
[
  {
    "type": "PICKUP",
    "search_request_id": "uuid-for-search-req-1",
    "lat": 12.9716,
    "lon": 77.5946
  },
  {
    "type": "DROPOFF",
    "search_request_id": "uuid-for-search-req-1",
    "lat": 12.9816,
    "lon": 77.6046
  }
]
```

---

### Table: `SharedEstimate`

Wraps multiple `Estimate` entities. Represents the combined fare estimate for a matched group.

**YAML Reference:** `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedEstimate.yaml`

**Domain Fields:**
- `id`: `Id SharedEstimate` (Primary Key)
- `sharedSearchRequestId`: `Id SharedSearchRequest` (Secondary Key)
- `estimateIds`: `[Id Estimate]`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `bppSharedEstimateId`: `Text`
- `providerId`: `Text`
- `providerName`: `Text`
- `providerUrl`: `BaseUrl`
- `serviceTierName`: `Maybe Text`
- `estimatedTotalFare`: `HighPrecMoney`
- `totalFareRangeMin`: `HighPrecMoney`
- `totalFareRangeMax`: `HighPrecMoney`
- `estimatedDuration`: `Maybe Seconds`
- `estimatedDistance`: `Maybe Distance`
- `vehicleServiceTierType`: `ServiceTierType|NoRelation`
- `vehicleServiceTierSeatingCapacity`: `Maybe Int`
- `nightShiftCharge`: `Maybe Money`
- `nightShiftChargeAmount`: `Maybe HighPrecMoney`
- `oldNightShiftCharge`: `Maybe Centesimal`
- `nightShiftStart`: `Maybe TimeOfDay`
- `nightShiftEnd`: `Maybe TimeOfDay`
- `status`: `EstimateStatus`
- `tripCategory`: `Maybe TripCategory`
- `validTill`: `UTCTime`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `shared_search_request_id`: `uuid` (Foreign Key, Secondary Key)
- `estimate_ids`: `uuid[]`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `bpp_shared_estimate_id`: `text`
- `provider_id`: `character varying(255)`
- `provider_name`: `character varying(255)`
- `provider_url`: `character varying(255)` (via beam transformation)
- `service_tier_name`: `text`
- `estimated_total_fare`: `numeric(30,10)`
- `total_fare_range_min`: `numeric(30,10)`
- `total_fare_range_max`: `numeric(30,10)`
- `estimated_duration`: `integer`
- `estimated_distance`: `double precision` (via beam transformation)
- `estimated_distance_value`: `double precision` (via beam transformation)
- `distance_unit`: `character varying(255)` (via beam transformation)
- `vehicle_variant`: `character varying(255)` (via beam transformation)
- `vehicle_service_tier_seating_capacity`: `integer`
- `night_shift_charge`: `numeric(30,2)`
- `night_shift_charge_amount`: `numeric(30,10)`
- `old_night_shift_charge`: `numeric(10,2)`
- `night_shift_start`: `time`
- `night_shift_end`: `time`
- `status`: `character varying(255)`
- `trip_category`: `character varying(255)`
- `valid_till`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Beam Transformations:**
- `providerUrl`: `BaseUrl` ↔ `Text` (showBaseUrl/parseBaseUrl)
- `estimatedDistance` splits into: `estimatedDistance` (HighPrecMeters), `estimatedDistanceValue` (HighPrecDistance), `distanceUnit` (DistanceUnit)
- `vehicleServiceTierType` mapped to `vehicleVariant` in beam

**Queries:**
- `findAllBySharedSearchRequestId(sharedSearchRequestId)`
- `updateStatus(status)` WHERE `id`
- `findByStatus(status)`

---

### Table: `SharedBooking`

Wraps multiple `Booking` entities. Represents the confirmed booking for a matched group before a driver is assigned.

**YAML Reference:** `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedBooking.yaml`

**Domain Fields:**
- `id`: `Id SharedBooking` (Primary Key)
- `sharedEstimateId`: `Id SharedEstimate` (Secondary Key)
- `bookingIds`: `[Id Booking]`
- `transactionId`: `Text`
- `bppSharedBookingId`: `Text`
- `status`: `BookingStatus`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `driverId`: `Maybe (Id Person)`
- `providerId`: `Text`
- `providerUrl`: `BaseUrl`
- `vehicleServiceTierType`: `ServiceTierType|NoRelation`
- `estimatedTotalFare`: `Price`
- `estimatedDuration`: `Maybe Seconds`
- `estimatedDistance`: `Maybe Distance`
- `pairingTime`: `UTCTime`
- `distanceUnit`: `DistanceUnit`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `shared_estimate_id`: `uuid` (Foreign Key, Secondary Key)
- `booking_ids`: `uuid[]`
- `transaction_id`: `character varying(36)`
- `bpp_shared_booking_id`: `text`
- `status`: `character varying(255)`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `driver_id`: `character varying(36)`
- `provider_id`: `character varying(255)`
- `provider_url`: `character varying(255)` (via beam transformation)
- `vehicle_service_tier_type`: `character varying(255)`
- `estimated_total_fare`: `numeric(30,10)` (via beam transformation)
- `currency`: `character varying(255)` (via beam transformation)
- `estimated_duration`: `integer`
- `estimated_distance`: `double precision` (via beam transformation)
- `estimated_distance_value`: `double precision` (via beam transformation)
- `distance_unit`: `character varying(255)`
- `pairing_time`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Beam Transformations:**
- `providerUrl`: `BaseUrl` ↔ `Text` (showBaseUrl/parseBaseUrl)
- `estimatedTotalFare` splits into: `estimatedTotalFare` (HighPrecMoney), `currency` (Currency)
- `estimatedDistance` splits into: `estimatedDistance` (HighPrecMeters), `estimatedDistanceValue` (HighPrecDistance)

**Queries:**
- `findBySharedEstimateId(sharedEstimateId)`
- `updateStatus(status, updatedAt)` WHERE `id`
- `updateDriverId(driverId, updatedAt)` WHERE `id`
- `findByStatus(status)`

---

### Table: `SharedRide`

Wraps multiple `Ride` entities. Represents the active, in-progress shared ride after a driver has been assigned.

**YAML Reference:** `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedRide.yaml`

**Domain Fields:**
- `id`: `Id SharedRide` (Primary Key)
- `sharedBookingId`: `Id SharedBooking` (Secondary Key)
- `rideIds`: `[Id Ride]`
- `bppSharedRideId`: `Text`
- `status`: `SharedRideStatus`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `driverId`: `Id Person`
- `vehicleNumber`: `Text`
- `vehicleModel`: `Text`
- `vehicleVariant`: `VehicleVariant|NoRelation`
- `vehicleServiceTierType`: `Maybe ServiceTierType|NoRelation`
- `waypoints`: `Value`
- `trackingUrl`: `Maybe BaseUrl`
- `totalFare`: `Maybe HighPrecMoney`
- `chargeableDistanceValue`: `Maybe HighPrecDistance`
- `traveledDistanceValue`: `Maybe HighPrecDistance`
- `rideStartTime`: `Maybe UTCTime`
- `rideEndTime`: `Maybe UTCTime`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `shared_booking_id`: `uuid` (Foreign Key, Secondary Key)
- `ride_ids`: `uuid[]`
- `bpp_shared_ride_id`: `text`
- `status`: `character varying(255)`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `driver_id`: `character varying(36)`
- `vehicle_number`: `character varying(255)`
- `vehicle_model`: `character varying(255)`
- `vehicle_variant`: `character varying(60)`
- `vehicle_service_tier_type`: `character varying(255)`
- `waypoints`: `jsonb`
- `tracking_url`: `character varying(255)` (via beam transformation)
- `total_fare`: `numeric(30,10)`
- `chargeable_distance_value`: `double precision`
- `traveled_distance_value`: `double precision`
- `ride_start_time`: `timestamptz`
- `ride_end_time`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Beam Transformations:**
- `trackingUrl`: `Maybe BaseUrl` ↔ `Maybe Text` (showBaseUrl/parseBaseUrl)

**Queries:**
- `findBySharedBookingId(sharedBookingId)`
- `updateStatus(status, updatedAt)` WHERE `id`
- `updateRideStartTime(rideStartTime, updatedAt)` WHERE `id`
- `updateRideEndTime(rideEndTime, updatedAt)` WHERE `id`
- `updateTotalFare(totalFare, updatedAt)` WHERE `id`
- `findByDriverId(driverId)`
- `findByStatus(status)`

---

### Table: `SharedRideConfigs`

Configuration parameters for shared ride feature in the rider app.

**YAML Reference:** `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedRideConfigs.yaml`

**Domain Fields:**
- `id`: `Id SharedRideConfigs` (Primary Key)
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity` (Secondary Key)
- `vehicleCategory`: `VehicleCategory`
- `pickupLocationSearchRadius`: `Meters`
- `searchThresholdForSharedEstimate`: `Int`
- `searchRequestExpirySeconds`: `Seconds`
- `searchExpiryBufferSeconds`: `Seconds`
- `customerRemainingThresholdForFlowContinuation`: `Int`
- `dropLocationSearchRadius`: `Meters`
- `actualPickupDistanceThreshold`: `Meters`
- `actualDropDistanceThreshold`: `Meters`
- `routeMatchingThreshold`: `Double` (stored as percentage/100, e.g., 0.85 for 85%)
- `geoHashPrecisionForRouteMatching`: `Int`
- `routeOverlapThreshold`: `Double` (stored as percentage/100, e.g., 0.70 for 70%)
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)` (Secondary Key)
- `vehicle_category`: `character varying(255)`
- `pickup_location_search_radius`: `double precision`
- `search_threshold_for_shared_estimate`: `integer`
- `search_request_expiry_seconds`: `integer`
- `search_expiry_buffer_seconds`: `integer`
- `customer_remaining_threshold_for_flow_continuation`: `integer`
- `drop_location_search_radius`: `double precision`
- `actual_pickup_distance_threshold`: `double precision`
- `actual_drop_distance_threshold`: `double precision`
- `route_matching_threshold`: `double precision`
- `geo_hash_precision_for_route_matching`: `integer`
- `route_overlap_threshold`: `double precision`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Queries:**
- `findByMerchantOperatingCityIdAndVehicleCategory(merchantOperatingCityId, vehicleCategory)`
- `findByMerchantOperatingCityId(merchantOperatingCityId)`
- `updateConfigValues(...)` WHERE `id`

---

## dynamic-offer-driver-app Schema

### Table: `SharedSearchRequest`

Wraps multiple `SearchRequest` entities for the driver app. This table holds aggregated data for a potential shared ride offer.

**YAML Reference:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedSearchRequest.yaml`

**Domain Fields:**
- `id`: `Id SharedSearchRequest` (Primary Key)
- `transactionId`: `Text` (References `SharedSearchRequest.id` in rider-app)
- `status`: `SearchRequestStatus`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `fromLocationIds`: `[Id Location]`
- `toLocationIds`: `[Id Location]`
- `estimatedDistance`: `Maybe Meters`
- `estimatedDuration`: `Maybe Seconds`
- `vehicleCategory`: `Maybe VehicleCategory`
- `tollCharges`: `Maybe HighPrecMoney`
- `tollNames`: `Maybe [Text]`
- `tripCategory`: `Maybe TripCategory`
- `validTill`: `UTCTime`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `transaction_id`: `character varying(36)`
- `status`: `character varying(255)`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `from_location_ids`: `uuid[]`
- `to_location_ids`: `uuid[]`
- `estimated_distance`: `double precision`
- `estimated_duration`: `integer`
- `vehicle_category`: `character varying(255)`
- `toll_charges`: `double precision`
- `toll_names`: `text[]`
- `trip_category`: `character varying(255)`
- `valid_till`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Queries:**
- `findByTransactionId(transactionId)`
- `updateStatus(status, updatedAt)` WHERE `id`
- `findByStatus(status)`
- `findActiveRequests()` WHERE `status`

---

### Table: `SharedEstimate`

Wraps multiple `Estimate` entities for the driver app.

**YAML Reference:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedEstimate.yaml`

**Domain Fields:**
- `id`: `Id SharedEstimate` (Primary Key)
- `transactionId`: `Text` (References `SharedSearchRequest.id` in rider-app)
- `status`: `EstimateStatus`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `totalMinFare`: `HighPrecMoney`
- `totalMaxFare`: `HighPrecMoney`
- `currency`: `Currency`
- `estimatedDistance`: `Maybe Meters`
- `estimatedDuration`: `Maybe Seconds`
- `distanceUnit`: `DistanceUnit`
- `vehicleServiceTier`: `ServiceTierType|NoRelation`
- `tripCategory`: `TripCategory|NoRelation`
- `tollNames`: `Maybe [Text]`
- `validTill`: `UTCTime`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `transaction_id`: `character varying(36)`
- `status`: `character varying(255)`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `total_min_fare`: `numeric(30,2)`
- `total_max_fare`: `numeric(30,2)`
- `currency`: `character varying(255)`
- `estimated_distance`: `double precision`
- `estimated_duration`: `integer`
- `distance_unit`: `character varying(255)`
- `vehicle_service_tier`: `character varying(255)`
- `trip_category`: `character varying(255)`
- `toll_names`: `text[]`
- `valid_till`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Queries:**
- `findByTransactionId(transactionId)`
- `updateStatus(status, updatedAt)` WHERE `id`
- `findByStatus(status)`
- `findByVehicleServiceTier(vehicleServiceTier)`
- `findActiveEstimates()` WHERE `status`

---

### Table: `SharedBooking`

Wraps multiple `Booking` entities for the driver app.

**YAML Reference:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedBooking.yaml`

**Domain Fields:**
- `id`: `Id SharedBooking` (Primary Key)
- `transactionId`: `Text` (References `SharedSearchRequest.id` in rider-app)
- `sharedEstimateId`: `Id SharedEstimate` (Secondary Key)
- `bookingIds`: `[Id Booking]`
- `status`: `BookingStatus`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `providerId`: `Text`
- `providerUrl`: `BaseUrl`
- `driverId`: `Maybe (Id Person)`
- `fromLocationIds`: `[Id Location]`
- `toLocationIds`: `[Id Location]`
- `vehicleServiceTier`: `ServiceTierType|NoRelation`
- `estimatedTotalFare`: `HighPrecMoney`
- `estimatedDuration`: `Maybe Seconds`
- `estimatedDistance`: `Maybe Meters`
- `tollNames`: `Maybe [Text]`
- `distanceUnit`: `DistanceUnit`
- `pairingTime`: `UTCTime`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `transaction_id`: `character varying(36)`
- `shared_estimate_id`: `uuid` (Foreign Key, Secondary Key)
- `booking_ids`: `uuid[]`
- `status`: `character varying(255)`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `provider_id`: `character varying(255)`
- `provider_url`: `character varying(255)` (via beam transformation)
- `driver_id`: `character varying(36)`
- `from_location_ids`: `uuid[]`
- `to_location_ids`: `uuid[]`
- `vehicle_service_tier`: `character varying(255)`
- `estimated_total_fare`: `numeric(30,10)`
- `estimated_duration`: `integer`
- `estimated_distance`: `double precision`
- `toll_names`: `text[]`
- `pairing_time`: `timestamptz`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Beam Transformations:**
- `providerUrl`: `BaseUrl` ↔ `Text` (showBaseUrl/parseBaseUrl)

**Queries:**
- `findByTransactionId(transactionId)`
- `findBySharedEstimateId(sharedEstimateId)`
- `updateStatus(status, updatedAt)` WHERE `id`
- `updateDriverId(driverId, updatedAt)` WHERE `id`
- `findByDriverId(driverId)`
- `findByStatus(status)`

---

### Table: `SharedRide`

Wraps multiple `Ride` entities for the driver app.

**YAML Reference:** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedRide.yaml`

**Domain Fields:**
- `id`: `Id SharedRide` (Primary Key)
- `transactionId`: `Text` (References `SharedSearchRequest.id` in rider-app)
- `sharedBookingId`: `Id SharedBooking` (Secondary Key)
- `rideIds`: `[Id Ride]`
- `status`: `SharedRideStatus`
- `merchantId`: `Id Merchant`
- `merchantOperatingCityId`: `Id MerchantOperatingCity`
- `driverId`: `Id Person`
- `vehicleNumber`: `Text`
- `vehicleModel`: `Text`
- `vehicleVariant`: `VehicleVariant|NoRelation`
- `vehicleServiceTierType`: `Maybe ServiceTierType|NoRelation`
- `waypoints`: `Value`
- `trackingUrl`: `Maybe BaseUrl`
- `totalFare`: `Maybe HighPrecMoney`
- `chargeableDistance`: `Maybe Meters`
- `traveledDistance`: `HighPrecMeters`
- `rideStartTime`: `Maybe UTCTime`
- `rideEndTime`: `Maybe UTCTime`
- `tripCategory`: `TripCategory|NoRelation`
- `numberOfSnapToRoadCalls`: `Maybe Int`
- `numberOfOsrmSnapToRoadCalls`: `Maybe Int`
- `numberOfSelfTuned`: `Maybe Int`
- `numberOfDeviation`: `Maybe Bool`
- `estimatedTollCharges`: `Maybe HighPrecMoney`
- `estimatedTollNames`: `Maybe [Text]`
- `tollCharges`: `Maybe HighPrecMoney`
- `tollNames`: `Maybe [Text]`
- `rideEndedBy`: `Maybe RideEndedBy`
- `passedThroughDestination`: `Maybe Bool`
- `isPickupOrDestinationEdited`: `Maybe Bool`
- `createdAt`: `UTCTime`
- `updatedAt`: `UTCTime`

**Database Schema (Beam):**
- `id`: `uuid` (Primary Key)
- `transaction_id`: `character varying(36)`
- `shared_booking_id`: `uuid` (Foreign Key, Secondary Key)
- `ride_ids`: `uuid[]`
- `status`: `character varying(255)`
- `merchant_id`: `character varying(36)`
- `merchant_operating_city_id`: `character varying(36)`
- `driver_id`: `character varying(36)`
- `vehicle_number`: `character varying(255)`
- `vehicle_model`: `character varying(255)`
- `vehicle_variant`: `character varying(60)`
- `vehicle_service_tier_type`: `character varying(255)`
- `waypoints`: `jsonb`
- `tracking_url`: `character varying(255)` (via beam transformation)
- `total_fare`: `numeric(30,10)`
- `chargeable_distance`: `double precision`
- `traveled_distance`: `double precision`
- `ride_start_time`: `timestamptz`
- `ride_end_time`: `timestamptz`
- `trip_category`: `character varying(255)`
- `number_of_snap_to_road_calls`: `integer`
- `number_of_osrm_snap_to_road_calls`: `integer`
- `number_of_self_tuned`: `integer`
- `number_of_deviation`: `boolean`
- `estimated_toll_charges`: `numeric(30,10)`
- `estimated_toll_names`: `text[]`
- `toll_charges`: `numeric(30,10)`
- `toll_names`: `text[]`
- `ride_ended_by`: `character varying(255)`
- `passed_through_destination`: `boolean`
- `is_pickup_or_destination_edited`: `boolean`
- `created_at`: `timestamptz`
- `updated_at`: `timestamptz`

**Beam Transformations:**
- `trackingUrl`: `Maybe BaseUrl` ↔ `Maybe Text` (showBaseUrl/parseBaseUrl)

**Queries:**
- `findByTransactionId(transactionId)`
- `findBySharedBookingId(sharedBookingId)`
- `updateStatus(status, updatedAt)` WHERE `id`
- `updateRideStartTime(rideStartTime, updatedAt)` WHERE `id`
- `updateRideEndTime(rideEndTime, updatedAt)` WHERE `id`
- `updateTotalFare(totalFare, updatedAt)` WHERE `id`
- `updateTollCharges(tollCharges, tollNames, updatedAt)` WHERE `id`
- `findByDriverId(driverId)`
- `findByStatus(status)`
- `findActiveRides()` WHERE `driverId AND status`

---

## YAML Reference Paths

For implementing new shared ride tables or modifications, refer to the existing YAML specifications:

**Rider App YAML Files:**
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedSearchRequest.yaml`
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedEstimate.yaml`
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedBooking.yaml`
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedRide.yaml`
- `Backend/app/rider-platform/rider-app/Main/spec/Storage/SharedRideConfigs.yaml`

**Driver App YAML Files:**
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedSearchRequest.yaml`
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedEstimate.yaml`
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedBooking.yaml`
- `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SharedRide.yaml`

**DSL Rules Reference:**
For detailed DSL syntax and best practices, refer to: `feature-ride-sharing/memory-bank/dslRulesAndRegulations.md`

---

## Type Definitions

All enums and status types used across the shared ride tables.

```yaml
SharedSearchRequestStatus:
  enum: "POOLING, MATCHED, EXPIRED, CANCELLED"
  derive: "HttpInstance"

SearchRequestStatus:
  enum: "NEW, INPROGRESS, CONFIRMED, COMPLETED, CLOSED"
  derive: "HttpInstance"

EstimateStatus:
  enum: "NEW, DRIVER_QUOTE_REQUESTED, CANCELLED, GOT_DRIVER_QUOTE, DRIVER_QUOTE_CANCELLED, COMPLETED"
  derive: "HttpInstance"

BookingStatus:
  # Rider App
  enum: "NEW, CONFIRMED, AWAITING_REASSIGNMENT, REALLOCATED, COMPLETED, CANCELLED, TRIP_ASSIGNED"
  derive: "HttpInstance"
  
  # Driver App (different enum values)
  enum: "NEW, TRIP_ASSIGNED, COMPLETED, CANCELLED, REALLOCATED"
  derive: "HttpInstance"

SharedRideStatus:
  enum: "UPCOMING, NEW, INPROGRESS, COMPLETED, CANCELLED"
  derive: "HttpInstance"
```

---

**Last Updated:** 2025-01-28  
**Synchronized with:** Current YAML implementations in both rider-app and dynamic-offer-driver-app
