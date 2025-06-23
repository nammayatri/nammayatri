# Public Transport System Database Schema Documentation

## Overview
This document provides comprehensive documentation for the public transport system's database schema. The system is designed to handle route management, station operations, and sophisticated fare calculations across multiple transportation providers and cities.

## Table Structure

### Core Transportation Tables

#### 1. Route (`atlas_app.route`)
Manages transportation routes across the network.

**Primary Fields:**
- `code` (text) - Unique route identifier
- `id` (varchar[36]) - UUID for the route
- `short_name`, `long_name` (text) - Route identifiers for display
  - `short_name`: Brief route identifier (e.g., "R1", "Blue Line")
  - `long_name`: Detailed route description (e.g., "Central Station to Airport Express")
- `start_lat`, `start_lon`, `end_lat`, `end_lon` (double precision) - Geographic coordinates for route endpoints
- `vehicle_type` (text) - Type of vehicle (e.g., bus, train, metro)
- `polyline` (text) - Encoded geographic path representation
- `color` (text) - UI/Display color code for the route
- `time_bounds` (text) - Operating time constraints (default: 'Unbounded')

**Administrative Fields:**
- `merchant_id` (varchar[36]) - Operating organization identifier
- `merchant_operating_city_id` (varchar[36]) - Operating city identifier
- `integrated_bpp_config_id` (varchar[36]) - Beckn Protocol Provider configuration

#### 2. Station (`atlas_app.station`)
Represents physical stops/stations in the network.

**Primary Fields:**
- `id` (varchar[36]) - Primary key UUID
- `code` (text) - Unique station identifier
- `name` (text) - Station display name
- `address` (text) - Physical location address
- `lat`, `lon` (double precision) - Geographic coordinates
- `vehicle_type` (text) - Supported vehicle types
- `possible_types` (text) - Additional station classifications
- `time_bounds` (text) - Operating hours (default: 'Unbounded')

**Constraints:**
- Primary Key on `id`
- Unique constraint on combination of (code, merchant_operating_city_id, vehicle_type, integrated_bpp_config_id)

#### 3. Route Stop Mapping (`atlas_app.route_stop_mapping`)
Links routes and stops with sequence information.

**Primary Fields:**
- `route_code`, `stop_code` (text) - Composite primary key
- `sequence_num` (integer) - Stop order in route
- `stop_name` (text) - Stop reference name
- `stop_lat`, `stop_lon` (double precision) - Stop coordinates
- `estimated_travel_time_from_previous_stop` (integer) - Expected travel duration
- `provider_code` (text) - Transport service provider identifier

**Constraints:**
- Primary Key on combination of (route_code, stop_code)

### Fare Management System

#### 1. FRFS Fare Policy (`atlas_app.frfs_fare_policy`)
Defines base fare calculation policies.

**Primary Fields:**
- `id` (varchar[36]) - Policy UUID
- `type` (text) - Policy type identifier
- `description` (text) - Policy details
- `applicable_discount_ids` (text[]) - Array of applicable discount identifiers

#### 2. FRFS Route Fare Product (`atlas_app.frfs_route_fare_product`)
Associates fare policies with specific routes and service tiers.

**Primary Fields:**
- `id` (varchar[36]) - Product UUID
- `fare_policy_id` (varchar[36]) - Reference to fare policy
- `route_code` (text) - Associated route
- `vehicle_service_tier_id` (varchar[36]) - Service tier reference
- `vehicle_type` (text) - Vehicle type
- `time_bounds` (text) - Validity period

#### 3. FRFS Route Stop Stage Fare (`atlas_app.frfs_route_stop_stage_fare`)
Manages stage-based pricing for stops within routes.

**Primary Fields:**
- `fare_policy_id` (varchar[36]) - Reference to fare policy
- `route_code` (text) - Route identifier
- `stop_code` (text) - Stop identifier
- `stage` (integer) - Fare stage number

**Constraints:**
- Primary Key on combination of (fare_policy_id, route_code, stop_code)

#### 4. FRFS Stage Fare (`atlas_app.frfs_stage_fare`)
Defines actual fare amounts for different stages.

**Primary Fields:**
- `fare_policy_id` (varchar[36]) - Reference to fare policy
- `stage` (integer) - Stage number
- `amount` (double precision) - Fare amount
- `currency` (text) - Currency code (default: 'INR')

**Constraints:**
- Primary Key on combination of (fare_policy_id, stage)

#### 5. Route Stop Fare (`atlas_app.route_stop_fare`)
Manages direct fare calculations between stops.

**Primary Fields:**
- `start_stop_code`, `end_stop_code` (text) - Stop pair identifiers
- `route_code` (text) - Route reference
- `amount` (double precision) - Fare amount
- `currency` (text) - Currency code (default: 'INR')
- `fare_policy_id` (varchar[36]) - Associated fare policy

**Constraints:**
- Primary Key on combination of (end_stop_code, fare_policy_id, route_code, start_stop_code)

#### 6. FRFS Vehicle Service Tier (`atlas_app.frfs_vehicle_service_tier`)
Defines vehicle service classifications and tiers.

**Primary Fields:**
- `id` (varchar[36]) - Service tier UUID
- `type` (text) - Service tier type
- `provider_code` (text) - Transport provider identifier
- `short_name`, `long_name` (text) - Service tier identifiers
- `description` (text) - Detailed tier description

### Common Fields
Most tables include the following administrative fields:
- `merchant_id` (varchar[36]) - Organization identifier
- `merchant_operating_city_id` (varchar[36]) - Operating city identifier
- `created_at` (timestamp with time zone) - Record creation timestamp
- `updated_at` (timestamp with time zone) - Last update timestamp
- `integrated_bpp_config_id` (varchar[36]) - Beckn Protocol Provider configuration

## System Features

### Multi-tenant Support
- Organization-level separation via `merchant_id`
- City-level operation management via `merchant_operating_city_id`
- Support for multiple transport providers via `provider_code`

### Fare Calculation Methods
1. **Stage-based Pricing**
   - Progressive fare calculation based on journey stages
   - Flexible stage definition per route
   - Support for different fare policies

2. **Direct Stop-to-Stop Pricing**
   - Point-to-point fare calculation
   - Route-specific pricing
   - Currency support (default: INR)

### Service Tier Management
- Multiple vehicle types support
- Service tier classification
- Provider-specific configurations

### Integration Capabilities
- Beckn Protocol integration support
- Flexible provider configuration
- Time-bound service management

## Usage Guidelines

### Adding New Routes
1. Create route entry in `route` table
2. Define stops in `station` table
3. Map stops to route in `route_stop_mapping`
4. Configure fare policy and products

### Fare Management
1. Create fare policy in `frfs_fare_policy`
2. Define stage fares or direct stop fares
3. Associate with routes and service tiers
4. Apply any applicable discounts

### Service Tier Setup
1. Define service tiers in `frfs_vehicle_service_tier`
2. Associate with routes and fare products
3. Configure provider-specific details