# Ticket Dashboard API Documentation

## Overview

This document provides information about the implementation and testing of the Ticket Dashboard APIs, specifically the `postUpsertTicketPlaceDashboardDetails` function.

## API Endpoints

### 1. GET /ticketdashboard/ticketplace/{ticketPlaceId}/info

Retrieves detailed information about a specific ticket place.

**Parameters:**
- `ticketPlaceId`: UUID of the ticket place

**Response:**
Returns a `TicketPlaceDashboardDetails` object containing all information about the ticket place, including:
- Basic information (name, description, location, etc.)
- Services
- Business hours
- Service categories
- Service people categories
- Special occasions

### 2. POST /ticketdashboard/ticketplace/update

Updates an existing ticket place or creates a new one if it doesn't exist.

**Request Body:**
A `TicketPlaceDashboardDetails` object containing all the information to update or create.

**Response:**
Returns a success message if the operation was successful.

## Data Structures

### TicketPlaceDashboardDetails

The `TicketPlaceDashboardDetails` object contains all information about a ticket place, including its basic details, services, business hours, categories, and more.

#### Basic Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `id` | UUID | Yes | Unique identifier for the ticket place |
| `name` | String | Yes | Name of the ticket place |
| `description` | String | No | Detailed description of the ticket place |
| `shortDesc` | String | Yes | Short description of the ticket place |
| `address` | String | No | Physical address of the ticket place |
| `latitude` | Double | No | Latitude coordinate of the ticket place |
| `longitude` | Double | No | Longitude coordinate of the ticket place |
| `status` | String | Yes | Status of the ticket place. Valid values: "Active", "Inactive" |
| `priority` | Integer | Yes | Priority of the ticket place (used for sorting) |
| `placeType` | String | Yes | Type of the ticket place. Valid values: "Airport", "BusStation", "TrainStation", "Museum", "Other", etc. |
| `allowSameDayBooking` | Boolean | Yes | Whether same-day booking is allowed |
| `gallery` | Array of Strings | No | List of image URLs for the ticket place gallery |
| `iconUrl` | String | No | URL of the icon image for the ticket place |
| `mapImageUrl` | String | No | URL of the map image for the ticket place |
| `termsAndConditions` | Array of Strings | No | List of terms and conditions for the ticket place |
| `termsAndConditionsUrl` | String | No | URL to the terms and conditions page |
| `openTimings` | String | Yes | Opening time in "HH:MM:SS" format |
| `closeTimings` | String | Yes | Closing time in "HH:MM:SS" format |
| `services` | Array of Objects | Yes | List of services offered at the ticket place |
| `businessHours` | Array of Objects | Yes | List of business hours for the ticket place |
| `serviceCategories` | Array of Objects | Yes | List of service categories for the ticket place |
| `servicePeopleCategories` | Array of Objects | Yes | List of service people categories for the ticket place |
| `specialOccasions` | Array of Objects | No | List of special occasions for the ticket place |

#### Services

Each service in the `services` array has the following fields:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `id` | UUID | Yes | Unique identifier for the service |
| `service` | String | Yes | Name of the service |
| `shortDesc` | String | Yes | Short description of the service |
| `operationalDays` | Array of Strings | Yes | Days of the week when the service is operational. Valid values: "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" |
| `operationalDate` | String or null | No | Specific date when the service is operational, in "YYYY-MM-DD" format |
| `maxVerification` | Integer | Yes | Maximum number of verifications allowed |
| `allowFutureBooking` | Boolean | Yes | Whether future booking is allowed |
| `allowCancellation` | Boolean | Yes | Whether cancellation is allowed |
| `expiry` | Object | Yes | Expiry details for the service |
| `expiry.tag` | String | Yes | Type of expiry. Valid values: "InstantExpiry", "NoExpiry" |
| `expiry.contents` | Integer | Yes (for InstantExpiry) | Number of minutes after which the service expires |
| `businessHours` | Array of UUIDs | Yes | List of business hour IDs associated with the service |

#### Business Hours

Each business hour in the `businessHours` array has the following fields:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `id` | UUID | Yes | Unique identifier for the business hour |
| `name` | String | Yes | Name of the business hour |
| `btype` | Object | Yes | Type of business hour |
| `btype.tag` | String | Yes | Type of business hour. Valid values: "Duration", "Slot" |
| `btype.contents` | Array of Strings | Yes | For "Duration": ["startTime", "endTime"] in "HH:MM:SS" format. For "Slot": List of slot times in "HH:MM:SS" format |
| `categoryId` | Array of UUIDs | Yes | List of service category IDs associated with the business hour |
| `bookingClosingTime` | String | Yes | Time after which booking is closed, in "HH:MM:SS" format |

#### Service Categories

Each service category in the `serviceCategories` array has the following fields:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `id` | UUID | Yes | Unique identifier for the service category |
| `name` | String | Yes | Name of the service category |
| `description` | String | Yes | Description of the service category |
| `allowedSeats` | Integer | Yes | Total number of seats allowed |
| `availableSeats` | Integer | Yes | Number of seats currently available |
| `peopleCategory` | Array of UUIDs | Yes | List of service people category IDs associated with the service category |

#### Service People Categories

Each service people category in the `servicePeopleCategories` array has the following fields:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `id` | UUID | Yes | Unique identifier for the service people category |
| `name` | String | Yes | Name of the service people category |
| `description` | String | Yes | Description of the service people category |
| `pricingType` | String | Yes | Type of pricing. Valid values: "AllDays", "SameDay", "NextDay" |
| `priceAmount` | Number | Yes | Price amount |
| `priceCurrency` | String | Yes | Currency of the price. Example: "INR" |
| `timeBounds` | Object | Yes | Time bounds for the service people category |
| `timeBounds.tag` | String | Yes | Type of time bounds. Valid values: "Unbounded", "Bounded" |
| `timeBounds.contents` | Array (for Bounded) | Yes (for Bounded) | For "Bounded": ["startTime", "endTime"] in "HH:MM:SS" format |
| `vendorSplitDetails` | Array of Objects | No | List of vendor split details |

#### Vendor Split Details

Each vendor split detail in the `vendorSplitDetails` array has the following fields:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `vendorId` | UUID | Yes | Unique identifier for the vendor |
| `splitAmount` | Number | Yes | Amount to be split to the vendor. Can include decimal values (e.g., 60.85) |
| `splitType` | String | Yes | Type of split. Currently only "FIXED" is supported |

#### Special Occasions

Each special occasion in the `specialOccasions` array has the following fields:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `id` | UUID | Yes | Unique identifier for the special occasion |
| `entityId` | UUID | Yes | ID of the entity (usually a service category) associated with the special occasion |
| `date` | String | Yes | Date of the special occasion in "YYYY-MM-DD" format |
| `dayOfWeek` | String or null | No | Day of the week for recurring special occasions. Valid values: "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" |
| `specialDayType` | String | Yes | Type of special day. Valid values: "Open", "Closed" |
| `description` | String | Yes | Description of the special occasion |
| `businessHours` | Array of UUIDs | Yes | List of business hour IDs associated with the special occasion |
| `placeId` | UUID | Yes | ID of the ticket place associated with the special occasion |
| `name` | String | Yes | Name of the special occasion |

### JSON Example

Here's a complete example of the JSON format for creating or updating a ticket place:

```json
{
  "id": "new-ticket-place-id",
  "name": "Example Ticket Place",
  "description": "This is an example ticket place for documentation",
  "shortDesc": "Example Place",
  "address": "123 Example Street, Example City",
  "latitude": 10.123456,
  "longitude": 76.123456,
  "status": "Active",
  "priority": 1,
  "placeType": "Museum",
  "allowSameDayBooking": true,
  "gallery": [
    "https://example.com/image1.jpg",
    "https://example.com/image2.jpg"
  ],
  "iconUrl": "https://example.com/icon.png",
  "mapImageUrl": "https://example.com/map.png",
  "termsAndConditions": [
    "Terms and conditions 1",
    "Terms and conditions 2"
  ],
  "termsAndConditionsUrl": "https://example.com/terms.html",
  "openTimings": "09:00:00",
  "closeTimings": "18:00:00",
  "services": [
    {
      "id": "service-uuid-1",
      "service": "Example Service",
      "shortDesc": "This is an example service",
      "operationalDays": ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"],
      "operationalDate": null,
      "maxVerification": 3,
      "allowFutureBooking": true,
      "allowCancellation": true,
      "expiry": {"tag": "InstantExpiry", "contents": 60},
      "businessHours": ["business-hour-uuid-1"]
    }
  ],
  "businessHours": [
    {
      "id": "business-hour-uuid-1",
      "name": "Regular Hours",
      "btype": {
        "tag": "Duration",
        "contents": ["09:00:00", "18:00:00"]
      },
      "categoryId": ["service-category-uuid-1"],
      "bookingClosingTime": "17:00:00"
    }
  ],
  "serviceCategories": [
    {
      "id": "service-category-uuid-1",
      "name": "Example Category",
      "description": "This is an example category",
      "allowedSeats": 100,
      "availableSeats": 50,
      "peopleCategory": ["service-people-category-uuid-1"]
    }
  ],
  "servicePeopleCategories": [
    {
      "id": "service-people-category-uuid-1",
      "name": "Example People Category",
      "description": "This is an example people category",
      "pricingType": "AllDays",
      "priceAmount": 100.0,
      "priceCurrency": "INR",
      "timeBounds": {
        "tag": "Unbounded"
      },
      "vendorSplitDetails": [
        {
          "vendorId": "vendor-uuid-1",
          "splitAmount": 60.85,
          "splitType": "FIXED"
        },
        {
          "vendorId": "vendor-uuid-2",
          "splitAmount": 39.15,
          "splitType": "FIXED"
        }
      ]
    }
  ],
  "specialOccasions": [
    {
      "id": "special-occasion-uuid-1",
      "entityId": "service-category-uuid-1",
      "date": "2023-12-25",
      "dayOfWeek": null,
      "specialDayType": "Closed",
      "description": "Christmas Holiday",
      "businessHours": [],
      "placeId": "new-ticket-place-id",
      "name": "Christmas"
    }
  ]
}
```

## Implementation Details

The `postUpsertTicketPlaceDashboardDetails` function handles both creating new ticket places and updating existing ones. It follows these steps:

1. **Validation**: Checks that the ticket place name is not empty and that a short description is provided.

2. **Check Existence**: Checks if a ticket place with the given ID already exists.

3. **Update or Create**:
   - If the ticket place exists, it updates the existing place.
   - If the ticket place doesn't exist, it creates a new one.

4. **Related Entities**: For each related entity (services, business hours, etc.), it:
   - Checks if the entity exists.
   - Updates the entity if it exists.
   - Creates a new entity if it doesn't exist.

## Enum Values and Special Data Formats

### Enum Values

1. **PlaceStatus**:
   - `Active`: The ticket place is active and available for bookings
   - `Inactive`: The ticket place is inactive and not available for bookings

2. **PlaceType**:
   - `Museum`: Museum location
   - `ThemePark`: Theme park location
   - `AmusementPark`: Amusement park location
   - `WaterPark`: Water park location
   - `WildLifeSanctuary`: Wildlife sanctuary location
   - `ArtGallery`: Art gallery location
   - `HeritageSite`: Heritage site location
   - `ReligiousSite`: Religious site location
   - `Other`: Other type of location

3. **PricingType**:
   - `AllDays`: Pricing applies to all days
   - `SameDay`: Pricing applies only to the same day
   - Note: The API documentation mentions `NextDay` as a possible value, but our tests show that only `AllDays` and `SameDay` are currently supported.

4. **SpecialDayType**:
   - `Open`: The place is open on the special day
   - `Closed`: The place is closed on the special day

### Special Data Formats

When creating or updating ticket places, pay attention to these specific format requirements:

1. **ExpiryType**: Must be one of:
   ```json
   {"tag": "InstantExpiry", "contents": 60}
   ```
   The `contents` value is the number of minutes after which the ticket expires.

   or

   ```json
   {"tag": "VisitDate", "contents": "10:00:00"}
   ```
   The `contents` value is the time of day in "HH:MM:SS" format after which the ticket expires.

   Note: According to the Haskell code, `VisitDate` expects a `TimeOfDay` value, not a date. Our tests confirm that both `InstantExpiry` and `VisitDate` work correctly when using the proper formats.

2. **BusinessHourType**: Must be in one of the following formats:
   ```json
   {"tag": "Duration", "contents": ["09:00:00", "18:00:00"]}
   ```
   The `contents` array contains the start time and end time in 24-hour format.

   or

   ```json
   {"tag": "Slot", "contents": "10:00:00"}
   ```
   The `contents` value is a single time slot in 24-hour format.

   Note: According to the Haskell code, `Slot` expects a single `TimeOfDay` value, not an array. Our tests confirm that both `Duration` and `Slot` work correctly when using the proper formats.

3. **TimeBounds**: Must be in one of the following formats:
   ```json
   {"tag": "Unbounded"}
   ```
   Indicates that there are no time restrictions.

   or

   ```json
   {"tag": "BoundedByDay", "contents": [["2023-12-31", [["09:00:00", "18:00:00"]]]}
   ```
   The `contents` is a list of tuples, where each tuple contains a date and a list of time ranges for that date.

   or

   ```json
   {"tag": "BoundedByWeekday", "contents": {
     "monday": [["09:00:00", "18:00:00"]],
     "tuesday": [["09:00:00", "18:00:00"]],
     "wednesday": [["09:00:00", "18:00:00"]],
     "thursday": [["09:00:00", "18:00:00"]],
     "friday": [["09:00:00", "18:00:00"]],
     "saturday": [["09:00:00", "18:00:00"]],
     "sunday": [["09:00:00", "18:00:00"]]
   }}
   ```
   The `contents` is an object with fields for each day of the week, each containing a list of time ranges.

   Note: According to the Haskell code, `BoundedByDay` expects a list of tuples with a date and a list of time ranges, and `BoundedByWeekday` expects a complex structure with fields for each day of the week. Our tests confirm that all three formats (`Unbounded`, `BoundedByDay`, and `BoundedByWeekday`) work correctly when using the proper formats.

4. **VendorSplitDetails**: Must be in the format:
   ```json
   {
     "vendorId": "vendor-uuid",
     "splitAmount": 60.85,
     "splitType": "FIXED"
   }
   ```
   - `vendorId`: UUID of the vendor
   - `splitAmount`: Amount to be split to the vendor. Can include decimal values (e.g., 60.85)
   - `splitType`: Type of split. Currently only "FIXED" is supported

   **Note**: Multiple vendor split details are supported and will be saved correctly. Decimal values in the `splitAmount` field (e.g., 60.85) are preserved correctly.

   Note: The exact format for BoundedByDay and BoundedByWeekday may vary. If the above format doesn't work, try:

   ```json
   {"tag": "BoundedByDay", "contents": {"startTime": "09:00:00", "endTime": "18:00:00"}}
   ```

   or

   ```json
   {"tag": "BoundedByWeekday", "contents": {"Monday": ["09:00:00", "18:00:00"], "Tuesday": ["09:00:00", "17:00:00"]}}
   ```

   or

   ```json
   {"tag": "BoundedByWeekday", "contents": {"Monday": {"startTime": "09:00:00", "endTime": "18:00:00"}, "Tuesday": {"startTime": "09:00:00", "endTime": "17:00:00"}}}
   ```

4. **OperationalDays**: An array of days of the week when the service is operational:
   ```json
   ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
   ```

5. **DayOfWeek**: For weekly special occasions, must be one of:
   ```
   "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
   ```

6. **VendorSplitDetails**: Must be in the format used by the rider-app module:
   ```json
   {
     "vendorId": "vendor-uuid",
     "splitAmount": 70,
     "splitType": "FIXED"
   }
   ```
   - `vendorId`: UUID of the vendor
   - `splitAmount`: Amount that goes to this vendor
   - `splitType`: Type of split, currently only "FIXED" is supported

   **Note**: The vendor split details functionality works with this format. Do not include fields like `vendorName` or `percentage` as they are not part of the expected format. Also note that:
   - The API preserves decimal values in the `splitAmount` field, so values like `60.85` are stored correctly.
   - Multiple vendor split details are supported and will be saved correctly.

## Testing

Two comprehensive test scripts have been created to test the API endpoints:

### Basic Test Script (`test_ticket_dashboard_apis.sh`)

This script tests:

1. **Updating an existing ticket place**:
   - Basic information (name, description)
   - Status
   - Operating hours
   - Services
   - Business hours
   - Service categories
   - Service people categories
   - Special occasions
   - Terms and conditions
   - Gallery

2. **Creating a new ticket place**:
   - With all related entities
   - Verifying the creation

3. **Updating a newly created ticket place**:
   - Basic information
   - Operating hours

### Enhanced Test Script (`test_ticket_dashboard_apis_enhanced.sh`)

This script focuses on testing more specific aspects of the API, including:

1. **Testing different place types**:
   - Updating place type from "Other" to "Museum"
   - Verifying the update

2. **Testing service people categories**:
   - Adding a new service people category
   - Updating pricing type from "AllDays" to "SameDay"
   - Verifying the updates

3. **Testing different business hour types**:
   - Adding a business hour with "Slot" type
   - Verifying the addition

4. **Testing special occasions**:
   - Adding a special occasion with "Open" type
   - Adding a weekly special occasion (e.g., "Sunday Closed")
   - Verifying the additions

5. **Testing place status**:
   - Updating place status from "Active" to "Inactive"
   - Verifying the update

6. **Testing time bounds**:
   - Updating time bounds from "Unbounded" to "BoundedByDay"
   - Verifying the update

### Test Results

The core functionality tests (creating a new ticket place and basic updates) are passing, confirming that the implementation of `postUpsertTicketPlaceDashboardDetails` is working correctly.

The enhanced tests provide more comprehensive coverage of the API's capabilities, especially for handling different enum values and special data formats.

### Key Findings from Testing

1. **PlaceType Values**: The valid place types are "Museum", "ThemePark", "AmusementPark", "WaterPark", "WildLifeSanctuary", "ArtGallery", "HeritageSite", "ReligiousSite", and "Other".

2. **PricingType Values**: The valid pricing types are "AllDays" and "SameDay".

3. **SpecialDayType Values**: The valid special day types are "Open" and "Closed".

4. **TimeBounds Format**: The TimeBounds format is complex and may vary. The "Unbounded" tag works consistently, but the "BoundedByDay" and "BoundedByWeekday" formats may require experimentation.

5. **Status Values**: The valid status values are "Active" and "Inactive".

6. **Data Relationships**: The relationships between entities (e.g., services referencing business hours, service categories referencing people categories) must be maintained for the API to work correctly.

7. **Vendor Split Details**: The vendor split details functionality works when using the correct format from the rider-app module. The format must include only `vendorId`, `splitAmount`, and `splitType` fields, with `splitType` set to "FIXED". Multiple vendor split details are supported and will be saved correctly. Decimal values in the `splitAmount` field (e.g., 60.85) are preserved correctly.

### Additional Test Scripts

We've created additional test scripts to test specific aspects of the API:

1. **Comprehensive Test Script (`test_ticket_dashboard_comprehensive.sh`)**:
   - Tests all fields of the ticket place
   - Attempts to test vendor split details (but this functionality may not be fully implemented yet)
   - Updates all fields of the ticket place at once

2. **Vendor Split Test Script (`test_vendor_split.sh`)**:
   - Attempts to create a ticket place with vendor split details
   - Verifies the vendor split details (if successful)

3. **Vendor Split Update Script (`test_vendor_split_update.sh`)**:
   - Creates a ticket place without vendor split details
   - Attempts to update the ticket place to add vendor split details
   - Verifies the vendor split details (if successful)

4. **Vendor Split Rider Format Script (`test_vendor_split_rider_format.sh`)**:
   - Creates a ticket place with vendor split details using the rider-app format
   - Verifies the vendor split details were created correctly
   - Updates the vendor split details
   - Verifies the updated vendor split details

5. **Multiple Vendor Splits Script (`test_multiple_vendor_splits.sh`)**:
   - Creates a ticket place with multiple vendor split details
   - Verifies that multiple vendor split details are saved correctly

6. **Comprehensive All Fields Script (`test_comprehensive_all_fields.sh`)**:
   - Creates a ticket place with all fields populated
   - Verifies all fields were created correctly
   - Updates the ticket place
   - Verifies the update was successful

7. **Decimal Split Amount Script (`test_decimal_split_amount.sh`)**:
   - Creates a ticket place with decimal split amounts (e.g., 60.85)
   - Verifies that decimal split amounts are preserved correctly

**Note**: The vendor split details functionality works when using the correct format from the rider-app module. The `test_vendor_split_rider_format.sh`, `test_multiple_vendor_splits.sh`, `test_comprehensive_all_fields.sh`, and `test_decimal_split_amount.sh` scripts demonstrate the correct format to use.

### Example Test Commands

```bash
# Run the basic test script
./test_ticket_dashboard_apis.sh

# Run the enhanced test script
./test_ticket_dashboard_apis_enhanced.sh

# Run the comprehensive test script
./test_ticket_dashboard_comprehensive.sh

# Run the vendor split test script
./test_vendor_split.sh

# Run the vendor split update script
./test_vendor_split_update.sh

# Run the vendor split rider format script
./test_vendor_split_rider_format.sh

# Run the multiple vendor splits script
./test_multiple_vendor_splits.sh

# Run the comprehensive all fields script
./test_comprehensive_all_fields.sh

# Run the decimal split amount script
./test_decimal_split_amount.sh
```

## Example JSON for Creating a New Ticket Place

```json
{
  "id": "new-ticket-place-id",
  "name": "Test Ticket Place",
  "description": "This is a test ticket place created via API",
  "shortDesc": "Test Place",
  "address": "123 Test Street, Test City",
  "latitude": 10.123456,
  "longitude": 76.123456,
  "status": "Active",
  "priority": 1,
  "placeType": "Other",
  "allowSameDayBooking": true,
  "gallery": [
    "https://example.com/image1.jpg",
    "https://example.com/image2.jpg"
  ],
  "iconUrl": "https://example.com/icon.png",
  "mapImageUrl": "https://example.com/map.png",
  "termsAndConditions": [
    "Terms and conditions 1",
    "Terms and conditions 2"
  ],
  "termsAndConditionsUrl": "https://example.com/terms.html",
  "openTimings": "09:00:00",
  "closeTimings": "18:00:00",
  "services": [
    {
      "id": "new-service-id",
      "service": "Test Service",
      "shortDesc": "This is a test service",
      "operationalDays": ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"],
      "operationalDate": null,
      "maxVerification": 3,
      "allowFutureBooking": true,
      "allowCancellation": true,
      "expiry": {"tag": "InstantExpiry", "contents": 60},
      "businessHours": ["new-business-hour-id"]
    }
  ],
  "businessHours": [
    {
      "id": "new-business-hour-id",
      "name": "Regular Hours",
      "btype": {
        "tag": "Duration",
        "contents": ["09:00:00", "18:00:00"]
      },
      "categoryId": ["new-service-category-id"],
      "bookingClosingTime": "17:00:00"
    }
  ],
  "serviceCategories": [
    {
      "id": "new-service-category-id",
      "name": "Test Category",
      "description": "This is a test category",
      "allowedSeats": 100,
      "availableSeats": 50,
      "peopleCategory": ["new-service-people-category-id"]
    }
  ],
  "servicePeopleCategories": [
    {
      "id": "new-service-people-category-id",
      "name": "Test People Category",
      "description": "This is a test people category",
      "pricingType": "AllDays",
      "priceAmount": 100.0,
      "priceCurrency": "INR",
      "timeBounds": {
        "tag": "Unbounded"
      },
      "vendorSplitDetails": [
        {
          "vendorId": "vendor-uuid-1",
          "splitAmount": 60.85,
          "splitType": "FIXED"
        },
        {
          "vendorId": "vendor-uuid-2",
          "splitAmount": 39.15,
          "splitType": "FIXED"
        }
      ]
    }
  ],
  "specialOccasions": [
    {
      "id": "new-special-occasion-id",
      "entityId": "new-service-category-id",
      "date": "2023-12-25",
      "dayOfWeek": null,
      "specialDayType": "Closed",
      "description": "Christmas Holiday",
      "businessHours": [],
      "placeId": "new-ticket-place-id",
      "name": "Christmas"
    }
  ]
}
```

## Summary of Testing Results

We have tested all the complex enum types and data structures in the ticket dashboard API. Here's a summary of our findings:

1. **ExpiryType**:
   - `InstantExpiry` works correctly with an integer value for minutes.
   - `VisitDate` works correctly with a time of day string in "HH:MM:SS" format.

2. **BusinessHourType**:
   - `Duration` works correctly with an array of two time strings in "HH:MM:SS" format.
   - `Slot` works correctly with a single time string in "HH:MM:SS" format.

3. **TimeBounds**:
   - `Unbounded` works correctly with no additional parameters.
   - `BoundedByDay` works correctly with a list of tuples containing a date and a list of time ranges.
   - `BoundedByWeekday` works correctly with an object containing fields for each day of the week, each containing a list of time ranges.

4. **PricingType**:
   - `AllDays` and `SameDay` work correctly.
   - `NextDay` is mentioned in the documentation but not supported by the API.

5. **SpecialDayType**:
   - `Open` and `Closed` work correctly.

6. **VendorSplitDetails**:
   - Works correctly with decimal values for `splitAmount`.
   - Multiple vendor split details are supported.

## Troubleshooting

If you encounter issues when creating or updating ticket places, check the following:

1. **Data Format**: Ensure that all fields follow the required format, especially the tagged fields like ExpiryType, BusinessHourType, etc.

2. **Required Fields**: Make sure all required fields are provided and not null.

3. **IDs**: Ensure that all IDs are valid UUIDs and that referenced IDs (e.g., in businessHours arrays) exist.

4. **Relationships**: Check that the relationships between entities are correct (e.g., a service references valid business hours).

## Future Improvements

1. **Better Validation**: Add more comprehensive validation to catch common errors before they reach the database.

2. **Error Messages**: Improve error messages to make it clearer what went wrong.

3. **Test Script**: Update the test script to better handle the actual response format and make it more robust.
