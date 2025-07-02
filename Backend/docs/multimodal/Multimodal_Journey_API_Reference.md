# Multimodal Journey System - API Reference

## Table of Contents
1. [Core Journey Management](#core-journey-management)
2. [Journey Control Operations](#journey-control-operations)
3. [Leg Management](#leg-management)
4. [Payment & Ticketing](#payment--ticketing)
5. [User Preferences & Configuration](#user-preferences--configuration)
6. [Feedback & Completion](#feedback--completion)

## Core Journey Management

### 1. `postMultimodalInitiate`
**Purpose**: Initialize a journey with route planning and fare calculation
**Method**: POST
**Path**: `/journey/{journeyId}/initiate`

**Flow**:
- Gets journey legs from route planning
- Calculates fares for each leg using `JLI.getFare`
- Creates journey legs with proper sequencing
- Updates journey status to `INITIATED`
- Returns journey information with all legs

**Key Function**: `postMultimodalInitiate`
```haskell
postMultimodalInitiate :: (PersonId?, MerchantId) -> JourneyId -> Flow JourneyInfoResp
```

---

### 2. `postMultimodalConfirm`
**Purpose**: Confirm and start journey execution
**Method**: POST
**Path**: `/journey/{journeyId}/confirm`

**Request Body**:
```json
{
  "journeyConfirmReqElements": [
    {
      "journeyLegOrder": 1,
      "skipBooking": false,
      "ticketQuantity": 2,
      "childTicketQuantity": 0,
      "crisSdkResponse": {...}
    }
  ]
}
```

**Flow**:
- Validates leg selections and quantities
- Calls `JM.startJourney` to begin leg bookings
- Updates journey status to `CONFIRMED`
- Creates recent location cache for multimodal
- Returns success response

---

### 3. `getMultimodalBookingInfo`
**Purpose**: Get comprehensive journey information
**Method**: GET
**Path**: `/journey/{journeyId}/info`

**Response**:
```json
{
  "journeyId": "uuid",
  "status": "INPROGRESS",
  "legs": [
    {
      "order": 1,
      "travelMode": "Walk",
      "status": "Completed",
      "estimatedDuration": 300,
      "legExtraInfo": {...}
    }
  ]
}
```

---

### 4. `getMultimodalJourneyStatus`
**Purpose**: Get real-time journey status with live tracking
**Method**: GET
**Path**: `/journey/{journeyId}/status`

**Flow**:
- Calls `JM.getAllLegsStatus` to get current state
- Applies movement detection for bus tracking
- Updates journey expiry based on ticket validity
- Triggers feedback collection when complete
- Returns real-time status for all legs

## Journey Control Operations

### 5. `postMultimodalRiderLocation`
**Purpose**: Update rider location and trigger leg transitions
**Method**: POST
**Path**: `/journey/{journeyId}/location`

**Request Body**:
```json
{
  "latLong": {
    "lat": 12.9716,
    "lon": 77.5946
  },
  "accuracy": 10.5,
  "timestamp": "2024-01-01T10:00:00Z"
}
```

**Flow**:
- Adds location point to journey tracking
- Checks leg completion status
- Auto-starts next taxi legs when previous leg completes
- Returns updated journey status

---

### 6. `postMultimodalJourneyCancel`
**Purpose**: Cancel entire journey and all remaining legs
**Method**: POST
**Path**: `/journey/{journeyId}/cancel`

**Flow**:
- Calls `JM.cancelRemainingLegs` for all active legs
- Updates journey status to `CANCELLED`
- Handles refunds for cancelled bookings
- Returns success response

---

### 7. `postMultimodalJourneyLegSkip`
**Purpose**: Skip a specific journey leg
**Method**: POST
**Path**: `/journey/{journeyId}/leg/{legOrder}/skip`

**Flow**:
- Validates leg can be skipped (not Walk mode)
- Checks leg status allows skipping
- Calls `JM.skipLeg` with leg order
- Updates leg status to `Skipped`

---

### 8. `postMultimodalExtendLeg`
**Purpose**: Extend or modify a journey leg
**Method**: POST
**Path**: `/journey/{journeyId}/extend`

**Request Body**:
```json
{
  "startLocation": {
    "lat": 12.9716,
    "lon": 77.5946
  },
  "endLocation": {
    "lat": 12.9716,
    "lon": 77.5946
  },
  "fare": {...},
  "distance": 1500,
  "duration": 900,
  "bookingUpdateRequestId": "uuid"
}
```

## Leg Management

### 9. `postMultimodalOrderSwitchTaxi`
**Purpose**: Switch to different taxi option for a leg
**Method**: POST
**Path**: `/journey/{journeyId}/leg/{legOrder}/switch-taxi`

**Request Body**:
```json
{
  "estimateId": "new-estimate-uuid"
}
```

**Flow**:
- Validates leg is taxi mode and switchable
- Cancels previous search if driver assigned
- Updates search pricing ID
- Calls `JLI.confirm` for new estimate
- Returns updated journey info

---

### 10. `postMultimodalOrderSwitchFRFSTier`
**Purpose**: Switch transit service tier (AC/Non-AC, Express/Regular)
**Method**: POST
**Path**: `/journey/{journeyId}/leg/{legOrder}/switch-tier`

**Request Body**:
```json
{
  "quoteId": "new-quote-uuid"
}
```

**Flow**:
- Updates FRFS search pricing ID
- Recalculates total fare for new tier
- Updates payment order amount
- Updates route alternative names
- Returns updated journey info

---

### 11. `postMultimodalOrderSetStatus`
**Purpose**: Manually set leg status (for operator use)
**Method**: POST
**Path**: `/journey/{journeyId}/leg/{legOrder}/status/{newStatus}`

**Allowed Statuses**: `Completed`, `Cancelled`

## Payment & Ticketing

### 12. `getMultimodalBookingPaymentStatus`
**Purpose**: Check payment status for FRFS bookings
**Method**: GET
**Path**: `/journey/{journeyId}/payment-status`

**Response**:
```json
{
  "journeyId": "uuid",
  "paymentOrder": {
    "sdkPayload": {...},
    "status": "SUCCESS"
  },
  "paymentFareUpdate": [
    {
      "journeyLegOrder": 2,
      "oldFare": {...},
      "newFare": {...}
    }
  ]
}
```

---

### 13. `postMultimodalPaymentUpdateOrder`
**Purpose**: Update payment order with new quantities/amounts
**Method**: POST
**Path**: `/journey/{journeyId}/payment/update`

**Request Body**:
```json
{
  "quantity": 3,
  "childTicketQuantity": 1
}
```

**Flow**:
- Updates all FRFS bookings with new quantities
- Recalculates total fare
- Updates payment order amount
- Returns updated SDK payload

---

### 14. `postMultimodalTicketVerify`
**Purpose**: Verify transit tickets via QR scanning
**Method**: POST
**Path**: `/multimodal/ticket/verify`

**Request Body**:
```json
{
  "provider": "MTC",
  "tickets": ["qr-code-1", "qr-code-2"]
}
```

**Response**:
```json
{
  "provider": "MTC",
  "legInfo": [
    {
      "order": 2,
      "travelMode": "Bus",
      "status": "Booked"
    }
  ]
}
```

## User Preferences & Configuration

### 15. `getMultimodalUserPreferences`
**Purpose**: Get user's transport mode preferences
**Method**: GET
**Path**: `/user/preferences/multimodal`

**Response**:
```json
{
  "allowedTransitModes": ["Taxi", "Metro", "Bus", "Walk"],
  "journeyOptionsSortingType": "MOST_RELEVANT",
  "busTransitTypes": ["ORDINARY", "EXPRESS"],
  "subwayTransitTypes": ["FIRST_CLASS", "SECOND_CLASS"]
}
```

---

### 16. `postMultimodalUserPreferences`
**Purpose**: Update user transport preferences
**Method**: POST
**Path**: `/user/preferences/multimodal`

**Request Body**: Same as GET response above

**Flow**:
- Always includes Walk mode (added automatically)
- Updates existing preferences or creates new
- Used for journey planning and filtering

---

### 17. `getPublicTransportData`
**Purpose**: Get transit system information (stations, routes)
**Method**: GET
**Path**: `/public-transport/data?city={city}&version={version}`

**Response**:
```json
{
  "ss": [
    {
      "cd": "STATION001",
      "nm": "Central Station",
      "lt": 12.9716,
      "ln": 77.5946,
      "vt": "METRO"
    }
  ],
  "rs": [
    {
      "cd": "ROUTE001",
      "sN": "Blue Line",
      "lN": "Airport to City Center",
      "vt": "METRO"
    }
  ],
  "ptcv": "config-version-hash"
}
```

## Feedback & Completion

### 18. `postMultimodalJourneyFeedback`
**Purpose**: Submit journey feedback and ratings
**Method**: POST
**Path**: `/journey/{journeyId}/feedback`

**Request Body**:
```json
{
  "rating": 4,
  "additionalFeedBack": "Great experience!",
  "rateTravelMode": [
    {
      "legOrder": 1,
      "travelMode": "Metro",
      "isExperienceGood": true
    }
  ]
}
```

**Flow**:
- Creates journey-level feedback
- Creates leg-specific feedback for each mode
- Marks journey as `COMPLETED`
- Updates existing feedback if already provided

---

### 19. `getMultimodalFeedback`
**Purpose**: Retrieve submitted feedback
**Method**: GET
**Path**: `/journey/{journeyId}/feedback`

**Response**: Same structure as POST request body

---

### 20. `postMultimodalComplete`
**Purpose**: Mark journey as complete (operator function)
**Method**: POST
**Path**: `/journey/{journeyId}/complete`

**Flow**:
- Marks all legs as `Completed`
- Updates journey to `FEEDBACK_PENDING` status
- Used when automatic completion fails

## Error Responses

All endpoints return standardized error responses:

```json
{
  "errorCode": "JOURNEY_NOT_FOUND",
  "errorMessage": "Journey with ID xyz not found",
  "userMessage": "Journey information is not available"
}
```

## Common Status Codes
- `200` - Success
- `400` - Bad Request (validation errors)
- `404` - Resource not found
- `409` - Conflict (invalid state transition)
- `500` - Internal server error