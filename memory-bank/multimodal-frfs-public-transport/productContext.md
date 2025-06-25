# Multimodal Public Transport (FRFS) Flow: Product Context

This document outlines the UI API interaction flow and the overall user journey for the Multimodal Public Transport (FRFS) booking system.

## UI API Interaction Flow

The flow describes a typical user journey for booking and managing multimodal public transport. It can be broken down into several stages:

### 1. User Authentication

*   **User Action:** User opens the app and enters their mobile number.
*   **API Call:** `POST /auth`
*   **User Action:** User receives an OTP and enters it into the app.
*   **API Call:** `POST /auth/:authId/verify`

### 2. Journey Search

*   **User Action:** User inputs origin and destination (GPS coordinates or addresses) and selects "Public Transport" or a general "One Way" search.
*   **API Call:** `POST /multimodalSearch` (for PT Search or Normal One Way)
*   **User Action:** After the initial search, the UI polls or receives updates for search results.
*   **API Call:** `GET /rideSearch/:searchId/results`
*   **User Action:** User views the list of available journey options (quotes) and selects one.

### 3. Journey Details and Booking Confirmation

*   **User Action:** User selects a journey option and proceeds to confirm.
*   **API Call:** `POST /multimodal/:journeyId/initiate` or `POST /frfs/quote/v2/:quoteId/confirm`
*   **User Action:** User confirms the booking details (e.g., number of tickets, payment method).
*   **API Call:** `POST /multimodal/:journeyId/confirm` or `POST /frfs/quote/:quoteId/confirm`

### 4. In-Journey Status and Updates

*   **User Action:** User views their active journey or the app needs to track the user's location.
*   **API Call:** `GET /multimodal/:journeyId/booking/info`
*   **API Call:** `POST /multimodal/:journeyId/rider/location` (for rider location updates)
*   **API Call:** `GET /multimodal/:journeyId/booking/paymentStatus` (to check payment status)
*   **API Call:** `GET /multimodal/journey/:journeyId/status` (redundant to rider location, but provides status)

### 5. Journey Modification and Cancellation

*   **User Action:** User decides to modify (e.g., extend a leg, switch mode) or cancel a journey.
*   **API Calls:**
    *   `POST /multimodal/extend/:journeyId/leg/getfare` (Get fare for extension)
    *   `POST /multimodal/extend/:journeyId/leg` (Confirm extension)
    *   `POST /multimodal/:journeyId/switch` (Switch mode for a leg)
    *   `POST /multimodal/:journeyId/order/:legOrder/switchTaxi` (Switch taxi variant)
    *   `POST /multimodal/journey/:journeyId/cancel` (Cancel journey)
    *   `POST /multimodal/journey/:journeyId/leg/:legOrder/skip` (Skip a leg)
    *   `POST /multimodal/journey/:journeyId/leg/:legOrder/addSkippedLeg` (Add back a skipped leg)
    *   `POST /frfs/booking/{bookingId}/canCancel` (Check if booking can be cancelled)
    *   `GET /frfs/booking/{bookingId}/canCancel/status` (Get cancellation status)
    *   `POST /frfs/booking/{bookingId}/cancel` (Confirm cancellation)
    *   `GET /frfs/booking/cancel/{bookingId}/status` (Get final cancellation status)
    *   `POST /frfs/ticket/verify` (Verify ticket QR data)
    *   `GET /frfs/config` (Get FRFS configuration)
    *   `GET /frfs/autocomplete` (Get route/stop suggestions)
