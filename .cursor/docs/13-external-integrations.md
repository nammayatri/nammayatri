# External Integrations

## Overview

Namma Yatri integrates with 21+ external services. This doc maps each integration to its code location.

## Payment

### Juspay (Primary Payment Gateway)

| Component | Location |
|-----------|----------|
| Payment library | `lib/payment/src/Lib/Payment/` |
| Core payment actions | `lib/payment/src/Lib/Payment/Domain/Action.hs` |
| Payment API | `lib/payment/src/Lib/Payment/API.hs` |
| Payout handling | `lib/payment/src/Lib/Payment/Payout/` |
| Rider-app payment | `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Payment.hs` |
| Rider-app SharedLogic | `app/rider-platform/rider-app/Main/src/SharedLogic/Payment.hs` |
| Driver-app payment | `app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Payment.hs` |
| Driver-app SharedLogic | `app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Payment.hs` |
| Driver fee logic | `app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/DriverFee.hs` |

Key functions:
- `juspayWebhookHandler` — Webhook endpoint (both rider & driver apps)
- `juspayWebhookHandlerV2` — V2 webhook (driver-app only)
- `createOrderService` — Create payment order
- `orderStatusService` — Check payment status
- `createRefundService` — Process refund
- `createPayoutService` — Driver payout

## Maps & Routing

### Google Maps / OSRM

| Component | Location |
|-----------|----------|
| Rider-app maps | `app/rider-platform/rider-app/Main/src/Tools/Maps.hs` |
| Driver-app maps | `app/provider-platform/dynamic-offer-driver-app/Main/src/Tools/Maps.hs` |
| Location updates lib | `lib/location-updates/` |
| Driver location tracking | `app/provider-platform/dynamic-offer-driver-app/Main/src/Lib/LocationUpdates.hs` |
| Google Maps shared logic | `app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/GoogleMaps.hs` |
| Google translate | `app/rider-platform/rider-app/Main/src/SharedLogic/GoogleTranslate.hs` |
| Mock Google service | `app/mocks/google/src/` |

Maps functions:
- `autoComplete` — Place autocomplete
- `getDistance` / `getDistances` — Distance calculation
- `getPlaceDetails` / `getPlaceName` — Location details
- `getRoutes` / `getPickupRoutes` / `getTripRoutes` — Routing
- `snapToRoad` — Snap GPS coordinates to road
- `getFrfsAutocompleteDistances` — FRFS-specific distances
- `getMultimodalWalkDistance` — Multimodal support

## Document Verification

### Idfy

| Component | Location |
|-----------|----------|
| Webhook handler | `app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/DriverOnboarding/IdfyWebhook.hs` |
| Verification tools | `app/provider-platform/dynamic-offer-driver-app/Main/src/Tools/Verification.hs` |
| Mock service | `app/mocks/idfy/src/` |

### HyperVerge

| Component | Location |
|-----------|----------|
| Verification type | `app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Domain/Types/HyperVergeVerification.hs` |
| SDK logs | `app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Domain/Types/HyperVergeSdkLogs.hs` |

## Notifications

### FCM (Firebase Cloud Messaging)

| Component | Location |
|-----------|----------|
| Rider-app notifications | `app/rider-platform/rider-app/Main/src/Tools/Notifications.hs` |
| Rider-app trigger | `app/rider-platform/rider-app/Main/src/Domain/Action/UI/TriggerFCM.hs` |
| Driver-app scheduled FCMs | `app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Allocator/Jobs/FCM/RunScheduledFCMS.hs` |
| Driver-app soft block | `app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Allocator/Jobs/FCM/SoftBlockNotification.hs` |
| Mock service | `app/mocks/fcm/src/` |

### SMS

| Component | Location |
|-----------|----------|
| Rider-app SMS | `app/rider-platform/rider-app/Main/src/Tools/SMS.hs` |
| Driver-app SMS | `app/provider-platform/dynamic-offer-driver-app/Main/src/Tools/SMS.hs` |
| Mock service | `app/mocks/sms/src/` |

### WhatsApp

| Component | Location |
|-----------|----------|
| Rider-app WhatsApp | `app/rider-platform/rider-app/Main/src/Tools/Whatsapp.hs` |
| Driver-app WhatsApp | `app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Whatsapp.hs` |

## Telephony

### Exotel

| Component | Location |
|-----------|----------|
| End ride via call | `app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/ExotelEndRide.hs` |
| Call status check | `app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Allocator/Jobs/ScheduledRides/CheckExotelCallStatusAndNotifyBAP.hs` |
| Call management | `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Call.hs` |

## Insurance

| Component | Location |
|-----------|----------|
| Rider-app insurance | `app/rider-platform/rider-app/Main/src/SharedLogic/Insurance.hs` |
| Rider-app UI | `app/rider-platform/rider-app/Main/src/Domain/Action/UI/Insurance.hs` |
| Driver-app insurance | `app/provider-platform/dynamic-offer-driver-app/Main/src/Tools/Insurance.hs` |
| Driver-app UI | `app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Insurance.hs` |

## Toll Detection

| Component | Location |
|-----------|----------|
| Toll detector | `app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/TollsDetector.hs` |
| GPS toll behavior | `app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/BehaviourManagement/GpsTollBehavior.hs` |

## Configuration

- **Dhall configs**: `dhall-configs/dev/` — service configurations including external service endpoints
- **Paseto**: Token-based API authentication for inter-service communication

## Mock Services (`app/mocks/`)

| Mock | Path | Purpose |
|------|------|---------|
| Google Maps | `app/mocks/google/src/` | Maps API mock |
| Idfy | `app/mocks/idfy/src/` | Document verification mock |
| FCM | `app/mocks/fcm/src/` | Push notification mock |
| SMS | `app/mocks/sms/src/` | SMS gateway mock |

## Related Docs

- Payment library details: `11-libraries.md`
- Ride flow (where integrations are used): `06-ride-flow.md`
- Driver onboarding: `04-driver-app.md`
