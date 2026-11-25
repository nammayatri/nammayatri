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

#### Juspay payout: encrypted order create

Juspay payout orders go to the plain `POST /payout/merchant/v1/orders` by default. Encryption is a separate,
optional `merchant_service_config` row with service name **`JuspayPayoutEncryption`**, in addition to the
`Payout_Juspay` / `Payout_AAJuspay` row. The `Payout_Juspay` config itself is unchanged.

- **Row present** for the city: `Tools.Payout.createPayoutOrder` (rider and driver) passes the row to
  shared-kernel `Payout.createPayoutOrderWithEncryption`, which follows the four steps of Juspay's
  [encryption guide](https://juspay.io/in/docs/payout/docs/resources/encryption-guide):
  1. Stringify the order JSON.
  2. Encrypt it to Juspay's public key as a compact JWE. The defaults are `RSA-OAEP-256` + `A128CBC-HS256`,
     and the header `kid` is `juspayKeyId`.
  3. Sign the JWE with our private key as a compact JWS (`RS256`, header `kid` = `merchantKeyId`). This is on
     by default; `signRequest: false` skips it and sends the bare JWE.
  4. `POST /payout/merchant/v1.1/orders` ([encrypted API](https://juspay.io/in/docs/payout/docs/integration/order-create-encrypted))
     with `{"payload": <jws or jwe>, "kid": juspayKeyId, "sid": merchantKeyId, "format": "compact"}`.

  `UPI_ID` beneficiaries are sent the same way.
- **Row absent:** no encryption; the plain `v1` API, exactly as before.
- **Response:** Juspay answers the encrypted API with the plain order JSON (confirmed on the sandbox). Encrypted
  responses are not supported; one would fail as an undecodable response.
- Only order create is affected. Order status is still read from the plain `v1` API.
- The row is looked up with the same dimensions as the payout row: by city and merchant in the rider app, and
  by city only in the driver app. One row covers every Juspay payout of that city
  (`Payout_`, `RentalPayout_` and `RidePayout_` in the driver app).

`config_json` of the `JuspayPayoutEncryption` row:

```json
{
  "juspayPublicKey": "<passetto-encrypted PEM of Juspay's public key>",
  "juspayKeyId": "<kid issued by Juspay>",
  "merchantKeyId": "<sid issued by Juspay for our public key>",
  "merchantPrivateKey": "<passetto-encrypted PEM of the key pair whose public half was shared with Juspay>",
  "signRequest": true,
  "keyWrapAlgorithm": "RSA_OAEP_256",
  "contentEncryption": "A128CBC_HS256"
}
```

- `merchantPrivateKey` is required while `signRequest` is on (the default).
- `keyWrapAlgorithm` (`RSA_OAEP_256` | `RSA_OAEP`) and `contentEncryption` (`A128CBC_HS256` | `A256CBC_HS512` |
  `A256GCM`) are optional and default to what the guide names (`RSA_OAEP_256`, `A128CBC_HS256`). Store them with `_`
  exactly as written here; the JWE header sent to Juspay still uses the standard names (`RSA-OAEP-256`, `A128CBC-HS256`).
  Juspay's own samples disagree with the guide (`RSA-OAEP`, and `A256CBC-HS512` on the attempt API), so confirm the
  values, and whether to sign, with Juspay when the keys are exchanged.
- Delete the row to go back to the plain API. No code change or deploy is needed.

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
