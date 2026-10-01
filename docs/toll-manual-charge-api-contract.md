# Toll charge confirmation: API contract for the driver and rider apps

The feature is off unless the city's `manual_toll_charge_trip_categories` config lists the ride's trip category (`OneWay`, `CrossCity`, `InterCity`, `Rental`, `IntercityRental`, `Delivery`, `EasyBooking`, `RideShare`, `Ambulance`).

## Driver app

### 1. Ride response
`endRideRequirementsCheckRequired: boolean` on the ride object.
- `false`: call `POST /driver/ride/{rideId}/end` as today.
- `true`: call the requirements endpoint below before ending.

### 2. Requirements
`GET /driver/ride/{rideId}/end-ride/requirements?lat={lat}&lon={lon}`

`lat`/`lon` are the driver's current position, optional but should be sent together -- they stand in for the real drop point so the preview runs the same pickup/drop threshold check end-ride itself will use. Omitting them falls back to assuming the route was as expected.

```json
{
  "manualTollCharge": null
}
```
`null` means no panel is needed. End the ride as usual.

```json
{
  "manualTollCharge": {
    "tollNames": ["Airport toll"],
    "suggestedAmount": 120,
    "maxAllowedAmount": 120,
    "currency": "INR",
    "approvalMode": "CUSTOMER_APPROVAL",
    "approvalStatus": null,
    "requestedAmount": null,
    "timeoutSeconds": 180
  }
}
```
- `tollNames`: every toll on the route, not just one. `suggestedAmount`/`maxAllowedAmount` are the sum across all of them; join `tollNames` for display so the driver sees which tolls make up that total.
- `suggestedAmount`: prefill for the panel. `null` means leave it blank.
- `maxAllowedAmount`: the driver cannot enter more than this. Entering `0` is allowed and means no toll was paid.
- `approvalMode`: `DRIVER_DECLARATION_WITH_CAP` or `CUSTOMER_APPROVAL`.
- `requestedAmount`: the amount already sent for approval, so the app can resume after a restart. `null` until a request was sent.
- `approvalStatus`: only set in `CUSTOMER_APPROVAL` mode after a request was sent.
  `TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL`, `TOLL_CHARGE_APPROVED_BY_CUSTOMER`, `TOLL_CHARGE_REJECTED_BY_CUSTOMER`, `TOLL_CHARGE_AUTO_APPROVED` (rider did not answer within `timeoutSeconds`).

### 3. Declaration mode (`DRIVER_DECLARATION_WITH_CAP`)
The driver enters the amount and ends the ride:

`POST /driver/ride/{rideId}/end` with the usual body plus `"manualTollCharge": 120`.

### 4. Customer approval mode (`CUSTOMER_APPROVAL`)
1. `POST /driver/ride/{rideId}/end-ride/charge-approval` with `{ "amount": 120 }`. Returns `{"result":"Success"}`. Sending the same amount again while it is pending or approved does nothing.
2. Poll the requirements endpoint and read `approvalStatus`.
3. On `TOLL_CHARGE_APPROVED_BY_CUSTOMER` or `TOLL_CHARGE_AUTO_APPROVED`, call `POST /driver/ride/{rideId}/end` with `"manualTollCharge": 120`, the same amount that was approved.
4. On `TOLL_CHARGE_REJECTED_BY_CUSTOMER`, either send a new amount (back to step 1) or end the ride without `manualTollCharge`. The system's own toll then applies.

A declared amount of `0` skips this whole flow: step 1 resolves it immediately without waiting on or notifying the rider, so the requirements poll comes back approved on the very next check.

### Errors
| code | meaning |
|---|---|
| `MANUAL_TOLL_CHARGE_NOT_ALLOWED` | Feature not enabled for this ride, no cap available, exempt vehicle tier, or the wrong mode for the call |
| `MANUAL_TOLL_CHARGE_ABOVE_LIMIT` | Amount is below 0 or above `maxAllowedAmount` |
| `TOLL_CHARGE_APPROVAL_REQUIRED` | End ride was called with an amount the rider has not yet decided on |
| `TOLL_CHARGE_APPROVAL_REJECTED` | End ride was called with an amount the rider already rejected; declare a new amount instead |
| `TOLL_CHARGE_APPROVAL_NOT_PENDING` | Decision received with nothing pending |
| `TOLL_CHARGE_APPROVAL_REQUEST_FAILED` | The request could not be delivered to the rider. Retry. |

## Rider app

### 1. Push
Sent through the merchant push template with key `TOLL_CHARGE_APPROVAL_REQUIRED`. The payload carries `rideId`, `tollNames`, `amount`, `currency`, `approvalTimeoutSeconds`. Template variables: `{amount}`, `{tollName}` (the push text itself gets a single, comma-joined string, not the array).

### 2. Read the pending request
A pending request is delivered two ways; either is enough, and both return the same shape:

- It rides along on `tollChargeApproval` in the booking-status and ride-status responses the app already polls throughout the ride (`BookingStatusAPIEntity`, `RideAPIEntity`), so no extra call is needed if the app is already polling one of those.
- `GET /ride/{rideId}/tollChargeApproval` is a dedicated endpoint for the same data, useful right when the push opens the app or brings it to the foreground, before the next scheduled status poll.

Both return `null` when nothing is pending or the request has expired, otherwise:
```json
{
  "tollNames": ["Airport toll"],
  "amount": 120,
  "currency": "INR",
  "requestedAt": "2026-09-26T10:00:00Z",
  "expiresAt": "2026-09-26T10:03:00Z"
}
```

### 3. Decide
`POST /ride/{rideId}/tollChargeApproval` with `{ "approved": true, "amount": 120 }`.

`amount` must equal the pending request's amount. A request that has expired is rejected.

### Availability
The prompt is offered only to riders whose app bundle version is at least the city's `manualChargeApprovalMinCustomerVersion`. Older versions never receive a request, and their driver uses declaration mode.
