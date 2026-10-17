# Shared cab: driver-app architecture

Facts read from the code on branch `backend/feat/shared-cab`. Paths are relative to `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/` unless stated. The driver-app holds no session or allocation state: sessions, bookings and allocation live in rider-app (see `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/SharedCab/ARCHITECTURE.md`). The driver-app proxies driver actions to rider-app, owns one DB flag, and runs one reconciler job.

## 1. Module map

| File | Owns |
|---|---|
| `API/UI/SharedCab.hs` | Servant API and handlers for `/ui/sharedCab/{routes,route/select,session,seats,route/end,resume,trips}`, all `TokenAuth`. |
| `API/UI/SharedCabBooking.hs` | `/ui/sharedCab/booking/{bookingId}/{cancel,boardedWithoutCode,dropped}` and `/ui/sharedCab/cabFull`, all `TokenAuth`. |
| `API/Types/UI/SharedCab.hs` | Proxy DTOs (`SelectRouteReq`, `SelectRouteMode`, `SelectRouteResp`, `AffectedRider`, `SeatsReq`, `CancelBookingReq`, `EndRouteReq`, `SharedCabTrip`, `SharedCabTripsResp`, `SharedCabRoutesResp`); re-exports `Domain.Types.SharedCabSession`. |
| `Domain/Action/UI/SharedCab.hs` | Route, session, seats, end, resume, trips actions; vehicle validation; flag set/clear calls; reconciler seed hook. |
| `Domain/Action/UI/SharedCabBooking.hs` | `bookingAction` (`Cancel reason`, `BoardedWithoutCode`, `Dropped`) and `cabFull`. |
| `SharedLogic/SharedCab/RiderClient.hs` | HTTP client to rider-app `/internal/sharedCab/*`; maps rider-app errors onto `SharedCabBAPError`. |
| `SharedLogic/SharedCab/Flag.hs` | Single writer of `driver_information.shared_cab_session_active`. |
| `SharedLogic/SharedCab/ReconcilerSeed.hs` | Reconciler chain seeding, seed marker key and TTL. |
| `SharedLogic/Allocator/Jobs/SharedCab/Reconciler.hs` | The `SharedCabReconciler` allocator job. |
| `API/Internal/SharedCabAllocation.hs` + `Domain/Action/Internal/SharedCabAllocation.hs` | `POST /internal/sharedCabAllocationFCM`, called by rider-app. |
| `Tools/Notifications.hs` (`notifySharedCabAllocation`) | FCM push type `SHARED_CAB_ALLOCATION`. |
| `Tools/Error.hs` (`SharedCabBAPError`) | Pass-through error of a rider-app error. |
| `Storage/Queries/DriverInformationExtra.hs` | `updateSharedCabSessionActive`, `findSharedCabActiveDriversAfter`. |

Shared types: `Backend/lib/beckn-spec/src/Domain/Types/SharedCabSession.hs` (used by both apps) defines `SessionStatus` (ACTIVE, PAUSED, ENDED), `PauseReason` (NO_LOCATION, DRIVER_OFFLINE, ABSENT, OFF_ROUTE), `SessionMovement` (`AT_STOP {sinceMin, stopName}` / `MOVING`), `RiderStatus` (AT_STOP, MINUTES_AWAY, ARRIVING, BOARDED, UNKNOWN_STATUS), `SharedCabRoute`, `SessionRoute`, `BoardingRider`, `AlightingRider`, `RidersAtStop`, `DemandAtStop`, `LowDemandAlternative`, `LowDemandCard`, `OffRoute`, `EndRouteNext` (RETURN, CHANGE, END), and `SharedCabSession` with fields `route`, `queuedRoute`, `status`, `pauseReason`, `movement`, `capacity`, `walkupCount`, `available`, `version`, `ridersByStop`, `demandAhead`, `lowDemandCard`, `offRoute`.

## 2. Proxy to rider-app

Every driver action runs `validateSharedCabDriver` first: the person must have role DRIVER, a `Vehicle` row, and `vehicle.variant == SHARED_CAB`; otherwise `InvalidRequest` (or `DriverNotFound` / `DriverWithoutVehicle`). The driver id and vehicle number sent to rider-app come from `TokenAuth` and the `Vehicle` row (`registrationNo`), not from the client. Auth to rider-app is the `token` header carrying `appBackendBapInternal.apiKey`; base URL is `appBackendBapInternal.url` (dhall `appBackendBapInternal`).

| Driver-app endpoint (prefix `/ui`) | Handler (`Domain/Action/UI/SharedCab*.hs`) | Rider-app endpoint (prefix `/internal`) |
|---|---|---|
| `GET /sharedCab/routes?lat&lon` | `getSharedCabRoutes` | `GET /sharedCab/routes?agencyId&lat&lon` (agency = SHARED_CAB `IntegratedBPPConfig.agencyKey` for the city, APPLICATION platform) |
| `POST /sharedCab/route/select` | `selectSharedCabRoute` | `POST /sharedCab/route/select` |
| `GET /sharedCab/session` | `getSharedCabSession` | `GET /sharedCab/session?driverId&vehicleNumber`; response `Maybe SharedCabSession` |
| `POST /sharedCab/seats` | `setSharedCabSeats` | `POST /sharedCab/seats` |
| `POST /sharedCab/route/end` | `endSharedCabRoute` | `POST /sharedCab/route/end` |
| `POST /sharedCab/resume` | `resumeSharedCab` | `POST /sharedCab/resume` |
| `GET /sharedCab/trips?date` | `getSharedCabTrips` | `GET /sharedCab/trips?driverId&date` (date defaults to today in IST, `utctDay (now + 19800 s)`) |
| `POST /sharedCab/booking/{id}/cancel` | `bookingAction (Cancel reason)` | `POST /sharedCab/booking/{id}/cancel` (body carries `reason`) |
| `POST /sharedCab/booking/{id}/boardedWithoutCode` | `bookingAction BoardedWithoutCode` | `POST /sharedCab/booking/{id}/boardedWithoutCode` |
| `POST /sharedCab/booking/{id}/dropped` | `bookingAction Dropped` | `POST /sharedCab/booking/{id}/dropped` |
| `POST /sharedCab/cabFull` | `cabFull` | `POST /sharedCab/cabFull` |

Request enrichment on `route/select` (`BAPSelectRouteReq`): `agencyId`, `serviceTierType` = `"SHARED_CAB"`, `capacity` = `vehicle.capacity` or 4 (`sharedCabDefaultCapacity`), `driverName` = first + last name, `vehicleModel` = `vehicle.model`, `mode` and `walkupCount` from the request.

## 3. `driver_information.shared_cab_session_active`

- Column default `false` (`spec/Storage/DriverInformation.yaml`; `Registration.hs` creates rows with `False`).
- Single writer: `Flag.setSharedCabSessionActive` / `clearSharedCabSessionActive`; header states nothing else may call `updateSharedCabSessionActive`.
- Write path (`Flag.writeSharedCabSessionActive`): `Redis.runInMasterCloudRedisCellWithCrossAppRedis . Redis.withMasterRedis` around `QDriverInformationExtra.updateSharedCabSessionActive`, which does `updateOneWithKV` on the DB (sets the flag and `updatedAt`) and then `LTSSync.syncDriverPoolDataToLTS` with `sharedCabSessionActive = Set value` (the LTS shadow of the driver-pool data). The cross-app cell is used because any driver-app service (Main, Allocator) may write and each has its own key prefix.
- Fail-closed: `True` excludes the driver from plain-taxi dispatch. Set to `True` before the rider-app call in `selectSharedCabRoute` (`setSharedCabSessionActiveBeforeSelect`; skipped if the DB already reads `True`), so a failed call leaves the driver excluded. Cleared in `endSharedCabRoute` only when rider-app returns `null` (END); a session payload (RETURN, CHANGE) keeps it. The reconciler is the only other clearer.
- Readers: `Storage/Queries/Person/GetNearestDrivers.hs` (`guard $ not dpd.sharedCabSessionActive`, driver-pool exclusion); `SharedLogic/Ride.hs` `throwErrorSharedCabSession` (accept-time check, throws `DriverInSharedCabSession`: "Unable to offer a quote while in a shared-cab taxi-pool session").

## 4. Reconciler job (`SharedCabReconciler`, `AllocatorJobType`)

Purpose: clear a stuck flag when rider-app has no session for the driver. One self-re-enqueuing job per (merchant, city); data `SharedCabReconcilerJobData {merchantId, merchantOperatingCityId}` (`SharedLogic/Allocator.hs`); handler `runSharedCabReconcilerJob` registered in `Allocator/src/App.hs`.

| Item | Value |
|---|---|
| Cadence | `sharedCabReconcilerInterval` = 900 s (15 min), re-enqueued with `createJobIn` after each sweep |
| Per-city gate | `transporter_config.shared_cab_reconciler_enabled` (Maybe Bool, default `false`); Nothing or false means skip and end the chain |
| Order per fire | 1. refresh seed marker; 2. read `TransporterConfig`; 3. if disabled: release marker, return `Complete` (chain ends); 4. else sweep, re-enqueue |
| Sweep | keyset pages of 100 (`sharedCabReconcilerBatchSize`) over `findSharedCabActiveDriversAfter` (flag True in the city, ordered by `driverId`, resumes after the last id) |
| Per driver | load `Vehicle`; none: log error, leave flag. Else `GET /internal/sharedCab/session?driverId&vehicleNumber` on rider-app |
| Clear signal | the call fails with a body decoded as the kernel `APICallError` whose `errorCode` is `SHARED_CAB_SESSION_NOT_FOUND` or `SHARED_CAB_SESSION_HELD_BY_ANOTHER_DRIVER` (`clearSignalErrorCodes`); then `Flag.clearSharedCabSessionActive` |
| Keep flag | any 2xx (live session), bare 404/409 without decodable body, other error codes, timeout, 5xx, probe exception, no vehicle |
| Log line | `SharedCabReconciler city=... checked=... cleared=... left=...`; a sweep error is logged and the job still re-enqueues |

Seed marker and seeding (`ReconcilerSeed.hs`):

| Item | Value |
|---|---|
| Marker key | `sharedcab:reconciler:seeded:{merchantOperatingCityId}` (raw cross-app key, master cell) |
| Marker TTL | `sharedCabReconcilerSeedMarkerTtlSeconds` = 3 x 900 = 2700 s |
| Refresh | every fire, `setExp` (no NX), before the enabled check; failure logged, never kills the chain |
| Release | when the job finds the city disabled; on seed failure after the marker was set |
| `seedSharedCabReconcilerChain` | skip if a Pending `scheduler_job` row of type `SharedCabReconciler` exists for the city (`SharedCabSeedSkippedLiveRow`); else atomic `SET key true EX 2700 NX`; on OK create the job with `JC.createJobIn` (`SharedCabChainSeeded`); marker already set returns `SharedCabSeedSkippedMarker`; any Redis error reply or create failure throws `InternalError` (marker released on create failure) |
| Seed hooks | `trySeedSharedCabReconcilerChain` (fire-and-forget, failures swallowed) at the start of every `selectSharedCabRoute`; dashboard `postMerchantSchedulerTrigger` with `SharedCabReconcilerTrigger` (`Domain/Action/Dashboard/Management/Merchant.hs`; a skipped seed is returned as `InvalidRequest`) |
| Dead-chain recovery | a dead chain stops refreshing the marker; after at most 2700 s the next `route/select` seeds it again |

Dhall (`dhall-configs/dev/dynamic-offer-driver-app.dhall`): `SharedCabReconciler` is in the `AllocatorJobType` union and in `jobInfoMapx` with value `False`. That map is the scheduler's `isLongRunning` lookup (job status writes also go to the DB table when `True`); it does not enable or disable the job. Enabling is the per-city `shared_cab_reconciler_enabled` column. The dashboard merchant-config update also writes it (`Domain/Action/Dashboard/Management/Merchant.hs`, field `sharedCabReconcilerEnabled`).

## 5. Config read by driver-app

| Source | Field | Default | Use |
|---|---|---|---|
| `transporter_config` | `shared_cab_reconciler_enabled` (boolean) | `false` | reconciler gate (section 4) |
| `integrated_bpp_config` | `agencyKey` of the SHARED_CAB config (`findIntegratedBPPConfig Nothing city "SHARED_CAB" APPLICATION`) | n/a | agency id sent to rider-app |
| `vehicle` | `variant`, `capacity`, `model`, `registrationNo` | n/a | validation and request fields |
| dhall | `appBackendBapInternal` (`apiKey`, `url`) | n/a | rider-app endpoint and token |
| `merchant` | `internalApiKey` | n/a | auth of `/internal/sharedCabAllocationFCM` |

No other `shared_cab` column exists in the driver-app storage specs (`spec/Storage/Merchant.yaml`: `sharedCabReconcilerEnabled`; `spec/Storage/DriverInformation.yaml`: `sharedCabSessionActive`).

## 6. Error pass-through

- `SharedCabBAPError {fromBAPErrorCode, fromBAPErrorMessage, fromBAPErrorPayload, fromBAPHttpCode}` (`Tools/Error.hs`). `FromResponse` decodes the rider-app body as the kernel `APIError` (code, message, payload) and takes the HTTP status from the response (`codeToHttpCodeWith500Default`). `toErrorCode`, `toHttpCode`, `toMessage`, `toPayload` return those fields, so the driver app sees rider-app's `errorCode` and HTTP status unchanged (for example 409 `SHARED_CAB_SESSION_HELD_BY_ANOTHER_DRIVER`). Error type is `DOMAIN_ERROR` for Beckn.
- `RiderClient.callRider` uses `EC.callApiUnwrappingApiError (identity @SharedCabBAPError)` with tag `BAP_INTERNAL_API_ERROR` and the `internalEndPointHashMap` URL rewrite. A failure that is not a decodable APIError surfaces as `ExternalAPICallError (Just "BAP_INTERNAL_API_ERROR") ...`.
- Exception: `RiderClient.getSharedCabSession` calls `callApiExtractingApiError` itself; an APIError with code `SHARED_CAB_SESSION_NOT_FOUND` returns `Nothing`, so the driver-app `GET /ui/sharedCab/session` answers 200 with `null`. Every other APIError is rethrown unchanged.
- The reconciler uses its own decoding (`APICallError`) so that only a decoded error code, not a bare status, clears the flag.

## 7. Internal allocation FCM endpoint

`POST /internal/sharedCabAllocationFCM` (`API/Internal/SharedCabAllocation.hs`, header `token`), body `SharedCabAllocationReq {bookingId, driverId, seats, boardingCode, vehicleNumber, boardStopCode, etaSeconds}`, handler `Domain.Action.Internal.SharedCabAllocation.sharedCabAllocationFCM`.

- Auth: loads the driver (`Person`, replica read) and its merchant; the `token` must equal `merchant.internalApiKey`. An unknown driver, unknown merchant and wrong key all return `AuthBlocked "Invalid BPP internal api key"` (no driver-id probing).
- Action: `notifySharedCabAllocation` sends an FCM via `dynamicFCMNotifyPerson` with key and type `SHARED_CAB_ALLOCATION`, entity data = the request fields, template params `seats`, `boardingCode`, `vehicleNumber`, `boardStopCode`, `etaSeconds`; language = person language, default ENGLISH; returns `Success`.
- Caller: rider-app `Allocation/Claim.notifyDriverOfAllocation` (forked; failures logged only).

## Masked call to the rider and the driver number lookup

`POST /sharedCab/booking/{bookingId}/call` (`Domain.Action.UI.SharedCabCall.callRider`, TokenAuth) fetches the rider's phone from rider-app (`RiderClient.getRiderCallNumber`, which also checks the call window and that this driver owns the booking) and connects driver and rider through Exotel exactly as `initiateCallToCustomer` does for a ride. The `call_status` entity id is the booking id; `sendFCMToBAPOnFailedCallStatus` finds no ride for it and skips the BAP nudge, so the status callback just records the status. `GET /sharedCab/call/{callId}/status` polls it. rider-app calls `GET /internal/sharedCab/driver/{driverId}/callNumber` (`API.Internal.SharedCabCallNumber`, merchant `internalApiKey`) for the ExoPhone dial-in. Details: rider-app `SharedLogic/SharedCab/ARCHITECTURE.md` section 11.

`POST /sharedCab/booking/{bookingId}/message {message}` (`Domain.Action.UI.SharedCabCall.sendMessage`) is the booking-keyed twin of `POST /onMessage`: it forwards the text to rider-app (`RiderClient.postRiderMessage`), which checks the booking is on this driver's live cab and sends the rider the CHAT_MESSAGE push.
