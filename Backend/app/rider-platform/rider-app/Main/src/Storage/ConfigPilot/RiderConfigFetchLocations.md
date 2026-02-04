# RiderConfig fetch locations in rider-app (migration reference)

RiderConfig is fetched in two ways:

1. **Cached (with optional experiment version):** `Storage.CachedQueries.Merchant.RiderConfig`
   - `findByMerchantOperatingCityId id mbConfigInExperimentVersions` → `m (Maybe RiderConfig)`
   - `findByMerchantOperatingCityIdInRideFlow id configInExperimentVersions` → same, with `Just configInExperimentVersions`
2. **Direct Queries (no cache/experiment):** `Storage.Queries.RiderConfig`
   - `findByMerchantOperatingCityId id` → `m (Maybe RiderConfig)`

---

## 1. Tools.ConfigPilot

| Location | Function | Parameters |
|----------|----------|------------|
| `returnConfigs` (RIDER_CONFIG RiderConfig branch) | QRC.findByMerchantOperatingCityId | (cast merchantOpCityId), Just [] |
| `handleConfigDBUpdate` (RIDER_CONFIG RiderConfig branch) | SQR.findByMerchantOperatingCityId | used via normalizeMaybeFetch for update flow |

*Note: returnConfigs is already wired to `getConfig` via MonadConfigFetcher; handleConfigDBUpdate stays as-is for updates.*

---

## 2. Domain.Action.UI.MultimodalConfirm

| Location | Function | Parameters |
|----------|----------|------------|
| ~694 | QRiderConfig.findByMerchantOperatingCityId | personCityInfo.merchantOperatingCityId |
| ~858 | QRiderConfig.findByMerchantOperatingCityId | merchantOperatingCityId (Maybe request city \| person) |
| ~902 | QRiderConfig.findByMerchantOperatingCityId | merchantOperatingCityId |
| ~1464 | QRC.findByMerchantOperatingCityId | reqJourneyLeg.merchantOperatingCityId, Nothing |
| ~1932 | QRiderConfig.findByMerchantOperatingCityId | person.merchantOperatingCityId |
| ~2096 | QRiderConfig.findByMerchantOperatingCityId | journey.merchantOperatingCityId |
| ~2231 | QRiderConfig.findByMerchantOperatingCityId | personCityInfo.merchantOperatingCityId |

*Uses Queries (QRiderConfig) in most places; one CachedQueries (QRC) at 1464.*

---

## 3. Tools.Notifications

| Location | Function | Parameters |
|----------|----------|------------|
| ~307 | QRC.findByMerchantOperatingCityIdInRideFlow | person.merchantOperatingCityId, booking.configInExperimentVersions |
| ~571 | QRC.findByMerchantOperatingCityIdInRideFlow | person.merchantOperatingCityId, booking.configInExperimentVersions |
| ~1284 | QRC.findByMerchantOperatingCityIdInRideFlow | rider.merchantOperatingCityId, booking.configInExperimentVersions |

*All use CachedQueries with experiment versions (ride flow).*

---

## 4. SharedLogic.Scheduler.Jobs

| File | Location | Function | Parameters |
|------|----------|----------|------------|
| PostRideSafetyNotification | ~51 | QRC.findByMerchantOperatingCityId | person.merchantOperatingCityId, Nothing |
| Payout/MetroIncentivePayout | ~68 | QRC.findByMerchantOperatingCityId | merchantOpCityId, Nothing |
| NyRegularMaster | ~35 | RiderConfig.findByMerchantOperatingCityId | cityId, Nothing |
| CheckRefundStatus | ~81 | QRC.findByMerchantOperatingCityId | person.merchantOperatingCityId, Nothing |
| SafetyCSAlert | ~71 | QRC.findByMerchantOperatingCityId | person.merchantOperatingCityId, Nothing |
| CallPoliceApi | ~58 | QRC.findByMerchantOperatingCityIdInRideFlow | merchantOperatingCityId, booking.configInExperimentVersions |

---

## 5. SharedLogic (non-Scheduler)

| File | Location | Function | Parameters |
|------|----------|----------|------------|
| Payment | ~443 | QRC.findByMerchantOperatingCityId | merchantOperatingCityId, Nothing |
| FRFSStatus | ~245 | QRC.findByMerchantOperatingCityId | merchantOperatingCity.id, Nothing |
| PTCircuitBreaker | ~295, ~313 | QRiderConfig.findByMerchantOperatingCityId | mocId, Nothing (+ updateByPrimaryKey) |
| Offer | ~51, ~61 | QRC.findByMerchantOperatingCityId | merchantOperatingCityId, Nothing |

---

## 6. Lib.JourneyModule

| File | Location | Function | Parameters |
|------|----------|----------|------------|
| Utils | ~450 | QRiderConfig.findByMerchantOperatingCityId | mocid, Nothing |
| Utils | ~1469 | QRiderConfig.findByMerchantOperatingCityId | merchantOpCityId, Nothing |
| Types | ~1046 | QRC.findByMerchantOperatingCityId | merchantOperatingCityId, Nothing |
| Base | ~146 | QRC.findByMerchantOperatingCityId | journeyReq.merchantOperatingCityId, Nothing |
| Base | ~278 (getRiderConfig) | QRiderConfig.findByMerchantOperatingCityId | journey.merchantOperatingCityId, Nothing |
| Base | ~388 | QRiderConfig.findByMerchantOperatingCityId | merchantOperatingCityId, Nothing |
| Base | ~1038, ~1060 | QRC.findByMerchantOperatingCityId | currentLeg.merchantOperatingCityId, Nothing |
| JourneyLeg/Common/FRFS | ~121 | QRiderConfig.findByMerchantOperatingCityId | booking.merchantOperatingCityId, Nothing |
| JourneyLeg/Common/FRFSJourneyUtils | ~107 | QRiderConfig.findByMerchantOperatingCityId | merchantOperatingCityId, Nothing |

---

## 7. Domain.Action.UI

| File | Location | Function | Parameters |
|------|----------|----------|------------|
| Serviceability | ~97 | QRC.findByMerchantOperatingCityId | person.merchantOperatingCityId, Nothing |
| Select | ~235 | QRC.findByMerchantOperatingCityId | cast searchRequest.merchantOperatingCityId, Nothing |
| Search | ~360 | QRC.findByMerchantOperatingCityIdInRideFlow | merchantOperatingCityId, configVersionMap |
| Pass | ~782 | QRiderConfig.findByMerchantOperatingCityId | person.merchantOperatingCityId, Nothing |
| FRFSTicketService | ~591 | QRC.findByMerchantOperatingCityId | search.merchantOperatingCityId, Nothing |
| CallEvent | ~74 | QRC.findByMerchantOperatingCityIdInRideFlow | booking.merchantOperatingCityId, booking.configInExperimentVersions |

---

## 8. Domain.Action.Beckn.Common

| Location | Function | Parameters |
|----------|----------|------------|
| ~487 | QRC.findByMerchantOperatingCityIdInRideFlow | booking.merchantOperatingCityId, booking.configInExperimentVersions |
| ~581 | QRC.findByMerchantOperatingCityIdInRideFlow | (same) |
| ~699 | QRC.findByMerchantOperatingCityIdInRideFlow | (same) |
| ~973 | QRC.findByMerchantOperatingCityIdInRideFlow | (same) |

---

## 9. API.UI.Search

| Location | Function | Parameters |
|----------|----------|------------|
| ~221 | QRC.findByMerchantOperatingCityIdInRideFlow | dSearchRes.searchRequest.merchantOperatingCityId, configInExperimentVersions |
| ~281 | (same) | (same) |
| ~956 | (same) | (same) |

---

## 10. Domain.Action.Dashboard.Merchant

| Location | Function | Parameters |
|----------|----------|------------|
| ~528 | QRC.findByMerchantOperatingCityId | newMerchantOperatingCityId, Just [baseVersion] |
| ~530 | QRC.findByMerchantOperatingCityId | baseOperatingCityId, Just [baseVersion] |
| + create/clearCache | QRC.create, QRC.clearCache | (for new MOC clone) |

---

## 11. ExternalBPP.CallAPI.Confirm

| Location | Function | Parameters |
|----------|----------|------------|
| ~80 | QRC.findByMerchantOperatingCityId | merchantOperatingCity.id, Nothing |

---

## Migration notes

- **New ConfigPilot flow:** `getConfig` takes a dimension record (e.g. `RiderDimensions { merchantOperatingCityId, merchantId, riderCategory, deviceType }`) and returns `m TableDataResp`. So it’s for the “return configs as table data” API, not for in-app use of `RiderConfig` as a domain type.
- **In-app RiderConfig:** Call sites need the full `RiderConfig` domain type (fields like `permissibleModes`, `timeDiffFromUtc`, etc.). The new `getConfig` returns `TableDataResp` (list of JSON configs), not `RiderConfig`.
- **Migration options:**
  1. **Keep current fetches for in-app use** – Continue using `QRC.findByMerchantOperatingCityId` / `findByMerchantOperatingCityIdInRideFlow` and `QRiderConfig.findByMerchantOperatingCityId` wherever you need `RiderConfig`; use `getConfig` only for ConfigPilot/table-data APIs.
  2. **Unify via a helper** – If you want all “get RiderConfig for this MOC” to go through one layer, add e.g. `getRiderConfigForMoc :: RiderDimensions -> m (Maybe RiderConfig)` that builds dimensions from MOC, calls the existing cache/query internally (not `getConfig`), and returns the domain type.
  3. **Experiment-aware flow** – Call sites that use `findByMerchantOperatingCityIdInRideFlow id configInExperimentVersions` depend on experiment version; the new ConfigPilot dimensions don’t carry that yet. Either keep those as CachedQueries or extend dimension types (e.g. optional `configVersionMap`) and fetcher contract where you need experiments.

**Summary:** 30+ call sites; most use **QRC** (CachedQueries) with `Nothing` or `Just configVersionMap` / `booking.configInExperimentVersions`; a few use **QRiderConfig** (Queries) for direct DB. Use this list to decide which calls stay as-is, which move to a shared helper, and which (if any) should go through ConfigPilot’s `getConfig` for table-data only.
