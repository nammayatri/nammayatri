---
name: WS13 Backend Plan
overview: "Backend implementation plan for SmartRide WS13: extend SOS options, add driver-home VAS banners, capture/export driver GMC/GPA insurance, and expose ops-ready command-centre APIs by extending existing dashboard Ride surfaces."
todos:
  - id: sos-extend
    content: Extend SosType/RiderConfig; remove hardcoded 112; wire Ambulance/Fire + safety-team notify
    status: pending
  - id: vas-banners
    content: Add vas_banner_config storage + driver GET + dashboard CRUD
    status: pending
  - id: gmc-gpa-capture
    content: Add driver_group_insurance table + APIs; reuse identity nominee defaults where useful
    status: pending
  - id: insurance-export-job
    content: Add Allocator DriverGroupInsuranceExport job (TDS email-attachment pattern); config from/to emails
    status: pending
  - id: cc-rideinfo-mvp
    content: Extend RideInfoRes + ride search filters for anomaly/SOS flags; reuse rideRoute/rideInfo
    status: pending
  - id: cc-anomaly-followup
    content: "Follow-up: persist/query violation-stop events and/or CH/LTS live view"
    status: pending
isProject: false
---

# WS13 Backend: CRM, Command Centre, VAS & Insurance

Scope is **backend only** (nammayatri driver/rider apps + dashboard CommonAPIs). Control-centre / any frontend UI lives in a separate repo and is **out of scope for this doc** for now (revisit later).

## Decisions locked in

- **SOS**: extend `SosType` + city config for numbers/labels; remove hardcoded `"112 called"`.
- **VAS**: new `vas_banner_config` table + driver GET API + **dashboard CRUD in MVP**; banner click = **Option A (client deep-link)** until product explicitly asks for Option B (server-side WhatsApp via `Tools/Whatsapp.hs`).
- **GMC/GPA**: new table `driver_group_insurance` (not stuffing all fields into `driver_identity_info`); export job mirrors **TDS** (batch + `Email.sendEmailWithAttachment` + self-reschedule), optionally with SAP-style day lock/idempotency.
- **Command centre**: no new Haskell service — extend provider dashboard Ride APIs; MVP uses Postgres ride flags + existing `rideInfo`/`rideRoute`; ops `timeline` is **backend-computed** from ride timestamps on `RideInfoRes` (not `/flowDebug`); live LTS/CH anomaly stream is a follow-up.

| Reuse | Net-new |
|-------|---------|
| SOS + Kapture | Ambulance / Fire flows |
| rideInfo / rideRoute | RideInfoRes + search filters + timeline |
| Email.sendEmailWithAttachment | Daily insurer export job (fed by GMC/GPA rows) |
| Rider on_search insurance | *(unchanged)* |
| | `driver_group_insurance` table + CRUD |
| | VAS banner table + driver GET + dashboard CRUD |

## 1. SOS — Ambulance / Fire + config-driven labels

**Steps**

1. Extend `SosType` in Safety [`shared-services spec/Safety/Storage/Sos.yaml`](Backend/lib/shared-services/spec/Safety/Storage/Sos.yaml), add `Ambulance`,`Fire`:

```yaml
    SosType:
      enum: "Police,CustomerCare,EmergencyContact EmergencyContactId,SafetyFlow,CSAlertSosTicket,AudioRecording,KaptureDashboard,Ambulance,Fire"
```

2. Extend [`rider-app spec/Storage/RiderConfig.yaml`](Backend/app/rider-platform/rider-app/Main/spec/Storage/RiderConfig.yaml) — option list / numbers / safety-team channel (fields only; JSON via existing `externalSOSConfig` pattern or sibling field):

```yaml
    # types: (under RiderConfig types)
    SosOptionKind:
      enum: "Police,Ambulance,Fire,CustomerCare,SafetyTeam"
    SosOptionConfig:
      kind: SosOptionKind
      label: Text              # e.g. "112 called", "Ambulance 108", "Fire 101"
      phoneNumber: Maybe Text  # dial number when client/IVR needs it
      enabled: Bool
    SafetyTeamNotifyChannel:
      enum: "Kapture,Sms,Email,Webhook,None"
    SafetyTeamNotifyConfig:
      channel: SafetyTeamNotifyChannel
      target: Maybe Text       # queue / phone / email / webhook URL

    # fields: (on rider_config)
    sosOptions: Maybe [SosOptionConfig]
    safetyTeamNotify: Maybe SafetyTeamNotifyConfig
```

   Optionally also extend `ExternalSOSFlow` if a new external provider is required:

```yaml
    ExternalSOSFlow:
      enum: "ERSS,GJ112,Trinity"   # add new values only if MSIL needs a new provider
```

3. Wire dispatch in [`rider-app Domain/Action/UI/Sos.hs`](Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Sos.hs) for `Ambulance` / `Fire` (ticket and/or external flow / safety-team notify from config). Rider [`rider-app spec/API/sos.yaml`](Backend/app/rider-platform/rider-app/Main/spec/API/sos.yaml) `TriggerApi` stays as-is unless a new trigger kind is required:

```yaml
  TriggerApi:
    enum: "POLICE,KAPTURE,SUPPORT_TICKET"   # extend only if needed
```

4. Replace hardcoded label in [`rider-app SharedLogic/Scheduler/Jobs/SafetyCSAlert.hs`](Backend/app/rider-platform/rider-app/Main/src/SharedLogic/Scheduler/Jobs/SafetyCSAlert.hs) (~`Police -> "112 called"`) — resolve `label` from `sosOptions` by `SosOptionKind` / `SosType` (fallback to current string if config missing). Same if driver SOS hardcodes it.

5. Keep dashboard Management [`dashboard spec/RiderPlatform/Management/API/Sos.yaml`](Backend/app/dashboard/CommonAPIs/spec/RiderPlatform/Management/API/Sos.yaml) `SosType` in sync (it duplicates the Safety enum today):

```yaml
  SosType:
    - enum: Police, CustomerCare, SafetyFlow, CSAlertSosTicket, AudioRecording, KaptureDashboard, Ambulance, Fire
```

6. Leave IGM (`shared-services/IssueManagement`) untouched unless an explicit SOS→issue link is required (no spec change).

## 2. VAS — banner config + driver-home surface

**Steps**

1. New storage YAML [`driver-app spec/Storage/VasBannerConfig.yaml`](Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/VasBannerConfig.yaml) → table `vas_banner_config`:

```yaml
VasBannerConfig:
  tableName: vas_banner_config

  types:
    VasBannerLinkType:
      enum: "Service,SmartFinance,WhatsApp,ExternalUrl,Other"

  fields:
    id: Id VasBannerConfig
    merchantId: Id Merchant
    merchantOperatingCityId: Id MerchantOperatingCity
    title: Text
    subtitle: Maybe Text
    imageUrl: Text
    deepLink: Maybe Text
    whatsappTemplateId: Maybe Text
    linkType: VasBannerLinkType
    priority: Int
    enabled: Bool
    validFrom: Maybe UTCTime
    validTo: Maybe UTCTime
```

2. Queries on `VasBannerConfig` (in the same storage YAML):

```yaml
  queries:
    findAllEnabledByCity:
      kvFunction: findAllWithOptionsKV
      where:
        and: [merchantOperatingCityId, enabled]
      orderBy: priority
```

3. Driver UI API [`driver-app spec/API/VasBanner.yaml`](Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/API/VasBanner.yaml):

```yaml
module: VasBanner
types:
  VasBannerListRes:
    banners: "[VasBannerConfig]"   # or a DTO mirroring storage fields

apis:
  - GET:
      endpoint: /vasBanner/list
      auth: TokenAuth PROVIDER_TYPE
      response:
        type: VasBannerListRes
```

4. Dashboard CRUD [`dashboard CommonAPIs Management/API/VasBanner.yaml`](Backend/app/dashboard/CommonAPIs/spec/ProviderPlatform/Management/API/VasBanner.yaml) — **in MVP**:

```yaml
module: VasBanner
apis:
  - GET:
      endpoint: /list
      auth: ApiAuthV2
      query:
        enabled: Bool
      response:
        type: VasBannerListRes
  - POST:
      endpoint: /create
      auth: ApiAuthV2
      request:
        type: VasBannerCreateReq
      response:
        type: VasBannerConfig
  - POST:
      endpoint: /{bannerId}/update
      auth: ApiAuthV2
      params:
        bannerId: Id VasBannerConfig
      request:
        type: VasBannerUpdateReq
      response:
        type: VasBannerConfig
  - POST:
      endpoint: /{bannerId}/delete
      auth: ApiAuthV2
      params:
        bannerId: Id VasBannerConfig
      response:
        type: APISuccess
```
5. Banner click / WhatsApp — two options; **we choose Option A** until product explicitly asks otherwise.

   **Option A (chosen): client deep-link**
   - Banner row already has `deepLink` / `whatsappTemplateId` / `linkType`.
   - `GET` banners returns these fields unchanged.
   - Driver app opens the URL / `wa.me` / in-app screen on tap.
   - **Reuse:** nothing from WhatsApp stack.
   - **Add:** nothing beyond table + GET (no call to `Tools/Whatsapp.hs`).

   **Option B (deferred): server-side WhatsApp send**
   - Add a thin action (e.g. `POST .../vasBanner/{id}/sendWhatsapp`): resolve driver phone + banner `whatsappTemplateId` (+ template vars / URL button if needed).
   - Call existing `whatsAppSendMessageWithTemplateIdAPI` from [`driver-app Tools/Whatsapp.hs`](Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Tools/Whatsapp.hs) — same pattern as onboarding / payment link send.
   - **Reuse:** `Tools/Whatsapp.hs` as-is + existing merchant WhatsApp `MerchantServiceConfig` (do not edit the WhatsApp module).
   - **Add:** UI/dashboard glue handler + endpoint only.
   - Out of scope unless product explicitly switches from A to B.

## 3. Driver GMC / GPA capture

New storage YAML (e.g. `DriverGroupInsurance.yaml`) → table `driver_group_insurance` (preferred over extending [`driver-app spec/Storage/DriverIdentityInfo.yaml`](Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/DriverIdentityInfo.yaml) alone). Fields only; nominee values may default from existing `nominee*` on identity info in application code:

```yaml
DriverGroupInsurance:
  tableName: driver_group_insurance

  types:
    DriverGroupInsuranceType:
      enum: "GMC,GPA"
    DriverGroupInsuranceStatus:
      enum: "Draft,Verified,Enabled,Exported"
    DriverGroupInsuranceGender:
      enum: "Male,Female,Other"

  fields:
    id: Id DriverGroupInsurance
    driverId: Id Person
    merchantId: Id Merchant
    merchantOperatingCityId: Id MerchantOperatingCity
    insuranceType: DriverGroupInsuranceType
    fullName: Text
    mobile: Text
    dob: Maybe Day
    age: Maybe Int
    gender: Maybe DriverGroupInsuranceGender
    nomineeName: Maybe Text
    nomineeRelationship: Maybe Text
    nomineeDob: Maybe Day
    secondBotCheckAt: Maybe UTCTime
    enabledAt: Maybe UTCTime
    status: DriverGroupInsuranceStatus
    lastExportedAt: Maybe UTCTime
```

**Steps**

1. Storage YAML queries / Extra (on `DriverGroupInsurance` or Extra file):

```yaml
  queries:
    findByDriverIdAndType:
      kvFunction: findOneWithKV
      where:
        and: [driverId, insuranceType]
    findAllByDriverId:
      kvFunction: findAllWithKV
      where: driverId
    updateStatus:
      kvFunction: updateOneWithKV
      params: [status]
      where: id
    updateSecondBotCheckAt:
      kvFunction: updateOneWithKV
      params: [secondBotCheckAt]
      where: id
    updateEnabledAtAndStatus:
      kvFunction: updateOneWithKV
      params: [enabledAt, status]
      where: id
  # Extra (hand-written) e.g.:
  # findPendingExport :: limit -> [DriverGroupInsurance]  -- status in {Enabled} / pending export
  # markExported :: id -> lastExportedAt -> status=Exported
```

2. Driver and/or dashboard APIs (open: which surface — see Open questions). Sketch:

```yaml
# Driver UI — e.g. spec/API/DriverGroupInsurance.yaml
module: DriverGroupInsurance
types:
  DriverGroupInsuranceUpsertReq:
    insuranceType: DriverGroupInsuranceType
    fullName: Text
    mobile: Text
    dob: Maybe Day
    age: Maybe Int
    gender: Maybe DriverGroupInsuranceGender
    nomineeName: Maybe Text
    nomineeRelationship: Maybe Text
    nomineeDob: Maybe Day
  DriverGroupInsuranceRes:
    insurance: DriverGroupInsurance

apis:
  - GET:
      endpoint: /driverGroupInsurance
      auth: TokenAuth PROVIDER_TYPE
      query:
        insuranceType: DriverGroupInsuranceType
      response:
        type: DriverGroupInsuranceRes
  - POST:
      endpoint: /driverGroupInsurance
      auth: TokenAuth PROVIDER_TYPE
      request:
        type: DriverGroupInsuranceUpsertReq
      response:
        type: DriverGroupInsuranceRes

# Dashboard / ops — e.g. Management API (optional / if needed)
# POST /{id}/secondBotCheck  -> sets secondBotCheckAt
# POST /{id}/enable          -> sets enabledAt, status=Enabled
```

3. **Do not change** rider [`rider-app spec/Storage/Insurance.yaml`](Backend/app/rider-platform/rider-app/Main/spec/Storage/Insurance.yaml) / [`rider-app Domain/Action/Beckn/OnSearch.hs`](Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/OnSearch.hs) insurance path.

**Export job** (TDS-shaped)

1. Add `AllocatorJobType` e.g. `DriverGroupInsuranceExport` + job data in [`driver-app SharedLogic/Allocator.hs`](Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Allocator.hs). Register in [`driver-app Allocator/App.hs`](Backend/app/provider-platform/dynamic-offer-driver-app/Allocator/src/App.hs).
2. Handler under `SharedLogic/Allocator/Jobs/Insurance/`: select Enabled/pending rows → build file from template → `Email.sendEmailWithAttachment` → mark exported → `createJobIn` (+1 day). Pattern reference: [`driver-app SharedLogic/Allocator/Jobs/TDSDistribution/ScheduledTDSDistribution.hs`](Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Allocator/Jobs/TDSDistribution/ScheduledTDSDistribution.hs); optional Redis day-idempotency from SAP dispatch.
3. Config: from/to emails on TransporterConfig (same idea as `tdsFromEmail`).
4. Migration for template / from-to config once MSIL provides the insurer template.

## 4. Command centre APIs

**MVP (ship first)**

1. Extend provider [`dashboard spec/ProviderPlatform/Management/API/Ride.yaml`](Backend/app/dashboard/CommonAPIs/spec/ProviderPlatform/Management/API/Ride.yaml). Today only deviation is exposed:

```yaml
  RideInfoRes:
    # ... existing fields ...
    # ADD:
    - safetyAlertTriggered: Bool
    - numberOfDeviation: Maybe Bool
    - timeline: [RideTimelineEvent]   # computed by backend (see below)
    - rideSosId: Maybe (Id Sos)       # optional (if needed); mirror rider RideInfoRes
    - sosStatus: Maybe SosStatus      # optional (if needed); SosStatus = Resolved|NotResolved|Pending|...

  RideTimelineEvent:
    - stage: RideTimelineStage
    - at: UTCTime

  RideTimelineStage:
    - enum: "Booked, DriverArrived, Started, Ended, Cancelled"
```

Source columns already on ride storage ([`driver-app spec/Storage/ride.yaml`](Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/ride.yaml)):

```yaml
    driverDeviatedFromRoute: Maybe Bool
    numberOfDeviation: Maybe Bool
    safetyAlertTriggered: Bool
```

**Ops timeline (backend-computed):** build `timeline` in [`driver-app Domain/Action/Dashboard/Ride.hs`](Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Dashboard/Ride.hs) from existing `RideInfoRes` timestamps (`rideBookingTime`, arrival times, `rideStartTime` / `rideEndTime`, `cancelledTime`). Do **not** use `/flowDebug` (`RideFlowDebug`) — that API is beckn funnel diagnostics (search/try/estimate/quote), not for command-centre UI.

2. Fill the new anomaly fields + compute `timeline` in [`driver-app Domain/Action/Dashboard/Ride.hs`](Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Dashboard/Ride.hs) from the ride row.

3. Extend ride list/search filters. Existing `/list` query (excerpt):

```yaml
  - GET: # RideListAPI
      endpoint: /list
      query:
        # ... existing params (incl. paymentMode: PaymentMode = CASH|ONLINE) ...
        # ADD e.g.:
        driverDeviatedFromRoute: Bool
        safetyAlertTriggered: Bool
        fleetOwnerId: Text                      # optional (if needed)
        paymentCollectedBy: PaymentCollectedBy  # optional (if needed); Beckn enum BAP|BPP — who collects payment (not paymentMode)
```

**Follow-up (after MVP)**

- Persist or query anomaly events for a live monitoring view (`ViolationDetection` / `StopDetection` mostly log today).
- Live location via LTS or short CH GPS tail — not Beam/Postgres.

## Delivery order

1. SOS enums/config + wiring
2. VAS table + driver GET + dashboard CRUD (specs → generate → thin handlers)
3. GMC/GPA table + CRUD
4. Insurance export job (stub file OK; align columns + TransporterConfig from/to when MSIL answers)
5. Command-centre `RideInfoRes` + search filters (MVP)
6. Optional alert-event / CH live view

## Dependencies / risks

- WS9 ride data quality for command centre; WS5 reporting overlap for export-like jobs.
- Anomaly telemetry outside Postgres (ClickHouse + LTS).
- Insurer column layout + from/to emails are MSIL-owned: blocks production config / final file format, **not** writing the export job or capture APIs (stub CSV + later config migration is fine).

## Open questions

1. ~~**VAS dashboard CRUD**~~ — **resolved: include in MVP** (specs 1–4 above).
2. **GMC/GPA write APIs** — driver app, dashboard/ops, or both.
3. **MSIL safety-team notify channel** on SOS — Kapture ticket, SMS, email, or webhook (needs MSIL/product input).
4. **SOS → IGM link** — keep IGM untouched, or create/link an issue ticket when SOS fires.
5. **Insurer export file layout + from/to emails** — pending MSIL; blocks production config / final column mapping (and can defer TransporterConfig migration), **not** implementing the job/APIs with a stub.
6. **Export job Redis day-lock / idempotency** — add SAP-style day idempotency, or rely on row `status` / `lastExportedAt` only (TDS-like).
7. **Command-centre live anomalies (follow-up)** — persist detector events to Postgres vs query ClickHouse/LTS (MVP ships without this).
