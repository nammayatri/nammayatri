# Onboarding — Diagrams Reference

This document contains **Mermaid diagrams** for driver and document onboarding in **dynamic-offer-driver-app**. Use it alongside [ONBOARDING_GUIDE.md](./ONBOARDING_GUIDE.md). Diagrams render in GitHub, GitLab, and many Markdown viewers.

---

## 1. High-level onboarding flow

```mermaid
flowchart TB
  subgraph Client["Driver App (UI)"]
    A[Submit documents / Get status]
  end

  subgraph API["API Layer"]
    B["DriverOnboarding (v1)\n/dl, /rc, /pan, /aadhaar, /status..."]
    B2["DriverOnboardingV2\n/configs, PAN, Aadhaar, DigiLocker..."]
  end

  subgraph Handlers["Document Handlers"]
    C[DriverLicense]
    D[VehicleRegistrationCertificate]
    E[PanVerification]
    F[AadhaarVerification]
    G[GstVerification]
    H[UdyamVerification]
  end

  subgraph External["External Verification"]
    I[Idfy / HyperVerge / DigiLocker]
  end

  subgraph Webhooks["Webhooks"]
    J[IdfyWebhook]
    K[HyperVergeWebhook]
  end

  subgraph Status["Status & Enabling"]
    L["SharedLogic.DriverOnboarding.Status\n(enabled, mandatory docs)"]
  end

  subgraph Reminders["Reminder System"]
    M[Reminder.Helper\ncreate/cancel]
    N[ProcessReminder Job\nFCM, SMS, disable]
  end

  A --> B
  A --> B2
  B --> C & D & E & F & G & H
  B2 --> Handlers
  C & D & E & F & G & H --> I
  I --> J & K
  J & K --> L
  Handlers --> L
  L --> M
  M --> N
  N -.-> L
```

---

## 2. Async document verification (e.g. DL)

```mermaid
sequenceDiagram
  participant App as Driver App
  participant API as DriverOnboarding API
  participant DL as DriverLicense handler
  participant Verif as Tools.Verification
  participant DB as DB (idfy_verification, driver_license)
  participant Idfy as Idfy/HyperVerge
  participant Webhook as IdfyWebhook
  participant Status as Status handler
  participant Reminder as Reminder.Helper

  App->>API: POST /driver/register/dl
  API->>DL: verifyDL
  DL->>Verif: extractDLImage, verifyDLAsync
  Verif->>Idfy: async verify request
  Verif->>DB: INSERT idfy_verification (pending)
  DL->>App: 202 / response

  Idfy->>Webhook: callback with result
  Webhook->>DB: UPDATE driver_license, idfy_verification
  Webhook->>Reminder: createReminder (expiry)

  App->>API: GET /driver/register/status
  API->>Status: statusHandler
  Status->>DB: read DocumentVerificationConfig, doc tables
  Status->>App: StatusRes (enabled, driverDocuments, ...)
```

---

## 3. Status computation (“enabled” and document list)

```mermaid
flowchart TB
  subgraph Input["Inputs"]
    P[Person / driverId]
    MOC[merchantOperatingCityId]
    VC[vehicleCategory optional]
  end

  subgraph Config["Config (read)"]
    TC[TransporterConfig\nCAC by merchantOpCityId]
    DVC[DocumentVerificationConfig\nby merchantOpCityId + vehicleCategory + documentType]
  end

  subgraph Tables["Document tables"]
    T1[driver_license]
    T2[vehicle_registration_certificate]
    T3[aadhaar_card]
    T4[driver_pan_card]
    T5[image]
    T6[driver_rc_association]
    T7[...]
  end

  subgraph Output["Output"]
    R[StatusRes / StatusRes']
    R --> driverDocuments
    R --> vehicleDocuments
    R --> commonDocuments
    R --> enabled
    R --> manualVerificationRequired
  end

  P & MOC & VC --> statusHandler
  statusHandler --> TC & DVC
  TC & DVC --> statusHandler
  statusHandler --> T1 & T2 & T3 & T4 & T5 & T6 & T7
  statusHandler --> R
```

---

## 4. Reminder lifecycle

```mermaid
stateDiagram-v2
  [*] --> PENDING: createReminder + schedule ProcessReminder job
  PENDING --> SENT: ProcessReminder sends FCM/SMS (doc not expired)
  PENDING --> PENDING: reschedule (expired + non-mandatory)
  PENDING --> SENT: expired + mandatory → disable driver/RC, invalidate doc
  PENDING --> CANCELLED: cancelRemindersForEntity / ByDriver / ByRC
  PENDING --> COMPLETED: recordDocumentCompletion
  SENT --> [*]
  CANCELLED --> [*]
  COMPLETED --> [*]
```

---

## 5. Reminder creation vs processing

```mermaid
flowchart LR
  subgraph Create["Who creates reminders"]
    A1[DriverLicense\nafter DL verify]
    A2[VehicleRegistrationCertificate\nafter RC verify / link]
    A3[Dashboard DriverRegistration\nmany doc types]
    A4[EndRide\ncheckAndCreateReminderIfNeeded\nrides threshold]
  end

  subgraph Helper["Reminder.Helper"]
    H1[createReminder]
    H2[createRemindersWithIntervals]
    H3[scheduleProcessReminderJob]
  end

  subgraph DB["DB"]
    R[reminder]
    RC[reminder_config]
  end

  subgraph Job["Allocator"]
    J[ProcessReminder job]
  end

  subgraph Process["ProcessReminder"]
    P1[Document expiry?\nFCM+SMS; if expired+mandatory disable]
    P2[Inspection/Training?\nset approved=false; reschedule 24h]
  end

  A1 & A2 & A3 & A4 --> H1 & H2
  H1 & H2 --> R
  H3 --> J
  J --> RC
  J --> P1 & P2
  P1 & P2 --> R
```

---

## 6. Config hierarchy

```mermaid
flowchart TB
  subgraph City["Per merchantOperatingCityId"]
    TC[TransporterConfig]
    TC --- "reminderSystemEnabled"
    TC --- "aadhaarVerificationRequired"
    TC --- "onboardingTryLimit"
    TC --- "digilockerEnabled"
    TC --- "requiresOnboardingInspection"
  end

  subgraph DocType["Per documentType (+ city)"]
    DVC[DocumentVerificationConfig\nmerchantOpCityId + vehicleCategory + documentType]
    DVC --- "isMandatory, isMandatoryForEnabling"
    DVC --- "checkExtraction, checkExpiry"
    DVC --- "order, supportedVehicleClasses"
    RC[ReminderConfig\nmerchantOpCityId + documentType]
    RC --- "reminderIntervals [30,15,1]"
    RC --- "enabled, isMandatory"
    RC --- "daysThreshold, ridesThreshold"
  end

  TC --> DVC
  TC --> RC
  DVC --> Status[Status / Document handlers]
  RC --> ProcessReminder[ProcessReminder job]
  TC --> ProcessReminder
```

---

## 7. Table relationships (onboarding core)

```mermaid
erDiagram
  person ||--o{ driver_license : "has"
  person ||--o{ driver_rc_association : "has"
  person ||--o{ aadhaar_card : "has"
  person ||--o{ driver_pan_card : "has"
  person ||--o{ driver_gstin : "has"
  person ||--o{ idfy_verification : "has"
  person ||--o{ reminder : "driverId"
  person }o--|| merchant_operating_city : "belongs to"

  vehicle_registration_certificate ||--o{ driver_rc_association : "linked by"
  vehicle_registration_certificate ||--o{ vehicle_permit : "has"
  vehicle_registration_certificate ||--o{ vehicle_insurance : "has"
  vehicle_registration_certificate ||--o{ vehicle_puc : "has"
  vehicle_registration_certificate ||--o{ vehicle_fitness_certificate : "has"

  document_verification_config }o--|| merchant_operating_city : "per city"
  reminder_config }o--|| merchant_operating_city : "per city"
  transporter_config }o--|| merchant_operating_city : "per city"

  reminder }o--|| reminder_config : "uses"
  reminder ||--o{ document_reminder_history : "completion"

  image ||--o{ driver_license : "documentImageId"
  image ||--o{ vehicle_registration_certificate : "documentImageId"
  image ||--o{ idfy_verification : "documentImageId1"

  person {
    id Id
    merchant_operating_city_id Id
  }

  driver_license {
    id Id
    driver_id Id
    verification_status VerificationStatus
    document_expiry UTCTime
  }

  vehicle_registration_certificate {
    id Id
    verification_status VerificationStatus
    approved Bool
  }

  document_verification_config {
    merchant_operating_city_id Id
    vehicle_category VehicleCategory
    document_type DocumentType
    is_mandatory Bool
    check_expiry Bool
  }

  reminder {
    id Id
    document_type DocumentType
    entity_id Text
    driver_id Id
    due_date UTCTime
    status ReminderStatus
  }

  reminder_config {
    merchant_operating_city_id Id
    document_type DocumentType
    reminder_intervals "[]Int"
    enabled Bool
    is_mandatory Bool
  }
```

---

## 8. Document types and handlers (quick map)

```mermaid
flowchart LR
  subgraph Docs["Document type"]
    D1[DriverLicense]
    D2[VehicleRegistrationCertificate]
    D3[PanCard]
    D4[AadhaarCard]
    D5[GSTCertificate]
    D6[UDYAMCertificate]
  end

  subgraph Handlers["Handler module"]
    H1[DriverLicense]
    H2[VehicleRegistrationCertificate]
    H3[PanVerification]
    H4[AadhaarVerification]
    H5[GstVerification]
    H6[UdyamVerification]
  end

  D1 --> H1
  D2 --> H2
  D3 --> H3
  D4 --> H4
  D5 --> H5
  D6 --> H6
```

---

## 9. Reminder types (expiry vs inspection/training)

```mermaid
flowchart TB
  subgraph Expiry["Document expiry reminders"]
    E1[DriverLicense]
    E2[VehicleRegistrationCertificate]
    E3[VehicleInsurance]
    E4[VehiclePermit]
    E5[VehiclePUC]
    E6[VehicleFitnessCertificate]
    E7[BusinessLicense]
  end

  subgraph Inspection["Inspection / training reminders"]
    I1[VehicleInspectionForm]
    I2[DriverInspectionForm]
    I3[TrainingForm]
  end

  Expiry --> "reminderIntervals\nT-30, T-15, T-1 days"
  Inspection --> "daysThreshold / ridesThreshold\n+ reschedule every 24h"
```

---

*For narrative and file paths, see [ONBOARDING_GUIDE.md](./ONBOARDING_GUIDE.md).*
