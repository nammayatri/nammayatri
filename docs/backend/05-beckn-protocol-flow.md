# BECKN Protocol Flow

## Overview

Namma Yatri implements the BECKN/ONDC protocol with two sides:
- **BAP** (Beckn Application Platform) = rider-app (customer side)
- **BPP** (Beckn Provider Platform) = dynamic-offer-driver-app (driver side)

ACL (Anti-Corruption Layer) modules translate between BECKN protocol types and internal domain types.

## Full Protocol Step Table

Each step has an ACL module on both sides. The BAP sends requests; the BPP processes and responds with callbacks.

### Ride Discovery & Booking

| Step | BAP ACL (rider-app) | BPP ACL (driver-app) | Purpose |
|------|---------------------|---------------------|---------|
| `search` | `Beckn/ACL/Search.hs` | `Beckn/ACL/Search.hs` | Customer searches for ride |
| `on_search` | `Beckn/ACL/OnSearch.hs` | `Beckn/ACL/OnSearch.hs` | BPP returns available options |
| `select` | `Beckn/ACL/Select.hs` | `Beckn/ACL/Select.hs` | Customer selects an option |
| `on_select` | `Beckn/ACL/OnSelect.hs` | `Beckn/ACL/OnSelect.hs` | BPP confirms selection |
| `init` | `Beckn/ACL/Init.hs` | `Beckn/ACL/Init.hs` | Initialize booking |
| `on_init` | `Beckn/ACL/OnInit.hs` | `Beckn/ACL/OnInit.hs` | BPP confirms initialization |
| `confirm` | `Beckn/ACL/Confirm.hs` | `Beckn/ACL/Confirm.hs` | Customer confirms booking |
| `on_confirm` | `Beckn/ACL/OnConfirm.hs` | `Beckn/ACL/OnConfirm.hs` | BPP confirms booking |

### Ride Management

| Step | BAP ACL (rider-app) | BPP ACL (driver-app) | Purpose |
|------|---------------------|---------------------|---------|
| `status` | `Beckn/ACL/Status.hs` | `Beckn/ACL/Status.hs` | Check ride status |
| `on_status` | `Beckn/ACL/OnStatus.hs` | `Beckn/ACL/OnStatus.hs` | Return current status |
| `track` | `Beckn/ACL/Track.hs` | `Beckn/ACL/Track.hs` | Request real-time tracking |
| `on_track` | `Beckn/ACL/OnTrack.hs` | `Beckn/ACL/OnTrack.hs` | Return tracking info |
| `cancel` | `Beckn/ACL/Cancel.hs` | `Beckn/ACL/Cancel.hs` | Cancel ride |
| `on_cancel` | `Beckn/ACL/OnCancel.hs` | `Beckn/ACL/OnCancel.hs` | Confirm cancellation |
| `update` | `Beckn/ACL/Update.hs` | `Beckn/ACL/Update.hs` | Update ride details |
| `on_update` | `Beckn/ACL/OnUpdate.hs` | `Beckn/ACL/OnUpdate.hs` | Confirm update |
| `rating` | `Beckn/ACL/Rating.hs` | `Beckn/ACL/Rating.hs` | Submit driver rating |

All paths are relative to:
- BAP: `app/rider-platform/rider-app/Main/src/`
- BPP: `app/provider-platform/dynamic-offer-driver-app/Main/src/`

## ACL Pattern

Each ACL module follows a consistent pattern:

1. **Outgoing (BAP → BPP)**: Convert internal domain types → BECKN protocol types
   - Function typically named `buildXxxReqV2` or similar
   - Uses types from `lib/beckn-spec/`

2. **Incoming (BPP → BAP)**: Convert BECKN protocol types → internal domain types
   - Function typically named `buildOnXxxMessage` or similar
   - Validates and extracts fields from BECKN message

## OnDemand Transformers

Additional transformation layer for the on-demand (ride-hailing) use case:

### BAP (rider-app) — `Beckn/OnDemand/`
| Module | Purpose |
|--------|---------|
| `Transformer/Init.hs` | Transform init request |
| `Transformer/OnSearch.hs` | Transform search results |
| `Transformer/Search.hs` | Transform search request |
| `Utils/Common.hs` | Common utilities |
| `Utils/Init.hs` | Init utilities |
| `Utils/OnSearch.hs` | OnSearch utilities |

### BPP (driver-app) — `Beckn/OnDemand/`
| Module | Purpose |
|--------|---------|
| `Transformer/Init.hs` | Transform init |
| `Transformer/OnSearch.hs` | Transform on_search |
| `Transformer/OnUpdate.hs` | Transform on_update |
| `Transformer/Search.hs` | Transform search |
| `Utils/Callback.hs` | Callback utilities |
| `Utils/Common.hs` | Common utilities |
| `Utils/Init.hs` | Init utilities |
| `Utils/OnSearch.hs` | OnSearch utilities |
| `Utils/OnUpdate.hs` | OnUpdate utilities |
| `Utils/Search.hs` | Search utilities |

## BECKN Tags

All BECKN tags must be defined in:
```
lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs
```
before they can be used in ACL modules.

## Domain Action Handlers for Beckn Callbacks

### BAP (rider-app) — `Domain/Action/Beckn/`
Handles incoming callbacks from BPP:
- `OnSearch.hs`, `OnSelect.hs`, `OnInit.hs`, `OnConfirm.hs`
- `OnStatus.hs`, `OnTrack.hs`, `OnCancel.hs`, `OnUpdate.hs`

### BPP (driver-app) — `Domain/Action/Beckn/`
Handles incoming requests from BAP:
- `Search.hs`, `Select.hs`, `Init.hs`, `Confirm.hs`
- `Status.hs`, `Track.hs`, `Cancel.hs`, `Update.hs`, `Rating.hs`

## IGM (Issue & Grievance Management)

Both sides have IGM ACL modules:
- BAP: `Beckn/ACL/IGM/Issue.hs`, `IssueStatus.hs`, `Utils.hs`
- BPP: `Beckn/ACL/IGM/Utils.hs`
- Shared: `lib/shared-services/src/IssueManagement/Beckn/ACL/`

## Protocol Types

BECKN protocol types are defined in:
- `lib/beckn-spec/src/BecknV2/` — V2 protocol types
- `lib/beckn-spec/src/Domain/Types/` — Shared domain types (BookingStatus, RideStatus, etc.)

## Related Docs

- Rider app details: `03-rider-app.md`
- Driver app details: `04-driver-app.md`
- Complete ride lifecycle: `06-ride-flow.md`
- FRFS protocol flow: `10-frfs-public-transport.md`
- Status enums: `16-status-definitions.md`
