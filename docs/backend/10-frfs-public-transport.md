# FRFS — Public Transport (Metro / Bus)

## Overview

FRFS (Fare & Retail Financial Services) handles public transport: metro, bus, and subway. Unlike ride-hailing, FRFS is **BAP-only** — BPPs are external transit operators (CMRL, CRIS, EBIX), with no driver-side component in Namma Yatri.

## Architecture

Two flow paths:
1. **ONDC (Beckn)**: Async callbacks via gateway — standard Beckn protocol
2. **Direct**: Synchronous API calls to operator APIs, no Beckn callbacks

## Key Files

### Domain Actions — `app/rider-platform/rider-app/Main/src/Domain/Action/`

| Module | Purpose |
|--------|---------|
| `UI/FRFSTicketService.hs` | UI-facing ticket booking operations |
| `UI/PartnerOrganizationFRFS.hs` | Partner organization FRFS |
| `UI/MultimodalConfirm.hs` | Multimodal journey confirmation |
| `UI/NearbyBuses.hs` | Nearby bus detection |
| `Beckn/FRFS/Common.hs` | Shared FRFS Beckn logic |
| `Beckn/FRFS/OnSearch.hs` | Handle on_search callback |
| `Beckn/FRFS/OnSelect.hs` | Handle on_select callback |
| `Beckn/FRFS/OnInit.hs` | Handle on_init callback |
| `Beckn/FRFS/OnConfirm.hs` | Handle on_confirm callback |
| `Beckn/FRFS/OnStatus.hs` | Handle on_status callback |
| `Beckn/FRFS/OnCancel.hs` | Handle on_cancel callback |
| `Beckn/FRFS/OnUpdate.hs` | Handle on_update callback |
| `Beckn/FRFS/GWLink.hs` | Gateway linking |

### FRFS Beckn ACL — `app/rider-platform/rider-app/Main/src/Beckn/ACL/FRFS/`

| Module | Purpose |
|--------|---------|
| `Search.hs` | Build Beckn search for FRFS |
| `OnSearch.hs` | Parse on_search callback |
| `Select.hs` | Build Beckn select |
| `OnSelect.hs` | Parse on_select callback |
| `Init.hs` | Build Beckn init |
| `OnInit.hs` | Parse on_init callback |
| `Confirm.hs` | Build Beckn confirm |
| `OnConfirm.hs` | Parse on_confirm callback |
| `Status.hs` | Build Beckn status |
| `OnStatus.hs` | Parse on_status callback |
| `Cancel.hs` | Build Beckn cancel |
| `OnCancel.hs` | Parse on_cancel callback |
| `OnUpdate.hs` | Parse on_update callback |
| `Utils.hs` | Context building (`buildContext`), `mkCloudBapUri` |

### SharedLogic — `app/rider-platform/rider-app/Main/src/SharedLogic/`

| Module | Purpose |
|--------|---------|
| `FRFSUtils.hs` | Core FRFS utilities (66KB — large file) |
| `FRFSCancel.hs` | Cancellation logic |
| `FRFSConfirm.hs` | Confirmation logic |
| `FRFSStatus.hs` | Status tracking |
| `FRFSFareCalculator.hs` | Fare calculation |
| `CallFRFSBPP.hs` | Call external BPP APIs |

### External BPP Integrations — `app/rider-platform/rider-app/Main/src/ExternalBPP/`

#### CMRL (Chennai Metro)
| Module | Purpose |
|--------|---------|
| `ExternalAPI/Metro/CMRL/Auth.hs` | Authentication |
| `ExternalAPI/Metro/CMRL/BusinessHour.hs` | Operating hours |
| `ExternalAPI/Metro/CMRL/DurationDetails.hs` | Duration calculations |
| `ExternalAPI/Metro/CMRL/FareMatrix.hs` | Fare information |
| `ExternalAPI/Metro/CMRL/Order.hs` | Order API |
| `ExternalAPI/Metro/CMRL/StationList.hs` | Station list |
| `ExternalAPI/Metro/CMRL/TicketStatus.hs` | Ticket status |
| `ExternalAPI/Metro/CMRL/V2/` | V2 API (Auth, Order, StationList, etc.) |

#### CRIS (Delhi Metro / Subway)
| Module | Purpose |
|--------|---------|
| `ExternalAPI/Subway/CRIS/Auth.hs` | Authentication |
| `ExternalAPI/Subway/CRIS/BookJourney.hs` | Journey booking |
| `ExternalAPI/Subway/CRIS/Encryption.hs` | AES encryption |
| `ExternalAPI/Subway/CRIS/OtpGeneration.hs` | OTP generation |
| `ExternalAPI/Subway/CRIS/RouteFare.hs` | Fare calculation |
| `ExternalAPI/Subway/CRIS/RouteFareV3.hs` | V3 fare calculation |
| `ExternalAPI/Subway/CRIS/Uts.hs` | Ticket system |

#### EBIX (Bus)
| Module | Purpose |
|--------|---------|
| `ExternalAPI/Bus/EBIX/Auth.hs` | Authentication |
| `ExternalAPI/Bus/EBIX/Order.hs` | Order creation |
| `ExternalAPI/Bus/EBIX/Payment.hs` | Payment handling |
| `ExternalAPI/Bus/EBIX/Status.hs` | Status tracking |

#### Direct API (Generic)
| Module | Purpose |
|--------|---------|
| `ExternalAPI/Direct/Order.hs` | Direct order creation |
| `ExternalAPI/Direct/Status.hs` | Direct status check |
| `ExternalAPI/Direct/Verify.hs` | Verification |
| `ExternalAPI/Direct/Utils.hs` | Utilities |

### Flow Integration Modules
| Module | Purpose |
|--------|---------|
| `ExternalBPP/Flow/Common.hs` | Common flow logic. NOTE: `bppSubscriberUrl` stores BAP URL as placeholder for direct integrations |
| `ExternalBPP/CallAPI/Types.hs` | Type aliases (`FRFSSearchFlow`, `FRFSConfirmFlow`, etc.) |

## Multi-Cloud for FRFS

- `mkCloudBapUri` in `Beckn/ACL/FRFS/Utils.hs` replaces host of `subscriberUrl` with runtime `nwAddress` for multi-cloud callback routing
- All FRFS Beckn context building goes through `buildContext` in the same file

## Constraint Propagation

When adding `HasFlowEnv` constraints:
- Prefer adding to type aliases (`BecknAPICallFlow`, `FRFSSearchFlow`, `FRFSConfirmFlow`) in `ExternalBPP/CallAPI/Types.hs` and `SharedLogic/CallFRFSBPP.hs`
- This cascades to all callers automatically

## Domain Types

| Type | Location |
|------|----------|
| `FRFSTicketBooking` | `app/rider-platform/rider-app/Main/src-read-only/Domain/Types/FRFSTicketBooking.hs` |
| `FRFSTicketBookingStatus` | `lib/beckn-spec/src/Domain/Types/FRFSTicketBookingStatus.hs` |
| `FRFSQuote` | `app/rider-platform/rider-app/Main/src-read-only/Domain/Types/FRFSQuote.hs` |
| `FRFSSearch` | `app/rider-platform/rider-app/Main/src-read-only/Domain/Types/FRFSSearch.hs` |
| `FRFSConfig` | `app/rider-platform/rider-app/Main/src-read-only/Domain/Types/FRFSConfig.hs` |
| `FRFSRouteDetails` | `app/rider-platform/rider-app/Main/src-read-only/Domain/Types/FRFSRouteDetails.hs` |

## Scheduler Jobs

| Job | Purpose |
|-----|---------|
| `SharedLogic/Scheduler/Jobs/CrisRecon.hs` | CRIS reconciliation |
| `SharedLogic/Scheduler/Jobs/CheckMultimodalConfirmFail.hs` | Multimodal confirmation failure checks |

## Related Docs

- BECKN protocol: `05-beckn-protocol-flow.md`
- Status enums: `16-status-definitions.md`
- Multi-cloud: `12-multi-cloud.md`
