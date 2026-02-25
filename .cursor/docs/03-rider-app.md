# Rider App (BAP) Deep Dive

**Package**: `rider-app` | **Port**: 8013 | **Path**: `app/rider-platform/rider-app/`
**Schema**: `atlas_app`

The rider-app is the customer-facing service implementing the BAP (Beckn Application Platform) side of the protocol.

## Directory Structure

```
app/rider-platform/rider-app/Main/
├── spec/
│   ├── API/           # 37 YAML API specs
│   └── Storage/       # 88 YAML Storage specs
├── src/
│   ├── Domain/Action/UI/        # 81 business logic handlers
│   ├── Domain/Action/Beckn/     # Beckn callback handlers
│   ├── Domain/Types/Extra/      # Supplementary types, orphan instances
│   ├── SharedLogic/             # 82 shared logic modules
│   ├── Beckn/ACL/               # Beckn ACL translations (18 modules)
│   ├── Beckn/ACL/FRFS/          # FRFS-specific ACL (14 modules)
│   ├── Beckn/OnDemand/          # OnDemand transformers and utils
│   ├── ExternalBPP/             # External BPP integrations (CMRL, CRIS, EBIX)
│   ├── Storage/Queries/         # Extra (hand-written) queries
│   ├── Storage/CachedQueries/   # Hand-written cached queries
│   └── Tools/                   # Maps, SMS, Notifications, Whatsapp, Verification
├── src-read-only/
│   ├── API/                     # Generated Servant API types
│   ├── Domain/Types/            # Generated domain types
│   ├── Storage/Beam/            # Generated Beam ORM types
│   ├── Storage/Queries/         # Generated DB queries
│   └── Storage/CachedQueries/   # Generated cached queries
└── test/                        # Tests
```

## Key UI Action Handlers (`src/Domain/Action/UI/`)

### Core Ride Flow
| Module | Purpose |
|--------|---------|
| `Search.hs` | Ride search initiation |
| `Estimate.hs` | Fare estimates display |
| `Quote.hs` | Quote handling |
| `Select.hs` | Quote/estimate selection |
| `Confirm.hs` | Booking confirmation |
| `Booking.hs` | Booking management |
| `Cancel.hs` | Ride cancellation |
| `Ride.hs` | Active ride operations |
| `Feedback.hs` | Post-ride feedback |
| `Rating.hs` | Driver rating |

### Payment
| Module | Purpose |
|--------|---------|
| `Payment.hs` | Payment processing, Juspay webhooks |
| `RidePayment.hs` | Ride-specific payments |
| `Invoice.hs` | Invoice generation |
| `InvoiceGeneration.hs` | Invoice PDF generation |

### User Management
| Module | Purpose |
|--------|---------|
| `Registration.hs` | User registration/OTP |
| `Person.hs` | Profile management |
| `Profile.hs` | Profile operations |
| `SocialLogin.hs` | Social login (Google, etc.) |

### Safety & Tracking
| Module | Purpose |
|--------|---------|
| `Sos.hs` | SOS/emergency features |
| `FollowRide.hs` | Real-time ride sharing |
| `TrackRoute.hs` | Route tracking |

### FRFS / Public Transport
| Module | Purpose |
|--------|---------|
| `FRFSTicketService.hs` | Metro/bus ticket booking |
| `PartnerOrganizationFRFS.hs` | Partner organization FRFS |
| `MultimodalConfirm.hs` | Multimodal journey confirmation |
| `NearbyBuses.hs` | Nearby bus detection |

### Other Key Modules
| Module | Purpose |
|--------|---------|
| `Maps.hs` | Map operations, place search |
| `DriverOffer.hs` | Driver offer display |
| `FavouriteDriver.hs` | Favourite driver management |
| `EditLocation.hs` | Edit pickup/drop location |
| `HotSpot.hs` | Hotspot suggestions |
| `Insurance.hs` | Ride insurance |
| `TicketService.hs` | Ticketing (non-FRFS) |
| `Support.hs` | Customer support |
| `SavedReqLocation.hs` | Saved locations |

## SharedLogic Modules (`src/SharedLogic/`)

Key modules (82 total):

| Module | Purpose |
|--------|---------|
| `Search.hs` | Core search logic |
| `Confirm.hs` | Booking confirmation logic |
| `Booking.hs` | Booking utilities |
| `Ride.hs` | Ride utilities |
| `Cancel.hs` | Cancellation logic |
| `Payment.hs` | Payment processing |
| `Quote.hs` | Quote handling |
| `Offer.hs` | Offer processing |
| `CallBPP.hs` | Call BPP APIs |
| `CallBPPInternal.hs` | Internal BPP calls |
| `CallFRFSBPP.hs` | Call FRFS BPP APIs |
| `FRFSUtils.hs` | FRFS utilities (66KB — core FRFS logic) |
| `FRFSCancel.hs` | FRFS cancellation |
| `FRFSConfirm.hs` | FRFS confirmation |
| `FRFSStatus.hs` | FRFS status tracking |
| `FRFSFareCalculator.hs` | FRFS fare calculation |
| `Insurance.hs` | Insurance logic |
| `IntegratedBPPConfig.hs` | Integrated BPP configuration |
| `MessageBuilder.hs` | Message template building |
| `PickupETA.hs` | Pickup ETA calculation |
| `Person.hs` | Person utilities |
| `Serviceability.hs` | Area serviceability checks |
| `LocationMapping.hs` | Location mapping |
| `Merchant.hs` | Merchant utilities |

## Beckn ACL Modules (`src/Beckn/ACL/`)

BAP sends requests, receives callbacks:

| Module | Direction | Purpose |
|--------|-----------|---------|
| `Search.hs` | Outgoing | Build search request |
| `OnSearch.hs` | Incoming | Handle search results |
| `Select.hs` | Outgoing | Send selection |
| `OnSelect.hs` | Incoming | Handle selection response |
| `Init.hs` | Outgoing | Initialize booking |
| `OnInit.hs` | Incoming | Handle init response |
| `Confirm.hs` | Outgoing | Confirm booking |
| `OnConfirm.hs` | Incoming | Handle confirmation |
| `Cancel.hs` | Outgoing | Send cancellation |
| `OnCancel.hs` | Incoming | Handle cancellation response |
| `Status.hs` | Outgoing | Request status |
| `OnStatus.hs` | Incoming | Handle status response |
| `Track.hs` | Outgoing | Request tracking |
| `OnTrack.hs` | Incoming | Handle tracking response |
| `Update.hs` | Outgoing | Send update |
| `OnUpdate.hs` | Incoming | Handle update response |
| `Rating.hs` | Outgoing | Send rating |
| `Common.hs` | — | Shared ACL utilities |

## External BPP Integrations (`src/ExternalBPP/`)

Direct integrations with transit operators (non-Beckn):

| Integration | Path | Purpose |
|------------|------|---------|
| CMRL (Chennai Metro) | `ExternalBPP/ExternalAPI/Metro/CMRL/` | Auth, FareMatrix, Order, StationList, TicketStatus |
| CMRL V2 | `ExternalBPP/ExternalAPI/Metro/CMRL/V2/` | V2 API (Auth, Order, StationList, TicketStatus) |
| CRIS (Delhi Metro) | `ExternalBPP/ExternalAPI/Subway/CRIS/` | Auth, BookJourney, RouteFare, OTP, Encryption |
| EBIX (Bus) | `ExternalBPP/ExternalAPI/Bus/EBIX/` | Auth, Order, Payment, Status |
| Direct | `ExternalBPP/ExternalAPI/Direct/` | Generic direct API integration |

## Tools (`src/Tools/`)

| Module | Purpose |
|--------|---------|
| `Maps.hs` | autoComplete, getDistance, getRoutes, snapToRoad, getPlaceName |
| `SMS.hs` | SMS gateway integration |
| `Notifications.hs` | FCM push notifications |
| `Whatsapp.hs` | WhatsApp messaging |

## Related Docs

- BECKN protocol details: `05-beckn-protocol-flow.md`
- Ride lifecycle: `06-ride-flow.md`
- FRFS details: `10-frfs-public-transport.md`
- Driver app counterpart: `04-driver-app.md`
