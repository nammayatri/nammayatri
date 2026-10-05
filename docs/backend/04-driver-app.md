# Driver App (BPP) Deep Dive

**Package**: `dynamic-offer-driver-app` | **Port**: 8016 | **Path**: `app/provider-platform/dynamic-offer-driver-app/`
**Schema**: `atlas_driver_offer_bpp`

The driver app is the provider-facing service implementing the BPP (Beckn Provider Platform) side of the protocol.

## Directory Structure

```
app/provider-platform/dynamic-offer-driver-app/
├── Main/
│   ├── spec/
│   │   ├── API/           # 31 YAML API specs
│   │   └── Storage/       # 99 YAML Storage specs
│   ├── src/
│   │   ├── Domain/Action/UI/        # 71 business logic handlers
│   │   ├── Domain/Action/Beckn/     # Beckn protocol handlers
│   │   ├── Domain/Types/Extra/      # Supplementary types
│   │   ├── SharedLogic/             # 106 shared logic modules
│   │   ├── SharedLogic/Allocator/   # Driver allocation jobs
│   │   ├── Beckn/ACL/               # Beckn ACL translations
│   │   ├── Storage/Queries/         # Extra queries
│   │   ├── Storage/CachedQueries/   # Cached queries
│   │   ├── Storage/Cac/             # Config-as-code caching
│   │   ├── Tools/                   # Maps, SMS, Verification, Insurance
│   │   └── Lib/                     # Internal libraries
│   ├── src-read-only/               # Generated code
│   └── test/
├── Allocator/                       # driver-offer-allocator (port 9996)
│   ├── src/
│   └── server/Main.hs
└── Scheduler/                       # Background job scheduler
```

## Key UI Action Handlers (`Main/src/Domain/Action/UI/`)

### Core Ride Flow
| Module | Purpose |
|--------|---------|
| `Driver.hs` | Main driver operations (large file — ride accept, start, end) |
| `Ride.hs` | Ride management |
| `SearchRequestForDriver.hs` | Search requests received by driver |
| `EditBooking.hs` | Booking edit handling |
| `ExotelEndRide.hs` | End ride via Exotel call |

### Driver Management
| Module | Purpose |
|--------|---------|
| `Registration.hs` | Driver registration/OTP |
| `DriverOnboardingV2.hs` | Document upload, verification flow |
| `DriverProfile.hs` | Profile management |
| `DriverProfileQuestions.hs` | Profile questions |
| `DriverProfileSummary.hs` | Profile summary |
| `Person.hs` | Person details |
| `SocialLogin.hs` | Social login |
| `VehicleDetails.hs` | Vehicle information |
| `VehicleInfo.hs` | Vehicle info queries |

### Payments & Fees
| Module | Purpose |
|--------|---------|
| `Payment.hs` | Payment processing, Juspay webhooks |
| `Payout.hs` | Driver payouts |
| `ReferralPayout.hs` | Referral payout processing |
| `Plan.hs` | Subscription plan management |
| `DriverCoin.hs` | Driver coin/rewards system |
| `DriverWallet.hs` | Wallet management |
| `Penalty.hs` | Penalty handling |
| `Invoice.hs` | Invoice generation |
| `FareCalculator.hs` | Fare calculation |

### Fleet Management
| Module | Purpose |
|--------|---------|
| `FleetDriverAssociation.hs` | Fleet-driver associations |
| `FleetOwnerList.hs` | Fleet owner listing |

### Driver Experience
| Module | Purpose |
|--------|---------|
| `LeaderBoard.hs` | Driver leaderboard |
| `Performance.hs` | Performance metrics |
| `Rating.hs` | Driver ratings |
| `Message.hs` | In-app messaging |
| `LmsModule.hs` | Learning management system |
| `Reels.hs` | Video reels content |
| `DemandHotspots.hs` | Demand hotspot display |

### Other
| Module | Purpose |
|--------|---------|
| `DriverGoHomeRequest.hs` | Go-home feature |
| `DriverHomeLocation.hs` | Home location |
| `DriverReferral.hs` | Referral system |
| `Call.hs` | Call management |
| `Location.hs` | Location updates |
| `Maps.hs` | Map operations |
| `Insurance.hs` | Insurance |
| `WMB.hs` | Walk My Bus |
| `OperationHub.hs` | Operations hub |
| `Operator.hs` | Operator management |
| `StclMembership.hs` | STCL membership |
| `Tokenization.hs` | Token management |
| `MeterRide.hs` | Meter ride support |

## Allocator System (`Main/src/SharedLogic/Allocator/`)

The driver allocation engine runs as a separate service (`driver-offer-allocator` on port 9996).

### Core Allocation
| Module | Purpose |
|--------|---------|
| `Jobs/SendSearchRequestToDrivers.hs` | Main allocation orchestrator |
| `Jobs/SendSearchRequestToDrivers/Handle.hs` | Handler logic |
| `Jobs/SendSearchRequestToDrivers/Handle/Internal.hs` | Internal logic |
| `Jobs/SendSearchRequestToDrivers/Handle/Internal/DriverPool.hs` | Driver pool selection |
| `Jobs/SendSearchRequestToDrivers/Handle/Internal/DriverPoolUnified.hs` | Unified pool algorithm |
| `Jobs/SendSearchRequestToDrivers/Handle/Internal/DriverPool/Config.hs` | Pool configuration |

### Background Jobs (`Jobs/`)
| Job | Purpose |
|-----|---------|
| `Cautio/InstallationStatus.hs` | Cautio device tracking |
| `CongestionCharge/CongestionChargeAvg.hs` | Congestion charge calculation |
| `Document/VerificationRetry.hs` | Document verification retries |
| `DriverFeeUpdates/DriverFee.hs` | Driver fee calculations |
| `DriverFeeUpdates/BadDebtCalculationScheduler.hs` | Bad debt processing |
| `FCM/RunScheduledFCMS.hs` | Scheduled push notifications |
| `FCM/SoftBlockNotification.hs` | Soft block notifications |
| `FleetAlert/SendFleetAlert.hs` | Fleet operator alerts |
| `Mandate/Execution.hs` | Mandate execution |
| `Mandate/Notification.hs` | Mandate notifications |
| `Mandate/OrderAndNotificationStatusUpdate.hs` | Status updates |
| `Overlay/SendOverlay.hs` | UI overlay messages |
| `Payout/DriverReferralPayout.hs` | Referral payouts |
| `Payout/SpecialZonePayout.hs` | Special zone payouts |
| `ScheduledRides/` | Scheduled ride handling |
| `SupplyDemand/SupplyDemandRatio.hs` | Supply-demand tracking |
| `UnblockDriverUpdate/UnblockDriver.hs` | Driver unblocking |
| `Webhook/Webhook.hs` | Webhook processing |
| `SendFeedbackPN.hs` | Feedback notifications |

## Key SharedLogic Modules (`Main/src/SharedLogic/`)

| Module | Purpose |
|--------|---------|
| `Booking.hs` | Booking utilities |
| `CallBAP.hs` | Call BAP (rider) APIs |
| `CallBAPInternal.hs` | Internal BAP calls |
| `DriverPool.hs` | Driver pool management |
| `DriverFee.hs` | Driver fee logic |
| `FareCalculator.hs` | Fare computation |
| `FareProduct.hs` | Fare product configuration |
| `DynamicPricing.hs` | Surge/dynamic pricing |
| `Pricing.hs` | Pricing logic |
| `Payment.hs` | Payment processing |
| `Fleet.hs` | Fleet management |
| `SyncRide.hs` | Ride synchronization |
| `DriverCancellationPenalty.hs` | Cancellation penalty logic |
| `DriverOnboarding.hs` | Onboarding flow |
| `DriverFlowStatus.hs` | Driver status management |
| `SearchTryLocker.hs` | Concurrent search locking |
| `TollsDetector.hs` | Toll detection |
| `BlockedRouteDetector.hs` | Blocked route detection |
| `RiderDetails.hs` | Rider details for driver |
| `UserCancellationDues.hs` | Cancellation dues tracking |
| `GoogleMaps.hs` | Google Maps integration |
| `EventTracking.hs` | Event tracking |

## Beckn ACL Modules (`Main/src/Beckn/ACL/`)

BPP receives requests, sends callbacks:

| Module | Direction | Purpose |
|--------|-----------|---------|
| `Search.hs` | Incoming | Receive search request |
| `OnSearch.hs` | Outgoing | Send search results |
| `Select.hs` | Incoming | Receive selection |
| `OnSelect.hs` | Outgoing | Send selection response |
| `Init.hs` | Incoming | Receive init |
| `OnInit.hs` | Outgoing | Send init response |
| `Confirm.hs` | Incoming | Receive confirmation |
| `OnConfirm.hs` | Outgoing | Send confirmation response |
| `Cancel.hs` | Incoming | Receive cancellation |
| `OnCancel.hs` | Outgoing | Send cancellation response |
| `Status.hs` | Incoming | Receive status request |
| `OnStatus.hs` | Outgoing | Send status response |
| `Track.hs` | Incoming | Receive tracking request |
| `OnTrack.hs` | Outgoing | Send tracking response |
| `Update.hs` | Incoming | Receive update |
| `OnUpdate.hs` | Outgoing | Send update response |
| `Rating.hs` | Incoming | Receive rating |
| `Common.hs` | — | Shared ACL utilities |
| `Common/Order.hs` | — | Order building utilities |

## Tools (`Main/src/Tools/`)

| Module | Purpose |
|--------|---------|
| `Maps.hs` | Google Maps, OSRM integration |
| `SMS.hs` | SMS gateway |
| `Verification.hs` | Idfy/HyperVerge document verification |
| `Insurance.hs` | Insurance integration |

## Related Docs

- BECKN protocol: `05-beckn-protocol-flow.md`
- Ride lifecycle: `06-ride-flow.md`
- Rider app counterpart: `03-rider-app.md`
- Fare calculation: part of ride flow in `06-ride-flow.md`
