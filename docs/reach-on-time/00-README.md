# Reach on Time - Feature Documentation

## Overview

"Reach on Time" adds time-aware journey planning to the Chennai One app, enabling three modes:
- **Leave Now** (existing, enhanced with real-time data)
- **Arrive By** — "When should I leave to reach by 9:30 AM?"
- **Depart At** — "If I leave at 4 PM, when will I arrive?"

## Documents

| Document | Purpose | Status |
|----------|---------|--------|
| [01-PRD.md](./01-PRD.md) | Product Requirements (v2, post-review) | Final |
| [02-UI-UX-Designs.md](./02-UI-UX-Designs.md) | UI/UX specifications and screen designs | Final |
| [03-Implementation-Plan.md](./03-Implementation-Plan.md) | Detailed implementation plan across all repos (v2, post-review) | Final |
| [04-Review-Feedback-Log.md](./04-Review-Feedback-Log.md) | All review feedback and actions taken | Reference |

## Affected Repositories (Implementation Order)

| # | Repository | Changes | Effort |
|---|-----------|---------|--------|
| 1 | `gtfs-inmemory-server-rust` | Time-based schedule query APIs, data quality monitoring | 6 days |
| 2 | `shared-kernel` | JourneyTimeConstraint, DepartureAdvisory, RiskLevel types | 2 days |
| 3 | `nammayatri` (rider-app) | Core search/advisory logic, saved trips, notifications, OTP integration | 12 days |
| 4 | `nammayatri` (Frontend/ui-customer) | PureScript UI components (time selector, picker, risk badges, advisor) | 10 days |
| 5 | `ny-react-native` | Native time picker bridge, push notification handlers | 5 days |
| 6 | `eta-compute` | Reverse ETA computation, traffic-aware time estimates | 4 days |
| 7 | `location-tracking-service` | Transit vehicle proximity ETA | 3 days |
| 8 | `gps-server` | Per-stop vehicle ETA cache | 3 days |
| 9 | `nandi` | Analytics dashboard for feature monitoring | 3 days |

**Total estimated effort**: ~48 engineering days (~12 weeks with 1 engineer, ~6 weeks with 2)

## Key Architectural Decisions

1. **OpenTripPlanner for "Arrive By"**: Use OTP's proven backward RAPTOR algorithm instead of building reverse routing from scratch in Haskell.
2. **Feature flag rollout**: Enable per city/merchant for staged launch.
3. **GTFS data quality first**: Start monitoring pipeline on Day 1 (Phase 0).
4. **Reuse existing patterns**: Extend JourneyLeg type class, NammaDSL specs, existing multimodal flow.
5. **Crowding-aware**: Add peak-hour boarding buffers for realistic Chennai bus ETAs.
6. **Safety-aware**: Flag late-night walking segments; suggest auto alternatives.

## Review Process

All documents went through a Maker-Reviewer loop:
1. **PRD**: Created → Independently reviewed (rated 6.5/10) → Revised with 15 feedback items
2. **UI/UX**: Created → Independently reviewed → Revised with accessibility, localization, edge state improvements
3. **Implementation Plan**: Created → Independently reviewed against existing codebase architecture → Revised with OTP integration, data quality pipeline, crowding logic
