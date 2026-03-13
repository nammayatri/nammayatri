# Reach on Time - Review Feedback Log

**Date**: 2026-03-13

This document captures all review feedback from independent reviewers and the changes made in response.

---

## PRD Review (Reviewer 1 - Product Perspective)

**Overall Rating**: 6.5/10 — "Good skeleton, needs substantial flesh"

### Critical Feedback Items

| # | Feedback | Severity | Action Taken |
|---|----------|----------|--------------|
| P1 | Missing: Women's safety / time-of-day routing concerns | High | Added to PRD Section 2.2 and 3.4 |
| P2 | Missing: Accessibility-constrained routing (wheelchair, elderly) | High | Added to PRD personas and edge cases |
| P3 | Missing: Mid-journey replanning ("I missed my bus") | High | Added user story US-7 |
| P4 | Missing: Weather/flood-aware routing (Chennai monsoons) | Medium | Added to edge cases and dependencies |
| P5 | Missing: Peak-hour crowding (bus arrival ≠ boarding) | High | Added crowding buffer to advisory |
| P6 | "Arrive By" reverse computation under-specified | High | Added OTP integration strategy to impl plan |
| P7 | 100K concurrent searches is unrealistic/ungrounded | Medium | Revised to "10x current peak load" |
| P8 | GTFS data quality is #1 risk, not "Medium" | High | Elevated to critical risk with concrete mitigation |
| P9 | ML data pipeline should start Phase 1 | Medium | Added data collection from day 1 |
| P10 | Missing: Return trip planning | Low | Added to saved trips feature |
| P11 | Missing: Group travel considerations | Low | Deferred to v2 |
| P12 | "Fallback to Google Transit" is weak mitigation | High | Added crowdsourced data + MTC partnership strategy |
| P13 | Missing: Cross-day planning (night services) | Medium | Added to edge cases |
| P14 | Missing: Bandh/strike awareness | Medium | Added service disruption alerts |
| P15 | Missing: Hard vs soft deadline distinction | Low | Added to buffer configuration |

### Positive Feedback
- "Well-structured and clearly written"
- "Correctly identifies the barrier between booking tool and trip advisor"
- "Phase 1 (Depart At) first is smart"
- "Persona table is useful"
- "Edge case table handles obvious failure modes"

---

## UI/UX Review (Reviewer 2 - Design Perspective)

*(Feedback incorporated from parallel review)*

### Key Design Improvements
- Added empty/error/loading states to all screens
- Added crowding indicators for peak hours
- Added safety indicators for late-night routes
- Improved Tamil localization with phonetic alternatives
- Added haptic feedback for time picker interactions
- Added "swap trip" button for return trips

---

## Implementation Plan Review (Reviewer 3 - Architecture Perspective)

*(Feedback incorporated from parallel review)*

### Key Technical Improvements
- Use OpenTripPlanner for "Arrive By" reverse routing instead of building from scratch
- Add rate limiting to schedule query APIs
- Add circuit breaker for GTFS server calls
- Use existing NammaDSL code generation patterns consistently
- Add data quality monitoring pipeline from Phase 1
- Reduce concurrency targets to realistic numbers
