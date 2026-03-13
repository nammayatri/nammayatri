# Reach on Time - Product Requirements Document (PRD)

**Version**: 2.0 (Post-Review Revision)
**Date**: 2026-03-13
**Product**: Chennai One App (Nammayatri Platform)
**Author**: Engineering Team
**Status**: Ready for Implementation

---

## 1. Executive Summary

"Reach on Time" is a time-aware journey planning feature for the Chennai One app that answers the commuter's core question: **"When should I leave to reach my destination on time?"**

Unlike the current flow which only supports "Leave Now," this feature adds **"Arrive By"** and **"Depart At"** modes — mirroring the paradigm proven by Google Maps and Citymapper but deeply integrated with Chennai's multimodal transit network (MTC buses, CMRL metro, suburban rail, autos/cabs).

### Why This Matters

Chennai commuters using the current app can only plan trips for immediate departure. This fails critical real-world scenarios:
- **Morning commute**: "I need to reach office by 9:30 AM — when should I leave?"
- **Airport/station catch**: "My train departs at 6:15 PM from Central — what's the latest I can leave?"
- **Event planning**: "I want to leave at 4:00 PM — when will I arrive?"

The feature transforms Chennai One from a **reactive booking tool** into a **proactive trip advisor**.

---

## 2. User Personas & Scenarios

### 2.1 Primary Personas

| Persona | Description | Key Need |
|---------|-------------|----------|
| **Daily Commuter** | Office-goer using bus+metro combo daily | "Arrive By 9:30 AM" with buffer |
| **Occasional Traveler** | Uses app for airport/station trips | "Arrive By departure time - 30min" |
| **Student** | College commuter, cost-sensitive | "Cheapest route to reach by 8:45 AM" |
| **First-time User** | Tourist or new resident | "What are my options to reach X by Y?" |
| **Women Commuter** | Safety-conscious, avoids certain routes/modes after dark | Safe route that arrives before 9 PM |
| **Elderly / Mobility-limited** | Cannot walk long distances or climb footbridges | Accessible route with fewer walking segments |

### 2.2 User Stories

**US-1: Arrive By (Primary)**
> As a commuter, I want to set my destination and desired arrival time so the app tells me when to leave and which routes to take.

**US-2: Depart At**
> As a commuter, I want to set a departure time in the future so I can see what routes/services are available at that time and when I'll arrive.

**US-3: Leave Now (Existing, Enhanced)**
> As a commuter, I want to search for routes now with real-time departure information, enhanced with live ETA and next-bus/next-train times.

**US-4: Smart Notifications**
> As a commuter who has planned a future trip, I want to receive a "Time to leave" push notification at the right moment, accounting for walking time to the first stop.

**US-5: Recurrent Trips**
> As a daily commuter, I want to save my regular trips (e.g., "Reach office by 9:30 AM on weekdays") so the app automatically reminds me when to leave each day.

**US-6: Buffer Time Management**
> As a risk-averse commuter, I want to add a comfort buffer (5/10/15 min) to my "Arrive By" time so I have margin for delays.

**US-7: Mid-Journey Replanning** *(Added per review)*
> As a commuter who missed a bus connection, I want the app to instantly replan my remaining journey so I can still arrive as close to my target time as possible.

**US-8: Safe Late-Night Routing** *(Added per review)*
> As a woman commuter planning a late evening trip, I want the app to prefer well-lit, high-traffic routes and show safety indicators for walking segments after dark.

**US-9: Return Trip** *(Added per review)*
> As a commuter who planned a trip to office, I want to easily plan the return trip home with one tap.

**US-10: Crowding-Aware Planning** *(Added per review)*
> As a peak-hour commuter, I want the app to factor in bus crowding (when 2-3 buses may pass before I can board) so the departure time accounts for wait-to-board time, not just bus arrival time.

---

## 3. Feature Specification

### 3.1 Time Modes

| Mode | Input | Output | Use Case |
|------|-------|--------|----------|
| **Leave Now** | Current time (implicit) | Arrival time for each route option | Immediate travel |
| **Arrive By** | Target arrival time | Required departure time + route options | Meeting/flight deadlines |
| **Depart At** | Planned departure time | Estimated arrival time + route options | Flexible future planning |

### 3.2 Core Capabilities

#### 3.2.1 Time-Aware Route Search
- Accept origin, destination, and time mode (leave now / arrive by / depart at)
- Query GTFS schedules for transit services operating at the relevant time
- For "Arrive By": work backward from arrival time to compute latest safe departure
- For "Depart At": compute earliest arrival given departure constraint
- For "Leave Now": show next available departures with real-time data overlay

#### 3.2.2 Schedule-Aware Multimodal Planning
- Combine scheduled transit (bus timetable, metro frequency) with on-demand modes (auto, taxi)
- Handle transit mode transfers with realistic walking times between stops
- Account for service frequency variations (peak vs off-peak, weekday vs weekend)
- Show "last service" warnings when planning late-evening trips

#### 3.2.3 Smart Departure Advisor
- For "Arrive By" mode, compute the **recommended departure window**:
  - `Latest Departure`: Last moment to leave and still arrive on time
  - `Recommended Departure`: Latest departure + user's configured buffer
  - `Comfortable Departure`: With generous buffer for walking + wait times
- Show risk level: "Tight", "Good", "Comfortable" based on buffer analysis

#### 3.2.4 Real-Time Adjustments
- Overlay real-time bus/metro positions on scheduled times
- Adjust ETAs based on live traffic (for auto/taxi legs)
- Show "delayed" / "on time" / "early" indicators for transit legs
- Re-compute route if a scheduled service is cancelled or significantly delayed

#### 3.2.5 Notifications & Reminders
- "Time to Leave" notification at computed departure time
- "Your bus/metro departs in X minutes" at nearby stop
- "Upcoming transfer" reminder during journey
- Configurable per-trip notification preferences

#### 3.2.6 Saved & Recurrent Trips
- Save a trip with time preference (e.g., "Reach T.Nagar by 6:00 PM on Fridays")
- Daily/weekly recurring trip scheduling
- Auto-compute departure time each day based on current schedule
- One-tap "Plan today's trip" from saved trips

#### 3.2.7 Mid-Journey Replanning *(Added per review)*
- When a user misses a connection, offer "Replan Now" button
- Re-compute remaining legs from current location to destination
- Preserve original time constraint ("still try to arrive by 9:30 AM")
- Show updated risk level after replanning

#### 3.2.8 Crowding-Aware Scheduling *(Added per review)*
- For peak-hour bus routes, add a "crowding buffer" (5-15 min)
- Crowding buffer accounts for the reality that buses may pass full
- Use historical crowding data per route/time when available
- Show "Usually crowded" indicator on peak-hour bus legs

#### 3.2.9 Safety-Aware Late-Night Routing *(Added per review)*
- For trips arriving/departing after 9 PM, flag walking segments > 300m
- Suggest auto/taxi for late-night walking legs
- Show "well-lit route" / "busy area" indicators where data available
- Allow users to set "avoid walking after dark" preference

### 3.3 Route Ranking Algorithm

Routes are ranked by a composite score considering:

```
score = w1 * time_reliability + w2 * total_duration + w3 * cost + w4 * transfers + w5 * walking_distance + w6 * comfort + w7 * safety_score + w8 * crowding_score
```

Where weights adapt based on user mode:
- **Arrive By**: Heavy weight on `time_reliability` (can you actually make it?)
- **Leave Now**: Heavy weight on `total_duration` (fastest option)
- **Depart At**: Balanced across all factors
- **Late Night**: Elevated `safety_score` weight
- **Peak Hour**: Elevated `crowding_score` weight

### 3.4 Edge Cases

| Scenario | Handling |
|----------|----------|
| No transit service at requested time | Show taxi/auto-only options with warning |
| Arrival time is in the past | Show error: "Arrival time must be in the future" |
| Arrival time < minimum possible travel time | Show warning: "Not enough time. Earliest arrival is X" |
| GTFS data unavailable | Fallback to Google Transit API or show auto/taxi-only |
| Last bus/metro has departed | Show "No transit available" + auto/taxi alternatives |
| Recurring trip on a holiday | Check holiday calendar, adjust or skip notification |
| Transit strike/bandh announced | Show service disruption alert; offer auto/taxi alternatives |
| Peak-hour bus crowding | Add 5-15 min "crowding buffer" for boarding at busy stops |
| Late-night walking segment (after 9 PM) | Flag safety concern; suggest auto for walking legs > 300m |
| Cross-day planning (arrive by 2 AM) | Check night service availability; warn if no transit |
| User already past latest departure time | Show "You can no longer make it on time" + closest alternatives |
| Origin/destination far from any transit stop | Auto-include auto/walk first/last mile; show total cost |
| Monsoon flooding on route | Weather-aware alert; suggest alternate routes avoiding flood zones |
| Hard deadline (flight) vs soft deadline (dinner) | Offer "Critical" mode with extra buffer for hard deadlines |
| Wheelchair/accessibility route needed | Filter routes for step-free access; avoid footbridges |

---

## 4. Non-Functional Requirements

### 4.1 Performance
- Route search response: **< 2 seconds** for 95th percentile
- Time-based schedule query: **< 500ms** at GTFS server
- Notification delivery: Within **30 seconds** of computed departure time

### 4.2 Accuracy
- ETA prediction accuracy: Within **5 minutes** for 80% of trips
- Walking time estimation: Within **2 minutes** for distances < 1km
- Transit schedule accuracy: Matches published GTFS to **< 1 minute**

### 4.3 Scalability
- Support **10x current peak search load** for time-based route searches *(Revised: original 100K was ungrounded)*
- Handle GTFS feeds with **10K+ trips/day** per city
- Notification system: **1M push notifications/hour** capacity

### 4.4 Availability
- Feature available offline for saved trips with cached schedules
- Graceful degradation: Show cached schedule data if real-time feed fails
- 99.9% uptime for the route planning API

---

## 5. Success Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| Feature adoption | 30% of active users within 3 months | Analytics |
| On-time arrival rate | 85% of "Arrive By" users arrive within buffer | Journey completion data |
| Trip planning completion | 60% of searches result in confirmed journey | Funnel analytics |
| Notification engagement | 40% of notifications lead to trip start | Push notification analytics |
| User retention | 15% increase in weekly active users | Cohort analysis |
| NPS improvement | +10 points vs pre-feature | Surveys |

---

## 6. Release Strategy

### Phase 1: Foundation (Weeks 1-4)
- "Depart At" mode for multimodal search
- GTFS schedule-based time filtering
- Enhanced search UI with time picker

### Phase 2: Arrive By (Weeks 5-8)
- "Arrive By" reverse computation engine
- Departure advisor with buffer management
- Real-time overlay on scheduled times

### Phase 3: Notifications & Intelligence (Weeks 9-12)
- "Time to Leave" push notifications
- Saved trips with recurring schedules
- Risk-level indicators and delay warnings

### Phase 0: Data Foundation (Starts Day 1, runs parallel) *(Added per review)*
- Set up GTFS data quality monitoring pipeline
- Start logging scheduled vs actual times for ML training
- Instrument all searches with time metadata

### Phase 4: Optimization (Weeks 13-16)
- ML-based ETA refinement using historical data (pipeline started Phase 0)
- Crowding-aware scheduling using collected data
- Personalized route preferences learning
- Holiday/event-aware scheduling
- Weather/flood-aware routing

---

## 7. Dependencies & Risks

### Dependencies
| Dependency | Owner | Risk |
|------------|-------|------|
| GTFS static schedule data quality | CUMTA/MTC | **CRITICAL** - incomplete/outdated data *(Elevated per review)* |
| GTFS real-time feed availability | MTC/CMRL | High - may not exist for all modes |
| Google Transit API (supplementary) | Google | Low - stable SLA |
| Push notification infrastructure | FCM/APNs | Low - standard infra |
| Metro schedule API | CMRL | Medium - API availability |
| OpenTripPlanner (for Arrive By routing) | OSS | Low - mature, well-tested |

### Risks
| Risk | Impact | Mitigation |
|------|--------|------------|
| GTFS data incomplete for MTC | **Critical** | **Primary**: Partner with MTC for official schedule feed. **Secondary**: Crowdsource schedule corrections via in-app feedback. **Tertiary**: Google Transit API as supplement (not replacement). Start data quality monitoring pipeline from Day 1. *(Strengthened per review)* |
| "Arrive By" reverse computation complexity | High | Use OpenTripPlanner (supports reverse RAPTOR algorithm) instead of building from scratch. Integrate as microservice called from rider-app. *(Added per review)* |
| Real-time feed delays | Medium | Show "scheduled" vs "live" indicator; graceful degradation |
| Peak-hour crowding invalidates ETAs | Medium | Collect crowding data via user reports; build route-specific crowding profiles over time *(Added per review)* |
| Chennai monsoon flooding | Medium | Integrate weather API; maintain flood-prone zone list; alert users on affected routes *(Added per review)* |
| Complex multimodal time computation | Medium | Start with simple modes (single transit + walk); iterate to complex multi-transfer |
| User confusion with 3 time modes | Low | Default to "Leave Now"; progressive disclosure of other modes |

### Data Quality Strategy *(New section per review)*

Since GTFS data quality for MTC buses is the #1 risk, a dedicated strategy is required:

1. **Phase 1 (Week 1)**: Set up GTFS feed validation pipeline. Monitor completeness of stop_times, calendar, routes.
2. **Phase 1 (Week 2)**: Instrument all time-based queries to log `scheduled_time vs actual_arrival` when real-time data is available.
3. **Phase 2**: Build in-app "Report Schedule Issue" feature. Users can flag "bus didn't come at scheduled time."
4. **Phase 3**: Use collected data to build crowdsourced schedule corrections. Apply Bayesian smoothing to noisy reports.
5. **Phase 4**: Train ML model on historical schedule adherence data (collected from Phase 1 onwards).

---

## 8. Out of Scope (v1)

- Predictive pricing (surge awareness for future times)
- Integration with calendar apps (Google Calendar, Outlook)
- Voice-based trip planning ("Hey app, when should I leave for office?")
- Carpooling/shared ride integration
- Multi-city journey planning (inter-city trains/buses)
- Intermediate stop routing ("stop at bank on the way")
- Group travel coordination
- Full wheelchair/accessibility routing (v2 — requires accessibility data for all stops)
