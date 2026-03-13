# Reach on Time - UI/UX Design Specification

**Version**: 2.0 (Post-Review Revision)
**Date**: 2026-03-13
**Platform**: React Native (Android/iOS) + PureScript (Web)

---

## 1. Design Principles

1. **Progressive Disclosure**: "Leave Now" is the default. "Arrive By" and "Depart At" are one tap away — never hidden, never overwhelming.
2. **Glanceable Time Info**: Key departure/arrival times must be readable in < 2 seconds.
3. **Transit-First**: Public transit options shown first; auto/taxi as last-mile or fallback.
4. **Localized**: Tamil + English bilingual. Auto-rickshaw iconography. MTC bus color coding.
5. **Accessible**: WCAG 2.1 AA compliance. Min touch target 48dp. High contrast mode.

---

## 2. Screen Inventory

### 2.1 Modified Screens (Existing)
1. **Search Location Screen** — Add time mode selector
2. **Route Results Screen** — Add time-aware route cards
3. **Journey Detail Screen** — Add departure advisor
4. **Home Screen** — Add saved trips widget

### 2.2 New Screens
5. **Time Picker Modal** — Date + time selection
6. **Departure Advisor Screen** — Detailed departure recommendations
7. **Saved Trips Screen** — Manage recurring trips
8. **Trip Notification Settings** — Per-trip notification config

---

## 3. Detailed Screen Designs

### 3.1 Search Location Screen (Modified)

```
┌──────────────────────────────────────┐
│  ← Search                            │
│                                       │
│  ┌─────────────────────────────────┐ │
│  │ 📍 Current Location              │ │
│  └─────────────────────────────────┘ │
│  ┌─────────────────────────────────┐ │
│  │ 📌 Where to?                     │ │
│  └─────────────────────────────────┘ │
│                                       │
│  ┌─────────────────────────────────┐ │
│  │ ⏰ Leave Now  ▼                  │ │  ← NEW: Time Mode Selector
│  └─────────────────────────────────┘ │
│                                       │
│  ─── Saved Places ───                │
│  🏠 Home          🏢 Office          │
│  ⭐ T.Nagar       ⭐ Airport         │
│                                       │
│  ─── Saved Trips ───                 │  ← NEW: Saved Trips Section
│  ┌─────────────────────────────────┐ │
│  │ 🔄 Office Commute               │ │
│  │    Arrive by 9:30 AM · Mon-Fri  │ │
│  │    [Plan Today's Trip →]        │ │
│  └─────────────────────────────────┘ │
│                                       │
└──────────────────────────────────────┘
```

**Time Mode Selector (Collapsed)**:
- Shows as a pill/chip below destination input
- Default text: "Leave Now"
- Tap to expand into 3 options

**Time Mode Selector (Expanded)**:
```
┌──────────────────────────────────────┐
│  When do you want to travel?         │
│                                       │
│  ┌──────────┐ ┌──────────┐ ┌──────┐ │
│  │ Leave    │ │ Arrive   │ │Depart│ │
│  │ Now  ●   │ │ By       │ │  At  │ │
│  └──────────┘ └──────────┘ └──────┘ │
│                                       │
│  [Set time picker appears below      │
│   when "Arrive By" or "Depart At"    │
│   is selected]                       │
│                                       │
└──────────────────────────────────────┘
```

### 3.2 Time Picker Modal

```
┌──────────────────────────────────────┐
│  ──── Arrive By ────                 │
│                                       │
│  ┌─────────────────────────────────┐ │
│  │        Today, Mar 13            │ │
│  │     ┌────┐   ┌────┐            │ │
│  │     │ 09 │ : │ 30 │  AM        │ │
│  │     │ ▲  │   │ ▲  │  ▲         │ │
│  │     │ ▼  │   │ ▼  │  ▼         │ │
│  │     └────┘   └────┘            │ │
│  └─────────────────────────────────┘ │
│                                       │
│  Quick options:                      │
│  ┌──────┐ ┌──────┐ ┌──────┐        │
│  │In 30m│ │ In 1h│ │ In 2h│        │
│  └──────┘ └──────┘ └──────┘        │
│                                       │
│  Buffer: ┌──┐ ┌──┐ ┌──┐ ┌──┐      │
│          │0m│ │5m│ │10│ │15│       │
│          └──┘ └──┘ └──┘ └──┘       │
│                                       │
│  ┌─────────────────────────────────┐ │
│  │         Set Time                 │ │
│  └─────────────────────────────────┘ │
│                                       │
└──────────────────────────────────────┘
```

**Key Design Decisions**:
- **Quick options**: "In 30m", "In 1h", "In 2h" for fast selection
- **Buffer control**: Explicit buffer slider (0-15 min) for "Arrive By" mode only
- **Date picker**: Today/Tomorrow toggle; full calendar for further dates
- **Smart defaults**: "Arrive By" defaults to next round hour; "Depart At" defaults to +30min

### 3.3 Route Results Screen (Modified)

```
┌──────────────────────────────────────┐
│  ← Tambaram → T.Nagar               │
│  Arrive by 9:30 AM · Today          │
│  ┌─────────────────────────────────┐ │
│  │ [Leave Now] [Arrive By●][Depart]│ │  ← Sticky time mode toggle
│  └─────────────────────────────────┘ │
│                                       │
│  ─── Recommended ───                 │
│  ┌─────────────────────────────────┐ │
│  │ 🟢 COMFORTABLE                  │ │  ← Risk indicator
│  │ Leave by 8:15 AM  →  Arr 9:18AM│ │
│  │                                   │ │
│  │ 🚶2m → 🚌 Bus 47B 35m → 🚇 M  │ │  ← Visual journey timeline
│  │         etro Blue 18m → 🚶3m    │ │
│  │                                   │ │
│  │ ₹35 · 58 min · 2 transfers      │ │
│  │ Next bus: 3 min ● Live           │ │
│  └─────────────────────────────────┘ │
│                                       │
│  ┌─────────────────────────────────┐ │
│  │ 🟡 GOOD                         │ │
│  │ Leave by 8:35 AM  →  Arr 9:22AM│ │
│  │                                   │ │
│  │ 🚶3m → 🚌 Bus 17E 28m →        │ │
│  │   🚶5m → 🚇 Metro 15m → 🚶2m  │ │
│  │                                   │ │
│  │ ₹30 · 53 min · 2 transfers      │ │
│  │ Next bus: 18 min                 │ │
│  └─────────────────────────────────┘ │
│                                       │
│  ┌─────────────────────────────────┐ │
│  │ 🔴 TIGHT                        │ │
│  │ Leave by 9:02 AM  →  Arr 9:28AM│ │
│  │                                   │ │
│  │ 🛺 Auto 26m                     │ │
│  │                                   │ │
│  │ ~₹180 · 26 min · Direct         │ │
│  │ Available now                     │ │
│  └─────────────────────────────────┘ │
│                                       │
│  ─── Not Recommended ───            │
│  ┌─────────────────────────────────┐ │
│  │ ⚫ TOO LATE                      │ │
│  │ Cannot reach by 9:30 AM via     │ │
│  │ bus + metro. Last option was     │ │
│  │ 8:35 AM departure.              │ │
│  └─────────────────────────────────┘ │
│                                       │
└──────────────────────────────────────┘
```

**Route Card Anatomy**:
```
┌───────────────────────────────────────┐
│  Risk Badge    Departure → Arrival    │
│                                       │
│  Journey Timeline (visual bar)        │
│  [walk]─[bus icon + route#]─[metro]─  │
│                                       │
│  Cost · Duration · Transfer count     │
│  Real-time status (live/scheduled)  → │  ← Chevron indicates tappable
└───────────────────────────────────────┘
Note: [Save Trip] and [Set Reminder] moved to Journey Detail
screen to reduce card clutter (per review).
```
```

**Risk Badge Logic**:
| Badge | Color | Condition |
|-------|-------|-----------|
| COMFORTABLE | Green | Buffer >= 12 min before target |
| GOOD | Yellow | Buffer 5-12 min |
| TIGHT | Red | Buffer < 5 min |
| TOO LATE | Grey | Cannot arrive by target time |

### 3.4 Journey Detail Screen (Modified)

```
┌──────────────────────────────────────┐
│  ← Journey Details                   │
│                                       │
│  ┌─────────────────────────────────┐ │
│  │ DEPARTURE ADVISOR               │ │
│  │                                   │ │
│  │ 🟢 Leave by 8:15 AM             │ │
│  │    (12 min buffer included)      │ │
│  │                                   │ │
│  │ ⏰ Set Reminder                  │ │
│  │    └ Notify me 5 min before     │ │
│  │                                   │ │
│  │ Arrive: ~9:18 AM (target 9:30)  │ │
│  └─────────────────────────────────┘ │
│                                       │
│  ──── Journey Timeline ────          │
│                                       │
│  ● 8:15 AM — Start Walking          │
│  │ 🚶 Walk 250m to Bus Stop (2min) │
│  │                                   │
│  ● 8:17 AM — Tambaram Bus Stand     │
│  │ 🚌 Bus 47B toward T.Nagar       │
│  │ Departs: 8:20 AM (sched)        │
│  │ Platform: Bay 3                  │
│  │ Duration: ~35 min               │
│  │                                   │
│  ● 8:55 AM — Saidapet Metro Stn    │
│  │ 🚶 Walk to Metro Entry (3min)   │
│  │                                   │
│  ● 8:58 AM — Board Metro           │
│  │ 🚇 Blue Line toward Airport     │
│  │ Duration: ~18 min               │
│  │                                   │
│  ● 9:16 AM — T.Nagar Metro Stn     │
│  │ 🚶 Walk to destination (2min)   │
│  │                                   │
│  ◉ 9:18 AM — Arrive at T.Nagar     │
│                                       │
│  ┌─────────────────────────────────┐ │
│  │   ₹35 total · 63 min journey   │ │
│  │   Bus ₹15 + Metro ₹20          │ │  ← Fare breakdown (added per review)
│  │                                   │ │
│  │   [Start Journey]               │ │
│  └─────────────────────────────────┘ │
│                                       │
│  ┌─────────────────────────────────┐ │  ← "What if" fallback (added per review)
│  │ If you miss the 8:20 bus →      │ │
│  │ Next option: 8:35 bus, arr 9:25 │ │
│  │ [View backup plan ▾]            │ │
│  └─────────────────────────────────┘ │
│                                       │
└──────────────────────────────────────┘
```

### 3.5 Saved Trips Screen (New)

```
┌──────────────────────────────────────┐
│  ← Saved Trips                       │
│                                       │
│  ─── Regular Trips ───               │
│  ┌─────────────────────────────────┐ │
│  │ 🏢 Office Commute               │ │
│  │ Home → Office                    │ │
│  │ Arrive by 9:30 AM · Mon-Fri    │ │
│  │                                   │ │
│  │ Today: Leave by 8:15 AM        │ │
│  │ [Plan Trip] [Edit] [🔔 On]     │ │
│  └─────────────────────────────────┘ │
│                                       │
│  ┌─────────────────────────────────┐ │
│  │ 🏠 Evening Return               │ │
│  │ Office → Home                    │ │
│  │ Depart at 6:00 PM · Mon-Fri    │ │
│  │                                   │ │
│  │ Today: Arrive ~7:05 PM          │ │
│  │ [Plan Trip] [Edit] [🔔 On]     │ │
│  └─────────────────────────────────┘ │
│                                       │
│  ─── One-time Trips ───             │
│  ┌─────────────────────────────────┐ │
│  │ ✈️ Airport Trip                  │ │
│  │ Home → Chennai Airport           │ │
│  │ Arrive by 5:30 PM · Mar 15     │ │
│  │                                   │ │
│  │ Suggested: Leave by 3:15 PM    │ │
│  │ [Plan Trip] [Edit] [🔔 On]     │ │
│  └─────────────────────────────────┘ │
│                                       │
│  [+ Add New Trip]                    │
│                                       │
└──────────────────────────────────────┘
```

### 3.6 Notification UI

**"Time to Leave" Notification**:
```
┌──────────────────────────────────────┐
│ 🚌 Chennai One                       │
│ Time to leave for Office!            │
│ Leave now to catch Bus 47B at 8:20AM │
│ [Start Journey]     [Snooze 5 min]  │
└──────────────────────────────────────┘
```

**"Bus Approaching" Notification**:
```
┌──────────────────────────────────────┐
│ 🚌 Chennai One                       │
│ Bus 47B arriving in 3 minutes        │
│ Tambaram Bus Stand · Bay 3           │
│ [Track Bus]          [Dismiss]       │
└──────────────────────────────────────┘
```

---

## 4. Interaction Patterns

### 4.1 Time Mode Switch Flow

```
User taps "Leave Now" pill
  → Expands to 3 options: [Leave Now] [Arrive By] [Depart At]
  → User taps "Arrive By"
  → Time picker slides up from bottom
  → User sets time (scroll or quick option)
  → User optionally sets buffer
  → Taps "Set Time"
  → Pill updates to "Arrive by 9:30 AM"
  → Route search auto-triggers with new time constraint
```

### 4.2 Save Trip Flow

```
User views route results
  → Taps "Save Trip" on preferred route card
  → Bottom sheet: Name (auto-suggested), Recurrence (None/Daily/Weekdays/Custom)
  → Optional: Set notification preference
  → Taps "Save"
  → Trip appears in Home screen widget and Saved Trips screen
```

### 4.3 Time Picker Dismissal *(Added per review)*
```
User opens time picker → changes time → taps outside modal or presses back
  → Time reverts to previous value (unsaved)
  → Pill shows previous time mode/value
  → Only "Set Time" button confirms the selection
```

### 4.4 Swap Origin/Destination *(Added per review)*
```
User taps ⇄ icon between origin and destination fields
  → Fields swap instantly
  → If time constraint is set, search auto-triggers with swapped locations
  → Useful for return trip planning
```

### 4.5 "Leave Now" Re-selection *(Added per review)*
```
User is in "Arrive By" mode with time set
  → Taps "Leave Now" in time mode selector
  → Time constraint is cleared silently (no confirmation needed)
  → Search auto-triggers with current time
  → Pill reverts to "Leave Now"
```

### 4.6 "What If I Miss It?" Fallback *(Added per review)*
```
Journey Detail screen shows collapsed section at bottom:
  "If you miss the 8:20 bus → Next option departs 8:35"
  → User taps to expand
  → Shows alternative journey with updated times/risk
```

### 4.7 Notification → Journey Flow

```
User receives "Time to Leave" push notification
  → Taps notification
  → App opens to Journey Detail screen with latest computed route
  → Route automatically refreshed with real-time data
  → User taps "Start Journey"
  → Transitions to existing multimodal journey flow
```

---

## 5. Visual Design System

### 5.1 Color Palette (Risk-based)

| State | Color | Hex | Usage |
|-------|-------|-----|-------|
| Comfortable | Green | #2E7D32 | Safe departure window |
| Good | Deep Orange | #E65100 | Adequate buffer *(Revised: #F9A825 failed WCAG AA contrast on white)* |
| Tight | Red | #C62828 | Risky timing |
| Too Late | Grey | #757575 | Impossible to make it |
| Live | Blue | #1565C0 | Real-time data indicator |
| Scheduled | Grey | #9E9E9E | Static schedule data |

### 5.2 Transit Mode Icons

| Mode | Icon | Color |
|------|------|-------|
| Walk | 🚶 (walking person) | Grey #757575 |
| MTC Bus | 🚌 (bus) | MTC Red #D32F2F |
| Metro | 🚇 (metro) | CMRL Blue #1565C0 |
| Suburban Rail | 🚆 (train) | SR Green #388E3C |
| Auto | 🛺 (auto-rickshaw) | Yellow #FBC02D | *(Revised: differentiated from "Good" risk color)* |
| Taxi/Cab | 🚕 (car) | Black #212121 |

### 5.3 Typography

| Element | Style | Size | Notes |
|---------|-------|------|-------|
| Departure time | Bold | 18sp | |
| Arrival time | Bold | 18sp | |
| Risk badge | Semi-bold, uppercase | 14sp | *(Revised from 12sp: must be readable at 200% scaling)* |
| Route summary | Regular | 14sp | |
| Real-time status | Regular, italic | 12sp | |
| Cost/duration | Medium | 14sp | |
| Saved trip name | Semi-bold | 16sp | *(Added per review)* |
| Recurrence pattern | Regular | 12sp | *(Added per review)* |
| Today's departure | Medium | 14sp | *(Added per review)* |

### 5.4 Journey Timeline Bar

A horizontal visual bar showing the journey composition:

```
[Walk ][     Bus 47B      ][Walk][  Metro Blue  ][W]
 gray       red              gray     blue        gray
 2min       35min            3min     18min       2min
```

- Width proportional to duration
- Color matches transport mode
- Icons at transition points
- Tap any segment for details

---

### 5.5 Dark Mode Colors *(Added per review)*

| Element | Light Mode | Dark Mode |
|---------|-----------|-----------|
| Background | #FFFFFF | #121212 |
| Card background | #FFFFFF | #1E1E1E |
| Primary text | #212121 | #E0E0E0 |
| Risk: Comfortable | bg #E8F5E9, text #2E7D32 | bg #1B3A1F, text #66BB6A |
| Risk: Good | bg #FFF3E0, text #E65100 | bg #3E2700, text #FFB74D |
| Risk: Tight | bg #FFEBEE, text #C62828 | bg #3E1010, text #EF5350 |
| Risk: Too Late | bg #F5F5F5, text #757575 | bg #2A2A2A, text #9E9E9E |
| Live indicator | #1565C0 | #42A5F5 |

---

### 5.6 Loading, Error, and Empty States *(Added per review)*

**Loading State (Route Search)**:
```
┌──────────────────────────────────────┐
│  ← Tambaram → T.Nagar               │
│  Arrive by 9:30 AM · Today          │
│                                       │
│  ┌─────────────────────────────────┐ │
│  │ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ │ │  ← Skeleton card 1
│  │ ░░░░░░░ ░░░░ ░░░░ ░░░░░░░░░░░ │ │
│  │ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ │ │
│  └─────────────────────────────────┘ │
│  ┌─────────────────────────────────┐ │
│  │ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ │ │  ← Skeleton card 2
│  │ ░░░░░░░ ░░░░ ░░░░ ░░░░░░░░░░░ │ │
│  └─────────────────────────────────┘ │
│                                       │
│  Finding the best routes for you...  │
│                                       │
└──────────────────────────────────────┘
```

**Error State (API Failure)**:
```
┌──────────────────────────────────────┐
│  ← Tambaram → T.Nagar               │
│                                       │
│           ⚠️ (large icon)            │
│                                       │
│    Something went wrong.             │
│    We couldn't find routes           │
│    right now.                        │
│                                       │
│    [Try Again]                       │
│                                       │
│    Or try auto/taxi instead:         │
│    [Book Auto →]                     │
│                                       │
└──────────────────────────────────────┘
```

**Empty State (No Saved Trips)**:
```
┌──────────────────────────────────────┐
│  ← Saved Trips                       │
│                                       │
│         (illustration)               │
│     📍 ─ ─ ─ ─ ─ ─ 📍              │
│                                       │
│    No saved trips yet               │
│                                       │
│    Save a trip to get daily          │
│    "Time to Leave" reminders.        │
│                                       │
│    [Plan a Trip →]                   │
│                                       │
└──────────────────────────────────────┘
```

**Stale/Offline State (Cached Schedule)**:
```
┌──────────────────────────────────────┐
│  ⚠️ Showing cached schedule          │
│  Last updated 2 hours ago            │
│  [Refresh when online]              │
└──────────────────────────────────────┘
```

---

## 6. Animations & Transitions

| Interaction | Animation | Duration |
|-------------|-----------|----------|
| Time mode expand | Slide up + fade in | 250ms |
| Time picker open | Bottom sheet slide up | 300ms |
| Route card load | Stagger fade in | 150ms per card |
| Risk badge appear | Scale + fade | 200ms |
| Timeline bar build | Left-to-right reveal | 400ms |
| Live indicator | Pulse animation | 2s loop |
| Notification slide | Top slide down | 300ms |

**Reduced Motion**: When `prefers-reduced-motion` is enabled, all animations are replaced with instant transitions (0ms). Pulse animations are replaced with static indicators. *(Added per review: WCAG 2.1 AA 2.3.3)*

---

## 7. Accessibility

### 7.1 Screen Reader Support
- Time mode selector: "Leave Now selected. Double tap to change travel time option."
- Risk badges: "Comfortable departure. Leave by 8:15 AM. 12 minute buffer."
- Journey timeline: Read each segment sequentially with mode, duration, stop names.

### 7.2 Motor Accessibility
- All touch targets minimum 48x48dp
- Swipe gestures have button alternatives
- Time picker supports keyboard input as fallback

### 7.3 Visual Accessibility
- All risk colors have text labels (never color-only indication)
- Support system-level text scaling up to 200%
- High contrast mode: Use outlined badges instead of filled

### 7.4 Cognitive Accessibility
- Max 3 options for time mode (no hidden sub-options)
- Clear labels: "Leave Now", "Arrive By", "Depart At" (no jargon)
- Confirmation before saving trips or setting reminders

---

## 8. Localization Notes (Tamil)

| English | Tamil | Notes |
|---------|-------|-------|
| Leave Now | இப்போது புறப்படு | Colloquial, not formal |
| Arrive By | இவ்வளவு நேரத்தில் சேர | "Reach by this time" |
| Depart At | புறப்படும் நேரம் | "Departure time" |
| Comfortable | வசதியான | |
| Tight | இறுக்கமான | |
| Set Reminder | நினைவூட்டல் அமை | |
| Saved Trips | சேமித்த பயணங்கள் | |
| Buffer | கூடுதல் நேரம் | "Extra time" - more intuitive |

---

## 9. Figma Design Notes

Since Figma requires a browser-based design tool, here are the specifications for creating the Figma designs:

### 9.1 Figma File Structure
```
📁 Reach on Time - Chennai One
├── 📄 Cover Page
├── 📁 Design System
│   ├── Colors (Risk palette + Transit mode colors)
│   ├── Typography (Scale + styles)
│   ├── Icons (Transit mode icons, risk badges)
│   ├── Components
│   │   ├── Time Mode Selector (3 states)
│   │   ├── Time Picker Modal
│   │   ├── Route Card (4 risk variants)
│   │   ├── Journey Timeline Bar
│   │   ├── Departure Advisor Card
│   │   ├── Saved Trip Card
│   │   └── Notification Cards
│   └── Spacing & Grid (8dp grid)
├── 📁 Screens - Light Mode
│   ├── Search Screen (with time selector)
│   ├── Time Picker - Arrive By
│   ├── Time Picker - Depart At
│   ├── Route Results - Leave Now
│   ├── Route Results - Arrive By
│   ├── Route Results - Depart At
│   ├── Journey Detail
│   ├── Saved Trips
│   └── Notification Examples
├── 📁 Screens - Dark Mode
│   └── (Mirror of Light Mode)
├── 📁 Flows
│   ├── Happy Path: Arrive By search
│   ├── Edge Case: Too Late
│   ├── Save Trip + Notification
│   └── Recurring Trip Setup
└── 📁 Handoff
    ├── Redline Specs
    └── Asset Export Guide
```

### 9.2 Component Specifications for Figma Creation

**Time Mode Selector Component**:
- Container: 344x48dp, rounded corners 24dp
- 3 segments, equal width
- Active segment: filled primary color, white text
- Inactive segment: transparent, grey text
- Auto-layout with 0 gap between segments

**Route Card Component**:
- Container: full width - 32dp margin, 16dp padding
- Corner radius: 12dp
- Elevation: 2dp shadow
- Risk badge: 8dp corner radius, 8x28dp, text 12sp uppercase
- Journey timeline bar: 8dp height, full width

**Departure Advisor Card**:
- Container: full width, primary color background (light tint)
- Left accent bar: 4dp width, risk color
- Clock icon: 24x24dp
- Departure time: 20sp bold
- Buffer text: 14sp regular, grey

---

## 10. Responsive Considerations

### 10.1 Small Screens (< 360dp width)
- Journey timeline bar wraps to 2 lines if > 4 segments
- Quick time options stack vertically
- Route card cost/duration/transfers stack vertically

### 10.2 Large Screens / Tablets
- Route results show in 2-column grid
- Journey detail shows map alongside timeline
- Time picker uses side-by-side date + time layout

### 10.3 Landscape Mode
- Time picker uses horizontal layout
- Journey timeline uses horizontal scroll
- Route cards use list layout (no cards)
