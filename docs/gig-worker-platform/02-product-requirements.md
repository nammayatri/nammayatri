# Product Requirements Document: Namma Yatri Gig Worker Intelligence Platform

## Document Info
- **Product Name**: Namma Yatri Driver Intelligence (NYDI)
- **Target Users**: Ride-hailing drivers (primary), delivery/logistics gig workers (future)
- **Markets**: India (primary), global emerging markets (future)
- **Platform**: Android (primary), iOS (secondary)

---

## 1. Product Vision

> Every gig worker on Namma Yatri has a personal business co-pilot — one that tells them where to go, when to work, how much they're really earning, helps them save on taxes, and gives them access to financial services and benefits they'd never get on their own.

---

## 2. Feature Modules

### Module 1: Smart Earnings Dashboard

#### 1.1 Real-Time Earnings Tracker
**What Gridwise does**: Syncs earnings from Uber, Lyft, DoorDash etc. into one dashboard with per-hour/per-trip breakdowns.

**What we build (better)**:
- **Real-time earnings display** — no sync needed, we own the data
- **Daily/weekly/monthly earnings summary** with trend charts
- Breakdown by:
  - Per hour worked (online time vs. earning time)
  - Per trip (average fare, tips, cancellation charges)
  - Per kilometer driven (profitability metric)
  - By time slot (morning/afternoon/evening/night)
  - By area/zone (which neighborhoods pay best)
- **Idle time tracking** — time online but not on a trip (dead miles awareness)
- **Net earnings calculator** — gross earnings minus fuel, platform subscription, vehicle EMI, maintenance
- **Earnings goal tracker** — set daily/weekly/monthly goals, see progress in real-time

**India-specific adaptations**:
- Currency in ₹, support for lakhs/crores formatting
- UPI transaction integration for tip tracking
- Include toll reimbursements in calculations
- Multi-language support (Kannada, Hindi, Tamil, Telugu, Malayalam, Bengali, Marathi, Gujarati)

**User Stories**:
- As a driver, I want to see my real earnings per hour so I know if I'm making enough to cover my costs
- As a driver, I want to compare this week's earnings to last week to track my progress
- As a driver, I want to set a daily target of ₹2000 and see how close I am throughout the day
- As a driver, I want to know which areas give me the best per-km rate

**Existing NY Foundation**: `RideSummary` module (DailyStats with earnings, distance, numRides, tipAmount, cancellationCharges, rideDuration), `EarningsFlow` screens (PostPaid/PrePaid)

---

#### 1.2 Expense Tracker
**What competitors do**: Manual expense entry, receipt photos (Everlance, Solo), mileage tracking (MileIQ).

**What we build**:
- **Auto-detect fuel expenses** via UPI transaction categorization (amount + merchant = fuel station → auto-categorize)
- **Manual expense entry** with categories: Fuel, Maintenance, Insurance, EMI, Phone Recharge, Tolls, Cleaning, Food, Fines, Other
- **Receipt photo capture** and OCR for amount extraction
- **Recurring expense templates** (monthly EMI, insurance premium, phone plan)
- **Vehicle maintenance reminders** based on km driven (oil change every 5000 km, tire rotation every 10000 km)
- **Expense summary** — daily/weekly/monthly with category breakdown
- **Net profit calculation** — earnings minus all tracked expenses

**India-specific**:
- UPI transaction auto-import (with user consent via Account Aggregator framework)
- Fastag toll tracking integration
- GST on fuel — track input credit eligibility for GST-registered drivers
- Support for CNG/EV charging costs alongside petrol/diesel

**User Stories**:
- As a driver, I want my fuel expenses auto-detected when I pay at a petrol pump via UPI
- As a driver, I want to see my true profit after all expenses, not just gross earnings
- As a driver, I want reminders when my vehicle needs servicing based on how much I've driven
- As a driver, I want to photograph my maintenance bills and have them categorized automatically

---

#### 1.3 Trip Analytics
**What Gridwise does**: Per-trip breakdown, earnings per hour trends, service comparison.

**What we build**:
- **Individual trip profitability** — fare minus estimated fuel cost for that trip distance
- **Trip efficiency score** — ratio of earning time to total time (including pickup travel)
- **Pickup distance analysis** — which trips had long dead-mile pickups
- **Cancellation analytics** — cancellation rate, reasons, earnings impact
- **Route efficiency** — did the actual route match the optimal route? Extra km driven?
- **Rating correlation** — do higher-rated trips correlate with better tips?
- **Peak vs. off-peak analysis** — earnings comparison during different times

**User Stories**:
- As a driver, I want to see which trips were actually profitable after fuel cost
- As a driver, I want to know my average pickup distance so I can decide which rides to accept
- As a driver, I want to understand why some hours I earn ₹500/hr and others only ₹200/hr

---

### Module 2: Demand Intelligence

#### 2.1 Enhanced Demand Hotspots
**What Gridwise does**: Neighborhood profitability ranking, "Where to Drive" and "When to Drive" features with historical data.

**What we build (better)**:
- **Upgrade existing DemandHotspots** — currently tracks unserved searches via geohash. Enhance with:
  - Time-series prediction ("This area will be hot in 30 minutes based on patterns")
  - Heatmap visualization on map (already have HotSpotScreen)
  - Historical patterns ("Every Friday 6-8 PM, Indiranagar has 2x demand")
  - Search-to-booking ratio by zone (shows where supply is short)
- **Demand forecast model** using:
  - Historical ride data by time/day/zone
  - Weather data integration (rain → higher demand)
  - Event calendar integration (IPL matches, concerts, festivals)
  - Airport flight schedules (Kempegowda, IGI, etc.)
  - Office area patterns (Whitefield morning, CBD evening)
  - Public holiday / bandh awareness

**India-specific**:
- Festival calendar (Diwali, Pongal, Durga Puja, Onam — city-specific)
- Public transport disruption alerts (metro breakdown, bus strike)
- Wedding season / event venue tracking
- Religious event calendars (temple festivals, Friday prayers → specific area demand)

**Existing NY Foundation**: `DemandHotspots` module using geohash-based Redis sorted sets tracking search vs. booking frequency

**User Stories**:
- As a driver, I want to know that demand will spike near Chinnaswamy Stadium at 10 PM because an IPL match ends
- As a driver, I want to see that it'll rain in 30 minutes so I should stay near busy areas
- As a driver, I want historical heatmaps showing the best zones for each hour of the day

---

#### 2.2 Event & Venue Alerts
**What Gridwise does**: Event listings with location, scale, end times, custom reminders, and real-time updates (overtime, early finish).

**What we build**:
- **Event aggregation engine** — pull from:
  - BookMyShow, Paytm Insider, MakeMyLive (concerts, movies, sports)
  - Stadium/venue schedules (IPL, ISL, concerts)
  - Conference/exhibition calendars
  - Wedding venue bookings (via partnership)
- **Smart alerts**: Push notification 30 min before event ends with estimated surge demand
- **Venue capacity and expected rider demand** — "Chinnaswamy Stadium: 40,000 capacity, estimated 8,000 needing rides"
- **End-time tracking** — real-time updates for events running late/ending early
- **Driver positioning suggestions** — "Head to MG Road by 10:30 PM to catch concert crowd"
- **Saved venues** — drivers can follow venues they frequently pick up from

**India-specific**:
- Cricket match schedule integration (biggest demand driver)
- Mall/cinema timing patterns
- IT park shift-change schedules (Whitefield, HITEC City, Magarpatta)
- School/college timing for afternoon demand

**User Stories**:
- As a driver, I want alerts about big events in my city with expected end time
- As a driver, I want to save Chinnaswamy Stadium and get alerts whenever there's a match
- As a driver, I want to know when the night shift ends at Manyata Tech Park

---

#### 2.3 Airport Intelligence
**What Gridwise does**: 24-hour flight arrival/departure data, plane sizes, delay alerts, queue time optimization.

**What we build**:
- **Flight arrival schedule** — aggregated from airport data
- **Expected passenger volume by hour** (based on flight count and aircraft size)
- **Queue position estimation** — "Currently 45 drivers in queue, estimated wait: 35 min"
- **Smart queue decision** — "Skip airport queue, earn ₹200 more doing 2 city rides in the same time"
- **Delay alerts** — flights delayed, queue getting longer
- **Terminal-specific intel** — which terminal has more arrivals now

**India-specific airports**: Bangalore (KIA), Delhi (IGI), Mumbai (CSIA), Hyderabad (RGIA), Chennai (MAA) — start with NY operational cities

**User Stories**:
- As a driver, I want to know if the airport queue is worth waiting in or if I should take city rides
- As a driver, I want flight arrival counts for the next 4 hours to plan my shift

---

#### 2.4 Weather-Demand Correlation
**What Gridwise does**: Weather alerts 10 minutes before bad weather.

**What we build**:
- **Weather API integration** for hyperlocal forecasts
- **Rain prediction alerts** — "Rain expected in Koramangala in 20 min, demand likely +40%"
- **Extreme weather warnings** — safety alerts for floods, storms
- **Historical weather-demand model** — show how earnings correlate with weather

**User Stories**:
- As a driver, I want to know that rain is coming so I can position myself in a high-demand area

---

### Module 3: Financial Management

#### 3.1 Tax Compliance Assistant
**What Gridwise/Solo do**: Mileage logs for IRS deductions, CSV/PDF export, tax filing integration.

**What we build for India**:
- **Advance tax calculator** — estimate quarterly tax liability based on:
  - Gross earnings from all gig work
  - Deductible expenses (fuel, maintenance, depreciation, insurance)
  - TDS already deducted (if any)
  - Section 80C and other deductions
- **Quarterly payment reminders** — "Your advance tax of ₹4,500 is due by Sep 15"
- **TDS tracking** — monitor Form 26AS/AIS for TDS credits
- **GST tracker** (for drivers above threshold) — input credit on fuel, output liability
- **Annual ITR data export** — pre-filled data for CA/tax-filing platforms
- **Tax savings tips** — "Track your phone recharge as business expense to save ₹X"

**Partnerships**: ClearTax / Quicko for ITR filing, auto-populate from NY data

**User Stories**:
- As a driver, I want to know how much advance tax I need to pay this quarter
- As a driver, I want to see my estimated tax savings from tracked expenses
- As a driver, I want a one-click export of all my earnings and expenses for my CA

---

#### 3.2 Mileage & Km Tracking
**What MileIQ/Everlance do**: Auto-detect trips, classify business vs. personal, IRS-compliant logs.

**What we build**:
- **Automatic km tracking** during active duty (already tracked per-ride, extend to full shift)
- **Dead km tracking** — km driven between rides, to pickup, returning from drop
- **Business vs. personal classification** — on-duty = business, off-duty = personal
- **Fuel efficiency calculator** — km per liter based on actual driving
- **Vehicle depreciation tracking** — based on km driven, for tax deduction

**India-specific**: Per-km deduction rules differ from US mileage deductions — focus on actual expense method + depreciation

**User Stories**:
- As a driver, I want to know my total km driven for work this month for expense claims
- As a driver, I want to see my fuel efficiency trend over time

---

#### 3.3 Savings & Financial Planning
**What competitors do**: Nothing — this is whitespace.

**What we build**:
- **Micro-savings jar** — auto-deduct ₹20-100 per ride into a digital savings pot
- **Emergency fund goal** — "Save ₹10,000 for unexpected repairs" with progress tracking
- **Earnings forecast** — "At your current rate, you'll earn ₹42,000 this month"
- **Daily withdrawal amount** — "You can safely spend ₹800 today and still hit your monthly target"
- **Seasonal planning** — alert about historically slow months ("Prepare: January earnings typically drop 20%")

**Partnerships**: Digital savings (Fi, Jupiter), micro-insurance (Digit, Acko)

**User Stories**:
- As a driver, I want to auto-save ₹50 from each ride towards my emergency fund
- As a driver, I want to know how much I can afford to spend today

---

### Module 4: Benefits Marketplace

#### 4.1 Insurance Hub
**What Gridwise does**: $10,000 no-cost insurance, discounted quarterly plans.

**What we build**:
- **Micro-insurance** — daily/weekly accident cover (₹5-10/day for ₹5 lakh cover)
- **Vehicle insurance** comparison and renewal reminders
- **Health insurance** — group rates for NY driver community
- **Family protection** — term insurance at group rates
- **Claims assistance** — in-app claim filing with document upload

**Existing NY Foundation**: `DriverInsuranceScreen`, `Insurance` module in shared-kernel

**User Stories**:
- As a driver, I want affordable daily accident insurance that I only pay for on days I work
- As a driver, I want reminders before my vehicle insurance expires with best renewal quotes

---

#### 4.2 Vehicle Services
**What Gridwise does**: 10-40% savings on vehicle maintenance via CarAdvise, 15% off car rentals.

**What we build**:
- **Service network** — partnered garages with negotiated rates for NY drivers
- **Service booking** — book oil change, tire rotation etc. in-app
- **Service history** — track all maintenance with mileage stamps
- **Roadside assistance** — tow, tire change, battery jump (24/7)
- **Fuel discounts** — partnered fuel stations with loyalty points
- **EV charging network** — for electric vehicle drivers, station finder + pricing

**User Stories**:
- As a driver, I want 15% off on servicing at partnered workshops near me
- As a driver, I want my service history tracked automatically so I know what's due

---

#### 4.3 Health & Wellness
- **Telemedicine** — free/subsidized doctor consultations
- **Health checkup camps** — periodic free checkups at driver hubs
- **Mental health** — stress management resources, counselor access
- **Eye/back care** — ergonomic tips, annual eye checkup reminders

---

#### 4.4 Financial Products
- **Vehicle loans** — pre-approved based on NY earnings history
- **Fuel credit line** — "Buy fuel now, pay from tomorrow's earnings"
- **Education loans** — for driver's children
- **Credit score building** — consistent earnings on NY as proof of income for formal credit

**User Stories**:
- As a driver, I want to get a vehicle loan without the hassle of income proof
- As a driver, I want a fuel credit line so I don't start the day with empty pockets

---

### Module 5: Peer Intelligence & Community

#### 5.1 Enhanced Leaderboard
**What Gridwise does**: Compare earnings against other drivers in your area.

**What we build (extend existing)**:
- **Current LeaderBoard** already tracks rank, totalRides, totalDistance. Enhance with:
  - Earnings percentile ("You're in the top 15% of drivers in Bangalore")
  - Efficiency leaderboard (earnings per hour, per km)
  - Improvement trend ("You moved up 20 positions this week")
  - Category boards: most rides, highest earnings, best rating, most km
  - Monthly/weekly/daily views

**Existing NY Foundation**: `LeaderBoard` module with DriversInfo (rank, name, totalRides, totalDistance, gender)

**User Stories**:
- As a driver, I want to see how my earnings compare to other drivers in my city
- As a driver, I want to compete on a weekly leaderboard with rewards

---

#### 5.2 Driver Community
- **City-specific driver forums** — share tips, road closure info, best areas
- **Mentorship** — experienced drivers help new drivers
- **Anonymous earnings sharing** — opt-in benchmark data
- **Driver stories** — success stories for motivation

---

### Module 6: Smart Notifications Engine

#### 6.1 Proactive Intelligence Alerts
**Inspired by Gridwise's real-time alerts system.**

Types of notifications:
- **Demand surge**: "Demand spiking in Indiranagar — head there now"
- **Event ending**: "IPL match at Chinnaswamy ends in 30 min — 5 km from you"
- **Weather**: "Rain starting in 20 min in your area — expect high demand"
- **Earnings milestone**: "₹500 more to hit your daily goal!"
- **Expense reminder**: "You haven't logged fuel expense today"
- **Tax deadline**: "Advance tax due in 5 days — estimated liability ₹4,200"
- **Maintenance due**: "Oil change due — 500 km since last service"
- **Benefit expiring**: "Insurance expires in 7 days — renew with 20% discount"
- **Performance**: "Great week! You earned 15% more than last week"
- **Opportunity**: "Airport queue is short (12 drivers) — might be worth heading there"

**Notification preferences**: Drivers choose which alerts they want, frequency limits, quiet hours.

**User Stories**:
- As a driver, I don't want to constantly check the app — just tell me when something important happens
- As a driver, I want to control which notifications I get so I'm not overwhelmed

---

## 3. Non-Functional Requirements

### 3.1 Performance
- Earnings dashboard loads in <1 second
- Demand heatmap updates every 5 minutes
- Push notifications delivered within 30 seconds of trigger event
- Analytics queries process within 2 seconds for daily aggregations

### 3.2 Scalability
- Support 600K+ active drivers with real-time analytics
- Handle 10M+ daily ride events for analytics processing
- Event calendar scales to 100+ cities

### 3.3 Privacy & Security
- All financial data encrypted at rest and in transit
- Earnings data visible only to the driver (not peers, not platform admins)
- Expense/UPI data processed locally where possible
- Explicit consent for any data sharing
- GDPR-equivalent compliance for global markets
- RBI data localization compliance for financial data

### 3.4 Accessibility
- Minimum 8 Indian languages from launch
- Voice readout of earnings summaries
- High-contrast mode for outdoor visibility
- Offline mode for basic earnings display (sync when online)
- Works on low-end Android devices (2GB RAM, Android 8+)

### 3.5 Localization
- Currency formatting per market
- Tax rules configurable per country/state
- Date/time formatting per locale
- Distance units (km/miles) per market
- Regulatory compliance per jurisdiction

---

## 4. Prioritization (MoSCoW)

### Must Have (Phase 1)
- Enhanced earnings dashboard with trend charts
- Expense tracker (manual + UPI auto-detect)
- Net profit calculator
- Demand hotspot enhancement (time-series, weather)
- Basic tax summary
- Daily/weekly earnings goal tracker
- Push notifications for demand and milestones

### Should Have (Phase 2)
- Event/venue alert system
- Airport queue intelligence
- Trip profitability analysis
- Peer benchmarking / enhanced leaderboard
- Vehicle maintenance reminders
- Receipt OCR
- Earnings forecast

### Could Have (Phase 3)
- Full tax compliance suite (advance tax, GST)
- Micro-savings integration
- Insurance marketplace
- Vehicle servicing marketplace
- Fuel discount partnerships
- Driver community forums

### Won't Have (This Year)
- Tax filing within app (partner integration sufficient)
- Full accounting suite (not our core)
- Multi-platform aggregation (ONDC future)
- International expansion
- Enterprise analytics B2B product

---

## 5. Competitive Feature Matrix

| Feature | Gridwise | Solo | Everlance | MileIQ | NY (Proposed) |
|---------|----------|------|-----------|--------|---------------|
| Real-time earnings | Sync delay | Sync delay | No | No | **Native, instant** |
| Multi-platform earnings | Yes (US apps) | Yes (US apps) | No | No | Single platform (ONDC future) |
| Demand forecasting | Events + historical | Pay guarantee | No | No | **ML on first-party search/booking data** |
| Event alerts | Yes (US events) | No | No | No | **India events + cricket + festivals** |
| Airport intelligence | Yes (US airports) | No | No | No | **India airports + queue estimation** |
| Expense tracking | Manual | Auto + manual | Auto + manual | No | **UPI auto-detect + manual** |
| Mileage/km tracking | Auto | Auto | Auto | Auto | **Native (already tracking rides)** |
| Tax tools | US mileage deductions | US tax filing | US mileage | US mileage | **India tax (advance tax, GST, TDS)** |
| Insurance | US only | No | No | No | **India micro-insurance** |
| Vehicle services | US partnerships | No | No | No | **India service network** |
| Financial products | No | No | No | No | **Loans, savings, credit** |
| Leaderboard | Peer comparison | No | No | No | **Existing + enhanced** |
| Languages | English | English | English | English | **8+ Indian languages** |
| Open source | No | No | No | No | **Yes** |
| Cost | $14.99/mo | $12-24/mo | $8.99-15/mo | $8.99/mo | **Core free, premium ₹99-299/mo** |

---

## 6. Success Metrics

| Metric | Definition | Phase 1 Target | Phase 3 Target |
|--------|-----------|---------------|---------------|
| **DAU on Intelligence features** | Daily active users opening earnings/demand screens | 25% of active drivers | 60% |
| **Expense tracking adoption** | Drivers who log ≥1 expense/week | 15% | 40% |
| **Demand alert engagement** | Click-through rate on demand notifications | 20% | 35% |
| **Earnings improvement** | Avg. earnings increase for engaged vs. non-engaged drivers | 10% | 25% |
| **Premium conversion** | % of active drivers subscribing to premium tier | 3% | 12% |
| **Marketplace revenue** | Monthly revenue from benefits marketplace | ₹2L | ₹50L |
| **Driver retention** | 6-month retention rate improvement | +10% | +30% |
| **Tax compliance** | Drivers using tax features | 5% | 25% |
