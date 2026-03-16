# Implementation Plan: Namma Yatri Gig Worker Intelligence Platform

## 1. Architecture Overview

### 1.1 Current System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    NAMMA YATRI ECOSYSTEM                        │
│                                                                 │
│  ┌──────────────┐    ┌──────────────────┐    ┌───────────────┐ │
│  │ ny-react-    │    │   nammayatri     │    │ shared-kernel  │ │
│  │ native       │    │   (Backend)      │    │               │ │
│  │              │    │                  │    │ mobility-core  │ │
│  │ /provider    │◄──►│ provider-platform│◄──►│ (Maps, Payment│ │
│  │  (Driver App)│    │ dynamic-offer-   │    │  Notification, │ │
│  │              │    │ driver-app       │    │  Insurance...) │ │
│  │ /consumer    │◄──►│ rider-platform   │    │               │ │
│  │  (Rider App) │    │                  │    └───────────────┘ │
│  └──────────────┘    │ kafka-consumers  │    ┌───────────────┐ │
│                      │ dashboard        │    │  euler-hs     │ │
│                      │                  │    │  (Framework)  │ │
│                      └──────────────────┘    └───────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

**Backend Stack**: Haskell (Servant for APIs, Beam for DB, Hedis for Redis, Kafka for events)
**Frontend Stack**: React Native (TypeScript), shared components in `/common`, driver app in `/provider`
**Database**: PostgreSQL (via Beam ORM), Redis (Hedis) for caching/real-time data
**Shared Kernel**: External integrations (Maps, Payment, Payout, Notification, Insurance, SMS, Encryption)

### 1.2 Key Existing Modules We Build Upon

| Existing Module | Location | What It Does | How We Extend |
|----------------|----------|-------------|---------------|
| **DemandHotspots** | `Domain/Action/UI/DemandHotspots.hs` | Geohash-based search/booking frequency in Redis | Add time-series prediction, weather/event correlation |
| **RideSummary** | `Domain/Action/UI/RideSummary.hs` | Daily stats: earnings, distance, rides, tips, cancellation charges, duration | Add per-hour breakdown, expense netting, trend calculation |
| **DailyStats** | `Domain/Types/DailyStats.hs` | Data type for daily driver statistics | Extend with expense data, efficiency metrics |
| **LeaderBoard** | `Domain/Action/UI/LeaderBoard.hs` | Rank by rides/distance (daily/weekly/monthly) | Add earnings percentile, efficiency rankings |
| **DriverWallet** | `Domain/Action/UI/DriverWallet.hs` | Wallet transactions, payout, topup | Extend for savings, expense tracking |
| **DriverCoin** | `Domain/Action/UI/DriverCoin.hs` | Gamification coin rewards | Integrate with benefits marketplace |
| **Performance** | `Domain/Action/UI/Performance.hs` | Referral tracking, payout stats | Extend with comprehensive performance metrics |
| **Insurance** | `shared-kernel/.../External/Insurance` | Insurance integration | Extend for micro-insurance marketplace |
| **EarningsFlow** | `ny-react-native/provider/src/screens/EarningsFlow/` | PostPaid/PrePaid earnings screens | Complete redesign with analytics dashboard |
| **BenefitsFlow** | `ny-react-native/provider/src/screens/BenefitsFlow/` | Benefits, insurance, LMS, UPI details | Extend with marketplace |
| **HotSpotScreen** | `ny-react-native/provider/src/screens/HotSpotScreen/` | Demand heatmap visualization | Enhance with predictions, events overlay |

### 1.3 Proposed Architecture Extension

```
┌─────────────────────────────────────────────────────────────────────┐
│                  NEW: GIG WORKER INTELLIGENCE LAYER                  │
│                                                                      │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────────────────┐ │
│  │ Analytics      │  │ Demand         │  │ Financial              │ │
│  │ Engine         │  │ Intelligence   │  │ Services               │ │
│  │                │  │                │  │                        │ │
│  │ - Earnings     │  │ - Predictions  │  │ - Expense Tracker      │ │
│  │   aggregation  │  │ - Event alerts │  │ - Tax Calculator       │ │
│  │ - Trend calc   │  │ - Weather      │  │ - Savings              │ │
│  │ - Peer compare │  │ - Airport      │  │ - Benefits Marketplace │ │
│  │ - Efficiency   │  │ - Notifications│  │ - Insurance Hub        │ │
│  │   metrics      │  │                │  │                        │ │
│  └───────┬────────┘  └───────┬────────┘  └───────────┬────────────┘ │
│          │                   │                        │              │
│  ┌───────▼───────────────────▼────────────────────────▼────────────┐ │
│  │              EXISTING NAMMA YATRI BACKEND                       │ │
│  │  (DailyStats, DriverWallet, DemandHotspots, LeaderBoard...)     │ │
│  └─────────────────────────────────────────────────────────────────┘ │
│                                                                      │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │              EXTERNAL DATA SOURCES                               │ │
│  │  Weather API │ Event APIs │ Airport Data │ UPI/Financial APIs    │ │
│  └─────────────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────────────┘
```

---

## 2. Backend Implementation (nammayatri repo)

### 2.1 New Domain Types

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Types/`

#### A. DriverExpense
```haskell
-- Domain/Types/DriverExpense.hs
data ExpenseCategory
  = Fuel
  | Maintenance
  | Insurance
  | VehicleEMI
  | PhoneRecharge
  | Tolls
  | Cleaning
  | Food
  | Fines
  | Other
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data DriverExpense = DriverExpense
  { id :: Id DriverExpense
  , driverId :: Id Person
  , category :: ExpenseCategory
  , amount :: HighPrecMoney
  , currency :: Currency
  , description :: Maybe Text
  , receiptUrl :: Maybe Text
  , expenseDate :: Day
  , isAutoDetected :: Bool        -- UPI auto-detect
  , upiTransactionId :: Maybe Text
  , merchantName :: Maybe Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
```

#### B. DriverEarningsAnalytics
```haskell
-- Domain/Types/DriverEarningsAnalytics.hs
data HourlyEarnings = HourlyEarnings
  { hour :: Int                    -- 0-23
  , earnings :: HighPrecMoney
  , rides :: Int
  , onlineMinutes :: Int
  , earningMinutes :: Int
  , distance :: Meters
  }

data ZoneEarnings = ZoneEarnings
  { geohash :: Text
  , zoneName :: Maybe Text
  , earnings :: HighPrecMoney
  , rides :: Int
  , avgFarePerKm :: HighPrecMoney
  , avgPickupDistance :: Meters
  }

data DriverAnalyticsSummary = DriverAnalyticsSummary
  { driverId :: Id Person
  , periodStart :: Day
  , periodEnd :: Day
  , grossEarnings :: HighPrecMoney
  , totalExpenses :: HighPrecMoney
  , netProfit :: HighPrecMoney
  , totalRides :: Int
  , totalDistance :: Meters
  , totalOnlineHours :: Double
  , earningsPerHour :: HighPrecMoney
  , earningsPerKm :: HighPrecMoney
  , idleTimePercent :: Double
  , cancellationRate :: Double
  , hourlyBreakdown :: [HourlyEarnings]
  , zoneBreakdown :: [ZoneEarnings]
  , cityPercentile :: Maybe Int    -- top X% of drivers
  }
```

#### C. DemandEvent
```haskell
-- Domain/Types/DemandEvent.hs
data EventSource = BookMyShow | Manual | GovernmentCalendar | AirportAPI | WeatherAPI
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DemandEvent = DemandEvent
  { id :: Id DemandEvent
  , cityId :: Id MerchantOperatingCity
  , eventName :: Text
  , eventType :: Text              -- cricket, concert, festival, flight_arrivals, weather
  , venue :: Maybe Text
  , location :: LatLong
  , geohash :: Text
  , startTime :: UTCTime
  , endTime :: Maybe UTCTime
  , estimatedAttendance :: Maybe Int
  , estimatedRideDemand :: Maybe Int
  , source :: EventSource
  , isActive :: Bool
  , metadata :: Maybe Value        -- flexible JSON for source-specific data
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
```

#### D. DriverTaxSummary
```haskell
-- Domain/Types/DriverTaxSummary.hs
data DriverTaxSummary = DriverTaxSummary
  { driverId :: Id Person
  , financialYear :: Text          -- "2025-26"
  , quarter :: Int                 -- 1-4
  , grossIncome :: HighPrecMoney
  , deductibleExpenses :: HighPrecMoney
  , estimatedTaxableIncome :: HighPrecMoney
  , estimatedTaxLiability :: HighPrecMoney
  , tdsDeducted :: HighPrecMoney
  , advanceTaxPaid :: HighPrecMoney
  , remainingLiability :: HighPrecMoney
  , nextDueDate :: Maybe Day
  , nextDueAmount :: Maybe HighPrecMoney
  }
```

#### E. DriverSavingsGoal
```haskell
-- Domain/Types/DriverSavingsGoal.hs
data SavingsGoalType = EmergencyFund | VehicleMaintenance | Education | Custom
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DriverSavingsGoal = DriverSavingsGoal
  { id :: Id DriverSavingsGoal
  , driverId :: Id Person
  , goalType :: SavingsGoalType
  , goalName :: Text
  , targetAmount :: HighPrecMoney
  , currentAmount :: HighPrecMoney
  , autoDeductPerRide :: Maybe HighPrecMoney
  , currency :: Currency
  , isActive :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
```

### 2.2 New API Endpoints

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/API/`

```
-- Analytics APIs
GET  /driver/analytics/summary?from=DATE&to=DATE&period=daily|weekly|monthly
GET  /driver/analytics/hourly?date=DATE
GET  /driver/analytics/zones?from=DATE&to=DATE
GET  /driver/analytics/trends?months=3
GET  /driver/analytics/percentile

-- Expense APIs
POST /driver/expenses
GET  /driver/expenses?from=DATE&to=DATE&category=CATEGORY
GET  /driver/expenses/summary?from=DATE&to=DATE
PUT  /driver/expenses/{expenseId}
DELETE /driver/expenses/{expenseId}
POST /driver/expenses/receipt/upload

-- Demand Intelligence APIs
GET  /driver/demand/forecast?geohash=HASH&hours=4
GET  /driver/demand/events?cityId=ID&from=DATE&to=DATE
GET  /driver/demand/events/{eventId}
POST /driver/demand/events/subscribe
GET  /driver/demand/airport?airportCode=BLR&hours=4
GET  /driver/demand/weather?lat=LAT&lon=LON

-- Tax APIs
GET  /driver/tax/summary?fy=2025-26&quarter=2
GET  /driver/tax/export?fy=2025-26&format=pdf|csv
GET  /driver/tax/deductions

-- Savings APIs
POST /driver/savings/goals
GET  /driver/savings/goals
PUT  /driver/savings/goals/{goalId}
POST /driver/savings/goals/{goalId}/contribute

-- Benefits Marketplace APIs
GET  /driver/benefits/marketplace?category=insurance|fuel|servicing|health
GET  /driver/benefits/marketplace/{offerId}
POST /driver/benefits/marketplace/{offerId}/claim
GET  /driver/benefits/insurance/quotes
POST /driver/benefits/insurance/purchase

-- Notification Preferences
GET  /driver/notifications/preferences
PUT  /driver/notifications/preferences

-- Enhanced LeaderBoard
GET  /driver/leaderboard/earnings?period=weekly&city=BLR
GET  /driver/leaderboard/efficiency?period=weekly&city=BLR
```

### 2.3 New Domain Actions

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/`

| New Module | Functions | Description |
|-----------|----------|-------------|
| `DriverAnalytics.hs` | `getEarningsSummary`, `getHourlyBreakdown`, `getZoneAnalytics`, `getTrends`, `getPercentile` | Core analytics engine. Aggregates from DailyStats, Ride, DriverFee tables |
| `DriverExpenseTracker.hs` | `addExpense`, `listExpenses`, `getExpenseSummary`, `uploadReceipt`, `autoDetectUPIExpense` | Expense CRUD and auto-detection logic |
| `DemandForecast.hs` | `getForecast`, `getEvents`, `subscribeToEvent`, `getAirportIntel`, `getWeatherDemand` | Demand prediction engine |
| `DriverTax.hs` | `getTaxSummary`, `exportTaxData`, `getDeductions`, `calculateAdvanceTax` | India tax compliance calculations |
| `DriverSavings.hs` | `createGoal`, `listGoals`, `contribute`, `autoDeductOnRideEnd` | Micro-savings management |
| `BenefitsMarketplace.hs` | `listOffers`, `getOffer`, `claimOffer`, `getInsuranceQuotes`, `purchaseInsurance` | Benefits and insurance marketplace |
| `SmartNotifications.hs` | `getPreferences`, `updatePreferences`, `triggerDemandAlert`, `triggerEarningsAlert`, `triggerEventAlert` | Intelligent notification orchestration |

### 2.4 Extending Existing Modules

#### DemandHotspots Enhancement
```
File: Domain/Action/UI/DemandHotspots.hs

Current: Geohash frequency tracking (search adds to sorted set, booking removes)
Add:
  - Time-weighted scoring (recent searches weighted higher)
  - Historical pattern detection (same geohash, same day-of-week, same time)
  - Weather correlation factor (fetch weather, multiply demand score)
  - Event proximity boost (events within 2km boost nearby geohash scores)
  - Prediction endpoint: given a future time, predict demand per geohash
```

#### RideSummary Enhancement
```
File: Domain/Action/UI/RideSummary.hs

Current: Returns earnings, distance, rides, tips, cancellation charges per day
Add:
  - Per-hour breakdown aggregation
  - Expense deduction to compute net profit
  - Efficiency metrics (earnings/hour, earnings/km)
  - Comparison with previous period
  - Idle time calculation (online time - ride time)
```

#### LeaderBoard Enhancement
```
File: Domain/Action/UI/LeaderBoard.hs

Current: Rank by totalRides and totalDistance
Add:
  - Earnings-based ranking (opt-in, anonymized)
  - Efficiency ranking (earnings per hour)
  - Percentile calculation
  - Improvement tracking (rank change week-over-week)
```

### 2.5 New External Integrations (shared-kernel)

**Location**: `shared-kernel/lib/mobility-core/src/Kernel/External/`

| Integration | Module | Data Source | Purpose |
|-------------|--------|-------------|---------|
| Weather | `Weather/` | OpenWeatherMap or IMD API | Hyperlocal weather forecasts for demand correlation |
| Events | `Events/` | BookMyShow API, public event feeds | Event data for demand alerts |
| Airport | `Airport/` | Flight data APIs (AviationStack, FlightAware) | Flight arrival/departure data |
| UPI Analytics | `UPIAnalytics/` | Account Aggregator framework | Auto-detect expenses from UPI transactions |
| Tax | `Tax/` | India Income Tax rules engine | Advance tax, TDS, GST calculations |

### 2.6 Kafka Event Streams

**Location**: `Backend/app/kafka-consumers/`

New Kafka topics and consumers:

| Topic | Producer | Consumer | Purpose |
|-------|----------|----------|---------|
| `driver-ride-completed` | Existing ride completion | Analytics aggregator | Update hourly/daily/zone stats in near-real-time |
| `demand-event-created` | Event scraper service | Notification service | Send event alerts to subscribed drivers |
| `weather-alert` | Weather polling service | Notification service | Send weather-demand alerts |
| `expense-auto-detected` | UPI integration | Expense tracker | Create auto-detected expense entries |
| `driver-earnings-milestone` | Analytics aggregator | Notification service | Trigger milestone notifications |
| `savings-auto-deduct` | Ride completion | Savings service | Auto-deduct savings per ride |

### 2.7 Database Schema Changes

New PostgreSQL tables:

```sql
-- Driver expenses
CREATE TABLE driver_expense (
    id VARCHAR(36) PRIMARY KEY,
    driver_id VARCHAR(36) NOT NULL REFERENCES person(id),
    category VARCHAR(50) NOT NULL,
    amount DOUBLE PRECISION NOT NULL,
    currency VARCHAR(10) NOT NULL DEFAULT 'INR',
    description TEXT,
    receipt_url TEXT,
    expense_date DATE NOT NULL,
    is_auto_detected BOOLEAN DEFAULT FALSE,
    upi_transaction_id VARCHAR(100),
    merchant_name VARCHAR(200),
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_driver_expense_driver_date ON driver_expense(driver_id, expense_date);
CREATE INDEX idx_driver_expense_category ON driver_expense(driver_id, category);

-- Demand events
CREATE TABLE demand_event (
    id VARCHAR(36) PRIMARY KEY,
    city_id VARCHAR(36) NOT NULL,
    event_name TEXT NOT NULL,
    event_type VARCHAR(50) NOT NULL,
    venue TEXT,
    lat DOUBLE PRECISION NOT NULL,
    lon DOUBLE PRECISION NOT NULL,
    geohash VARCHAR(12) NOT NULL,
    start_time TIMESTAMP NOT NULL,
    end_time TIMESTAMP,
    estimated_attendance INT,
    estimated_ride_demand INT,
    source VARCHAR(50) NOT NULL,
    is_active BOOLEAN DEFAULT TRUE,
    metadata JSONB,
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_demand_event_city_time ON demand_event(city_id, start_time);
CREATE INDEX idx_demand_event_geohash ON demand_event(geohash);

-- Driver event subscriptions
CREATE TABLE driver_event_subscription (
    id VARCHAR(36) PRIMARY KEY,
    driver_id VARCHAR(36) NOT NULL REFERENCES person(id),
    event_id VARCHAR(36) REFERENCES demand_event(id),
    venue_name TEXT,              -- subscribe to venue (all events)
    alert_minutes_before INT DEFAULT 30,
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP NOT NULL DEFAULT NOW()
);

-- Hourly analytics (materialized/pre-computed)
CREATE TABLE driver_hourly_stats (
    id VARCHAR(36) PRIMARY KEY,
    driver_id VARCHAR(36) NOT NULL REFERENCES person(id),
    stats_date DATE NOT NULL,
    hour INT NOT NULL,            -- 0-23
    earnings DOUBLE PRECISION DEFAULT 0,
    rides INT DEFAULT 0,
    distance_meters INT DEFAULT 0,
    online_minutes INT DEFAULT 0,
    earning_minutes INT DEFAULT 0,
    currency VARCHAR(10) NOT NULL DEFAULT 'INR',
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW(),
    UNIQUE(driver_id, stats_date, hour)
);
CREATE INDEX idx_hourly_stats_driver_date ON driver_hourly_stats(driver_id, stats_date);

-- Zone analytics (materialized/pre-computed)
CREATE TABLE driver_zone_stats (
    id VARCHAR(36) PRIMARY KEY,
    driver_id VARCHAR(36) NOT NULL REFERENCES person(id),
    geohash VARCHAR(12) NOT NULL,
    zone_name TEXT,
    stats_date DATE NOT NULL,
    earnings DOUBLE PRECISION DEFAULT 0,
    rides INT DEFAULT 0,
    distance_meters INT DEFAULT 0,
    avg_pickup_distance_meters INT DEFAULT 0,
    currency VARCHAR(10) NOT NULL DEFAULT 'INR',
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW(),
    UNIQUE(driver_id, geohash, stats_date)
);
CREATE INDEX idx_zone_stats_driver_date ON driver_zone_stats(driver_id, stats_date);

-- Savings goals
CREATE TABLE driver_savings_goal (
    id VARCHAR(36) PRIMARY KEY,
    driver_id VARCHAR(36) NOT NULL REFERENCES person(id),
    goal_type VARCHAR(50) NOT NULL,
    goal_name TEXT NOT NULL,
    target_amount DOUBLE PRECISION NOT NULL,
    current_amount DOUBLE PRECISION DEFAULT 0,
    auto_deduct_per_ride DOUBLE PRECISION,
    currency VARCHAR(10) NOT NULL DEFAULT 'INR',
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);

-- Driver notification preferences
CREATE TABLE driver_notification_preference (
    id VARCHAR(36) PRIMARY KEY,
    driver_id VARCHAR(36) NOT NULL REFERENCES person(id),
    demand_alerts BOOLEAN DEFAULT TRUE,
    event_alerts BOOLEAN DEFAULT TRUE,
    weather_alerts BOOLEAN DEFAULT TRUE,
    earnings_milestones BOOLEAN DEFAULT TRUE,
    expense_reminders BOOLEAN DEFAULT TRUE,
    tax_reminders BOOLEAN DEFAULT TRUE,
    maintenance_reminders BOOLEAN DEFAULT TRUE,
    quiet_hours_start TIME,
    quiet_hours_end TIME,
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW(),
    UNIQUE(driver_id)
);

-- Benefits marketplace
CREATE TABLE benefit_offer (
    id VARCHAR(36) PRIMARY KEY,
    category VARCHAR(50) NOT NULL,
    partner_name TEXT NOT NULL,
    offer_title TEXT NOT NULL,
    offer_description TEXT,
    discount_percent DOUBLE PRECISION,
    discount_amount DOUBLE PRECISION,
    terms_url TEXT,
    image_url TEXT,
    is_active BOOLEAN DEFAULT TRUE,
    valid_from TIMESTAMP,
    valid_until TIMESTAMP,
    city_ids TEXT[],               -- applicable cities, NULL = all
    created_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE TABLE driver_benefit_claim (
    id VARCHAR(36) PRIMARY KEY,
    driver_id VARCHAR(36) NOT NULL REFERENCES person(id),
    offer_id VARCHAR(36) NOT NULL REFERENCES benefit_offer(id),
    claimed_at TIMESTAMP NOT NULL DEFAULT NOW(),
    status VARCHAR(20) NOT NULL DEFAULT 'claimed'
);
```

### 2.8 Redis Data Structures

```
-- Existing (DemandHotspots)
SET    city:{cityId}:hotspot:geohashes          -- all geohashes in city
ZSET   city:{cityId}:geohash:{hash}:searches    -- search frequency sorted set

-- New additions
HASH   driver:{driverId}:today:hourly           -- real-time hourly accumulator
       field: "hour:14:earnings" -> "450.00"
       field: "hour:14:rides" -> "3"
       field: "hour:14:online_min" -> "45"

HASH   driver:{driverId}:goals:daily            -- daily earnings goal progress
       field: "target" -> "2000"
       field: "current" -> "1450"

ZSET   city:{cityId}:demand:predicted:{hour}    -- predicted demand by geohash for future hours
       member: geohash, score: predicted_demand

SET    driver:{driverId}:event:subscriptions     -- subscribed event IDs

STRING city:{cityId}:weather:current             -- cached weather data (TTL 10 min)
STRING city:{cityId}:weather:forecast            -- cached forecast (TTL 30 min)

ZSET   city:{cityId}:leaderboard:earnings:weekly -- earnings-based leaderboard
       member: driverId, score: earnings
```

---

## 3. Frontend Implementation (ny-react-native repo)

### 3.1 New Screens & Components

**Location**: `provider/src/screens/`

#### A. Enhanced EarningsFlow (redesign existing)
```
provider/src/screens/EarningsFlow/
├── EarningsDashboard/           -- NEW: main analytics dashboard
│   ├── View.tsx
│   ├── components/
│   │   ├── EarningsSummaryCard.tsx
│   │   ├── TrendChart.tsx        -- line chart: daily/weekly earnings trend
│   │   ├── HourlyBreakdown.tsx   -- bar chart: earnings by hour
│   │   ├── GoalProgress.tsx      -- circular progress: daily goal
│   │   ├── NetProfitCard.tsx     -- gross - expenses = net
│   │   └── QuickStats.tsx        -- per-hour, per-km, idle%
│   └── hooks/
│       └── useEarningsAnalytics.ts
├── TripAnalytics/                -- NEW: per-trip profitability
│   ├── View.tsx
│   └── components/
│       ├── TripProfitCard.tsx
│       └── TripEfficiencyScore.tsx
├── ZoneAnalytics/                -- NEW: zone-wise earnings map
│   ├── View.tsx
│   └── components/
│       └── ZoneHeatmap.tsx
├── PeerComparison/               -- NEW: percentile & benchmarks
│   ├── View.tsx
│   └── components/
│       ├── PercentileGauge.tsx
│       └── ComparisonChart.tsx
├── PostPaid/                     -- EXISTING
└── PrePaid/                      -- EXISTING
```

#### B. New ExpenseFlow
```
provider/src/screens/ExpenseFlow/
├── ExpenseList/
│   ├── View.tsx
│   └── components/
│       ├── ExpenseCard.tsx
│       ├── CategoryFilter.tsx
│       └── ExpenseSummaryHeader.tsx
├── AddExpense/
│   ├── View.tsx
│   └── components/
│       ├── CategoryPicker.tsx
│       ├── AmountInput.tsx
│       └── ReceiptCapture.tsx     -- camera + OCR
├── ExpenseSummary/
│   ├── View.tsx
│   └── components/
│       ├── CategoryBreakdown.tsx  -- pie chart
│       └── MonthlyComparison.tsx
└── AutoDetectedExpenses/
    └── View.tsx                   -- review & confirm UPI-detected expenses
```

#### C. Enhanced DemandIntelligence (extend HotSpotScreen)
```
provider/src/screens/DemandIntelligence/
├── DemandMap/                     -- enhanced hotspot with predictions
│   ├── View.tsx
│   └── components/
│       ├── PredictiveHeatmap.tsx
│       ├── TimeSlider.tsx         -- slide to see future demand
│       └── DemandLegend.tsx
├── EventAlerts/
│   ├── View.tsx
│   └── components/
│       ├── EventCard.tsx
│       ├── EventDetail.tsx
│       └── SubscribeButton.tsx
├── AirportIntel/
│   ├── View.tsx
│   └── components/
│       ├── FlightArrivals.tsx
│       ├── QueueEstimate.tsx
│       └── QueueDecisionCard.tsx  -- "queue vs. city rides" recommendation
└── WeatherDemand/
    └── View.tsx
```

#### D. New FinancialHub
```
provider/src/screens/FinancialHub/
├── TaxDashboard/
│   ├── View.tsx
│   └── components/
│       ├── TaxSummaryCard.tsx
│       ├── QuarterlyBreakdown.tsx
│       ├── DeductionsList.tsx
│       └── ExportButton.tsx
├── SavingsGoals/
│   ├── View.tsx
│   └── components/
│       ├── GoalCard.tsx
│       ├── CreateGoal.tsx
│       └── ContributeModal.tsx
└── FinancialOverview/
    ├── View.tsx
    └── components/
        ├── IncomeVsExpense.tsx
        └── MonthlyForecast.tsx
```

#### E. Enhanced BenefitsFlow (extend existing)
```
provider/src/screens/BenefitsFlow/
├── Benefits/                      -- EXISTING
├── Marketplace/                   -- NEW
│   ├── View.tsx
│   └── components/
│       ├── CategoryTabs.tsx       -- Insurance, Fuel, Servicing, Health
│       ├── OfferCard.tsx
│       ├── OfferDetail.tsx
│       └── ClaimButton.tsx
├── InsuranceHub/                  -- extend DriverInsuranceScreen
│   ├── View.tsx
│   └── components/
│       ├── MicroInsuranceCard.tsx
│       ├── QuoteComparison.tsx
│       └── ClaimAssistant.tsx
├── VehicleServices/               -- NEW
│   ├── View.tsx
│   └── components/
│       ├── ServiceReminder.tsx
│       ├── NearbyGarages.tsx
│       └── ServiceHistory.tsx
└── ... existing screens
```

#### F. Smart Notifications
```
provider/src/screens/NotificationPreferences/
├── View.tsx
└── components/
    ├── NotificationToggle.tsx
    ├── QuietHoursPicker.tsx
    └── AlertPreview.tsx
```

### 3.2 Shared Components (new)

**Location**: `provider/src/common/components/` or `common/`

```
Charts/
├── LineChart.tsx        -- earnings trends (use react-native-chart-kit or victory-native)
├── BarChart.tsx         -- hourly breakdown
├── PieChart.tsx         -- expense categories
├── CircularProgress.tsx -- goal progress
└── Heatmap.tsx          -- demand heatmap overlay on maps

Cards/
├── MetricCard.tsx       -- reusable stat card (icon, value, label, trend arrow)
├── AlertCard.tsx        -- notification/alert display
└── GoalCard.tsx         -- progress toward goal
```

### 3.3 Navigation Changes

Add new tabs/sections to driver app navigation:

```
Bottom Tab: "Insights" (new tab alongside Home, Rides, Earnings, Profile)
├── Earnings Dashboard (default view)
├── Demand Intelligence
├── Expenses
├── Financial Hub
└── Benefits Marketplace
```

Or alternatively, expand existing "Earnings" tab:
```
Earnings Tab (redesigned)
├── Dashboard (overview with key metrics)
├── Analytics (deep dive: hourly, zone, trip)
├── Expenses
├── Tax & Savings
└── Smart Insights feed (notification-style)
```

### 3.4 State Management

Use existing patterns from the codebase. New state slices:

```typescript
// Analytics state
interface AnalyticsState {
  earningsSummary: EarningsSummary | null;
  hourlyBreakdown: HourlyEarnings[];
  zoneBreakdown: ZoneEarnings[];
  trends: EarningsTrend[];
  percentile: number | null;
  isLoading: boolean;
  selectedPeriod: 'daily' | 'weekly' | 'monthly';
  selectedDate: string;
}

// Expense state
interface ExpenseState {
  expenses: DriverExpense[];
  summary: ExpenseSummary | null;
  pendingAutoDetected: DriverExpense[];
  isLoading: boolean;
}

// Demand Intelligence state
interface DemandState {
  forecast: DemandForecast[];
  events: DemandEvent[];
  subscribedEvents: string[];
  airportData: AirportIntel | null;
  weatherForecast: WeatherData | null;
}
```

---

## 4. Data Pipeline Architecture

### 4.1 Analytics Aggregation Pipeline

```
Ride Completed Event
       │
       ▼
  Kafka Topic: ride-completed
       │
       ▼
  ┌─────────────────────┐
  │ Analytics Consumer  │
  │                     │
  │ 1. Update Redis     │──► driver:{id}:today:hourly (real-time)
  │    hourly stats     │
  │                     │
  │ 2. Update Redis     │──► city:{id}:leaderboard:earnings:weekly
  │    leaderboard      │
  │                     │
  │ 3. Update PG        │──► driver_hourly_stats (persistent)
  │    hourly table     │
  │                     │
  │ 4. Update PG        │──► driver_zone_stats (persistent)
  │    zone table       │
  │                     │
  │ 5. Check goals      │──► If milestone → Kafka: earnings-milestone
  │                     │
  │ 6. Auto-deduct      │──► If savings goal → Kafka: savings-auto-deduct
  │    savings          │
  └─────────────────────┘
```

### 4.2 Demand Prediction Pipeline

```
  ┌─────────────────────────────────────────────┐
  │         Scheduled Jobs (every 15 min)        │
  │                                              │
  │  Weather Poller ──► Redis: weather cache     │
  │  Event Scraper  ──► PG: demand_event table   │
  │  Airport Poller ──► Redis: airport cache     │
  │  Pattern Calc   ──► Redis: predicted demand  │
  └─────────────────────────────────────────────┘
                    │
                    ▼
  ┌─────────────────────────────────────────────┐
  │         Demand Scoring Engine                │
  │                                              │
  │  base_score = historical_pattern(geohash,    │
  │               day_of_week, hour)             │
  │                                              │
  │  weather_factor = if rain then 1.4           │
  │                   elif extreme_heat then 1.2 │
  │                   else 1.0                   │
  │                                              │
  │  event_factor = nearby_events.sum(           │
  │    attendance * proximity_decay)             │
  │                                              │
  │  supply_factor = active_drivers_in_zone /    │
  │                  historical_avg_drivers       │
  │                                              │
  │  predicted_demand = base_score               │
  │    * weather_factor * event_factor           │
  │    / supply_factor                           │
  └─────────────────────────────────────────────┘
                    │
                    ▼
  Redis ZSET: city:{id}:demand:predicted:{hour}
  (served to drivers via API / push notifications)
```

### 4.3 Notification Pipeline

```
  Trigger Sources:
  ├── Demand surge detected (DemandHotspots threshold)
  ├── Event ending soon (DemandEvent.endTime - 30min)
  ├── Weather alert (Weather API severe condition)
  ├── Earnings milestone (goal X% reached)
  ├── Tax deadline (calendar-based)
  └── Maintenance due (km-based)
          │
          ▼
  ┌─────────────────────────┐
  │ Smart Notification      │
  │ Service                 │
  │                         │
  │ 1. Check driver prefs   │ ← driver_notification_preference
  │ 2. Check quiet hours    │
  │ 3. Rate limit (max 5/hr)│
  │ 4. Personalize message  │ ← driver language, city
  │ 5. Send via FCM/APNS    │ ← existing Notification in shared-kernel
  └─────────────────────────┘
```

---

## 5. Phase-wise Implementation Breakdown

### Phase 1: Foundation (Months 1-3)

#### Month 1: Core Analytics Backend + Basic Frontend
**Backend (nammayatri)**:
- [ ] Create `DriverExpense` domain type, Beam schema, queries
- [ ] Create `driver_hourly_stats` and `driver_zone_stats` tables + Beam schema
- [ ] Implement `DriverAnalytics.hs` — `getEarningsSummary`, `getHourlyBreakdown`
- [ ] Implement `DriverExpenseTracker.hs` — CRUD operations
- [ ] Add Kafka consumer for ride-completed → hourly/zone stat aggregation
- [ ] Extend `RideSummary` to include expense deduction, efficiency metrics
- [ ] API endpoint registration in Servant

**Frontend (ny-react-native)**:
- [ ] Design and build EarningsDashboard screen with summary cards
- [ ] Build TrendChart component (daily/weekly earnings line chart)
- [ ] Build GoalProgress component (daily earning goal)
- [ ] Build basic ExpenseList and AddExpense screens
- [ ] API integration hooks for analytics and expenses

#### Month 2: Demand Intelligence + Expense Enhancement
**Backend**:
- [ ] Create `DemandEvent` domain type, table, queries
- [ ] Integrate Weather API in shared-kernel (`Kernel/External/Weather/`)
- [ ] Enhance `DemandHotspots.hs` with time-series prediction
- [ ] Build event scraper service (BookMyShow integration)
- [ ] Implement `DemandForecast.hs` — forecast, events, weather endpoints
- [ ] Build demand scoring engine (historical patterns + weather + events)

**Frontend**:
- [ ] Build DemandMap with predictive heatmap and time slider
- [ ] Build EventAlerts screen with subscription
- [ ] Build WeatherDemand alert cards
- [ ] Enhance ExpenseFlow with receipt capture (camera + basic OCR)
- [ ] Build CategoryBreakdown pie chart

#### Month 3: Tax Basics + Notifications + LeaderBoard Enhancement
**Backend**:
- [ ] Implement `DriverTax.hs` — basic advance tax calculator for India
- [ ] Create `driver_notification_preference` table
- [ ] Implement `SmartNotifications.hs` with preference management
- [ ] Build notification triggers for demand surge, events, milestones
- [ ] Enhance `LeaderBoard.hs` with earnings percentile, efficiency ranking

**Frontend**:
- [ ] Build TaxDashboard with quarterly summary
- [ ] Build NotificationPreferences screen
- [ ] Build PeerComparison screen with percentile gauge
- [ ] Enhanced LeaderBoard UI with multiple ranking types
- [ ] Polish and integrate push notification handling

### Phase 2: Intelligence (Months 4-6)

#### Month 4: Airport Intelligence + Trip Analytics
**Backend**:
- [ ] Integrate airport flight data API in shared-kernel
- [ ] Build airport queue estimation logic
- [ ] Implement trip profitability calculation (fare - estimated fuel)
- [ ] Build "queue vs. city rides" decision engine

**Frontend**:
- [ ] Build AirportIntel screen with flight arrivals, queue estimate
- [ ] Build QueueDecisionCard component
- [ ] Build TripAnalytics screen with profitability per trip
- [ ] Build TripEfficiencyScore component

#### Month 5: AI-Powered Recommendations + Auto-Expense
**Backend**:
- [ ] Build ML model for personalized "when/where to drive" recommendations
- [ ] Implement UPI transaction auto-detection (Account Aggregator integration)
- [ ] Build vehicle maintenance km-based reminder system
- [ ] Earnings forecast model (based on driver's historical patterns)

**Frontend**:
- [ ] Build Smart Insights feed (personalized recommendation cards)
- [ ] Build AutoDetectedExpenses review screen
- [ ] Build vehicle maintenance reminder UI
- [ ] Build earnings forecast card

#### Month 6: Polishing + Multi-Language + Beta Launch
**Backend**:
- [ ] Notification content in 8+ Indian languages
- [ ] Performance optimization: query caching, Redis pipeline optimization
- [ ] A/B testing framework for analytics features
- [ ] Comprehensive API testing and load testing

**Frontend**:
- [ ] Full multi-language support for all new screens
- [ ] Offline mode for earnings dashboard
- [ ] Low-end device optimization (lazy loading, virtualized lists)
- [ ] End-to-end user testing, UX polish
- [ ] Beta release to select driver groups

### Phase 3: Financial Services (Months 7-12)

- [ ] Micro-savings goals system
- [ ] Micro-insurance marketplace
- [ ] Full tax compliance suite (GST, TDS tracking, ITR export)
- [ ] Benefits marketplace (fuel, servicing, health partnerships)
- [ ] Vehicle loan facilitation (earnings-based eligibility)
- [ ] Driver community features
- [ ] Enterprise analytics dashboard (B2B)

### Phase 4: Global & Multi-Modal (Months 12-18)

- [ ] Multi-modal: delivery/logistics worker support
- [ ] ONDC integration for cross-platform earnings
- [ ] International market adaptation (currency, tax, language)
- [ ] Open API for third-party integrations
- [ ] Community-contributed features (open-source)

---

## 6. Cross-Repo Change Summary

### nammayatri (Backend)

| Directory | Changes |
|-----------|---------|
| `Backend/app/provider-platform/.../Domain/Types/` | New: `DriverExpense.hs`, `DriverEarningsAnalytics.hs`, `DemandEvent.hs`, `DriverTaxSummary.hs`, `DriverSavingsGoal.hs`, `BenefitOffer.hs`, `DriverNotificationPreference.hs` |
| `Backend/app/provider-platform/.../Domain/Action/UI/` | New: `DriverAnalytics.hs`, `DriverExpenseTracker.hs`, `DemandForecast.hs`, `DriverTax.hs`, `DriverSavings.hs`, `BenefitsMarketplace.hs`, `SmartNotifications.hs`. Modified: `DemandHotspots.hs`, `RideSummary.hs`, `LeaderBoard.hs` |
| `Backend/app/provider-platform/.../API/` | New API route definitions for all new endpoints |
| `Backend/app/provider-platform/.../Storage/` | New Beam schemas and queries for all new tables |
| `Backend/app/kafka-consumers/` | New consumers: analytics aggregator, event processor, notification trigger |
| Database migrations | 8 new tables, indexes |

### shared-kernel

| Directory | Changes |
|-----------|---------|
| `lib/mobility-core/src/Kernel/External/Weather/` | New: Weather API integration (OpenWeatherMap / IMD) |
| `lib/mobility-core/src/Kernel/External/Events/` | New: Event data source integration |
| `lib/mobility-core/src/Kernel/External/Airport/` | New: Flight data API integration |

### ny-react-native (Frontend)

| Directory | Changes |
|-----------|---------|
| `provider/src/screens/EarningsFlow/` | Major redesign: new EarningsDashboard, TripAnalytics, ZoneAnalytics, PeerComparison |
| `provider/src/screens/ExpenseFlow/` | New flow: ExpenseList, AddExpense, ExpenseSummary, AutoDetected |
| `provider/src/screens/DemandIntelligence/` | New flow: DemandMap, EventAlerts, AirportIntel, WeatherDemand |
| `provider/src/screens/FinancialHub/` | New flow: TaxDashboard, SavingsGoals, FinancialOverview |
| `provider/src/screens/BenefitsFlow/` | Extended: Marketplace, InsuranceHub, VehicleServices |
| `provider/src/screens/NotificationPreferences/` | New: notification preference management |
| `common/` or `provider/src/common/components/` | New shared chart components, metric cards |
| Navigation config | New "Insights" tab or expanded "Earnings" tab |

### euler-hs (Framework)

| Changes | Description |
|---------|-------------|
| Minimal | May need new DB connection pool configs if analytics queries require read replicas |

---

## 7. Infrastructure Requirements

| Component | Current | Additional Needed |
|-----------|---------|-------------------|
| **PostgreSQL** | Existing cluster | Read replica for analytics queries; new tables ~5GB initial, growing with drivers |
| **Redis** | Existing (Hedis) | Additional memory for hourly accumulators, predicted demand (~2GB per city) |
| **Kafka** | Existing | 6 new topics, 3 new consumer groups |
| **Object Storage** | Existing (for receipts) | Receipt image storage (~500MB/month initial) |
| **External APIs** | Maps, Payment, SMS | Add: Weather API, Event API, Airport API (~$200-500/month) |
| **Cron/Scheduler** | Existing | New scheduled jobs: weather polling (10 min), event scraping (1 hr), pattern computation (15 min) |

---

## 8. Testing Strategy

| Layer | Approach |
|-------|----------|
| **Unit Tests** | Haskell: HSpec for domain logic (tax calculator, demand scoring, analytics aggregation) |
| **Integration Tests** | API endpoint tests with test database, Kafka consumer tests |
| **Frontend** | Jest + React Native Testing Library for components, Detox for E2E |
| **Load Testing** | Analytics endpoints with 100K concurrent driver requests |
| **A/B Testing** | Feature flags for gradual rollout, measure engagement metrics |
| **UAT** | Beta group of 500 drivers in Bangalore for 2-week validation |

---

## 9. Estimated Team Composition

| Role | Count | Focus |
|------|-------|-------|
| Backend Engineer (Haskell) | 2-3 | Analytics engine, demand forecast, tax module, new APIs |
| Frontend Engineer (React Native) | 2 | Dashboard UI, charts, new flows |
| Data Engineer | 1 | Kafka pipelines, analytics aggregation, ML model |
| Product Designer | 1 | UX for analytics, simplicity for low-literacy drivers |
| QA Engineer | 1 | Testing across Android devices, load testing |
| Product Manager | 1 | Feature prioritization, driver interviews, metrics |

**Total**: 8-10 people for Phase 1-2 (6 months)

---

## 10. Open Questions & Decisions Needed

1. **Premium tier pricing**: ₹99/mo, ₹199/mo, or ₹299/mo? Need driver willingness-to-pay research.
2. **UPI auto-detection**: Account Aggregator integration requires RBI-compliant TSP. Which AA to partner with?
3. **Weather API**: OpenWeatherMap (global, $) vs. IMD API (India-specific, free but limited)?
4. **Event data**: Build own scraper vs. partner with BookMyShow for API access?
5. **Airport data**: Public airport APIs vary by city. Need city-by-city assessment.
6. **Tax engine**: Build in-house vs. integrate ClearTax/Quicko API?
7. **Chart library**: react-native-chart-kit vs. victory-native vs. react-native-skia charts?
8. **Read replica**: Separate analytics read replica DB vs. query optimization on existing?
9. **Feature flags**: Use existing CAC (remote config) system or add dedicated feature flag service?
10. **Multi-modal timeline**: When to start logistics/delivery worker support?
