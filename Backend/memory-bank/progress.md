# Progress Tracking

## What Works (Current State)

### Core Infrastructure ✅
- **Multi-package Cabal project**: 40+ packages well-organized
- **Nix development environment**: Reproducible builds and dependencies
- **Service architecture**: Clear separation between rider/provider platforms
- **Build system**: Functional with both Cabal and Nix options

### Platform Services ✅
- **Rider Platform**: Core customer-facing APIs operational
  - `rider-app`: Main customer APIs
  - `rider-app-scheduler`: Background job processing
  - `search-result-aggregator`: Search consolidation
  - `public-transport-rider-platform`: Public transport integration

- **Provider Platform**: Driver/fleet management operational
  - `dynamic-offer-driver-app`: Driver APIs and business logic
  - `driver-offer-allocator`: Ride allocation service
  - Various supporting services (drainer, etc.)

### Shared Services ✅
- **beckn-spec & beckn-services**: BECKN/ONDC protocol implementation
- **location-updates**: Real-time tracking capabilities
- **payment**: Payment processing framework
- **scheduler**: Background job management
- **shared-services**: Common utilities and patterns

### Development Tooling ✅
- **Testing infrastructure**: Unit tests, integration tests, load testing
- **API documentation**: Swagger/OpenAPI generation
- **Monitoring**: Prometheus, Grafana integration
- **Development tools**: ghcid, profiling, mock services

## Current Status

### Memory Bank Implementation 🔄
- **Core files created**: All 6 essential memory bank files established
- **Project understanding**: High-level architecture and patterns documented
- **Integration pending**: Need to configure Cline workflows and test

### Knowledge State 📋
- **Architecture**: Comprehensive - 40+ microservices, detailed service map
- **Technology stack**: Complete - Haskell + 15+ external integrations
- **Business flows**: Detailed - 5 fare policy types, 24+ allocation jobs
- **Protocol compliance**: Specific - BECKN V1/V2, ONDC, multi-modal support
- **Database design**: Clear - Multi-schema PostgreSQL + ClickHouse analytics
- **External systems**: Mapped - Payment, verification, communication webhooks

## NYRegular Feature (Backend) - Progress Tracker

**Overall Goal:** Implement the backend functionality for "NYRegular" (recurring/subscription-based rides with fixed pricing). **Existing BECKN API call flows (`search`, `select`, `init`, `confirm`, etc.) and their formats must remain unchanged and not break existing functionality.**

**Status:** Planning Phase - Detailed plan formulated.

**Key Components & Plan Summary:**
1.  **New Database Table (`rider-app`): `ny_regular_subscriptions`**
    *   Stores subscription terms: user details, locations, vehicle category, recurrence rules, agreed fixed price (amount, currency, breakup details), BPP info, original quote ID, status (ACTIVE, PAUSED, CANCELLED).
    *   An entry is created and persisted here when a user successfully reserves/creates an NYRegular subscription.
2.  **New Database Table (`rider-app`): `ny_regular_instance_log`**
    *   Lightweight log: `instance_transaction_id` (PK), `ny_regular_subscription_id` (FK), `scheduled_pickup_time`, `automation_status` (e.g., PENDING, SEARCH_SENT, BOOKING_INITIATED, CONFIRMED, FAILED).
    *   Provides traceability for automated instances and state of automation, without altering core tables.
3.  **Existing `search_request` Table (`rider-app`):**
    *   No schema changes.
    *   Scheduler (Child Job) creates standard `SearchRequest` records for each instance. The `SearchRequest.id` becomes the `instance_transaction_id` for logging in `ny_regular_instance_log`.
4.  **Existing `booking` Table (`rider-app`):**
    *   No schema changes.
    *   Existing BECKN handlers create `Booking` records. `Booking.transaction_id` links to the `instance_transaction_id`.
5.  **API Endpoints (`rider-app`):** (Following same format as existing APIs)
    *   For users to initiate a quote for an NYRegular ride (BAP sends special `/search` to BPPs).
    *   For users to create/confirm the `NyRegularSubscription` with the agreed fixed price.
    *   Standard CRUD for managing subscriptions (view, update status e.g., pause/cancel).
6.  **Scheduler Jobs (`rider-app-scheduler`):**
    *   **Master Cron Job (`NyRegularMasterSchedulerJob`):**
        *   Runs daily.
        *   Reads the `ny_regular_subscriptions` table.
        *   Checks if any subscription has a booking instance due for that day (or within a look-ahead window), considering `ny_regular_instance_log` to avoid re-processing.
        *   Creates/enqueues Child Jobs as needed for these instances.
    *   **Child Job (`ProcessSingleNyRegularInstanceJob`):**
        *   **Timing:** Scheduled to run at `Calculated_Booking_Time - X_configurable_buffer_minutes`.
        *   Receives parameters for one instance (including `ny_regular_subscription_id`, `scheduled_pickup_time`, fixed price details, target BPP info).
        *   Generates a new unique `instance_transaction_id`.
        *   Logs to `ny_regular_instance_log`.
        *   **Automates Search:** Constructs and sends a BECKN `/search` message (using `instance_transaction_id` as `context.transaction_id`) to the specific BPP. Injects fixed price details and "reserved ride" tags into the BECKN `/search` message. This `/search` triggers the creation of a `SearchRequest` DB record by existing mechanisms.
        *   **Automates Full Flow:** After the system-triggered `/search` and receiving `/on_search` (where BPP honors fixed price tags):
            *   The system will automatically proceed to `/select` the appropriate offer.
            *   Then automatically call `/init`.
            *   Then automatically call `/confirm`.
            *   The entire write process (creation of `SearchRequest`, `Booking` entities) is completed programmatically by leveraging existing handlers.
        *   Updates `ny_regular_instance_log.automation_status` based on success (e.g., `Booking` confirmed via event from `/on_confirm` handler) or failure.
7.  **Modifications (`dynamic-offer-driver-app` - BPP):**
    *   Logic to recognize an "NYRegular quote request" tag in `/search` and return a fixed, quotable price.
    *   Logic to recognize "reserved ride" tags + fixed price details in an incoming `/search` (from Child Job) and use that fixed price in its `/on_search` response.

**Tasks:**

**Phase 1: Core NYRegular Subscription Management (`rider-app`)**
*   [X] **Task 1.1 (DB & Haskell):** Define `ny_regular_subscriptions` schema using Namma DSL in `spec/Storage/`. Generate SQL migration and Haskell Beam/domain types.
    *   Haskell Types / (SQL Types):
    *   `id`: `Id NyRegularSubscription` (PK, character(36))
    *   `userId`: `Id Person` (character(36))
    *   `pickupLocation`: `Location` (custom type)
    *   `dropoffLocation`: `Location` (custom type)
    *   `vehicleServiceTier`: `Maybe ServiceTierType` (nullable, text)
    *   `startDatetime`: `UTCTime` (timestamp with time zone)
    *   `recurrenceRuleDays`: `[DayOfWeek]` (text[])
    *   `scheduledTimeOfDay`: `TimeOfDay` (time)
    *   `recurrenceEndDate`: `Maybe Day` (nullable, date)
    *   `fixedPrice`: `Maybe Price` (nullable, custom type)
    *   `fixedPriceBreakupDetails`: `Maybe Value` (nullable, json)
    *   `fixedPriceExpiryDate`: `Maybe UTCTime` (nullable, timestamp with time zone)
    *   `initialBppQuoteId`: `Maybe Text` (nullable, text)
    *   `bppId`: `Text` (text)
    *   `status`: `NyRegularSubscriptionStatus` (enum, text, default 'NEW')
    *   `pauseStartDate`: `Maybe UTCTime` (nullable, timestamp with time zone)
    *   `pauseEndDate`: `Maybe UTCTime` (nullable, timestamp with time zone)
    *   `createdAt`: `UTCTime` (timestamp with time zone)
    *   `updatedAt`: `UTCTime` (timestamp with time zone)
    *   `metadata`: `Maybe Value` (nullable, json)
    *   (Implicit `merchant_id`, `merchant_operating_city_id` auto-added by DSL generator)
*   [X] **Task 1.2 (DB & Haskell):** Define `ny_regular_instance_log` schema using Namma DSL in `spec/Storage/`. Generate SQL migration and Haskell Beam/domain types.
    *   Haskell Types / (SQL Types):
    *   `instanceTransactionId`: `Text` (PK, text)
    *   `nyRegularSubscriptionId`: `Id NyRegularSubscription` (character(36), indexed via SecondaryKey)
    *   `scheduledPickupTime`: `UTCTime` (timestamp with time zone)
    *   `automationStatus`: `NyRegularInstanceAutomationStatus` (enum, text)
    *   `createdAt`: `UTCTime` (timestamp with time zone)
    *   `updatedAt`: `UTCTime` (timestamp with time zone)
    *   (Implicit `merchant_id`, `merchant_operating_city_id` auto-added by DSL generator)
*   [ ] **Task 1.3 (API):** Implement API endpoint to initiate a fixed price quote for an NYRegular subscription (`/ny-regular/subscriptions/initiate-quote`).
*   [ ] **Task 1.4 (API):** Implement API endpoint to create/confirm an `NyRegularSubscription` (`/ny-regular/subscriptions/create`).
*   [ ] **Task 1.5 (API):** Implement API endpoints for managing `NyRegularSubscription` entries (GET list, GET by ID, PUT to update status e.g., PAUSE/CANCEL).
*   [ ] **Task 1.6 (Queries):** Implement DB query functions for `ny_regular_subscriptions` and `ny_regular_instance_log` (using generated types).

**Phase 2: Scheduler Implementation (`rider-app-scheduler`)**
*   [ ] **Task 2.1 (Scheduler):** Define and implement Master Cron Job (`NyRegularMasterSchedulerJob`).
*   [ ] **Task 2.2 (Scheduler):** Define and implement Child Job (`ProcessSingleNyRegularInstanceJob`).
*   [ ] **Task 2.3 (Config):** Make the child job trigger buffer (`X` minutes) configurable.
*   [ ] **Task 2.4 (Scheduler):** Implement robust outcome tracking for Child Job (updating `ny_regular_instance_log` based on events from `Booking` status changes or other reliable mechanism).

**Phase 3: BPP Modifications (`dynamic-offer-driver-app`)**
*   [ ] **Task 3.1 (BPP Logic):** Modify `/search` handler for "NYRegular quote request" tag.
*   [ ] **Task 3.2 (BPP Logic):** Modify `/search` handler for "reserved ride instance" tags (honor fixed price).
*   [ ] **Task 3.3 (BPP Testing):** Test BPP compliance.

**Phase 4: Testing & Integration**
*   [ ] **Task 4.1 (Testing):** Unit tests for new logic in `rider-app` and `rider-app-scheduler`.
*   [ ] **Task 4.2 (Testing):** Unit tests for BPP modifications.
*   [ ] **Task 4.3 (Integration Testing):** End-to-end NYRegular flow.
*   [ ] **Task 4.4 (Monitoring):** Add logging/metrics for NYRegular feature.

**Open Questions / Areas for Further Investigation (before starting relevant tasks):**
*   [X] **Q1 (Resolved):** BECKN Tag strategy defined.
*   [X] **Q2 (Clarified):** NYRegular is a separate flow, not using `Booking.isScheduled`.
*   [X] **Q3 (Clarified):** Automation Success Confirmation will be event-driven (listening to events from `/on_confirm` or `/on_status` related to `Booking`).
*   [ ] **Q4 (Deferred):** Detailed error handling/notifications for failed automated bookings will be a separate task.
*   [X] **Q_New (Ride Entity - Clarified):** A `Ride` entity is created as usual if a driver is assigned; `Booking` status `TRIP_ASSIGNED` is a key indicator for the scheduler.
*   [ ] **Q_ExistingSchedulers:** Confirm no overlap or interference with existing scheduled ride jobs (e.g., `ScheduledRideNotificationsToDriver`) if they operate on different criteria or tables. (This is mostly resolved by making NYRegular distinct, but a quick check is good).

## What's Left to Build/Explore (General Project)

### Implementation Deep Dives 🔍
1. **Core Algorithm Implementations**
   - Supply-demand ratio calculation specifics (geohash-based)
   - Congestion charge calculation with ML-driven smart tips
   - Driver allocation scoring and preference matching
   - Location tracking with snap-to-road algorithms

2. **Business Rule Engine**
   - Yudhishthira decision engine implementation
   - CAC/SuperPosition feature flag logic
   - Dynamic pricing rule evaluation
   - Driver onboarding workflow state machines

3. **Real-time Systems**
   - Kafka event processing patterns
   - Redis-based job scheduling mechanics
   - Location update streaming architecture
   - Notification delivery optimization

### Development Workflows 🛠️
1. **Memory Bank Integration**
   - Configure Cline custom instructions
   - Test Plan/Act mode workflows
   - Establish update procedures
   - Validate context preservation

2. **Development Processes**
   - Understand common development tasks
   - Identify frequent modification patterns
   - Document debugging approaches
   - Establish testing workflows

### Operational Understanding 📊
1. **Performance Characteristics**
   - Current system performance profiles
   - Bottleneck identification
   - Scaling patterns and limits
   - Resource utilization patterns

2. **Deployment & Operations**
   - Production deployment strategies
   - Monitoring and alerting patterns
   - Error handling and recovery
   - Maintenance procedures

## Known Issues & Limitations

### Development Environment
- **Linker issues** on some systems (segmentation faults during linking)
  - **Workaround**: `ulimit -s 9999`
- **VSCode integration** requires specific setup for direnv
- **Nix cache** setup critical for reasonable build times

### System Complexity
- **40+ packages** create complexity in dependency management
- **Multi-service coordination** requires careful development orchestration
- **BECKN protocol compliance** adds integration complexity

### Documentation Gaps
- **Business logic details** not fully documented in code
- **API interaction patterns** need exploration
- **Performance characteristics** not well documented
- **Operational runbooks** may be incomplete

## Evolution of Project Decisions

### Architecture Evolution
- **Started with**: Monolithic approach considerations
- **Evolved to**: Microservices with clear domain boundaries
- **Current state**: Well-structured platform separation
- **Rationale**: Scalability and team development efficiency

### Technology Choices
- **Language choice**: Haskell for type safety in critical business logic
- **Build system**: Nix for reproducibility, Cabal for development speed
- **Protocol adoption**: BECKN/ONDC for ecosystem compatibility
- **Database strategy**: PostgreSQL for transactional consistency

### Development Approach
- **Initial focus**: Core functionality implementation
- **Current focus**: Scalability and integration patterns
- **Future direction**: Performance optimization and operational excellence

## Success Metrics & Goals

### Immediate Goals (Next 30 days)
- [ ] Complete memory bank integration with Cline workflows
- [ ] Deep dive into one core business flow (e.g., booking process)
- [ ] Establish efficient development workflow patterns
- [ ] Document key debugging and troubleshooting approaches

### Medium-term Goals (Next 90 days)
- [ ] Comprehensive understanding of all major business flows
- [ ] Performance optimization identification and implementation
- [ ] Integration testing enhancement
- [ ] Operational monitoring improvement

### Long-term Vision
- [ ] Contribute to open-source BECKN ecosystem
- [ ] Scale to handle 100,000+ concurrent users
- [ ] Multi-city deployment with localized features
- [ ] Advanced analytics and ML-driven optimizations

## Risk Assessment

### Technical Risks
- **Complexity management**: Large codebase coordination
- **Performance scaling**: High-concurrency requirements
- **Integration dependencies**: External service reliability
- **Protocol evolution**: BECKN/ONDC standard changes

### Mitigation Strategies
- **Modular architecture**: Continue microservices approach
- **Comprehensive testing**: Maintain high test coverage
- **Monitoring**: Proactive performance monitoring
- **Documentation**: Maintain updated technical documentation
