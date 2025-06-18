# System Patterns & Architecture

## Overall Architecture

### Microservices Architecture (40+ Services)
The system follows a sophisticated microservices pattern with clear domain boundaries:

```
├── Rider Platform (Customer Journey)
├── Provider Platform (Driver/Fleet Management)
├── Shared Services (Cross-platform utilities)
├── Dashboard Services (Operations & Analytics)
├── Kafka Consumers (Event Processing)
├── Mock Services (Development & Testing)
└── Utility Services (Supporting functions)
```

### Core Platforms Detail

#### Rider Platform Services
- **rider-app** (Main): Customer APIs, search, booking, payment (Port: 8013)
- **rider-app-scheduler**: Background jobs, notifications, scheduled tasks
- **rider-app-drainer**: Data pipeline processing for analytics
- **search-result-aggregator**: Multi-provider search result consolidation
- **public-transport-rider-platform**: FRFS services for buses/metro
- **public-transport-search-consumer**: Async search processing

#### Provider Platform Services
- **dynamic-offer-driver-app** (Main): Driver APIs, ride management (Port: 8016)
- **driver-offer-allocator**: Core allocation engine (Port: 9996)
- **dynamic-offer-driver-drainer**: Analytics data processing

#### Dashboard Services
- **rider-dashboard**: Customer operations dashboard
- **provider-dashboard**: Driver/fleet operations dashboard
- **safety-dashboard**: Safety monitoring and management

### Communication Patterns

#### BECKN/ONDC Protocol
- Standardized message format between platforms
- Event-driven communication for state changes
- Asynchronous processing for scalability

#### Internal Communication
- Direct API calls for synchronous operations
- Kafka-based messaging for async operations
- Shared database for transactional consistency

## Key Design Patterns

### Domain-Driven Design
- Clear domain boundaries between rider and provider contexts
- Shared kernel for common business logic
- Anti-corruption layers for external integrations

### Event Sourcing
- State changes captured as events
- Audit trail for all business operations
- Eventual consistency across services

### CQRS (Command Query Responsibility Segregation)
- Separate read and write models
- Optimized queries for different use cases
- Scalable data access patterns

## Critical Implementation Paths

### Ride Booking Flow
1. **Search Request**: Rider initiates search
2. **Provider Query**: System queries available providers
3. **Offer Generation**: Providers respond with offers
4. **Selection**: Rider selects preferred option
5. **Allocation**: System assigns driver
6. **Tracking**: Real-time location updates
7. **Completion**: Payment and feedback

### Driver Allocation Algorithm (24+ Job Types)
**Core Allocation Jobs:**
- `SendSearchRequestToDriver`: Main driver search distribution
- `UnblockDriver` & `UnblockSoftBlockedDriver`: Driver availability management
- `SoftBlockNotifyDriver`: Driver notification system

**Financial Management:**
- `CalculateDriverFees`: Driver fee calculation and processing
- `BadDebtCalculation`: Financial risk management
- `MandateExecution`: Payment mandate processing
- `DriverReferralPayout`: Referral payment distribution

**Analytics & Optimization:**
- `SupplyDemand`: Real-time supply-demand ratio calculations
- `CongestionCharge`: Dynamic congestion pricing
- `Daily/Weekly/Monthly/Quarterly`: Time-based analytics jobs

**Scheduled Operations:**
- `ScheduledRideNotificationsToDriver`: Advance booking notifications
- `ScheduledRideAssignedOnUpdate`: Scheduled ride management
- `CheckExotelCallStatusAndNotifyBAP`: Call status verification

## Component Relationships

### Shared Libraries (`lib/`)
- **beckn-spec**: BECKN/ONDC protocol implementations (V1 & V2)
- **beckn-services**: Common BECKN service logic and utilities
- **shared-services**: URL shortener, issue management, registry services
- **location-updates**: Real-time tracking with geospatial calculations
- **payment**: Juspay payment gateway abstractions
- **scheduler**: Redis-based job scheduling system
- **producer**: Kafka event production services
- **yudhishthira**: Decision engine for business rules

### External Dependencies
- **Database**: PostgreSQL (atlas_driver_offer_bpp, atlas_app schemas)
- **Analytics**: ClickHouse for event analytics and metrics
- **Cache**: Redis (single + cluster) for sessions, location data
- **Message Queue**: Kafka with multiple topics (location-updates, broadcast-messages)
- **Maps**: OSRM for routing, snap-to-road, geospatial calculations
- **Payments**: Juspay (payment + payout), multiple webhook integrations
- **Verification**: Idfy, HyperVerge for document verification
- **Communication**: SMS services, FCM notifications, Exophone calls
- **Storage**: S3 for file uploads and document storage
- **Translation**: Google Translate API for multilingual support

## Advanced Business Logic Patterns

### Fare Policy System (5 Types)
**Progressive Fare Policy:**
- Distance-based progressive pricing
- Congestion charge multipliers (BaseFareAndExtraDistanceFare, ExtraDistanceFare)
- Night shift bounds and surge pricing
- Platform fee models (Subscription, FixedAmount, SlabBased, NoCharge)

**Complex Pricing Components:**
- Service charges, parking charges, per-stop charges
- Government charges, toll charges, PET charges
- Insurance charges, card processing fees
- Smart tip suggestions with ML-driven reasoning
- Supply-demand ratio adjustments per geohash

**Public Transport Fare Management:**
- Stage-based progressive fares with route-stop mapping
- Direct stop-to-stop fare calculations
- Service tier differentiation (vehicle types)
- Time-bound fare validity and discount applications

## Scalability Patterns

### Horizontal Scaling
- Stateless service design
- Load balancer distribution
- Database read replicas
- Cache clustering

### Performance Optimization
- Connection pooling
- Query optimization
- Caching strategies (L1/L2 cache)
- Asynchronous processing

## Security Patterns

### Authentication & Authorization
- JWT-based session management
- Role-based access control
- API key management for external services

### Data Protection
- Encryption at rest and in transit
- PII data handling compliance
- Audit logging for sensitive operations