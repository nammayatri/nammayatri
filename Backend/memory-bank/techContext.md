# Technical Context

## Core Technologies

### Programming Language
- **Haskell**: Functional programming language with strong type system
- **GHC**: Glasgow Haskell Compiler for building applications
- **Cabal**: Package manager and build system

### Build & Development Environment
- **Nix**: Declarative package manager and build system
- **direnv**: Automatic environment management
- **Cabal Project**: Multi-package project management
- **HLS**: Haskell Language Server for IDE support

### Infrastructure & Services
- **PostgreSQL**: Multi-schema setup (atlas_driver_offer_bpp, atlas_app)
- **ClickHouse**: Analytics database for event tracking and metrics
- **Redis**: Both single instance (6379) and cluster (30001) configurations
- **Kafka**: Event streaming with topics (location-updates, broadcast-messages, dynamic-offer-driver-events-updates)
- **Docker**: Containerization for deployment
- **OSRM**: Routing engine with snap-to-road, distance calculations
- **CAC (Client Application Configuration)**: Feature management system
- **SuperPosition**: Advanced feature flagging and A/B testing

## Development Setup

### Prerequisites
1. **Nix Installation**: Required for reproducible builds
2. **direnv**: Automatic shell environment management
3. **VSCode**: Recommended IDE with Haskell extensions

### Development Workflow
```bash
# Environment setup
ln -s .envrc.backend .envrc
direnv allow

# Building
cd Backend
cabal build all

# Running services
, run-mobility-stack-dev

# Testing
cabal test all
```

### Port Configuration & External Integrations
| Service | Port | Key Features |
|---------|------|--------------|
| rider-app | 8013 | Customer APIs, FRFS, IGM, Payment webhooks |
| beckn-gateway | 8015 | BECKN protocol gateway |
| dynamic-offer-driver-app | 8016 | Driver APIs, Multiple webhooks |
| mock-registry | 8020 | Development registry service |
| transporter-scheduler | 8053 | Scheduling services |
| allocation-service | 9996 | Core driver allocation engine |

### External Integration Details
**Payment & Financial:**
- Juspay payment gateway (multiple webhook endpoints)
- Juspay payout system with mandate execution
- Manual payment link generation

**Verification Services:**
- Idfy document verification (multiple webhook configurations)
- HyperVerge verification (result + verification webhooks)
- Multi-merchant, multi-city verification support

**Communication:**
- SMS service with OTP handling (7891 fake SMS for dev)
- FCM notifications with scheduled delivery
- Exophone call status tracking and BAP notifications
- Safety alert system with webhook integration

**Analytics & Monitoring:**
- Event streaming to multiple Kafka topics
- ClickHouse analytics pipeline
- Location tracking service integration
- Vocalytics integration for voice analytics

## Technical Constraints

### Performance Requirements
- **Response Time**: < 500ms for critical APIs
- **Throughput**: Handle 10,000+ concurrent requests
- **Availability**: 99.9% uptime requirement
- **Scalability**: Horizontal scaling capability

### Memory & Resources
- **GHC Settings**: Optimized for production builds
- **Parallel Jobs**: 6 parallel compilation jobs by default
- **Development Flags**: Configurable optimization levels

### Protocol Compliance
- **BECKN/ONDC**: Mandatory protocol adherence
- **REST APIs**: RESTful service interfaces
- **Event Streaming**: Kafka-based async communication

## Dependencies Management

### Flake Inputs
External dependencies managed through `flake.nix`:
- **shared-kernel**: Common Haskell utilities
- **services-flake**: Infrastructure services
- **External repos**: Various third-party integrations

### Package Structure
Multi-package project with 40+ packages:
- **App packages**: Executable applications
- **Lib packages**: Shared libraries
- **Test packages**: Testing utilities

### Update Process
```bash
# Update specific input
nix flake lock --update-input shared-kernel

# Use local development
# Modify flake.nix to point to local path
```

## Tool Usage Patterns

### Development Tools
- **ghcid**: Fast compile feedback during development
- **cabal repl**: Interactive development environment
- **profiling**: Performance analysis tools
- **swagger**: API documentation generation

### Testing Infrastructure
- **Unit Tests**: Cabal test suites
- **Integration Tests**: Newman-based API testing
- **Load Testing**: Performance testing framework
- **Mock Services**: Local testing with mock external APIs

### Monitoring & Operations
- **Prometheus**: Metrics collection
- **Grafana**: Monitoring dashboards
- **PgAdmin**: Database administration
- **Log aggregation**: Centralized logging

## Security Considerations

### Development Security
- **Local development**: Isolated Nix environments
- **Secrets management**: Environment-based configuration
- **Database access**: Controlled through connection pools

### Production Security
- **Container security**: Docker best practices
- **Network security**: Service mesh configuration
- **Data encryption**: At-rest and in-transit encryption

## Development Optimization

### Fast Development Builds
- Uncomment development flags in `cabal.project`
- Use `optimization: 0` for faster compilation
- Local flag configuration for development

### Resource Management
- Stack size limit adjustments for linking issues
- Parallel compilation configuration
- Memory usage optimization