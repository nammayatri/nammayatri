# Active Context

## Current Work Focus

### Memory Bank Initialization
**Status**: Setting up Cline Memory Bank structure for Nammayatri backend
**Goal**: Enable consistent AI assistance across development sessions
**Progress**: Creating core memory bank files with project understanding

### Development Environment
**Current State**: Project uses Nix-based development with Cabal
**Active Setup**:
- Nix development shell with direnv
- Multi-package Cabal project (40+ packages)
- VSCode with Haskell Language Server

## Recent Changes

### Memory Bank Enhancement
- **Enhanced `projectbrief.md`** - Added specific service details, port configurations
- **Enhanced `productContext.md`** - Business context and user journeys
- **Enhanced `systemPatterns.md`** - Detailed 40+ microservices, 24+ allocation jobs
- **Enhanced `techContext.md`** - Comprehensive external integrations, webhooks
- **Status**: Completed detailed codebase analysis and memory bank enrichment

## Next Steps

### Immediate Priorities
1. **Complete Memory Bank Setup**
   - Finalize `activeContext.md` and `progress.md`
   - Test memory bank with Cline workflows
   - Establish update patterns

2. **Development Workflow Integration**
   - Configure Cline custom instructions
   - Test Plan/Act mode transitions
   - Validate context preservation

### Development Areas to Explore
1. **Rider Platform Deep Dive**
   - Understand booking flow implementation
   - Review API structures and patterns
   - Examine integration points

2. **Provider Platform Analysis**
   - Driver allocation algorithm details
   - Real-time tracking implementation
   - BECKN protocol integration

3. **Shared Services Understanding**
   - Payment processing workflows
   - Location update mechanisms
   - Scheduler and background jobs

## Active Decisions & Considerations

### Memory Bank Management
- **File Organization**: Core files in `/memory-bank/` directory
- **Update Frequency**: After significant changes or new understanding
- **Context Scope**: Focus on development-relevant information
- **Integration Strategy**: Use with Cline custom instructions

### Development Approach
- **Exploration Strategy**: Start with high-level understanding, then dive deep
- **Learning Path**: Business logic → Technical implementation → Integration patterns
- **Documentation**: Maintain memory bank as primary knowledge base

## Important Patterns & Preferences

### Code Organization Patterns
- **Platform Separation**: Clear rider/provider boundaries
- **Shared Libraries**: Common functionality in `lib/` packages
- **Service Isolation**: Each service has specific responsibilities
- **Protocol Compliance**: BECKN/ONDC standards throughout

### Development Preferences
- **Build System**: Prefer Cabal over Nix for local development
- **IDE Setup**: VSCode with Haskell extensions
- **Testing Approach**: Unit tests + integration tests + load testing
- **Environment**: Use development flags for faster compilation

## Learnings & Project Insights

### Architecture Insights
- **Microservices Design**: Well-structured separation of concerns
- **Event-Driven**: Heavy use of async communication patterns
- **Scalability Focus**: Built for high-concurrency scenarios
- **Protocol-First**: BECKN/ONDC compliance drives design decisions

### Technical Insights
- **Haskell Benefits**: Type safety for critical business logic
- **Nix Advantages**: Reproducible builds and development environments
- **Multi-package Strategy**: Enables focused development and testing
- **External Dependencies**: Careful management through flake inputs

### Business Logic Insights
- **Rider Journey**: Search → Select → Book → Track → Complete
- **Driver Allocation**: Complex algorithm balancing multiple factors
- **Multi-modal Support**: Single platform for various transport types
- **Real-time Requirements**: Critical for user experience

## Current Understanding - Comprehensive

### Well-Understood Areas ✅
1. **Service Architecture**: 40+ microservices with clear separation
2. **Database Schema**: Multi-schema PostgreSQL + ClickHouse analytics
3. **External Integrations**: Detailed webhook patterns for payments, verification
4. **Allocation System**: 24+ job types with sophisticated scheduling
5. **Fare System**: 5 fare policy types with complex pricing logic
6. **BECKN Protocol**: V1 & V2 implementations with ONDC compliance
7. **Public Transport**: FRFS implementation with stage-based fares

### Areas for Deep Dive
1. **Specific API Flows**: Individual endpoint implementations
2. **Business Rule Engine**: Yudhishthira decision logic
3. **Real-time Analytics**: Supply-demand calculation algorithms
4. **Location Tracking**: Geospatial calculation specifics
5. **Performance Optimization**: Actual bottlenecks and scaling patterns

### Investigation Priorities
- Supply-demand ratio calculation algorithm implementation
- Congestion charge calculation with geohash integration
- CAC and SuperPosition feature flag management
- Driver onboarding workflow with verification steps
- Scheduled ride management and notification systems

