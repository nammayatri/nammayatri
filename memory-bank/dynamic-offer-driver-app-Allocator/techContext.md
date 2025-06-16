# Tech Context

## Technologies Used
- Haskell
- EulerHS (for application runtime)
- PostgreSQL (primary and replica, via Esqueleto)
- Redis (for job storage, caching, session management)
- ClickHouse (for analytics)
- Kafka (for event streaming)
- Dhall (for configuration management)
- External APIs/SDKs: Location Tracking Service, Passetto, Idfy (for authentication managers).
- Centralized Access Control (CAC) system.
- SuperPosition (for feature flagging/tenant management).

## Development Setup
- VSCode
- Nix

## Technical Constraints
- Ensuring high performance and low latency for driver allocation.
- Reliable execution of scheduled jobs.
- Maintaining data consistency across various data stores.
- Scalability to handle a large number of drivers and ride requests.
- Secure integration with external systems.

## Dependencies
- Haskell libraries (e.g., EulerHS, Esqueleto, Hedis, Kafka client).
- PostgreSQL database.
- Redis server.
- Kafka broker.
- External service SDKs/clients for LTS, Passetto, Idfy.
- CAC and SuperPosition services.

## Tool Usage Patterns
- Git for version control.
- Cabal for building the project.
- Dhall for configuration management.
