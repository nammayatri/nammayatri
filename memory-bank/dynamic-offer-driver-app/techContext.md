# Tech Context

## Technologies Used
- Haskell
- Servant (for API definition)
- EulerHS (for application runtime)
- PostgreSQL (primary and replica)
- Redis (for caching, session management, KV store)
- ClickHouse (for analytics)
- Kafka (for event streaming)
- Dhall (for configuration management)
- External APIs/SDKs: Idfy, HyperVerge, Juspay, Juspay Payout, Safety Portal, Location Tracking Service, Passetto, Vocalytics, URL Shortener.
- Beckn Protocol (ONDC, NammaYatri registries and gateways).

## Development Setup
- VSCode
- Nix

## Technical Constraints
- Ensuring high availability and low latency for API responses.
- Secure handling of sensitive driver and financial data.
- Managing complex integrations with multiple external services.
- Maintaining data consistency across various data stores.
- Scalability to handle a large number of drivers and dynamic offers.

## Dependencies
- Haskell libraries (e.g., Servant, EulerHS, `postgresql-simple`, Hedis, Kafka client, Aeson).
- External service SDKs/clients for Idfy, HyperVerge, Juspay, etc.
- PostgreSQL database.
- Redis server.
- Kafka broker.
- S3 for image storage.

## Tool Usage Patterns
- Git for version control.
- Cabal for building the project.
- Dhall for configuration management.
- Swagger/OpenAPI for API documentation.
