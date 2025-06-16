# Tech Context

## Technologies Used
- Haskell
- PostgreSQL (`postgresql-simple`)
- Redis (KV store and Streams)
- Kafka
- EulerHS
- Dhall (for configuration)

## Development Setup
- VSCode
- Nix

## Technical Constraints
- Ensuring data consistency across Redis and PostgreSQL for dynamic offer and driver data.
- Handling high-volume data streams efficiently.
- Managing database connection pools effectively.

## Dependencies
- Haskell libraries (e.g., EulerHS, `postgresql-simple`, Hedis, Kafka client).
- PostgreSQL database.
- Redis server.
- Kafka broker.

## Tool Usage Patterns
- Git for version control.
- Cabal for building the project.
- Dhall for configuration management.
