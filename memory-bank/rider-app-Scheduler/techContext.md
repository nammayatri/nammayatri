# Tech Context

## Technologies Used
- Haskell
- PostgreSQL (via Esqueleto)
- Redis
- ClickHouse
- Kafka
- EulerHS
- Dhall (for configuration)

## Development Setup
- VSCode
- Nix

## Technical Constraints
- Haskell language limitations.
- Database limitations.
- Scheduling accuracy and reliability.
- Ensuring data consistency across Redis and PostgreSQL due to asynchronous updates.
- Managing Redis cluster and standalone instances effectively.

## Dependencies
- Haskell libraries (e.g., EulerHS, Esqueleto, Hedis, Kafka client).
- Database client libraries.

## Tool Usage Patterns
- Git for version control.
- Cabal for building the project.
