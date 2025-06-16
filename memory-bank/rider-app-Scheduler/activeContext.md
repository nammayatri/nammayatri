# Active Context

## Current Work Focus
- Documenting the `rider-app/Scheduler` project.

## Recent Changes
- Analyzed `server/Main.hs`, `src/Environment.hs`, and `src/App.hs`.
- Updated `projectbrief.md`, `productContext.md`, `systemPatterns.md`, and `techContext.md` with detailed information.

## Next Steps
- Proceed with analyzing the next project in the list.

## Active Decisions and Considerations
- Ensuring comprehensive documentation for all core projects.
- How to best manage scheduled jobs and background tasks.

## Important Patterns and Preferences
- Backend service design best practices.
- Use of Haskell and concurrency.
- Robust scheduling mechanisms.
- Integration with multiple data stores (PostgreSQL, Redis, ClickHouse) and Kafka.

## Learnings and Project Insights
- The `rider-app/Scheduler` is a critical component for executing a wide array of asynchronous and scheduled tasks.
- It acts as the backbone for many background operations, including notifications, safety alerts, payment processing, and data synchronization.
- The project demonstrates a complex setup with extensive integrations and configurations.
- The scheduling system is entirely Redis Stream-based, with a separate `producer` service feeding tasks to the `scheduler`.
- Redis is used for both critical and non-critical job storage, with support for standalone and clustered instances.
- PostgreSQL stores long-term job definitions and historical data, with asynchronous updates from the Redis Stream.
- The scheduler implements a robust retry mechanism for failed jobs.
