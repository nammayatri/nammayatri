# Active Context

## Current Work Focus
- Documenting the `dynamic-offer-driver-drainer` project.

## Recent Changes
- Analyzed `server/Main.hs`, `src/DBQuery/Functions.hs`, `src/DBQuery/Types.hs`, `src/DBSync/Create.hs`, `src/DBSync/Update.hs`, `src/DBSync/Delete.hs`, and `src/DBSync/DBSync.hs`, and `src/Types/DBSync.hs`.
- Updated `projectbrief.md`, `productContext.md`, `systemPatterns.md`, and `techContext.md` with detailed information.

## Next Steps
- Formulate clarifying questions about the `dynamic-offer-driver-drainer` project.
- Proceed with analyzing the next project in the list.

## Active Decisions and Considerations
- Ensuring comprehensive documentation for all core projects.
- How to best manage asynchronous data synchronization and fault tolerance for dynamic offer and driver data.

## Important Patterns and Preferences
- Backend service design best practices.
- Use of Haskell and concurrency.
- Event-driven architecture with Redis Streams.
- Direct SQL query generation for database interactions.
- Robust error handling and retry mechanisms.
- Reusability of core drainer logic.

## Learnings and Project Insights
- The `dynamic-offer-driver-drainer` is a specialized instance of the generic drainer service, responsible for maintaining data consistency and integrity for dynamic offer and driver data across Redis and PostgreSQL.
- It shares a highly similar architecture and implementation with `rider-app-drainer`, demonstrating code reusability and a consistent approach to asynchronous data persistence.
- The primary difference lies in the specific database schema (`atlas_driver_offer_bpp`) it interacts with.
