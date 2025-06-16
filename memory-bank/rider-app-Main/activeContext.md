# Active Context

## Current Work Focus
- Documenting the `rider-app/Main` project.

## Recent Changes
- Analyzed the OpenAPI spec, database schema, and Beckn ACL for `rider-app/Main`.
- Updated `projectbrief.md`, `productContext.md`, `systemPatterns.md`, and `techContext.md` with detailed information.

## Next Steps
- Formulate clarifying questions about the `rider-app/Main` project.
- Proceed with analyzing the next project in the list.

## Active Decisions and Considerations
- Ensuring comprehensive documentation for all core projects.

## Important Patterns and Preferences
- Backend service design best practices.
- Use of Haskell and concurrency.
- Adherence to Beckn Protocol.
- Modular database interactions with ClickHouse and PostgreSQL (via Beam).

## Learnings and Project Insights
- The `rider-app/Main` project is a central component, handling a wide range of APIs and integrating with various data storage and external services.
- Its architecture is modular, with clear separation of concerns for API handling, data access, and Beckn Protocol interactions.
- The project utilizes both relational (PostgreSQL via Beam) and analytical (ClickHouse) databases.
- The Beckn ACL is crucial for handling incoming and outgoing Beckn messages, including IGM.
