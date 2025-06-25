# Active Context: Namma DSL

## Current Work Focus
- Documenting the Namma DSL's capabilities for API and Storage code generation.
- Detailing the mechanisms for hybrid support with manual API implementations, QueriesExtra, and dedicated Transformer modules.

## Recent Changes
- Created `memory-bank/namma-dsl/` directory.
- Created `memory-bank/namma-dsl/projectbrief.md`.
- Created `memory-bank/namma-dsl/productContext.md`.

## Next Steps
- Create `memory-bank/namma-dsl/systemPatterns.md`.
- Create `memory-bank/namma-dsl/techContext.md`.
- Create `memory-bank/namma-dsl/progress.md`.
- Provide a comprehensive end-to-end analysis of the Namma DSL.

## Active Decisions and Considerations
- How to best articulate the interplay between generated and manual code.
- Highlighting specific examples from the codebase to illustrate DSL features and hybrid patterns.

## Important Patterns and Preferences
- DSL-driven development for boilerplate.
- Functional programming for custom logic.
- Clear separation of concerns between DSL definitions, generated code, and manual implementations.

## Learnings and Project Insights
- The Namma DSL significantly reduces boilerplate and enforces consistency.
- Its hybrid model is crucial for handling complex business logic and external integrations.
- Dedicated transformer modules (like `Storage.Queries.Transformers.Person.hs`) play a vital role in data manipulation and enrichment within the hybrid architecture.
