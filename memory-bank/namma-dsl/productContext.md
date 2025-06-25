# Product Context: Namma DSL

## Why This Project Exists
- To simplify and accelerate the development of backend services by automating the creation of API and database interaction code.
- To reduce the cognitive load on developers by abstracting away boilerplate and repetitive coding tasks.
- To ensure consistency in API contracts and database schemas across different services and modules.

## Problems It Solves
- **Boilerplate Reduction**: Eliminates the need to manually write repetitive data types, API routes, and basic CRUD operations in Haskell.
- **Consistency Issues**: Prevents discrepancies between API documentation, database schemas, and actual code.
- **Development Speed**: Speeds up the initial setup and ongoing maintenance of API and storage layers.
- **Error Proneness**: Reduces human error associated with manual code generation and synchronization.

## How It Should Work
- Developers define API endpoints and database schemas using concise YAML files.
- The Namma DSL compiler processes these YAML files to generate type-safe Haskell code.
- The generated code integrates seamlessly with hand-written Haskell code, allowing for custom business logic and complex transformations.

## User Experience Goals (for developers using the DSL)
- **Ease of Use**: Simple and intuitive YAML syntax for defining APIs and storage.
- **Efficiency**: Fast code generation and integration into the build process.
- **Flexibility**: Ability to extend generated code with custom logic (e.g., complex queries, external API calls, data transformers).
- **Reliability**: Generated code is type-safe and adheres to best practices, reducing bugs.
