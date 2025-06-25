# System Patterns: Namma DSL

## System Architecture
- The Namma DSL is a code generation tool that operates within a larger microservices architecture.
- It takes YAML specifications as input and generates Haskell source code.
- The generated code is then integrated into various Haskell backend services (e.g., `rider-app`).

## Key Technical Decisions (within the DSL itself)
- **YAML as Specification Language**: Chosen for its human-readability and hierarchical structure, suitable for defining API and database schemas.
- **Haskell as Target Language**: Leverages Haskell's strong type system, functional paradigms, and robust ecosystem for generated code.
- **Beam Integration**: Utilizes the Beam library for type-safe database interactions, abstracting SQL queries.
- **Servant Integration**: Utilizes the Servant library for defining type-safe web APIs.

## Design Patterns in Use (within the DSL and its generated output)
- **Code Generation**: Automates repetitive code creation.
- **Domain Specific Language (DSL)**: Provides a high-level, declarative way to define system components.
- **Hybrid Architecture**: Combines generated boilerplate with hand-written custom logic.
- **Type-Driven Development**: Ensures correctness and consistency through strong typing.

## Component Relationships
- **YAML Spec Files**: Input to the Namma DSL compiler.
    - `spec/API/*.yaml`: Defines API endpoints and types.
    - `spec/Storage/*.yaml`: Defines database schemas and queries.
- **Namma DSL Compiler (Alchemist)**: Processes YAML files and generates Haskell code.
- **Generated Haskell Code**:
    - `API.Types.*`: Haskell data types and Servant API definitions.
    - `Storage.Beam.*`: Beam schema definitions.
    - `Storage.Queries.*`: Basic CRUD and custom query functions.
- **Manual Haskell Code**:
    - `Storage.Queries.*Extra.hs`: Extends generated queries with custom logic.
    - `Storage.Queries.Transformers.*.hs`: Modules for data transformation and enrichment.
    - `Beckn.ACL.*.hs`: ACL (Access Control Layer) transformers for external API payload construction.
    - Business logic modules (e.g., `ExternalBPP.Flow.hs`, `ExternalBPP.CallAPI.hs`) that orchestrate interactions between generated APIs, storage, and external systems.

## Critical Implementation Paths (within the DSL's influence)
- **Code Generation Pipeline**: The process of compiling YAML specs into executable Haskell code.
- **Data Flow**: How data is transformed from API request to internal domain model, persisted to the database, retrieved, and transformed back for API responses.
- **External Integration**: How the DSL-generated API types are used in conjunction with manual logic to interact with third-party systems (e.g., Beckn APIs).

## File Structure Conventions (for DSL-related files)
- `spec/API/`: Directory for API YAML specifications.
- `spec/Storage/`: Directory for Storage YAML specifications.
- `src-read-only/Storage/Queries/`: Directory for generated storage query modules.
- `src/Storage/Queries/Transformers/`: Directory for manual data transformer modules.
- `src/ExternalBPP/`: Directory for manual external BPP integration logic, often using generated API types.
