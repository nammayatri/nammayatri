# Tech Context: Namma DSL

## Technologies Used (by Namma DSL itself)
- **Haskell**: The primary language for implementing the DSL compiler (Alchemist) and the target language for code generation.
- **YAML**: The declarative language used for defining API and Storage specifications.
- **Alchemist**: The specific code generation tool that implements the Namma DSL.
- **Beam**: Haskell library for type-safe database access, used by the Storage DSL for generating queries.
- **Servant**: Haskell library for defining web APIs, used by the API DSL for generating API types.
- **optparse-applicative**: (Likely used by Alchemist for command-line argument parsing).
- **text, bytestring, aeson, yaml**: Core Haskell libraries for text manipulation, byte handling, JSON serialization/deserialization, and YAML parsing.
- **template-haskell, casing, directory, filepath, turtle, extra, fdep, fieldInspector**: Haskell libraries and tools likely used in the code generation process itself (e.g., for metaprogramming, string casing, file system operations, dependency analysis).

## Development Setup (for Namma DSL development)
- **VSCode**: Integrated Development Environment.
- **Nix**: Used for reproducible builds and development environments for the DSL compiler itself.

## Technical Constraints (of the DSL)
- **YAML Expressiveness**: Limited by what can be declaratively expressed in YAML; complex logic requires hybrid manual code.
- **Haskell Ecosystem**: Relies on the capabilities and libraries available in the Haskell ecosystem.
- **Code Generation Scope**: Primarily focused on API and Storage layers, not general-purpose application logic.

## Dependencies (of the DSL compiler)
- `aeson`
- `base`
- `mobility-core` (likely contains shared types or utilities)
- `bytestring`
- `record-dot-preprocessor`
- `record-hasfield`
- `text`
- `time`
- `template-haskell`
- `casing`
- `directory`
- `yaml`
- `filepath`
- `turtle`
- `namma-dsl` (the DSL library itself, if it's modular)
- `extra`
- `fdep`
- `fieldInspector`

## Tool Usage Patterns (for Namma DSL development and usage)
- **`nix develop .#backend -c , run-generator --apply-hint`**: Command used to run the Alchemist code generator.
- **Git**: For version control of DSL specifications and the DSL compiler itself.
- **CI/CD Pipelines**: Integration into automated build and deployment processes to ensure generated code is always up-to-date.
