# Tech Context

## Technologies Used
- React
- Rust
- PostgreSQL
- Kubernetes
- AWS
- React Native
- Haskell
- YAML
- optparse-applicative
- Beckn API
- Namma DSL (for code generation)

## Development Setup
- VSCode
- Nix

## Dynamic Offer Driver App
- Haskell
- PostgreSQL
- Beam
- Concurrency
- Message Queue (e.g., Kafka)

## Rider App
- React Native
- Redux
- JavaScript
- REST APIs

## Alchemist
- Haskell
- Beam
- YAML

## Beckn CLI
- Haskell
- optparse-applicative
- Beckn API
- K6
- Ed25519

## General
- VSCode
- Nix

## Mock Google
- Haskell
- Servant
- EulerHS
- Mock data generation

## Technical Constraints
- Limited resources.
- Time constraints.

## Dependencies
- aeson
- base >=4.7 && <5
- mobility-core
- bytestring
- record-dot-preprocessor
- record-hasfield
- text
- time
- template-haskell
- casing
- directory
- yaml
- filepath
- turtle
- namma-dsl
- extra
- fdep
- fieldInspector
ghc-options:
  - -fwrite-ide-info
  - -hiedir=.hie
  # - -fplugin=RecordDotPreprocessor
  - -fplugin=Fdep.Plugin -fplugin-opt=Fdep.Plugin:{"path":"./tmp/fdep/","port":4444,"host":"::1","log":false,"tc_funcs":false}
  - -fplugin=FieldInspector.PluginFields -fplugin-opt=FieldInspector.PluginFields:{"path":"./tmp/fdep/","port":4444,"host":"::1","log":false,"tc_funcs":false}
  - -fplugin=FieldInspector.PluginTypes -fplugin-opt=FieldInspector.PluginTypes:{"path":"./tmp/fdep/","port":4444,"host":"::1","log":false,"tc_funcs":false}
- Time constraints.

## Dependencies
- React libraries.
- Rust crates.
- PostgreSQL extensions.

## Tool Usage Patterns
- Git for version control.
- Jenkins for CI/CD.
- Docker for containerization.
- Python scripts for various tasks, such as compressing directories and extracting dependency information.
- Nix configuration for building, running, and testing the project.
- Git ignore configuration for excluding build artifacts and sensitive information.
- Nix scripts for development and testing, such as running ghcid, generating documentation, running hpack, generating code, running the mobility stack, and running integration tests.
- Instructions for enabling profiling for Haskell projects.
- Nix package for joining the `nammayatriMetadata` output of all Haskell packages.
