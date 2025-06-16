# System Patterns

## System Architecture
- The alchemist project is a standalone code generation tool.

## Key Technical Decisions
- Using Haskell and Beam for code generation.
- Using YAML for specification files.

## Design Patterns in Use
- Template Method pattern.
- Visitor pattern.

## Component Relationships
- The alchemist tool takes a YAML specification as input and generates Haskell code, SQL queries, and other artifacts.

## Critical Implementation Paths
- Parsing the YAML specification.
- Generating the Haskell code.
- Generating the SQL queries.
