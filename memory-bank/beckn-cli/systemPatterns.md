# System Patterns

## System Architecture
- The beckn-cli project is a standalone command-line tool.

## Key Technical Decisions
- Using Haskell and optparse-applicative for the command-line interface.
- Using the Beckn API specification for interacting with the network.

## Design Patterns in Use
- Command pattern.
- Builder pattern.

## Component Relationships
- The beckn-cli tool takes command-line arguments as input and sends requests to the Beckn network.
- It supports two modes: GenerateRequestsForLoadTest and GenerateKeyPair.
- It uses the optparse-applicative library for parsing command-line arguments.

## Critical Implementation Paths
- Parsing command-line arguments.
- Generating and signing requests.
- Sending requests to the Beckn network.
