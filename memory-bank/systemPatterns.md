# System Patterns

## System Architecture
- Microservices architecture.
- The system consists of the following sub-projects:
  - rider-app: A mobile application for riders to request and manage rides.
  - alchemist: A code generation tool for the NammaYatri platform.
  - beckn-cli: A command-line interface for interacting with the Beckn network.
  - dynamic-offer-driver-drainer: A service for draining dynamic offer drivers.
  - dynamic-offer-driver-app/Main: The main application logic for the dynamic offer driver app.
  - dynamic-offer-driver-app/Allocator: The offer allocation logic for the dynamic offer driver app.
  - rider-app-drainer: A service for draining rider app users.
  - rider-app/Main: The main application logic for the rider app.
  - rider-app/Scheduler: Scheduling functionality for the rider app.
  - rider-app/search-result-aggregator: A service for aggregating search results for the rider app.

## Key Technical Decisions
- Using React for the frontend.
- Using Rust for the backend.
- Using PostgreSQL for the database.
- Using Haskell for backend services.
- Using Nix for reproducible builds.
- Using Redis for caching and session management.
- Using Kubernetes for orchestration.
- Using AWS for cloud infrastructure.

## Design Patterns in Use
- Functional programming.
- Dependency injection.

## Mock Google
- Simulates Google Maps APIs (Place Name, Snap to Road, Distance Matrix, Directions).
- Uses mock data for responses.

## Component Relationships
- The frontend communicates with the backend via REST APIs.
- The backend interacts with the database using an ORM.

## Critical Implementation Paths
- User authentication and authorization.
- Ride request and dispatch.
