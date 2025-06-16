# System Patterns

## System Architecture
- The dynamic-offer-driver-app/Main project is a backend service.
- It uses the Servant library for defining the API.
- It uses the EulerHS framework for managing the application runtime.
- It uses authentication and authorization middleware.
- It initializes the CAC and SuperPosition clients.

## Key Technical Decisions
- Using Haskell and concurrency for the backend service.
- Using a database for storing dynamic offer data.

## Design Patterns in Use
- Observer pattern.
- Strategy pattern.

## Component Relationships
- The dynamic-offer-driver-app/Main application interacts with the database to manage dynamic offers.
- It may also interact with other services via a message queue.

## Critical Implementation Paths
- Managing the dynamic offer lifecycle.
- Interacting with the database.
- Communicating with other services.
