# System Patterns

## System Architecture
- The rider-app/Main project is a backend service.
- It uses the Servant library for defining the API.
- It uses the EulerHS framework for managing the application runtime.
- It uses authentication and authorization middleware.
- It defines API endpoints and data types for managing user profiles, emergency contacts, payments, ride search and booking, ride management, support and feedback, mapping and location, public transport (FRFS), multimodal journeys, and BBPS.
- It uses a Redis KV Layer for high-speed, scalable storage of ongoing transactional data, with primary and secondary key indexing.
- Write operations are pushed synchronously to a Redis Stream.
- Database updates (to PostgreSQL via Beam) are performed asynchronously by a dedicated drainer service, ensuring data consistency and fault tolerance.
- Stream entries are also pushed to ClickHouse for advanced analytics and historical data queries.
- It implements the Beckn Protocol Access Control Layer (ACL) for handling message routing, validation, and transformation between internal Domain types and Beckn-compliant formats for various Beckn actions (Search, Select, Init, Confirm, Update, Cancel, Rating, Track) and their corresponding responses, including specific handling for FRFS (Fixed Route Fixed Schedule) and IGM (Issue Grievance Management) domains.

## Key Technical Decisions
- Using Haskell and concurrency for the backend service.
- Using a database for storing rider app data.

## Design Patterns in Use
- Observer pattern.
- Command pattern.

## Component Relationships
- The rider-app/Main application interacts with the database to manage rider app data.
- It may also interact with other services via a message queue.

## Critical Implementation Paths
- Managing user authentication.
- Handling ride requests.
- Communicating with other services.
