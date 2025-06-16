# System Patterns

## System Architecture
- The dynamic-offer-driver-drainer project is a backend service responsible for asynchronous data synchronization of dynamic offer and driver-related data.
- It uses EulerHS for managing the application runtime.
- It connects to PostgreSQL (via `postgresql-simple` and connection pools) and Redis (for KV store and Streams).
- It utilizes Kafka producers for event streaming.
- Data changes (Create, Update, Delete) are received from Redis Streams.
- It dynamically generates SQL queries based on a custom data structure (`DBQuery.Types`), specifically targeting the `atlas_driver_offer_bpp` schema.
- It executes SQL queries against PostgreSQL, with robust error handling and retry mechanisms.
- It supports conditional data flow, allowing data to be pushed selectively to PostgreSQL and/or Kafka.
- It includes a "force drain" mechanism to prevent the drainer from stopping on failures when enabled.

## Key Technical Decisions
- Asynchronous data persistence using Redis Streams and a dedicated drainer service.
- Direct SQL query generation for fine-grained control over database interactions.
- Robust error handling and fault tolerance for data synchronization.
- Reusability of core drainer logic across different domains.

## Design Patterns in Use
- Event-driven architecture (Redis Streams).
- Producer-Consumer pattern.
- Data Transfer Object (DTO) pattern for SQL query generation.

## Component Relationships
- The dynamic-offer-driver-drainer consumes data from Redis Streams, which are populated by other services (e.g., dynamic-offer-driver-app).
- It writes data to PostgreSQL (specifically the `atlas_driver_offer_bpp` schema) and optionally to Kafka.
- It interacts with Redis for KV configurations and stream management.

## Critical Implementation Paths
- Ensuring atomicity and consistency of dynamic offer and driver data during asynchronous synchronization.
- Handling network partitions and database outages gracefully.
- Efficiently processing large volumes of dynamic offer and driver data from Redis Streams.
