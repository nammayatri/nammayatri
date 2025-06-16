# System Patterns

## System Architecture
- The dynamic-offer-driver-app-Allocator project is a backend service responsible for driver allocation and various background tasks.
- It uses the EulerHS framework for managing the application runtime.
- It connects to PostgreSQL (primary and replica), various Redis instances (for job storage, caching, and session management), and ClickHouse (for analytics).
- It utilizes Kafka producers for event streaming.
- It integrates with centralized configuration/feature flagging systems (CAC, SuperPosition) for dynamic control.
- It manages scheduled jobs through a comprehensive set of `AllocatorJobType` handlers.

## Key Technical Decisions
- Robust and scalable job scheduling for critical business processes.
- Centralized configuration management for dynamic feature control.
- Integration with multiple data stores for diverse data needs (transactional, analytical, caching).
- Asynchronous processing for various tasks to ensure responsiveness and scalability.

## Design Patterns in Use
- Scheduler pattern.
- Event-driven architecture (Kafka).
- Centralized Configuration pattern (CAC, SuperPosition).
- Producer-Consumer pattern (for jobs).

## Component Relationships
- The dynamic-offer-driver-app-Allocator interacts with:
    - PostgreSQL database for persistent data (job definitions, driver states, financial records).
    - Redis for job queues, temporary data, and caching.
    - Kafka for event streaming (e.g., sending search requests, notifications).
    - External systems for notifications (Exotel), webhooks, and potentially other services.
    - CAC and SuperPosition for configuration.
    - `dynamic-offer-driver-app` (implicitly, as it handles driver-related data).

## Critical Implementation Paths
- Efficient and accurate matching of drivers to ride requests.
- Reliable execution of financial processes (fees, payouts).
- Timely delivery of critical notifications and alerts.
- Ensuring data consistency across various integrated systems.
- Maintaining high performance under heavy load for allocation tasks.
