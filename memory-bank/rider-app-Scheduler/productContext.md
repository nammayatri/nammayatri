# Product Context

## Why This Project Exists
- To provide scheduling and background task execution for the rider app.

## Problems It Solves
- Executes asynchronous and scheduled tasks such as notifications, safety alerts, payment processing, and data synchronization.
- Manages job queues and retries for failed tasks.

## How It Should Work
- The rider-app/Scheduler application should run as a background service.
- It should connect to various data stores (PostgreSQL, Redis, ClickHouse) and Kafka for job management and data processing.
- It should execute predefined job handlers for different task types.

## User Experience Goals
- N/A (This is a backend service with no direct user interface).
