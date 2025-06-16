# System Patterns

## System Architecture
- The rider-app/Scheduler project is a backend service responsible for executing asynchronous and scheduled tasks.
- It uses the EulerHS framework for managing the application runtime.
- It connects to PostgreSQL (via Esqueleto), Redis (for job storage and caching), and ClickHouse (for analytics).
- It utilizes Kafka producers for event streaming.
- It manages scheduled jobs, including notifications, safety alerts, payment processing, data synchronization, and updates related to tags and CRIS data.
- It implements a robust retry mechanism for failed jobs, using `markAsFailed`, `updateErrorCountAndFail`, and `reScheduleOnError` to handle transient failures and ensure task completion.

## Key Technical Decisions
- Using Haskell and scheduling libraries for the scheduling logic.
- Using a database for storing scheduled rides.

## Design Patterns in Use
- Observer pattern.
- Strategy pattern.

## Component Relationships
- The rider-app/Scheduler application interacts with the database to manage scheduled rides.
- It may also interact with other services via a message queue.

## Critical Implementation Paths
- Scheduling rides.
- Managing scheduled rides.
- Communicating with other services.
