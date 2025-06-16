# System Patterns

## System Architecture
- The dynamic-offer-driver-app project is a backend API service built with Haskell and Servant.
- It uses the EulerHS framework for managing the application runtime.
- It integrates with PostgreSQL (primary and replica), various Redis instances (for caching, session management, and KV store), and ClickHouse (for analytics).
- It utilizes Kafka producers for event streaming.
- It exposes a modular API, categorized into UI, Internal, Dashboard, and Beckn.
- It implements robust authentication and authorization middleware.
- It integrates with external identity verification services (Idfy, HyperVerge) via webhooks for driver onboarding.
- It integrates with external payment and payout gateways (Juspay) via webhooks for financial transactions.
- It interacts with Beckn registries (ONDC, NammaYatri) for service discovery and communication, with caching for performance.
- It uses Dhall for configuration management.

## Key Technical Decisions
- Modular API design for clear separation of concerns.
- Extensive use of external service integrations for specialized functionalities (identity verification, payments, safety).
- Robust authentication and authorization mechanisms.
- Caching strategies (Redis) for performance optimization.
- Asynchronous processing for certain tasks (e.g., payouts, safety portal checks).
- Centralized configuration management with Dhall.

## Design Patterns in Use
- API Gateway pattern (implicit through modular API).
- Webhook pattern for external service integrations.
- Caching pattern.
- Event-driven architecture (Kafka).
- Registry pattern (for Beckn).

## Component Relationships
- The dynamic-offer-driver-app interacts with:
    - External identity verification services (Idfy, HyperVerge).
    - External payment and payout gateways (Juspay).
    - External safety portals.
    - PostgreSQL database for persistent data.
    - Redis for caching and temporary data.
    - Kafka for event streaming.
    - Beckn registries and gateways.
    - `rider-app-Scheduler` for asynchronous job scheduling (e.g., `RetryDocumentVerificationJob`).
    - `dynamic-offer-driver-drainer` for asynchronous data persistence.

## Critical Implementation Paths
- Secure and reliable handling of sensitive driver and financial data.
- Ensuring accurate and timely document verification during onboarding.
- Robust processing of payment and payout transactions.
- Maintaining high availability and performance for API endpoints.
- Seamless integration and communication with various external and internal services.
