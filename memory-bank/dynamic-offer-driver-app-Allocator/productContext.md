# Product Context

## Why This Project Exists
- To provide the core allocation logic for matching drivers to ride requests in a dynamic offer system.
- To automate various background processes related to driver management, financial operations, and compliance.

## Problems It Solves
- Efficiently allocates available drivers to incoming ride requests.
- Manages driver states (e.g., soft blocking, unblocking) based on various criteria.
- Automates the calculation and processing of driver fees, bad debts, and referral payouts.
- Handles retries for failed document verifications.
- Delivers scheduled notifications and alerts to drivers (e.g., FCM, fleet alerts).
- Calculates and adjusts pricing based on supply/demand and congestion.
- Integrates with centralized configuration systems for dynamic feature control.

## How It Should Work
- The dynamic-offer-driver-app-Allocator should run as a background service, continuously processing scheduled jobs.
- It should connect to PostgreSQL, Redis, and Kafka for data storage, caching, and event streaming.
- It should execute a wide array of job handlers, each responsible for a specific business logic (e.g., sending search requests, processing payments, updating driver status).
- It should interact with external systems for notifications (Exotel), webhooks, and potentially other services.
- It should integrate with CAC and SuperPosition for centralized configuration and feature management.

## User Experience Goals
- For riders: Fast and reliable driver allocation.
- For drivers: Timely notifications, accurate fee calculations, and efficient payout processing.
- For internal teams: Automated operational tasks, clear visibility into allocation performance, and flexible configuration management.
