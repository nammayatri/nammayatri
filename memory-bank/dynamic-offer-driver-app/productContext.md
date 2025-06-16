# Product Context

## Why This Project Exists
- To provide the core API services for managing dynamic offers and driver-related functionalities.
- To facilitate the onboarding and compliance of drivers through various document verification processes.
- To handle financial transactions (payments and payouts) for drivers.

## Problems It Solves
- Manages the lifecycle of dynamic offers for drivers.
- Automates driver onboarding, including identity and document verification (DL, RC, PAN, GSTIN, Aadhaar) through integrations with external services.
- Processes driver payments and payouts, including webhook handling and mandate management.
- Ensures driver compliance and safety through integration with safety portals.

## How It Should Work
- The dynamic-offer-driver-app should expose a comprehensive set of API endpoints for UI, internal services, and dashboards.
- It should integrate with external identity verification services (Idfy, HyperVerge) and payment/payout gateways (Juspay) via webhooks.
- It should manage driver and vehicle-related data in PostgreSQL and leverage Redis for caching and session management.
- It should interact with Beckn registries for service discovery and communication within the Beckn network.
- It should provide a consolidated view of driver onboarding status.

## User Experience Goals
- For drivers: A smooth and efficient onboarding process, reliable payment and payout experiences.
- For internal teams: Automated compliance checks, clear visibility into driver status, and efficient management of dynamic offers.
