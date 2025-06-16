# Active Context

## Current Work Focus
- Documenting the `dynamic-offer-driver-app` project.

## Recent Changes
- Analyzed `Main/src/App/Server.hs`, `Main/src/API.hs`, `Main/src/Environment.hs`, `Main/src/Domain/Action/UI/DriverOnboarding/IdfyWebhook.hs`, `Main/src/Domain/Action/UI/Payment.hs`, `Main/src/Domain/Action/UI/SafetyWebhook.hs`, `Main/src/Domain/Action/UI/DriverOnboarding/HyperVergeWebhook.hs`, `Main/src/Domain/Action/UI/DriverOnboarding/DriverLicense.hs`, `Main/src/Domain/Action/UI/DriverOnboarding/VehicleRegistrationCertificate.hs`, and `Main/src/Domain/Action/UI/DriverOnboarding/Status.hs`.
- Updated `projectbrief.md`, `productContext.md`, `systemPatterns.md`, and `techContext.md` with detailed information.

## Next Steps
- Formulate clarifying questions about the `dynamic-offer-driver-app` project.
- Proceed with analyzing the next project in the list.

## Active Decisions and Considerations
- Ensuring comprehensive documentation for all core projects.
- How to best manage driver onboarding, payments, and dynamic offers.

## Important Patterns and Preferences
- Backend API service design best practices.
- Extensive use of external service integrations and webhooks.
- Robust authentication, authorization, and security measures.
- Modular API design and clear separation of concerns.
- Data consistency and integrity across multiple data stores.

## Learnings and Project Insights
- The `dynamic-offer-driver-app` is a central and complex service responsible for driver management, onboarding, and financial transactions.
- It heavily relies on integrations with various external identity verification, payment, and safety services.
- The driver onboarding process involves multi-step document verification with retry mechanisms and fallback to alternate providers.
- Payment and payout processes are robust, handling webhooks, mandates, and asynchronous tasks.
- The service plays a key role in the Beckn network through registry lookups and communication.
