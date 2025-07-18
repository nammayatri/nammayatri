# Project Brief: Namma Yatri

## 1. Project Overview
Namma Yatri is an open and driver-centric mobility platform. It aims to empower service providers (like auto drivers) with a high-tech, cost-effective app and open data platform. The main goals are to ensure fair earnings for drivers (zero commission), foster transparency and collaboration through open platforms/code/networks, achieve population scale growth with utility-like pricing, support multimodal transport, and promote shared mobility.

## 2. Core Requirements
-   **Driver-centric Applications:** Provide robust, user-friendly mobile applications for individual drivers.
-   **Rider/Customer Applications:** Develop intuitive mobile applications for end-users seeking rides and deliveries.
-   **Fleet Management Capabilities:** Offer dashboard and APIs for fleet owners/merchants to manage drivers, vehicles, operations, and earnings.
-   **Zero Commission / Subscription Model:** Primarily focus on a zero-commission model for drivers, potentially supplemented by subscription plans for platform access or value-added services.
-   **Open Platform & Beckn Integration:** Adhere to open data principles and integrate seamlessly with the Beckn open mobility network.
-   **Scalability & Reliability:** Design for population-scale growth with high service availability.
-   **Cost-Effectiveness:** Optimize all operational aspects to enable utility-like, affordable pricing.
-   **Multimodal & Multi-Service:**
    -   Support various vehicle categories (auto-rickshaws, taxis, bikes, etc.).
    -   Enable different service tiers (e.g., AC, Safety+, Pet-Friendly).
    -   Support parcel delivery services in addition to passenger transport.
-   **Dynamic Pricing & Fare Management:** Implement sophisticated fare estimation and dynamic pricing, considering supply/demand, congestion, and other factors. Provide configurable fare policies.
-   **Intelligent Dispatch & Allocation:** Develop advanced algorithms for matching drivers to ride/delivery requests, including considerations for driver performance, location, and forward batching.
-   **Payment & Payout System:** Integrate secure payment processing for riders and manage payouts, fees, and subscriptions for drivers/merchants.
-   **Communication Platform:** Provide tools for SMS, WhatsApp, Push Notifications, and In-App Overlays for various user interactions.
-   **Safety & Security:** Implement features like SOS, route deviation monitoring, and robust verification processes. Ensure data security, especially for PII.
-   **Configuration Management:** Allow for extensive, granular configuration of platform behavior per merchant and operating city.
-   **Community & Transparency:** Maintain open data, open code (where applicable), and transparent operational statistics.

## 3. Target Users
-   **Service Providers:**
    -   Individual Drivers (auto-rickshaws, taxis, bikes, etc.)
    -   Delivery Personnel
-   **Customers:**
    -   Riders (seeking personal mobility)
    -   Parcel Senders/Receivers
-   **Intermediaries/Aggregators:**
    -   Fleet Owners / Merchants
    -   Operators (managing routes/services)
-   **Platform Support & Administration:**
    -   Internal Admin Users (for dashboards and operational management)
-   **Community & Ecosystem:**
    -   Open Source Contributors
    -   Local Authorities

## 4. Scope
**Included:**
-   Development of driver, rider, and dashboard (fleet/admin) applications.
-   Backend services for ride-hailing, parcel delivery, dispatch, pricing, payments, subscriptions, user management, fleet management, and extensive configuration.
-   Integration with Beckn open network.
-   Integration with various third-party services (maps, payment gateways, communication providers, verification services, LLMs).
-   Open data initiatives and API provisions.
-   Support for multiple vehicle types and service tiers.
-   Advanced features like dynamic pricing, intelligent pooling, forward batching, and subscription models.
-   Tools for community engagement and open-source development.

**Likely Excluded (based on current information):**
-   Direct vehicle ownership or physical fleet operation by the Namma Yatri platform entity itself (focus is on enabling existing providers).
-   Traditional commission-based revenue models as the primary source of income (emphasis on zero-commission or subscription alternatives).
-   Hardware development (e.g., custom driver devices, dashcams - though integration with such hardware is possible).

## 5. Key Stakeholders
- **Drivers/Service Providers:** Their livelihood and earnings are directly impacted.
- **Citizens/Riders:** Users of the mobility services.
- **Namma Yatri Team/Founders:** Originators and core developers of the platform.
- **Open Source Community:** Contributors to the platform's code, data, and roadmap.
- **Local Communities/Cities:** Where the platform operates, benefiting from improved mobility.
## Core Architecture
The system is built around two main platforms with 40+ microservices:

### Rider Platform (Customer-facing)
- **rider-app**: Main customer APIs for booking, search, payment (Port: 8013)
- **rider-app-scheduler**: Background job processing and notifications
- **search-result-aggregator**: Consolidates search results from multiple providers
- **public-transport-rider-platform**: FRFS (Fixed Route Fixed Schedule) services

### Provider Platform (Driver/fleet-facing)
- **dynamic-offer-driver-app**: Driver APIs and business logic (Port: 8016)
- **driver-offer-allocator**: Sophisticated ride allocation service (Port: 9996)
- **dynamic-offer-driver-drainer**: Data processing and analytics

## Key Business Capabilities
- **Complex Fare Management**: Progressive/Slab/Rental/InterCity/Ambulance fare policies
- **Dynamic Pricing**: Congestion charges, supply-demand ratios, smart tip suggestions
- **Advanced Driver Allocation**: 24+ job types including search distribution, fee calculation
- **Multi-modal Transport**: Integration of private rides with public transport (buses, metro)
- **Real-time Analytics**: Supply-demand analysis, congestion monitoring
- **Financial Operations**: Payment processing, payout management, mandate execution

## Business Domain
- Ride-hailing and mobility services
- Public transportation integration
- Driver-partner ecosystem management
- Customer experience optimization
- Urban mobility solutions

## Technical Stack
- **Language**: Haskell
- **Build System**: Cabal with Nix
- **Architecture**: Microservices
- **Protocol**: BECKN/ONDC for inter-platform communication
- **Infrastructure**: Containerized services with Docker

## Project Goals
- Provide scalable mobility platform backend
- Enable seamless rider-driver matching
- Support multiple transport modes
- Maintain high performance and reliability
- Facilitate ONDC ecosystem participation

