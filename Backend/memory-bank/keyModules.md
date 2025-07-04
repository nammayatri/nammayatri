# Nammayatri Backend - Key Modules and Functionality

This document outlines the key Haskell modules and their primary functionalities within the Nammayatri backend, based on the analysis of the codebase.

## 1. Core Libraries (`lib/` directory)

These modules provide foundational functionalities and shared components across various services.

### `lib/beckn-spec/`
*   **Purpose:** Defines the core data types and API specifications for the BECKN/ONDC protocol (both V1 and V2). This is crucial for inter-platform communication.
*   **Key Functionality:**
    *   **Data Types:** Defines all BECKN message structures (e.g., `Context`, `Message`, `Order`, `Fulfillment`, `Item`, `Price`, `Location`, `Gps`, `Descriptor`, `Tags`, `Quote`, `Vehicle`, `PaymentInstrument`, `Address`, `Time`, `Duration`, `Scalar`).
    *   **API Endpoints:** Defines Servant APIs for all BECKN actions (e.g., `search`, `on_search`, `select`, `on_select`, `init`, `on_init`, `confirm`, `on_confirm`, `update`, `on_update`, `status`, `on_status`, `track`, `on_track`, `cancel`, `on_cancel`, `rating`, `on_rating`, `issue`, `on_issue`, `issue_status`, `on_issue_status`).
    *   **Enumerations:** Comprehensive sets of enums for various domain concepts (e.g., `VehicleCategory`, `FulfillmentType`, `PaymentType`, `OrderStatus`, `CancellationSource`, `IssueType`, `IssueSubCategory`), often with custom JSON serialization rules.
    *   **Utilities:** Contains helper functions for BECKN-specific tasks like context validation, tag retrieval, and ISO8601 time/duration handling.

### `lib/beckn-services/`
*   **Purpose:** Provides common services related to the BECKN protocol and external integrations.
*   **Key Functionality:**
    *   **AWS S3 Integration:** Functions for interacting with AWS S3 for file storage (`AWS.S3.Flow`, `AWS.S3.Types`).
    *   **Registry Interaction:** Interfaces for interacting with the BECKN registry (`Registry.Beckn.Interface`, `Registry.Beckn.Nammayatri.Flow`).

### `lib/dashcam/`
*   **Purpose:** Likely related to dashcam data processing or integration.
*   **Key Functionality:** Defines domain types and flows for dashcam-related operations (`Domain.Types`, `Domain.Interface`, `Domain.Cautio.Flow`, `Domain.Cautio.Types`).

### `lib/external/`
*   **Purpose:** Contains modules for integrating with various external third-party services.
*   **Key Functionality:**
    *   **Email (AWS SES):** Sending emails via AWS SES (`Email.AWS.Flow`, `Email.Types`).
    *   **Chat Completion (Azure OpenAI, Gemini):** Interfaces and APIs for interacting with LLM chat completion services (`ChatCompletion.Interface`, `ChatCompletion.AzureOpenAI.API`, `ChatCompletion.Gemini.API`, `ChatCompletion.Types`, `ChatCompletion.AzureOpenAI.Config`, `ChatCompletion.Gemini.Config`).
    *   **Kafka Logs:** Pushing transaction logs to Kafka (`KafkaLogs.TransactionLogs`).
    *   **Slack (AWS SNS):** Publishing messages to Slack via AWS SNS (`Slack.AWS.Flow`).
    *   **Transaction Logs (ONDC):** Pushing transaction logs to ONDC (`TransactionLogs.Interface.ONDC`, `TransactionLogs.ONDC.Types`).

### `lib/location-updates/`
*   **Purpose:** Handles real-time location tracking and distance calculation.
*   **Key Functionality:**
    *   **Interpolation and Distance Calculation:** Logic for interpolating location points and calculating distances, often integrating with external map services (OSRM, Google Maps).
    *   **Redis Integration:** Stores and retrieves location data in Redis.

### `lib/payment/`
*   **Purpose:** Manages all payment-related functionalities, including orders, transactions, payouts, and refunds.
*   **Key Functionality:**
    *   **Domain Types:** Defines domain models for payment orders, transactions, payouts, and refunds (`Domain.Types.PaymentOrder`, `Domain.Types.PaymentTransaction`, `Domain.Types.PayoutOrder`, `Domain.Types.Refunds`).
    *   **Database Storage (Beam/Esqueleto):** Defines Beam table schemas (`Storage.Beam.PaymentOrder`, `Storage.Beam.PaymentTransaction`, `Storage.Beam.PayoutOrder`, `Storage.Beam.Refunds`) and provides query/update functions using Esqueleto/Sequelize-like wrappers (`Storage.Queries.PaymentOrder`, `Storage.Queries.PaymentTransaction`, `Storage.Queries.PayoutOrderExtra`).
    *   **External Payment Interface:** Integrates with Juspay payment gateway (`Kernel.External.Payment.Interface`).
    *   **Encryption:** Handles encryption and hashing of sensitive payment data.

### `lib/producer/`
*   **Purpose:** Responsible for producing messages to Kafka topics.
*   **Key Functionality:** Provides core logic for sending messages to Kafka (`Producer.Flow`, `Producer.SchedulerJob`).

### `lib/scheduler/`
*   **Purpose:** Manages background jobs and scheduled tasks.
*   **Key Functionality:** Provides a framework for defining, scheduling, and executing jobs, with support for different storage types (e.g., Redis, DB) (`Lib.Scheduler`, `JobStorageType.DB.Table`, `JobStorageType.Redis.Queries`).

### `lib/sessionizer-metrics/`
*   **Purpose:** Collects and streams session-related metrics and events.
*   **Key Functionality:**
    *   **Event Definition:** Defines a comprehensive `Event` data type for metrics.
    *   **Event Streaming:** Dispatches events to Kafka and Prometheus (`EventStream`, `Kafka.Internal`, `Prometheus.Internal`).
    *   **Configuration:** Manages Kafka and Prometheus configurations for event streaming.

### `lib/shared-services/`
*   **Purpose:** Provides common, reusable services and utilities shared across the entire backend.
*   **Key Functionality:**
    *   **Issue Management:** Handles issue/grievance management, including API definitions, domain types, and storage (`IssueManagement.API`, `IssueManagement.Domain.Types`, `IssueManagement.Storage`).
    *   **Registry Interaction:** Provides interfaces for interacting with the BECKN registry.
    *   **Config Pilot Frontend:** Related to configuration management for the frontend.

### `lib/special-zone/`
*   **Purpose:** Manages special geographical zones and associated gates (e.g., airports, railway stations).
*   **Key Functionality:**
    *   **Domain Types:** Defines `SpecialLocation`, `GateInfo`, and related types (`Lib.Types.SpecialLocation`, `Lib.Types.GateInfo`).
    *   **Database Storage:** Defines Esqueleto table schemas (`Lib.Tabular.SpecialLocation`, `Lib.Tabular.GateInfo`, `Lib.Tabular.SpecialLocationGeom`, `Lib.Tabular.GateInfoGeom`) and provides query functions (`Lib.Queries.SpecialLocation`, `Lib.Queries.GateInfo`).
    *   **Geospatial Queries:** Performs queries based on geographical data.

### `lib/utils/`
*   **Purpose:** A collection of general-purpose utility functions.
*   **Key Functionality:**
    *   **Common Utilities:** General helper functions (`Utils.Common.Events`, `Utils.Common.JWT`).
    *   **Dynamic Logic:** Tools for dynamic logic.
    *   **CAC (Client Application Configuration):** Utilities for client-side configuration.

### `lib/yudhishthira/`
*   **Purpose:** Likely a business rule engine or decision-making module.
*   **Key Functionality:**
    *   **Event Handling:** Defines event types and job logic related to "Kaal Chakra" (time-based events/jobs) (`Lib.Yudhishthira.Event.KaalChakra.Jobs`).
    *   **Storage:** Interacts with database storage for dynamic logic elements and user data (`Lib.Yudhishthira.Storage`).
    *   **Types:** Defines various domain-specific types (`Lib.Yudhishthira.Types`).

## 2. Application Services (`app/` directory)

These are the executable microservices that compose the Nammayatri backend.

### `app/rider-platform/rider-app/`
*   **Purpose:** The main customer-facing application.
*   **Key Functionality:** Exposes APIs for booking, search, and payment.

### `app/rider-platform/rider-app-scheduler/`
*   **Purpose:** Handles background jobs and scheduled tasks for the rider platform.

### `app/rider-platform/search-result-aggregator/`
*   **Purpose:** Aggregates search results from multiple providers for the rider platform.

### `app/rider-platform/public-transport-rider-platform/`
*   **Purpose:** Manages Fixed Route Fixed Schedule (FRFS) services for public transport.

### `app/provider-platform/dynamic-offer-driver-app/`
*   **Purpose:** The main driver-facing application.
*   **Key Functionality:** Exposes APIs for driver management and business logic.

### `app/provider-platform/dynamic-offer-driver-app/Allocator/`
*   **Purpose:** The core ride allocation engine.

### `app/dashboard/rider-dashboard/`
*   **Purpose:** Provides an operational dashboard for customer-related activities.

### `app/dashboard/provider-dashboard/`
*   **Purpose:** Provides an operational dashboard for driver/fleet management.

### `app/safety-dashboard/`
*   **Purpose:** Provides a dashboard for safety monitoring and management.

### `app/kafka-consumers/`
*   **Purpose:** Contains various Kafka consumers for processing different event streams.

### `app/mocks/`
*   **Purpose:** Provides mock implementations of external services for development and testing.

### Other `app/` services:
*   `alchemist/`: Likely a code generation or transformation tool.
*   `beckn-cli/`: A command-line interface tool for BECKN interactions.
*   `example-service/`: A template or example microservice.
*   `sdk-event-pipeline/`: Processes events from SDKs.
*   `special-zone/`: A dedicated service for managing special geographical zones.
*   `utils/image-api-helper/`: Helper for image-related API operations.
*   `utils/route-extractor/`: Extracts route information.

## 3. Testing Modules (`test/` directory)

*   **Purpose:** Contains unit and integration tests for various parts of the backend.
*   **Key Functionality:**
    *   **`beckn-test/`:** General BECKN protocol tests.
    *   **`location-updates/test/`:** Integration tests for location updates, including mock services and route data.
    *   **`yudhishthira/test/`:** Unit tests for the Yudhishthira module, including time-based job calculations.
