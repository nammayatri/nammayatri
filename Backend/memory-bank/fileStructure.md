# Nammayatri Backend - File Structure Overview

The Nammayatri backend project is organized into a multi-package Cabal project, primarily structured under the `Backend/` directory. This structure facilitates modularity, separation of concerns, and independent development/deployment of microservices.

## 1. Top-Level Directories

*   **`Backend/`**: Contains all source code, configurations, and related files for the Haskell backend.
*   **`Frontend/`**: Contains code related to the frontend applications (e.g., Android native).
*   **`docs/`**: General project documentation.

## 2. `Backend/` Directory Structure

The `Backend/` directory is the core of the Haskell application and is further organized as follows:

*   **`app/`**: This directory holds the executable microservices. Each subdirectory within `app/` typically represents a distinct service.
    *   `app/alchemist/`: Code generation or transformation tool.
    *   `app/beckn-cli/`: Command-line interface for BECKN interactions.
    *   `app/dashboard/`: Contains various dashboard services (`CommonAPIs`, `provider-dashboard`, `rider-dashboard`).
    *   `app/example-service/`: A template or example microservice.
    *   `app/kafka-consumers/`: Services dedicated to consuming messages from Kafka topics.
    *   `app/mocks/`: Mock implementations of external services for testing (e.g., `fcm`, `google`, `idfy`, `public-transport-provider-platform`, `rider-platform`, `sms`).
    *   `app/provider-platform/`: Services specific to the driver/fleet platform (`driver-tracking-health-check`, `dynamic-offer-driver-app`, `dynamic-offer-driver-drainer`).
    *   `app/rider-platform/`: Services specific to the rider/customer platform (`public-transport-rider-platform`, `rider-app`, `rider-app-drainer`).
    *   `app/safety-dashboard/`: Safety monitoring dashboard.
    *   `app/scheduler-example/`: Example for scheduler integration.
    *   `app/sdk-event-pipeline/`: Processes events from SDKs.
    *   `app/special-zone/`: Service for managing special geographical zones.
    *   `app/utils/`: Utility services (`image-api-helper`, `route-extractor`).

*   **`lib/`**: This is a crucial directory containing shared libraries and core functionalities that are reused across multiple `app/` services. Each subdirectory here is typically a Haskell package.
    *   `lib/beckn-services/`: Common BECKN-related services (e.g., AWS S3 integration, registry interaction).
    *   `lib/beckn-spec/`: Defines BECKN/ONDC protocol data types and API specifications.
    *   `lib/dashcam/`: Dashcam data processing.
    *   `lib/external/`: Integrations with external third-party APIs (e.g., Email, Chat Completion, Kafka, Slack, Transaction Logs).
    *   `lib/location-updates/`: Real-time location tracking and distance calculation.
    *   `lib/payment/`: All payment-related functionalities.
    *   `lib/producer/`: Kafka message production.
    *   `lib/scheduler/`: Background job scheduling framework.
    *   `lib/sessionizer-metrics/`: Session-related metrics and event streaming.
    *   `lib/shared-services/`: General reusable services (e.g., issue management, registry).
    *   `lib/special-zone/`: Management of special geographical zones.
    *   `lib/utils/`: General-purpose utility functions.
    *   `lib/webhook/`: Webhook handling.
    *   `lib/yudhishthira/`: Business rule engine/decision-making module.

*   **`test/`**: Contains unit and integration tests for the backend.
    *   `test/beckn-test/`: General BECKN protocol tests.
    *   `test/app/`: Main test entry points.
    *   `test/src/`: Test source files.

*   **`memory-bank/`**: This directory contains documentation and knowledge base files generated or maintained by Cline, providing context about the project.
    *   `activeContext.md`
    *   `productContext.md`
    *   `progress.md`
    *   `projectbrief.md`
    *   `systemPatterns.md`
    *   `techContext.md`
    *   `haskellPatterns.md` (newly created)
    *   `systemArch.md` (newly created)
    *   `keyModules.md` (newly created)
    *   `AST.md` (newly created)
    *   `fileStructure.md` (this file)

*   **`dhall-configs/`**: Configuration files written in Dhall.
*   **`doc/`**: Additional documentation (e.g., architecture, auth, databases).
*   **`docs/`**: More general documentation (e.g., database schema).
*   **`nix/`**: Nix-related configuration files for reproducible builds and development environments.
*   **`swagger/`**: OpenAPI/Swagger documentation files.
*   **`blacklist_geo_config/`**: Geographical configuration for blacklisted areas.
*   **`geo_config/`**: Geographical configuration files.
*   **`dev/`**: Development-specific scripts and configurations (e.g., `psql.sh`, `sql_dump.sh`, `stan.sh`, `clickhouse`, `grafana`, `migrations`).
*   **`load-test/`**: Load testing configurations and scripts.
*   **`newman-tests/`**: Newman (Postman collection runner) tests.
*   **`.envrc.backend`, `.envrc.frontend`**: `direnv` configuration files for setting up environment variables.
*   **`.gitignore`, `.gitattributes`**: Git configuration files.
*   **`cabal.project`**: Cabal project file for multi-package management.
*   **`flake.nix`, `flake.lock`**: Nix flake files for dependency management.
*   **`README.md`**: Project README.
