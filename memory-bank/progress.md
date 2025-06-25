# Progress

## What Works
- Initialized the memory bank.
- Created the core memory bank files.
- Created and updated `project-dependencies.md` with a Mermaid chart illustrating project and feature dependencies, including the Beckn ONDC flow.
- Created `beckn-ondc-flow/beckn-ondc-flow.md` with a detailed explanation of the ONDC flow for FRFS and OnDemand scenarios.
- Created `beckn-ondc/systemPatterns.md`, `beckn-ondc/techContext.md`, and `beckn-ondc/code-flow.md` to detail the Beckn layer, ACL transformation, and code-level flows.
- Initialized the memory banks for the rider-app, alchemist, and beckn-cli projects. The beckn-cli project is a command-line interface for interacting with the Beckn network, and it includes tools for generating key pairs and performing load testing using the K6 framework and Ed25519 algorithm.
- Initialized the memory banks for the dynamic-offer-driver-drainer, dynamic-offer-driver-app/Main, dynamic-offer-driver-app/Allocator, rider-app-drainer, rider-app/Main, rider-app/Scheduler, and rider-app/search-result-aggregator projects with detailed information about their architecture, APIs, and data models.
- Documented the Multimodal Public Transport (FRFS) flow in `memory-bank/multimodal-frfs-public-transport/` with `productContext.md`, `techContext.md`, `systemPatterns.md`, and `flowDiagram.md`.
- Updated memory bank files (`task-tracker.md`, `activeContext.md`, `progress.md`) to reflect the current task.

## What's Left to Build
- Implement the frontend.
- Implement the backend.
- Implement the database.
- Deploy the application.
- Implement the requested codebase updates: schema changes, API changes, and flow changes related to replacing `GTFSFeedInfo` with `IntegratedBppConfigId`.

## Current Status
- Planning the implementation of codebase updates.

## Current Project In Focus
- None
- Status: Not Started

## Known Issues
- None.

## Evolution of Project Decisions
- Decided to replace `GTFSFeedInfo` with `IntegratedBppConfigId` as `feedId` for querying GTFS InMemory server or OTP JourneyPlanning response.
