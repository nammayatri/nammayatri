# Nammayatri Backend - Project Brief

## Overview
Nammayatri is a comprehensive mobility platform backend written in Haskell, implementing the BECKN/ONDC protocol for open network digital commerce. It powers both ride-hailing and public transport services, featuring sophisticated fare calculation, real-time driver allocation, and multi-modal transportation management.

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

