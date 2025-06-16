# Product Context

## Why This Project Exists
- To provide the core application logic for the rider app.

## Problems It Solves
- Handles user authentication, ride requests, and other core functionalities.
- Manages high-volume transactional data efficiently using a Redis KV store and asynchronous database updates.
- Provides robust error handling and fault tolerance for data persistence.
- Enables advanced analytics and historical data querying through ClickHouse integration.
- Facilitates seamless communication and data transformation within the Beckn Protocol ecosystem.

## How It Should Work
- The rider-app/Main application should implement the core logic for the rider app.
- It exposes a comprehensive set of APIs for user authentication, profile management, payments, ride search and booking, ride management, support and feedback, mapping and location, public transport (FRFS), multimodal journeys, and BBPS.
- It uses a DTO approach for data mapping: Tabular time -> Beam ORM -> Domain type.
- API endpoints are categorized into UI, Internal, Dashboard, and Beckn for clear separation of concerns and interaction patterns.
- Business logic for APIs is encapsulated within `Domain/Action` modules, with each file often corresponding to a new feature.

## User Experience Goals
- N/A (This is a backend service with no direct user interface).
