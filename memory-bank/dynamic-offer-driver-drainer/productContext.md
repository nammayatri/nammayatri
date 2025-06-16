# Product Context

## Why This Project Exists
- To ensure data consistency and integrity for dynamic offer and driver-related data across different data stores (Redis and PostgreSQL) in an asynchronous manner.
- To offload synchronous database writes from the main application flow for dynamic offer and driver data, improving performance and scalability.

## Problems It Solves
- Synchronizes data changes (Create, Update, Delete) from Redis Streams to PostgreSQL for dynamic offer and driver data.
- Handles potential database failures by retrying operations and providing a "force drain" mechanism for this specific domain.
- Enables selective data persistence to PostgreSQL and/or pushing to Kafka for dynamic offer and driver data.

## How It Should Work
- The dynamic-offer-driver-drainer service continuously reads database commands from a Redis Stream.
- It parses and validates these commands, generating corresponding SQL queries for dynamic offer and driver tables.
- It executes SQL queries against PostgreSQL using a connection pool.
- It handles errors during query execution and Kafka pushes, with configurable retry and failure handling strategies.
- It periodically synchronizes KV configurations from PostgreSQL to Redis.

## User Experience Goals
- N/A (This is a backend service with no direct user interface).
