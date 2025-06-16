# Product Context

## Why This Project Exists
- To ensure data consistency and integrity across different data stores (Redis and PostgreSQL) in an asynchronous manner.
- To offload synchronous database writes from the main application flow, improving performance and scalability.

## Problems It Solves
- Synchronizes data changes (Create, Update, Delete) from Redis Streams to PostgreSQL.
- Handles potential database failures by retrying operations and providing a "force drain" mechanism.
- Enables selective data persistence to PostgreSQL and/or pushing to Kafka.

## How It Should Work
- The rider-app-drainer service continuously reads database commands from a Redis Stream.
- It parses and validates these commands, generating corresponding SQL queries.
- It executes SQL queries against PostgreSQL using a connection pool.
- It handles errors during query execution and Kafka pushes, with configurable retry and failure handling strategies.
- It periodically synchronizes KV configurations from PostgreSQL to Redis.

## User Experience Goals
- N/A (This is a backend service with no direct user interface).
