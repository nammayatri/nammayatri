# Tech Context: KV Framework

This document outlines the key technologies and dependencies that constitute the KV framework.

## Technologies Used

-   **Redis:** The primary in-memory Key-Value store. Used for high-speed transactional reads and writes, and as the source for Redis Streams.
-   **Redis Streams:** A durable, append-only log data structure in Redis, used for asynchronous communication and reliable queuing of data changes.
-   **Haskell (`Hedis` library):** The programming language and its Redis client library used for direct application interaction with Redis.
-   **Relational Database (e.g., PostgreSQL):** The main persistent data store where transactional data is eventually stored.
-   **Kafka:** A distributed streaming platform used for building real-time data pipelines, specifically for streaming data to the analytics database.
-   **ClickHouse:** A columnar database management system used for online analytical processing (OLAP) and high-volume analytical queries.
-   **Drainer Service (Haskell/Other):** A custom service responsible for consuming from Redis Streams and writing to the relational database.

## Dependencies

-   **`Hedis` library:** For Redis client operations in Haskell.
-   **Database drivers/ORMs:** For interaction with the main relational database (e.g., Beam for PostgreSQL).
-   **Kafka client libraries:** For publishing and consuming messages from Kafka.
-   **ClickHouse client libraries:** For interacting with ClickHouse.

## Tool Usage Patterns

-   **`createWithKV`, `updateOneWithKV`, `findOneWithKV`, `findAllFromKvRedis`, etc.:** These are high-level functions within the `Storage.Queries` modules that abstract the underlying Redis and database interactions, providing a unified interface for KV operations.
-   **`L.runKVDB meshConfig.kvRedis`:** This pattern is used to execute Redis commands within the application's flow, indicating direct interaction with the Redis instance.
-   **`L.sadd`, `L.smembers`, `L.expire`:** Specific Redis commands used for managing sets (for secondary indexing) and setting key expirations.
