# System Patterns: KV Framework

The KV framework employs a layered architecture to manage data flow and ensure high performance, scalability, and consistency.

## Architecture Overview

```mermaid
graph TD
    A[Application Layer] -->|Synchronous R/W| B(KV Store: Redis)
    B -->|Synchronous Push| C[Redis Stream]
    C -->|Asynchronous Consume & Persist| D[Drainer Service]
    D --> E[Main Relational DB]
    C -->|Asynchronous Stream| F[Kafka]
    F --> G[ClickHouse (Analytics DB)]

    subgraph Write Path
        A --> B
        B --> C
        C --> D
        D --> E
        C --> F
        F --> G
    end

    subgraph Read Path
        A --> B
        B --> E_fallback[Main Relational DB (Fallback)]
    end
```

## Key Technical Decisions

-   **Redis as Primary KV Store:** Chosen for its in-memory speed, supporting low-latency transactional reads and writes.
-   **Redis Streams for Durability & Asynchronicity:** Acts as a reliable message queue for all write operations, decoupling application writes from database persistence.
-   **Drainer Service:** A dedicated microservice responsible for consuming events from the Redis Stream and batch-writing them to the main relational database, preventing database overload.
-   **Kafka for Analytics Pipeline:** Provides a scalable and fault-tolerant backbone for streaming data to the analytical database (ClickHouse).
-   **ClickHouse for Analytics:** A columnar database optimized for high-volume analytical queries, ensuring operational database performance is not impacted by reporting.

## Design Patterns in Use

-   **Command Query Responsibility Segregation (CQRS) Principles:** While not a full CQRS implementation, the separation of immediate writes (to Redis) from eventual persistence (to DB) and analytics (to ClickHouse) aligns with CQRS principles, optimizing for different data access patterns.
-   **Cache-Aside / Read-Through (for Reads):** Applications first attempt to read from Redis. On a cache miss, data is fetched from the main DB and then populated into Redis.
-   **Write-Through / Write-Behind (for Writes):** Writes are immediately applied to Redis (write-through for immediate consistency), and then asynchronously propagated to the main DB (write-behind for eventual consistency).
-   **Event-Driven Architecture:** The use of Redis Streams and Kafka facilitates an event-driven approach for data propagation and processing.

## Component Relationships

-   **Application & Redis:** Direct, synchronous interaction for all transactional data operations.
-   **Redis & Redis Stream:** Redis operations trigger events pushed to the Redis Stream.
-   **Redis Stream & Drainer Service:** The Drainer Service is a consumer of the Redis Stream.
-   **Drainer Service & Main DB:** The Drainer Service writes processed data to the main relational database.
-   **Redis Stream & Kafka:** Events from the Redis Stream are mirrored or transformed and pushed to Kafka topics.
-   **Kafka & ClickHouse:** ClickHouse consumes data from Kafka for analytical purposes.
