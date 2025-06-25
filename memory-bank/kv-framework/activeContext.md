# Active Context: KV Framework

This document captures the current understanding and active considerations regarding the KV framework.

## Current Work Focus

-   Deep dive into the KV framework's architecture, query patterns, and key concepts (Primary and Secondary Keys).
-   Understanding the interplay between Redis as a transactional KV store, asynchronous persistence to the main database, and data streaming to ClickHouse for analytics.

## Recent Changes

-   Clarified that Redis is the immediate transactional KV store, not just a caching layer.
-   Identified high-level KV functions (`createWithKV`, `updateOneWithKV`, `findOneWithKV`, `findAllFromKvRedis`, etc.) in `Storage.Queries` modules as the primary interface to the KV framework.
-   Confirmed the implementation of secondary keys using Redis Sets (`L.sadd`, `L.smembers`) in functions like `appendByDriverPhoneNumber`.

## Next Steps

-   Ensure comprehensive documentation of the KV framework in the Memory Bank.
-   Verify that the understanding aligns with all aspects of the system's data flow.

## Active Decisions and Considerations

-   The distinction between synchronous operations to Redis and asynchronous operations to the main DB and ClickHouse is crucial for understanding performance characteristics and data consistency models.
-   The abstraction provided by `Storage.Queries` functions simplifies application-level interaction with the complex KV architecture.

## Learnings and Project Insights

-   The KV framework is a critical component for achieving high throughput and low latency in transactional operations.
-   The asynchronous data flow to the main DB and analytics platform is key to preventing bottlenecks and enabling scalable reporting.
-   The use of Redis Streams and Kafka provides a robust and fault-tolerant mechanism for data propagation.
