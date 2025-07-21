# Feature: Shared Ride Matching and Fulfillment

### Overview

We are introducing a "Shared Ride" option into the existing ride flow. In this mode, two or more customers with overlapping routes can share a ride and split part of the fare.

---

### End-to-End Flow

1. Customer initiates a search.
2. Backend checks if nearby riders are also searching.
3. If enough riders are found, we offer a shared ride estimate.
4. If user selects shared ride:
   - Ask them to wait.
   - Add them to a rider pool.
   - A background cron pairs riders.
5. Once paired:
   - Find nearby drivers.
   - Send requests.
   - First accepting driver gets the ride.
6. Driver picks up all customers from nearest to farthest.
7. Driver drops off all customers and collects fare.
8. Fare from latest pickup to earliest drop is split evenly; deviations handled by individual customers.
9. Cancellation:
   - Allowed by both drivers and customers until **both pickups are complete**.

---

### Design Considerations

- Shared rides are opt-in at the estimate stage.
- Rider pairing is asynchronous via a cron job.
- Driver matching is triggered **after** riders are paired.
- Ride object or a Parent Shared ride object must support **multi-rider state tracking**. (We can have a parent child system for this, discussed in the flowchart)

---
### New Design Insights (from Chunk 2)

- **Seat Selection**: The user must specify the number of seats (`numSeats`) they require when selecting a shared ride.
- **Capacity Validation**: The system must enforce that the requested `numSeats` is less than the vehicle's total capacity minus one, to ensure there is space for at least one other rider.
- **Estimate State**: The system must validate that the estimate has not been cancelled before proceeding with the pooling logic.
- **Rider Waiting Pool**: If an immediate match isn't found, the rider's request is temporarily stored (e.g., in a Redis GSI with a 5-minute TTL) to be available for subsequent matching attempts.

---
### New Design Insights (from Chunk 3)

- **Batch Entity**: After successful rider pairing, a `batch` entity must be created in the database to formally group the riders.
- **Cumulative Estimate Entity**: A `cumulative_estimate` record must be created for the entire batch. This record is critical for driver pooling and should contain:
    - `batchId` (linking back to the batch entity)
    - A list of all individual `estimateIds` included in the batch.
    - All individual pickup and drop-off locations for the entire journey.
- **Driver Pooling**: A dedicated "Share Ride Driver Pooling Logic" will be invoked using the `cumulative_estimate` to find a suitable driver.
- **Post-Match Cancellation Handling**: The system must be resilient to cancellations that occur *after* a driver is matched to a batch. It requires re-validating that the remaining group is still viable (i.e., `status != Cancelled` and `customer_count > threshold`) before proceeding.
- **Driver Search Failure**: If no driver is found for a batch, the system must gracefully handle the failure by releasing locks on all involved customers and placing the most recent customer back into the waiting pool for another attempt.

---
### New Design Insights (from Chunk 4)

- **Batched Bookings and Rides**: The system will create individual `booking` and `ride` records for each customer, but these will be linked via a `batchBookingId` and `batchRideId` respectively. This maintains individual ride integrity while managing them as a group.
- **ONDC Compatibility**: A crucial consideration is ensuring the proposed booking flow is compatible with ONDC standards. The `init`, `on_init`, `confirm`, and `on_confirm` request/response models may need to be adapted for batching.
- **Pickup/Drop-off Route Optimization**: The system must calculate and provide the driver with an optimized route, sequencing pickups from nearest to farthest, and then sequencing drop-offs similarly.
- **Granular State Updates**: The state for each customer's booking and ride must be updated independently as the driver completes each pickup and drop-off milestone.
- **Advanced Cancellation Logic**:
    - **Driver Cancellation (Pre-Pickup)**: Must trigger a driver re-allocation process and extend the batch's time-to-live to allow for the new search.
    - **Customer Cancellation (Post-Match)**: Must check if the remaining group is still viable (i.e., has at least 2 riders) before continuing or dissolving the batch.
- **Open Discussion Point**: The "Customer No-Show" scenario needs a defined business logic, potentially involving a penalty system (e.g., "Coins flow").

---
### New Design Insights (from Chunk 5 - Rider Pooling Deep Dive)

- **Rider Pooling GSI**: The core of the rider pooling logic is a Redis Geospatial Index (`ShareRideCustomerLoc`) that stores waiting customers.
- **GSI Member Structure**: The `member` field in this GSI is critical, encoding multiple pieces of data: `searchId:validTill:numSeats`. This allows for efficient filtering.
- **Multi-stage Filtering**: Finding a match is not a simple radius search. It involves a cascade of strict filters to ensure ride compatibility:
    1.  **Lock Check**: Prevents a user from being added to multiple batches simultaneously.
    2.  **Time-to-Live Check**: Ensures a ride can be completed before the user's search expires.
    3.  **Capacity Check**: Matches riders based on available seats.

---
### New Design Insights (from Chunk 6 & 7 - Advanced Rider Pooling Refined)

- **Advanced Matching Formulas**: The logic for finding a match goes beyond simple proximity and includes specific formulas for filtering:
    - **Drop-off Deviation**: A formula (`((distance_between_drops) / min_ride_distance) * 100 > x3`) is used to prevent pairing riders whose destinations are too far apart relative to their individual trip lengths.
    - **Route Overlap Percentage**: A geo-hashing strategy (precision 9) is employed to calculate the percentage of route overlap between two potential riders. A configurable threshold (`x4`) ensures a significant portion of the journey is shared.
- **Vehicle-Specific Batching Rules**: The final composition of a batch is dependent on the vehicle type, with different rules for `AUTO` (max 1 additional rider) versus `CAR` (max 2 additional riders, with seat capacity checks).
- **Locking Mechanism**: A locking mechanism on `searchId` is critical to prevent race conditions where a single rider is assigned to multiple batches simultaneously. The system iterates through filtered candidates and attempts to lock them one by one until a valid group is formed.

---
### New Design Insights (from Chunk 8 - Driver Pooling Deep Dive)

- **BPP-Side Cumulative Estimate**: The driver pooling logic is initiated on the BPP side, requiring the creation of a `cumulative_estimate` record there, based on the data aggregated from the core backend.
- **Longest Leg Routing**: The primary strategy for finding drivers is to use the "longest leg" of the batched journey (the maximum distance from one rider's pickup to another's drop-off) as the basis for the search.
- **Driver Pooling Fallback**: The system should include a fallback mechanism. If a driver search fails using the longest leg of the first customer, it should retry the search using the routes of the other customers in the batch.
- **FCM Notification**: Drivers will be notified of a shared ride request via a standard FCM push notification, but with an additional flag in the data payload to identify it as a shared ride, allowing the driver app to handle it differently.

---
### New Design Insights (from Chunk 9 - Asynchronous Pooling)

- **Cron Job Trigger**: The asynchronous part of the rider pooling is driven by a cron job that runs at a regular, configurable interval.
- **State Management**: The cron job uses the same `ShareRideCustomerLoc` GSI as the synchronous flow. It relies on the `validTill` timestamp and a locking mechanism to manage the state of waiting riders and avoid processing requests that are expired or already being handled.
- **Re-use of Logic**: The asynchronous job is efficient as it re-uses the core **Rider Pooling Logic** (detailed in previous chunks) for each waiting rider, rather than implementing a separate matching algorithm.
- **Asynchronous Initiation**: When the cron job finds a successful match, it triggers the **Batch Creation** flow asynchronously.
