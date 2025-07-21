# Shared Ride Flow - Core Logic Summary

This document summarizes the core logic extracted from the shared ride feature flowchart. It will be populated as flowchart chunks are processed.

---

## Chunk 1: Initial Search and Pre-Pooling Checks

1.  **Customer Search Initiation**: The flow begins when a customer initiates a ride search.

2.  **Route Caching**:
    *   Upon search, the system caches the route response (e.g., from Google Maps) against the unique `searchId`.
    *   The cache timeout is set to accommodate the entire pooling process (rider pooling + driver pooling + buffer time).

3.  **Geospatial Indexing for Hotspots**:
    *   The `searchId` is added to a Redis Geospatial Index (GSI) named `searchHotSpots`. This allows for efficient querying of nearby active searches.

4.  **Check for Nearby Riders**:
    *   The system queries the `searchHotSpots` GSI to find the number of other active searches within a configurable radius (X).
    *   It checks if the count of nearby searches meets a configurable threshold (which must be > 1, as it includes the current customer).

5.  **Decision: Offer Shared Ride**:
    *   **If sufficient riders are NOT found**: The shared ride option is not presented. The flow proceeds as a standard, non-shared ride request.
    *   **If sufficient riders ARE found**: The system proceeds to show estimates for all applicable ride types, *including* shared ride options for each vehicle category (e.g., "Auto Share," "Cab Share").

6.  **Decision: Customer Selects Shared Ride**:
    *   The system checks if the customer selected one of the shared ride estimates.
    *   **If NOT selected**: The flow proceeds with the selected standard ride type.
    *   **If selected**: The flow proceeds to the next validation step.

7.  **KYC Verification**:
    *   The system checks if the customer has a completed KYC (Know Your Customer) verification.
    *   **If KYC is NOT complete**: The customer is prompted to complete their KYC. After completion, the flow loops back to re-initiate the search.
    *   **If KYC is complete**: The flow continues to the next stage.

---

## Chunk 2: Pre-Pooling Validation and Rider Pooling Logic

*This logic follows the successful KYC verification from Chunk 1.*

1.  **Seat Selection & Validation**:
    *   The customer selects the desired number of seats (`numSeats`).
    *   The system validates that `numSeats` is less than the vehicle's maximum capacity minus one.
    *   **If validation fails**: An error is thrown.

2.  **Estimate Status Validation**:
    *   The system checks that the status of the selected estimate is not "Cancelled".
    *   **If cancelled**: The `searchId` and its associated route cache are removed from Redis, and the flow terminates for this user.

3.  **Invoke Rider Pooling Logic**:
    *   If all validations pass, the core **Rider Pooling Logic** is invoked. This is a central component responsible for matching riders.
    *   **Input**: The `numSeats` required by the customer.

4.  **Handle Pooling Results**:
    *   **Case 1: No Match Found (Pooling returns an empty list)**
        *   The customer is informed they need to wait.
        *   Their location and search details are added to a geospatial index with a temporary key (`searchId:validTill`) that expires in 5 minutes, making them available for subsequent pooling attempts by other users.
    *   **Case 2: Match Found (Pooling returns a non-empty list of `searchIds`)**
        *   A successful pairing has been made.
        *   The flow proceeds to the next step (presumably driver matching, to be detailed in a future chunk).

---

## Chunk 3: Batch Creation and Driver Pooling

*This logic follows a successful rider pairing from Chunk 2.*

1.  **Pre-Batching Validation**:
    *   Before creating a ride batch, the system performs a final check: `estimate.status != Cancelled AND paired_customers.length > threshold`.
    *   **If this fails**: The lock acquired on the paired `searchIds` during the pooling logic is released, and the process is aborted for this group.

2.  **Batch and Cumulative Estimate Creation**:
    *   If validation passes, the system creates two key records:
        *   A **`batch` table entry** to group the paired riders.
        *   A **`cumulative_estimate` record** for the shared ride.
    *   The `cumulative_estimate` must contain the `batchId`, a list of all individual `estimateIds`, and the pickup/drop-off coordinates for all riders in the batch.

3.  **Invoke Driver Pooling Logic**:
    *   The newly created `cumulative_estimate` is passed as input to the **Share Ride Driver Pooling Logic**.

4.  **Handle Driver Pooling Results**:
    *   **Case 1: No Driver Found**:
        *   The system releases the locks acquired on all customers in the batch.
        *   The newest customer from the failed batch is placed back into the waiting pool (Redis GSI with a 5-minute TTL) to be matched again.
    *   **Case 2: Driver Accepts**:
        *   The flow proceeds to a post-acceptance validation step.

5.  **Post-Acceptance Cancellation Check**:
    *   This step handles cases where a customer might cancel *after* a driver has been found for the batch.
    *   It re-validates that `cumulative_estimate.status != Cancelled AND remaining_customers.length > threshold`.
    *   **If this fails**: The locks on the remaining customers' `searchIds` are released.
    *   **If it passes**: The flow continues to the final ride confirmation stage.

---

## Chunk 4: Booking, Fulfillment, and Exception Handling

*This logic follows a successful driver acceptance from Chunk 3.*

1.  **Booking and Ride Creation**:
    *   For each customer in the batch, a separate `booking` record is created. These are linked by a common `batchBookingId`.
    *   Similarly, upon confirmation, individual `ride` records are created and linked by a `batchRideId`.
    *   **Note**: ONDC compatibility needs to be checked, as request types for `init`, `on_init`, etc., may need to be adapted.

2.  **Pickup Sequence**:
    *   The driver is directed to pick up customers in order of proximity, from nearest to farthest.
    *   The system must provide the driver's mapping application with the correctly sorted pickup locations.
    *   As each pickup is completed, the corresponding customer's `booking` and `ride` status is updated.

3.  **Drop-off Sequence**:
    *   After all pickups are complete, the driver is shown the drop-off locations, also sorted from nearest to farthest.
    *   As each destination is reached, the system triggers the `endRide` process for that customer, which includes fare collection and ride completion.
    *   **Note**: A "Proximity based end ride" is mentioned as a potential enhancement.

4.  **Exception Handling - Cancellations**:
    *   **Driver Cancels (pre-pickup)**: If the driver cancels for an unpicked customer, the system should attempt to re-allocate a new driver. The batch's expiry time in the GSI should be extended to allow for this. Fare and status for the affected booking/ride must be updated.
    *   **Customer Cancels (post-driver-match)**:
        *   The system checks if the number of remaining customers is still `>= 2`.
        *   **If YES**: The ride continues. The driver is notified via FCM with the updated route.
        *   **If NO**: The entire batch ride is cancelled. All associated bookings are cancelled, and customer locks are released.
    *   **Customer No-Show**: This scenario is marked as needing further discussion, potentially involving a "Coins flow" for penalties.

---

## Chunk 5: Rider Pooling Logic (Detailed Drill-Down)

*This section provides a detailed breakdown of the "Invoke Rider Pooling Logic" step mentioned in Chunk 2.*

1.  **GSI Structure**: The logic relies on a Geospatial Index with the following structure:
    *   **Key**: `ShareRideCustomerLoc`
    *   **Member**: `searchId:validTill:numSeats`
    *   **Coordinates**: Latitude and Longitude of the customer's pickup location.

2.  **Initial Query**:
    *   The logic starts by querying the GSI to find all waiting customers within a radius `R` of the current customer's pickup location.

3.  **Filtering Cascade**: The returned list of potential matches is passed through a series of filters:
    *   **Filter 1: Locked Searches**: Remove any `searchIds` that are already locked (i.e., are part of another pending batch).
    *   **Filter 2: Expiry Time**: Remove searches where the `validTill` timestamp is less than `Y` seconds away. `Y` is a buffer to ensure there's enough time to complete the batching and driver pooling process. (Note: The exact calculation of `Y` is marked as "Under Review").
    *   **Filter 3: Seat Availability**: Remove searches where the requested `numSeats` is greater than the number of available seats in the current customer's potential vehicle.
    *   **Filter 4: Destination Proximity**: Remove searches whose destination is outside a radius `x1` of the current customer's destination.
    *   **Filter 5: Pickup Proximity**: Remove searches whose pickup location is more than `x2` units of actual travel distance (not just radius) away from the current customer's pickup.
    *   **Filter 6: Drop-off Proximity**: Remove searches whose drop-off location is more than `x6` units of actual travel distance away from the current customer's drop-off.

4.  **Output**:
    *   The logic returns the list of `searchIds` that survive the filtering cascade. This list is then used in the subsequent "Handle Pooling Results" step (Chunk 2, Step 4).

---

## Chunk 6: Rider Pooling Logic (Advanced Filtering)

*This section details the final filtering stages within the Rider Pooling Logic, occurring after the initial filters from Chunk 5.*

1.  **Advanced Drop-off Distance Check**:
    *   For the remaining potential matches, the system performs a more complex check on drop-off locations.
    *   **Logic**: It calculates the ratio of the distance between any two customers' drop-offs against the minimum ride distance of all filtered rides. `((distance_between_drops) / min_ride_distance) * 100 > x3`.
    *   Any pair of searches that exceeds the configurable threshold `x3` is filtered out.

2.  **Route Overlap Analysis (Geo-hashing)**:
    *   **Step A: Hashing the Primary Route**: The system iterates through all coordinate points of the *current customer's* route, calculates a geo-hash (precision 9) for each point, and stores them in a HashSet for efficient lookup.
    *   **Step B: Comparing Potential Matches**: For each remaining `searchId` in the potential match list:
        *   The system iterates through the points of that search's route.
        *   It counts how many of its geo-hashes are present in the primary customer's HashSet (`matchCnt`).
    *   **Step C: Overlap Threshold Check**: A match is only considered valid if the percentage of overlapping route points exceeds a configurable threshold `x4`. `((matchCnt / total_points_in_route) * 100 > x4)`.
    *   Searches that fail this check are rejected.

3.  **Final Grouping and Lock Acquisition**:
    *   For each search that passes all filters, the system attempts to acquire a lock on the `searchId`.
    *   **If lock fails**: The search is rejected.
    *   **If lock succeeds**: The search is added to the final list of matched riders.

4.  **Final Vehicle Capacity Check**:
    *   The system performs a final check based on vehicle type:
        *   **For AUTO**: The final list size must be `== 1`.
        *   **For CAR**: The final list size must be `== 2`, and the sum of `numSeats` for all riders must be less than or equal to the remaining capacity.
    *   If these conditions are not met, the process breaks, and the system moves on. If they are met, the final list of `searchIds` is returned, and the flow proceeds to Batch Creation (Chunk 3).

---

## Chunk 7: Rider Pooling Logic (Final Filters & Grouping)

*This section details the final filtering stages within the Rider Pooling Logic, occurring after the initial filters from Chunk 5 & 6.*

1.  **Drop-off Proximity (x6 units)**:
    *   Filters out `searchIds` whose drop-off points are more than `x6` travel units away from the current search's drop-off.

2.  **Drop-off Deviation Ratio (x3 threshold)**:
    *   For remaining pairs, it calculates a deviation ratio: `((distance between drops) / min ride distance) * 100`.
    *   If this ratio is greater than a configurable threshold `x3`, the pair is rejected.

3.  **Route Overlap via Geo-hashing (x4 threshold)**:
    *   The primary customer's route is converted into a HashSet of geo-hashes (precision 9).
    *   For each potential match, the system calculates the percentage of its route's geo-hashes that exist in the primary customer's HashSet.
    *   If this overlap percentage is not greater than a threshold `x4`, the match is rejected.

4.  **Final Grouping and Capacity Check**:
    *   The logic iterates through the final filtered list of potential `searchIds`.
    *   It attempts to acquire a lock on each `searchId`. If successful, it adds the search to the final list to be returned.
    *   It performs a final check on the list size based on vehicle category (`AUTO` size == 1, `CAR` size == 2) and total `numSeats`.
    *   If the conditions are met, the loop breaks, and the final list is returned. Otherwise, the process continues until a valid group is formed or the list is exhausted.

---

## Chunk 8: Driver Pooling Logic (Detailed Drill-Down)

*This section provides a detailed breakdown of the "Invoke Driver Pooling Logic" step mentioned in Chunk 3.*

1.  **Input**: The logic takes the `cumulative_estimate` as input.

2.  **BPP-Side Estimate Creation**:
    *   The system uses the data from the `cumulative_estimate` (list of `estimateIds`, pickup/drop-off locations) to create a new `cumulative_estimate` on the BPP (Beckn Provider Platform) side.

3.  **Route Optimization**:
    *   The system determines the longest possible ride within the batch by calculating the distance from each rider's pickup point to every other rider's drop-off point.
    *   The pair of `(pickupLatLon, dropLatLon)` with the maximum distance is selected as the primary route for finding drivers.

4.  **Driver Search**:
    *   The system uses the selected longest-distance route and the `cumulativeEstimateID` to invoke the existing driver pooling logic.
    *   **Note**: A fallback mechanism is mentioned. If no drivers are found using the longest route from one customer, the system should ideally try the routes of the other customers in the batch.

5.  **Request to Drivers**:
    *   The ride request is sent to the pooled drivers.
    *   A special flag is included in the FCM (Firebase Cloud Messaging) notification to indicate that it is a "Share Ride" request.

6.  **Output**:
    *   The logic concludes when a driver accepts the request, at which point the flow continues to the Post-Acceptance steps (Chunk 4, Step 5).

---

## Chunk 9: Asynchronous Rider Pooling (Cron Job)

*This section details the asynchronous cron job that periodically attempts to match riders who are in the waiting pool.*

1.  **Trigger**: A cron job runs at a configurable interval (e.g., every `x` seconds).

2.  **Fetch Waiting Riders**: The job fetches all members from the `ShareRideCustomerLoc` Geospatial Index.

3.  **Initial Filtering**:
    *   It filters out any `searchIds` that are already locked or are about to expire.
    *   The remaining `searchIds` are stored in a HashMap for efficient processing.

4.  **Iterative Matching Loop**: The job iterates through the HashMap of waiting riders.
    *   For each `searchId` (if it hasn't already been processed/matched in the current run):
        *   It marks the current `searchId` and all other fetched `searchIds` as `1` in the HashMap to signify they are being processed.
        *   It then **invokes the Rider Pooling Logic** (as detailed in Chunks 5, 6, and 7) for the current `searchId`.
        *   This triggers the entire filtering cascade (proximity checks, route overlap, etc.) against the other waiting riders.

5.  **Handle Match Found**:
    *   If the Rider Pooling Logic returns a successful match (a list of `searchIds`), the cron job's responsibility for these riders is complete.
    *   The flow for the matched riders proceeds to **Batch Creation** (Chunk 3), which is now initiated asynchronously.

6.  **Loop Continuation**: The process repeats for the next unprocessed `searchId` in the HashMap until all waiting riders have had a match attempted in the current cron cycle.
