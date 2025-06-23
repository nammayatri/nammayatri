# Product Context

## Why Nammayatri Exists

### Problem Statement
Traditional mobility solutions suffer from:
- Fragmented services across different transport modes
- Lack of standardized communication protocols
- Inefficient driver-rider matching algorithms
- Limited integration with public transport systems
- High operational costs and complexity

### Vision
Create a unified, open mobility platform that seamlessly connects riders with various transport options while enabling fair, transparent, and efficient urban mobility.

## Product Solutions

### For Riders
- **Unified Booking**: Single platform for multiple transport modes
- **Real-time Tracking**: Live updates on ride status and location
- **Transparent Pricing**: Clear fare calculation and payment
- **Reliable Service**: Consistent availability and quality
- **Multi-modal Options**: Choice between private rides and public transport

### For Drivers/Providers
- **Fair Allocation**: Transparent matching algorithms
- **Real-time Coordination**: Efficient communication with platform
- **Performance Tracking**: Detailed analytics and feedback
- **Flexible Operations**: Support for various service types

### For Cities/Regulators
- **ONDC Compliance**: Standardized protocol implementation
- **Data Transparency**: Open access to mobility data
- **Scalable Infrastructure**: Support for growing urban mobility needs

## How It Should Work

### Core User Journey
1. **Discovery**: Rider searches for transport options
2. **Booking**: System matches with available providers
3. **Allocation**: Intelligent driver assignment
4. **Tracking**: Real-time ride monitoring
5. **Completion**: Payment processing and feedback

### Key Principles
- **Open Standards**: BECKN/ONDC protocol compliance
- **Scalability**: Handle high volume of concurrent requests
- **Reliability**: 99.9% uptime with fault tolerance
- **Transparency**: Open algorithms and fair pricing
- **Integration**: Seamless multi-modal transport experience

## Success Metrics
- Ride completion rate > 95%
- Average matching time < 30 seconds
- System uptime > 99.9%
- Driver utilization efficiency
- Rider satisfaction scores

## NY Regular Subscriptions

### Feature Overview

The "NY Regular" feature allows users to set up recurring ride subscriptions. This is designed for users who have a regular commute, such as from home to work, and want to automate the process of booking a ride.

### Key Concepts

- **Subscription:** A user-defined schedule for a recurring ride. It includes pickup and drop-off locations, time of day, and the days of the week the ride is needed.
- **Instance:** A single occurrence of a ride within a subscription. For example, if a user has a subscription for every weekday, Monday's ride is one instance, Tuesday's is another, and so on.
- **Automation:** The system will automatically initiate a search for a ride and book it based on the subscription schedule.

### Data Models

#### `NyRegularSubscription`

This table stores the core information about a user's subscription.

- **Key Fields:**
    - `id`: Unique identifier for the subscription.
    - `userId`: The user who created the subscription.
    - `pickupLocation`, `dropoffLocation`: The start and end points of the ride.
    - `startDatetime`: When the subscription becomes active.
    - `recurrenceRuleDays`: The days of the week the subscription is active (e.g., Monday, Wednesday, Friday).
    - `scheduledTimeOfDay`: The time the ride should be scheduled for.
    - `recurrenceEndDate`: When the subscription expires.
    - `status`: The current state of the subscription (e.g., `ACTIVE`, `PAUSED`, `CANCELLED`).

#### `NyRegularInstanceLog`

This table logs each automated attempt to book a ride for a subscription.

- **Key Fields:**
    - `instanceTransactionId`: Unique identifier for this specific ride instance.
    - `nyRegularSubscriptionId`: A foreign key linking to the `NyRegularSubscription`.
    - `scheduledPickupTime`: The specific date and time this instance was scheduled for.
    - `automationStatus`: The outcome of the automated booking attempt (e.g., `CONFIRMED`, `FAILED_NO_OFFER`).
