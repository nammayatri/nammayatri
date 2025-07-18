# Multimodal Journey System - Flows and State Management

## Table of Contents
1. [Journey Lifecycle](#journey-lifecycle)
2. [State Management](#state-management)
3. [Core Flow Charts](#core-flow-charts)
4. [Error Handling Flows](#error-handling-flows)
5. [Key Functions](#key-functions)

## Journey Lifecycle

### 1. Journey Planning Phase
```mermaid
graph TD
    A[User Request] --> B[Multi-modal Route Planning]
    B --> C[Calculate Fares for Each Leg]
    C --> D[Create Journey Object]
    D --> E[Store Journey Legs]
    E --> F[Return Journey Options]
```

### 2. Journey Confirmation Phase
```mermaid
graph TD
    A[User Confirms Journey] --> B[Validate Leg Selections]
    B --> C[Update Journey Status: CONFIRMED]
    C --> D[Start Journey Execution]
    D --> E[Begin First Leg Booking]
```

### 3. Journey Execution Phase
```mermaid
graph TD
    A[Journey In Progress] --> B{Check Current Leg}
    B -->|Taxi| C[Monitor Booking/Ride]
    B -->|Metro/Bus/Subway| D[Monitor Ticket Status]
    B -->|Walk| E[Track Progress]
    C --> F{Leg Complete?}
    D --> F
    E --> F
    F -->|No| G[Continue Monitoring]
    F -->|Yes| H[Start Next Leg]
    G --> B
    H --> I{More Legs?}
    I -->|Yes| B
    I -->|No| J[Mark Journey Complete]
```

## State Management

### Journey Status Flow
```mermaid
stateDiagram-v2
    [*] --> INITIATED: postMultimodalInitiate
    INITIATED --> CONFIRMED: postMultimodalConfirm
    CONFIRMED --> INPROGRESS: Start journey execution
    INPROGRESS --> COMPLETED: All legs complete
    INPROGRESS --> CANCELLED: User cancellation
    COMPLETED --> FEEDBACK_PENDING: Feedback required
    FEEDBACK_PENDING --> [*]: Feedback submitted
    CANCELLED --> [*]

    INPROGRESS --> INPROGRESS: Leg status updates
```

### Journey Leg Status Flow
```mermaid
stateDiagram-v2
    [*] --> InPlan: Leg created
    InPlan --> Skipped: User skips
    InPlan --> Assigning: Start booking
    Assigning --> Booked: Booking confirmed
    Booked --> Arriving: Provider approaching
    Arriving --> Arrived: Provider arrived
    Arrived --> OnTheWay: Journey started
    OnTheWay --> Finishing: Near destination
    Finishing --> Completed: Leg finished

    Booked --> Cancelled: Booking cancelled
    Assigning --> Cancelled: Assignment failed

    Skipped --> [*]
    Completed --> [*]
    Cancelled --> [*]
```

### Transport Mode-Specific States

#### Taxi Leg States
- **InPlan** → **Assigning** → **Booked** → **Arrived** → **OnTheWay** → **Completed**
- Special handling for driver assignment and ride tracking

#### Metro/Bus/Subway Leg States
- **InPlan** → **Booked** → **Arriving** → **OnTheWay** → **Completed**
- QR ticket generation after booking
- Real-time vehicle tracking integration

#### Walk Leg States
- **InPlan** → **OnTheWay** → **Completed**
- GPS-based progress tracking
- No booking required

## Core Flow Charts

### Complete Journey Flow
```mermaid
graph TD
    A[User Plans Journey] --> B[Get Multi-modal Options]
    B --> C[User Selects Journey]
    C --> D[postMultimodalInitiate]
    D --> E[Journey: INITIATED]
    E --> F[User Confirms Details]
    F --> G[postMultimodalConfirm]
    G --> H[Journey: CONFIRMED]
    H --> I[Start First Leg]

    I --> J{Leg Type?}
    J -->|Taxi| K[Book Taxi]
    J -->|Metro/Bus/Subway| L[Book Transit Ticket]
    J -->|Walk| M[Start Walking]

    K --> N[Monitor Ride Status]
    L --> O[Monitor Transit Status]
    M --> P[Track Walking Progress]

    N --> Q{Leg Complete?}
    O --> Q
    P --> Q

    Q -->|No| R[Update Status]
    Q -->|Yes| S{More Legs?}
    R --> N
    R --> O
    R --> P

    S -->|Yes| T[Start Next Leg]
    S -->|No| U[Journey: COMPLETED]
    T --> J
    U --> V[Request Feedback]
    V --> W[Journey: FEEDBACK_PENDING]
    W --> X[End]
```

### Payment Flow
```mermaid
graph TD
    A[FRFS Legs Identified] --> B[Calculate Total Fare]
    B --> C[Create Payment Order]
    C --> D[User Payment]
    D --> E{Payment Success?}
    E -->|Yes| F[Update Booking Status]
    E -->|No| G[Payment Failed]
    F --> H[Generate Tickets]
    G --> I[Retry Payment]
    I --> D
    H --> J[Journey Can Proceed]
```

### Leg Switching Flow
```mermaid
graph TD
    A[User Requests Switch] --> B{Leg Type?}
    B -->|Taxi| C[Switch Taxi Estimate]
    B -->|FRFS| D[Switch Service Tier]

    C --> E[Validate New Estimate]
    E --> F[Cancel Previous Search]
    F --> G[Update Search ID]
    G --> H[Confirm New Booking]

    D --> I[Validate New Quote]
    I --> J[Recalculate Fare]
    J --> K[Update Payment Order]
    K --> L[Update Route Info]

    H --> M[Return Updated Journey]
    L --> M
```

## Error Handling Flows

### Error Handling Flow
```mermaid
graph TD
    A[Error Detected] --> B{Error Type?}
    B -->|Booking Failed| C[Try Alternative Options]
    B -->|Payment Failed| D[Retry Payment]
    B -->|Transport Missed| E[Find Next Available]
    B -->|Route Changed| F[Recalculate Journey]

    C --> G{Alternatives Available?}
    G -->|Yes| H[Book Alternative]
    G -->|No| I[Cancel Leg]

    D --> J{Retry Success?}
    J -->|Yes| K[Continue Journey]
    J -->|No| L[Cancel Journey]

    E --> M[Update Schedule]
    F --> N[Update Route]

    H --> K
    I --> O[Skip or Cancel]
    M --> K
    N --> K
```

### Recovery Strategies
1. **Booking Failures**: Try alternative transport modes
2. **Payment Issues**: Retry with different payment methods
3. **Schedule Conflicts**: Find next available service
4. **Route Changes**: Recalculate optimal path
5. **System Failures**: Graceful degradation with fallbacks

## Key Functions

### 1. Journey Initialization
```haskell
init :: JourneyInitData -> MultimodalUserPreferences -> m (Maybe Journey)
```
**Process**:
- Calculates fares for each leg using `JLI.getFare`
- Creates journey legs with proper sequencing
- Validates user preferences against available options
- Stores journey and legs in database
- Returns `Nothing` if any leg fails (unless testing mode)

### 2. Journey Confirmation & Start
```haskell
startJourney :: [JourneyConfirmReqElement] -> Maybe Int -> Id Journey -> m ()
```
**Process**:
- Processes user confirmation elements (ticket quantities, etc.)
- Calls `JLI.confirm` for each leg based on travel mode
- Handles forced booking scenarios
- Manages CRIS SDK responses for subway bookings
- Executes asynchronously with retry mechanisms

### 3. Status Monitoring
```haskell
getAllLegsStatus :: Journey -> m [JourneyLegState]
```
**Process**:
- Retrieves current status for all legs
- Applies movement detection for bus tracking
- Updates journey expiry based on ticket validity
- Triggers feedback collection when all legs complete
- Returns unified status across all transport modes

### 4. Leg Management Operations
```haskell
-- Skip a leg
skipLeg :: Id Journey -> Int -> Bool -> m ()

-- Cancel remaining legs
cancelRemainingLegs :: Id Journey -> Bool -> m ()

-- Extend/modify a leg
extendLeg :: Id Journey -> ExtendLegStartPoint -> Maybe LocationAPIEntity -> ... -> m ()
```

### 5. Real-time Location Processing
```haskell
postMultimodalRiderLocation :: (...) -> Id Journey -> RiderLocationReq -> m JourneyStatusResp
```
**Process**:
- Adds location point to journey tracking
- Checks for leg completion based on location
- Automatically starts next taxi legs when appropriate
- Applies movement detection algorithms
- Returns updated journey status

## State Transition Rules

### Journey-Level Transitions
- **INITIATED** can only go to **CONFIRMED** or **CANCELLED**
- **CONFIRMED** can go to **INPROGRESS**, **CANCELLED**
- **INPROGRESS** can go to **COMPLETED**, **CANCELLED**
- **COMPLETED** goes to **FEEDBACK_PENDING**
- **CANCELLED** and **FEEDBACK_PENDING** are terminal states

### Leg-Level Transitions
- **InPlan** is the initial state for all legs
- **Skipped** legs bypass all booking states
- **Cancelled** legs can be resumed if cancellable
- **Completed** legs cannot be modified
- State transitions depend on transport mode

### Validation Rules
- Walk legs cannot be skipped
- Only taxi legs support estimate switching
- FRFS legs support service tier switching
- Payment must succeed before journey progression
- Feedback is required for journey completion

## Performance Considerations

### Async Processing
- Journey operations use `fork` for non-blocking execution
- Status updates processed in background
- Location updates batched for efficiency

### Caching Strategy
- Journey status cached with expiry
- Station and route data cached globally
- User preferences cached per session

### Database Optimization
- Efficient indexing on journey and leg queries
- Batch updates for status changes
- Soft deletes to maintain history
