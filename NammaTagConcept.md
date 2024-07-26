# NammaTag Concept

## Use Cases

### Marketing Campaigns
- Display specific banners.
- Apply offers if applicable.
- Send targeted messages.

### Driver-Rider Matching (Pooling Logic)
- Match based on driver behavior to ensure good conduct.
- Avoid heated moments between drivers and customers.
- Ensure safer rides at night (assign night rides, match specific customers, restrict rides in particular areas).

### Profile Creation
- Enhance user profiles with relevant tags.

## Tag Levels

### Search Level
- Tags associated with specific search criteria or conditions.

### Driver Level
- Tags associated with driver attributes or behavior.

### Rider Level
- Tags associated with rider preferences or history.

## Methods to Add Tags

### Directly from Application
- Example: A search from a particular area can add a Search Level tag.

### From Dashboard
- Admin interface to upload UUIDs of drivers or customers to assign specific tags.
- Use Case: Assign tags for marketing campaigns.

### Using Daily/Weekly Cron Jobs
- Automated jobs running daily or weekly to filter and tag drivers and customers based on their recent activities.
- These jobs can utilize ClickHouse queries for efficient data processing.

## Additional Suggestions

### Tag Management System
- Develop a centralized system to manage the creation, updating, and deletion of tags.
- Include a UI for admins to manage tags easily.

### Dynamic Tagging Rules
- Implement a rule engine to define dynamic tagging criteria.
- Allow admins to configure rules without changing the code.

### Tag History
- Maintain a history of tags to track changes over time.
- Useful for auditing and analyzing behavior trends.

### Tag-Based Analytics
- Incorporate analytics to track the effectiveness of tags.
- Measure impacts on marketing campaigns, driver-rider matching success, and profile enhancements.

### Integration with Existing Systems
- Ensure tags are seamlessly integrated with other parts of the application (e.g., search algorithms, matching logic, profile displays).

### Tag Weighting
- Assign weights to tags to indicate their importance.
- Useful for prioritizing tags in matching algorithms.

## Example Workflow

### Event Level Tagging
- User searches for a ride from a high-demand area.
- The system automatically assigns a "High-Demand Area" tag to the search.
- Unsafe area
- Driver takes a deroute, should mark it as Unsafe ride

### Driver Level Tagging
- A driver receives positive feedback for good behavior.
- A cron job runs weekly to assign a "Good Behavior" tag to the driver based on feedback scores.

### Rider Level Tagging
- A rider frequently travels late at night.
- An application-level job assigns a "Night Rider" tag to the rider.

### Marketing Campaign Tagging
- Admin uploads a list of customer UUIDs for a new campaign.
- The dashboard interface allows assigning a "Campaign-X" tag to the selected customers.

# Implmentation Details
## Components

### Yudhishthira (could be in python)
Decides for tag value. Important points
1. If source is application then tag returned should be at transactional level, not domain level
2. If source is application then tag will not have any validity

**Input:**
- Source
- SourceData

**Process**
1. Fetch all tags for the given source
2. For each tag, run decision process if particular tag qualifies for the data passed. This decision process can depend TagRule if it is RuleEngine or LLM
3. If source is application, access for required data using special syntax in the tag rule
3. Return the tag result

**Output:**
- [NammaTagResponse]

### KaalChakra
Run jobs to assign tags on basis of daily, weekly, monthly and quaterly data

**Input**
No input, it will take data from clickhouse

**Process**
1. Create data on the basis Chakra from clickhouse and fetch all tags for that Chakra
```
inputField = driver or rider id
query :: Text
queryResult :: Value

[{
  query = "select driverId, totalRides, totalDistance, totalDuration from users where createdAt > now - 1hr group by driverId;",
  queryResult = [("totalRides", "Int"), ("totalDistance", "Int"), ("totalDuration", "Int")]
}]
First field of query should always be the driverId
```
2. Loop through each data point, for each tag, apply rule and update user_tag_mapping table, along with domain table of application
3. Delete all expired user_tag_mapping entries and also delete from domain tables of application (make sure to delete from domain table before deleting from user_tag_mapping)

**Output**
Nothing

### Lib.Yudhishthira
Library in application, which will have all the haskell types of NammaTags, event handler which handle all events coming from application and call Yudhishthira to decide on tag and update the tag in particular transaction object.

### Dronacharya
Dashboard side interface to create and validate tags and their rules. We should validate the rules by running them.

## Types

```haskell
data Source = Application Event | KaalChakra Chakra

data SourceData = Application ApplicationData | KaalChakra KaalChakraData

data Event = Search | RideAssign | RideStart | RideEnd | RideCancel

data Chakra = Daily | Weekly | Monthly | Quaterly

data ApplicationData = ApplicationData
  { ride :: Maybe Ride,
    booking :: Maybe Booking,
    search :: Maybe Search,
    riderDetails :: Maybe RiderDetails,
    driver :: Maybe Driver,
    driverInfo :: Maybe DriverInformation
  }

data KaalChakraRideData = KaalChakraRideData
  { rides :: Maybe Int,
    cancelledRide :: Maybe Int,
    completedRides :: Maybe Int,
    cancellationRate :: Maybe Int
  }

data KaalChakraSearchData = KaalChakraSearchData
  { acceptanceRate :: Maybe Int
  }

data KaalChakraDataPoint = KaalChakraDataPoint
  { ride: Maybe KaalChakraRideData,
    search: Maybe KaalChakraSearchData
  }

data KaalChakraData = KaalChakraData
  { userId :: Id User,
    daily :: Maybe KaalChakraDataPoint,
    weekly :: Maybe KaalChakraDataPoint,
    movingWeekly :: Maybe KaalChakraDataPoint,
    monthly :: Maybe KaalChakraDataPoint,
    movingMonthly :: Maybe KaalChakraDataPoint,
    total :: Maybe KaalChakraDataPoint,
  }

data TagRule = RuleEngine Value | LLM Context
data NammaTag = Application NammaTagApplication | KaalChakra NammaTagChakra
  
data NammaTagApplication = NammaTagApplication
  { tagCategory :: Text,
    tagName :: Text,
    tagPossibleValues :: [Text],
    tagStage :: Event,
    tagRule :: TagRule
  } 

data NammaTagChakra = NammaTagChakra
  { tagCategory :: Text,
    tagName :: Text,
    tagPossibleValues :: [Text],
    tagChakra :: Chakra,
    tagValidity :: Maybe Hours,
    tagRule :: TagRule
  }

data NammaTagResponse = NammaTagResponse
  { tagName :: Text,
    tagValue :: Text,
    tagCategory :: Text,
    tagValidity :: Maybe Hours
  }

```