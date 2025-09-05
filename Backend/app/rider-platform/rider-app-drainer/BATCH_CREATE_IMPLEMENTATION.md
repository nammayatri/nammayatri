# How Batch Create Works

## Overview
The batch create system converts multiple individual database INSERT operations into efficient bulk INSERT queries by intelligently grouping compatible operations.

## Core Concept: Column Signature Grouping

### What is a Column Signature?
A column signature identifies entries that can be safely batched together:

```haskell
data ColumnSignature = ColumnSignature
  { tableName :: DBModel,           -- e.g., "atlas_app.person"
    columnNames :: [Text],          -- e.g., ["id", "name", "phone", "created_at"]
    columnCount :: Int              -- e.g., 4
  }
  deriving (Eq, Ord, Show)
```

### Example: Person Table Entries
```haskell
-- Entry 1: Person with standard columns
{
  "id": "person-123",
  "name": "John Doe",
  "phone": "+91-9876543210",
  "created_at": "2025-01-03T10:00:00Z"
}
-- Column signature: ["created_at", "id", "name", "phone"] (sorted)

-- Entry 2: Person with same columns → Can batch together
{
  "id": "person-456",
  "name": "Jane Smith",
  "phone": "+91-9876543211",
  "created_at": "2025-01-03T10:01:00Z"
}
-- Column signature: ["created_at", "id", "name", "phone"] (same as Entry 1)

-- Entry 3: Person with additional column → Separate batch
{
  "id": "person-789",
  "name": "Bob Wilson",
  "phone": "+91-9876543212",
  "email": "bob@example.com",
  "created_at": "2025-01-03T10:02:00Z"
}
-- Column signature: ["created_at", "email", "id", "name", "phone"] (different)
```

**Result**: Entries 1 & 2 → Batch A, Entry 3 → Batch B

## Multi-Level Grouping Process

### Step 1: Parse and Generate Signatures
```haskell
-- From parseCreateEntry function
generateColumnSignature :: DBCreateObject -> ColumnSignature
generateColumnSignature createObj =
  let columns = map extractColumn createObj.contents
      sortedColumns = List.sort columns  -- Sort for consistent grouping
   in ColumnSignature
        { tableName = createObj.dbModel,
          columnNames = sortedColumns,
          columnCount = length sortedColumns
        }
```

### Step 2: Group by Table
```haskell
-- From groupByTable function
groupByTable :: [ParsedCreateEntry] -> Map DBModel [ParsedCreateEntry]

-- Example input: [Person, Person, Location, Person, Location]
-- Example output:
-- { "atlas_app.person" -> [PersonEntry1, PersonEntry2, PersonEntry3],
--   "atlas_app.location" -> [LocationEntry1, LocationEntry2] }
```

### Step 3: Group by Column Signature Within Table
```haskell
-- From groupByColumnSignature function
groupByColumnSignature :: [ParsedCreateEntry] -> Map ColumnSignature [ParsedCreateEntry]

-- Example for Person table:
-- Input: [PersonStandard1, PersonStandard2, PersonWithEmail1]
-- Output:
-- { ColumnSignature{tableName="person", columns=["id","name","phone"]} -> [PersonStandard1, PersonStandard2],
--   ColumnSignature{tableName="person", columns=["id","name","phone","email"]} -> [PersonWithEmail1] }
```

### Step 4: Generate Bulk INSERT Query
```haskell
-- From generateBulkInsertForSignature function
generateBulkInsertForSignature :: ColumnSignature -> [DBCreateObject] -> Maybe Text

-- Example output for Person entries:
"INSERT INTO atlas_app.person (created_at, id, name, phone) VALUES
 ('2025-01-03 10:00:00', 'person-123', 'John Doe', '+91-9876543210'),
 ('2025-01-03 10:01:00', 'person-456', 'Jane Smith', '+91-9876543211')
 ON CONFLICT DO NOTHING"
```

## Processing Decision Tree

### Individual vs Batch Processing
```haskell
-- From executeBatchedCreate function
-- Step 1: Split by processing strategy
let (forceIndividual, batchable) = List.partition (\entry -> entry.createObject.forceDrainToDB) parsedEntries

-- forceIndividual → Use runCreate (existing logic)
-- batchable → Use batch processing
```

### Batch Size Threshold
```haskell
-- From shouldBatchSignature function
shouldBatchSignature :: Int -> Flow Bool
shouldBatchSignature entryCount = do
  batchSize <- EL.runIO getInsertBatchSize  -- e.g., 10
  pure $ entryCount >= batchSize

-- Example:
-- 15 entries with same signature → Batch (15 >= 10)
-- 5 entries with same signature → Individual processing (5 < 10)
```

## Real Processing Example

### Input: Multimodal Journey Creation
```json
[
  {"table": "atlas_app.person", "columns": ["id", "name", "phone"], "forceDrainToDB": false},
  {"table": "atlas_app.person", "columns": ["id", "name", "phone"], "forceDrainToDB": false},
  {"table": "atlas_app.person", "columns": ["id", "name", "phone"], "forceDrainToDB": false},
  {"table": "atlas_app.location", "columns": ["id", "lat", "lon"], "forceDrainToDB": false},
  {"table": "atlas_app.location", "columns": ["id", "lat", "lon"], "forceDrainToDB": false},
  {"table": "atlas_app.journey_leg", "columns": ["id", "mode", "duration"], "forceDrainToDB": true}
]
```

### Processing Flow:
1. **Parse**: All entries parsed successfully
2. **Split by Flag**:
   - `forceDrainToDB=false`: 5 entries (batchable)
   - `forceDrainToDB=true`: 1 entry (individual)
3. **Group by Table**:
   - `atlas_app.person`: 3 entries
   - `atlas_app.location`: 2 entries
4. **Group by Signature**:
   - Person signature A: 3 entries → Batch
   - Location signature B: 2 entries → Individual (< batch size)
5. **Execute**:
   - 1 bulk INSERT for Person table (3 entries)
   - 2 individual INSERTs for Location table
   - 1 individual INSERT for journey_leg (forced)

### Result: 6 operations → 4 database queries (33% reduction)

## Bulk INSERT Query Example

### Generated Query:
```sql
INSERT INTO atlas_app.person (created_at, id, name, phone) VALUES
  ('2025-01-03 10:00:00', 'person-001', 'User One', '+91-1111111111'),
  ('2025-01-03 10:00:01', 'person-002', 'User Two', '+91-2222222222'),
  ('2025-01-03 10:00:02', 'person-003', 'User Three', '+91-3333333333')
ON CONFLICT DO NOTHING;
```

### Performance Impact:
- **Before**: 3 separate INSERT statements
- **After**: 1 bulk INSERT statement
- **Database round trips**: 3 → 1
- **Connection overhead**: Reduced by 67%

## Error Handling & Fallback

### Batch Failure → Individual Fallback
```haskell
-- From executeSingleBatch function
result <- executeQuery bulkInsertQuery
case result of
  Left (QueryError errorMsg) -> do
    EL.logError "BATCH_INSERT_FAILED" errorMsg
    -- Automatic fallback to individual processing
    executeIndividuallyForSignature dbStreamKey batchEntries
  Right _ -> do
    EL.logInfo "BATCH_INSERT_SUCCESS" (show entryCount)
```

### Example Fallback Scenario:
1. Attempt bulk INSERT for 20 Person entries
2. Database constraint violation on entry #15
3. Bulk INSERT fails
4. System automatically processes all 20 entries individually
5. 19 succeed, 1 fails → Graceful degradation

## Configuration

### Environment Variables:
```bash
BATCHED_CREATE_ENABLED=true    # Enable batch processing
INSERT_BATCH_SIZE=10           # Minimum entries for batching
```

### Integration Point:
```haskell
-- In DBSync.hs
isForcePushEnabled <- liftIO getBatchCreateEnabled
if isForcePushEnabled
  then executeBatchedCreate dbStreamKey createDataEntries  -- Batch processing
  else executeInSequence runCreate ([], []) dbStreamKey createDataEntries  -- Individual
```

---

**Summary**: The batch create system intelligently groups database operations by table and column structure, converting multiple individual INSERTs into efficient bulk operations while maintaining safety and providing graceful fallback mechanisms.