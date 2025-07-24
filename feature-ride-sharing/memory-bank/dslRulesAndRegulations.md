# NammaDSL Usage Rules & Syntax

This document summarizes the rules and syntax for using NammaDSL, based on the provided documentation. It covers YAML file creation, code generation, and the specific DSL syntax for APIs and Storage.

## I. General Workflow

1.  **YAML File Creation**:
    *   Create YAML files inside the `spec` folder of the relevant module (e.g., `rider-platform/rider-app/spec`).
    *   Place API specifications in `spec/API/` and storage specifications in `spec/Storage/`.

2.  **Code Generation**:
    *   Run `, run-generator` to generate Haskell Beam, query, domain files (in `src-read-only`), and SQL queries.
    *   This command typically only processes new or changed spec files by comparing file hashes with the HEAD commit.
    *   Use `, run-generator --all` to force generation for all spec files.

3.  **Compilation**:
    *   Compile the generated and existing code using `cabal build all`.

## II. API DSL Syntax

Key top-level fields in an API YAML specification:

*   `imports`: (Object) For importing predefined Haskell types. Module path is key, type name is value.
*   `importPackageOverrides`: (Object) To override import packages for specific types. Can be used to skip overrides for the current package via `dhall-configs`.
*   `module`: (String) Specifies the Haskell module name for the generated API.
*   `apiPrefix`: (String, Optional, Dashboard-specific) Overwrites the default API prefix (camelCase of `module`) for main and helper dashboard APIs. Empty string `""` means no prefix.
*   `helperApiPrefix`: (String, Optional, Dashboard-specific) Overwrites API prefix for helper dashboard APIs.
*   `types`: (Object, Optional) Defines request and response data types.
    *   **Modern Format (Recommended)**:
        ```yaml
        {TypeName}:
          - {field1}: {field1Type}
          - {field2}: {field2Type}
          - derive: {Any extra derivation, e.g., "'HideSecrets"} # For HideSecrets, use single quotes
          - recordType: NewType | Data (Default) | Type # Optional
        ```
    *   **Old Format (Deprecated)**:
        ```yaml
        {TypeName}:
          {field1}: {field1Type}
          {field2}: {field2Type}
        ```
    *   **Enums**:
        ```yaml
        {TypeName}:
          - enum: {enum1},{enum2}
          - derive: {Any extra derivation}
        ```
    *   **Newtype**:
        ```yaml
        {TypeName}:
          - recordType: NewType
          - {fieldName}: {fieldType}
          - derive: {Any extra derivation}
        ```
    *   **Type Alias**:
        ```yaml
        {TypeName}:
          - recordType: Type
          - type: {fieldType} # The actual type it aliases
        ```
*   `apis`: (List) Defines a list of API endpoints. Each API is an object keyed by its HTTP method.
    *   `{httpMethod}`: (String - GET | POST | PUT | DELETE)
        *   `endpoint`: (String) API path, e.g., `/path/{param1}/path2`.
        *   `name`: (String, Optional) Overwrites auto-generated API name (from path).
            *   `migrate`: (Object, Optional) If `name` is changed for an existing API, provide old values to aid migration:
                ```yaml
                migrate:
                  endpoint: <oldEndpointName_from_path_or_previous_name>
                  userActionType: <oldUserActionTypeName_from_path_or_previous_name>
                ```
        *   `response`: (Object)
            *   `type`: (String) Response type.
        *   `request`: (Object, Optional)
            *   `type`: (String) Request body type.
        *   `multipart`: (Object, Optional)
            *   `type`: (String) Request body type for multipart requests.
        *   `auth`: (String, Default: `TokenAuth`) Authentication method. Options:
            *   `AdminTokenAuth`
            *   `ApiTokenAuth`
            *   `TokenAuth` (can be followed by `RIDER_TYPE` or `PROVIDER_TYPE`)
            *   `NoAuth`
            *   `SafetyWebhookAuth MERCHANT_SERVER`
            *   `DashboardAuth {UserRole}` (e.g., `DASHBOARD_USER`, `DASHBOARD_ADMIN`, `FLEET_OWNER`, `DASHBOARD_RELEASE_ADMIN`, `MERCHANT_ADMIN`, `MERCHANT_MAKER`, `MERCHANT_CHECKER`, `MERCHANT_SERVER`, `MERCHANT_USER`)
            *   `ApiAuth {ServerName} {ApiEntity} {UserActionType}` (e.g., `ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS LIST`)
        *   `query`: (List of Objects) Query parameters. Format: `- {paramName}: {paramType}`.
        *   `mandatoryQuery`: (List of Objects) Mandatory query parameters. Format: `- {paramName}: {paramType}`.
        *   `params`: (Object) Path parameters. Format: `{pathParam1}: {pathParam1Type}`.
        *   `headers`: (List of Objects, Optional) Request headers. Format: `- {headerName}: {headerType}`.
        *   `helperApi`: (Object, Optional, Dashboard-specific) Recursively defines a helper API with the same structure.
        *   `validation`: (String, Optional) Qualified name of a request validation function.

## III. Storage DSL Syntax

Key top-level field in a Storage YAML specification is the `{dataTypeName}`.

*   `{dataTypeName}`: (Object) Specifies the main data type (domain model) for a database table.
    *   `tableName`: (String, Optional) Database table name. Defaults to snake_case of `{dataTypeName}`.
    *   `fields`: (Object) Lists fields of the domain type with their Haskell types.
        *   Format: `{fieldName}: {fieldType}`.
        *   Field types can be simple (auto-imported or from `imports`), complex (defined in `types`), or use `WithId` extensions.
        *   For `Id` and `ShortId`, the Beam Type will be `Text`.
    *   `constraints`: (Object) Defines constraints for fields (e.g., `PrimaryKey`, `SecondaryKey`, `NotNull`, `AUTOINCREMENT`). Format: `{fieldName}: {Constraint1|Constraint2}`.
    *   `importPackageOverrides`: (Object) As in API DSL, for overriding import packages.
    *   `types`: (Object, Optional) Defines complex Haskell types (records, enums, newtypes) used within the main `{dataTypeName}` or other types. Same syntax as API DSL `types`. `fromTType` must be mentioned for complex types (excluding enums) if they are fields in the main data type.
    *   `derives`: (String, Optional) Appends to the default list of derivations. For enums, the correct way to generate necessary Beam and HTTP instances is to use `derive: "HttpInstance"`. The generator will handle the rest of the standard derivations automatically.
    *   `derives'`: (String, Optional) **Overrides** the entire default list of derivations. Using this on enums is risky as it can suppress the automatic generation of crucial instances. It's generally safer to use `derive` to append specific instances.
    *   `beamType`: (Object, Optional) Specifies a different Haskell type for a field on the Beam (database) side. Requires `fromTType`/`toTType`.
        *   Format: `{domainFieldName}: {BeamSideHaskellType}`.
    *   `beamFields`: (Object, Optional) Customizes Beam field names or structure.
        *   Simple rename: `{domainFieldName}: "new_beam_field_name"`
        *   Complex structure:
            ```yaml
            {domainFieldName}:
              {beamField1Name}: {beamField1Type}
              {beamField2Name}: {beamField2Type}
            ```
            Requires `fromTType`/`toTType` for `domainFieldName`.
    *   `beamInstance`: (String or List, Optional) Specifies Beam instance generation.
        *   Options: `MakeTableInstances` (default), `MakeTableInstancesGenericSchema`, `MakeTableInstancesWithTModifier [("field", "db_col")]`, `Custom {InstanceName} {params...}`.
    *   `sqlType`: (Object, Optional) Specifies explicit SQL type for fields. Format: `{fieldName}: "SQL_TYPE"`.
    *   `default`: (Object, Optional) Default SQL values for fields. Format: `{fieldName}: "'sql_default_value'"` (note single quotes for strings).
    *   `queries`: (Object, Optional) Defines Beam queries.
        *   Format:
            ```yaml
            {queryName}:
              kvFunction: {kvFunctionName} # e.g., findOneWithKV, findAllWithKV, updateWithKV, deleteWithKV
              params: [{fieldToUpdate1}, {fieldToUpdate2}: {newValueVariable}] # For update queries
              where: {whereClause} # See below
              orderby: {fieldName} # Optional
            ```
        *   `whereClause`: Nested structure using operators (`and`, `or`, `in`, `eq`, `gt`, `lt`, `gte`, `lte`, `not_operator`).
            *   Fields can be domain fields, Beam fields (suffix `|B`), or constants (suffix `|CS`, `|CI`, `|CB`, `|CD`, `|CIM` or `|C`).
    *   `cachedQueries`: (Object, Optional) Defines cached queries using Redis.
        *   Format:
            ```yaml
            {queryName}:
              withCrossAppRedis: true | false # Default: false
              returnType/cacheDataType: One | Array # Default: auto-detected
              queryType: FindOnly | FindAndCache | CacheOnly | DeleteCache # Default: auto-detected or FindAndCache
              dbQuery: {beamQueryName} # Required for FindAndCache
              dbQueryParams: [{paramNameOrConstant}, ...]
              keyParams: [{paramNameOrConstantOrTypedVar}, ...] # e.g., fieldName, constant|CS, varName: VarType
              keyMaker: {customKeyMakerFunctionName} # Optional
              paramsOrder: [{paramName1}, {paramName2}] # Optional, for function signature
            ```
    *   `fromTType`: (Object, Optional) Defines transformation functions from Beam type to Domain type for specific fields.
        *   Format: `{fieldName}: {functionName}` or `{fieldName}: {Module.functionName|I}`.
        *   Suffix `|M` for monadic functions, `|E` for embedded expressions.
    *   `toTType`: (Object, Optional) Defines transformation functions from Domain type to Beam type. Same format as `fromTType`.
    *   `excludedFields`: (List, Optional) List of common fields (merchantId, merchantOperatingCityId, createdAt, updatedAt) to exclude from auto-generation.
    *   `extraIndexes`: (List, Optional) Defines additional database indexes.
        *   Format: `- columns: [{fieldName1}, {fieldName2}] \n  name: {optionalIndexName} \n  unique: {true|false}`
    *   `extraOperations`: (List, Optional) Special generator operations.
        *   `EXTRA_QUERY_FILE`: Generate editable extra query file.
        *   `EXTRA_DOMAIN_TYPE_FILE`: Generate editable extra domain type file.
        *   `EXTRA_CACHED_QUERY_FILE`: Generate editable extra cached query file.
        *   `GENERATE_INDEXES`: Enable index generation (default for SecondaryKey).
        *   `NO_DEFAULT_INDEXES`: Disable default index generation for SecondaryKey.
        *   `EXTRA_API_TYPES_FILE`: Generate editable extra API types file.
        *   `EXTRA_API_COMMON_TYPES_FILE`: Generate editable extra API common types file (can be imported by generated API types).

### Common Auto-Imported Types (Storage & API)
`Text`, `Maybe`, `Double`, `TimeOfDay`, `Day`, `Int`, `Bool`, `Id`, `ShortId`, `UTCTime`, `Meters`, `HighPrecMeters`, `Kilometers`, `HighPrecMoney`, `Seconds`, `Currency`.

### `WithId` Extensions (Storage DSL `fields`)
*   `{fieldType}|WithId`: Domain field is `{fieldType}`, Beam field is `Id {fieldType}`. Does not create `{fieldType}` on `create`.
*   `{fieldType}|WithCachedId`: Similar to `WithId`, but uses `Storage.CachedQueries` for find/create.
*   `{fieldType}|WithIdCreate`: Domain field is `{fieldType}`, Beam field is `Id {fieldType}`. Calls `{fieldType}.create` on main type creation.
*   `{fieldType}|WithCachedIdCreate`: Similar to `WithIdCreate`, but uses `Storage.CachedQueries`.

### Domain to Beam Transformation Concepts

*   **`beamFields` for Flattening:** The `beamFields` property is the primary mechanism for flattening a nested domain object (e.g., a `Device` object with `osType` and `osVersion`) into a flat structure suitable for a database table. The generator takes each key under `beamFields` (which corresponds to a field in the domain model) and replaces it with the nested fields defined within. This results in a Beam model with more, simpler columns.
*   **`beamType` for Type Overrides:** The `beamType` property is used to explicitly change the type of a field on the Beam (database) side. This is often used to convert domain-specific types (like `Version`) into more primitive database types (like `Text`) for storage.
*   **Automatic `Id` Conversion:** The DSL generator automatically converts `Id {DomainType}` types in the domain model to `Text` in the Beam model. This simplifies foreign key relationships and database queries, as the underlying type is a simple text representation of a UUID.

### The Transformation Bridge: `fromTType` and `toTType`

*   **`fromTType` (Beam to Domain):** The functions specified in the `fromTType` section are used to construct the rich domain model from the flat Beam record. This is where the logic for fetching related data (like loading a full `Location` object from a `locationId`) or re-composing complex types (like creating a `Device` object from separate OS and version fields) resides. The generator uses these functions to build the `fromTType'` instance.
*   **`toTType` (Domain to Beam):** The expressions in the `toTType` section are responsible for deconstructing the domain model into the flat Beam record. This involves extracting primitive values from nested objects and converting domain-specific types into their database representations (e.g., `Id` to `Text`). The generator uses these expressions to build the `toTType'` instance.
*   **Orphan Instances:** The generated `FromTType'` and `ToTType'` typeclass instances are placed in a dedicated `Storage.Queries.OrphanInstances` module. This is a key part of the generator's design, keeping the core data type definitions (Domain and Beam) separate from the transformation logic that connects them.

### Query Generation and Extension

*   **Basic Queries:** The `queries` section in the YAML is used to generate simple boilerplate functions for database interactions (e.g., `findOneBy`, `update`, `findAll`). The `kvFunction` determines the type of operation, and the `where` and `params` clauses define the conditions and data for the operation.
*   **Extending with Manual Code:** For complex queries that cannot be expressed by the DSL's `queries` syntax, the `extraOperations` list is used. Adding `EXTRA_QUERY_FILE` to this list instructs the generator to create a companion `...Extra.hs` file (e.g., `SearchRequestExtra.hs`).
*   **The `Extra` File Pattern:** The generated query module (e.g., `Storage.Queries.SearchRequest.hs`) will re-export the contents of the `Extra` file. This allows developers to write complex, manual Beam queries in the `Extra` file and have them seamlessly available alongside the auto-generated queries. This is the standard pattern for extending the DSL's capabilities when the built-in query builder is insufficient.

### Secondary Key Validation
*   **Safety Check:** The DSL generator validates that any field marked as a `SecondaryKey` is actually used in a `where` clause of a query defined in the `queries` section. This is a safety feature to prevent unused indexes, which add write overhead without any read benefit.
*   **Forcing an Index:** If an index is required for queries that will be written manually in an `Extra.hs` file, you can bypass the generator's check by using the `!SecondaryKey` syntax (e.g., `fieldName: !SecondaryKey`).
*   **Best Practice:** The best practice is to define the lookup query directly in the YAML file if possible. This makes the intended use of the key explicit and allows the generator to create both the index and the corresponding query function.

### Handling Foreign Domain Types
*   When a field in your domain model is another domain type (e.g., `myField :: OtherDomainType`), you **must** tell the generator how to represent this on the database side using `beamFields`.
*   If it's a simple type that can be stored directly (like an enum), map it to a single beam field (e.g., `myField: { my_field_col: OtherDomainType }`).
*   If it's a complex type that needs to be flattened, define the flattened fields under `beamFields`.
*   Failure to do this will cause the generator to make incorrect assumptions about how to fetch the related data, leading to compilation errors due to missing modules or functions.

### `toTType` Deconstruction Logic
*   When using `beamFields` to split a complex domain type (like `Distance { value :: Double, unit :: DistanceUnit }`) into multiple database columns, the `toTType` transformations for those columns must correctly deconstruct the single source domain field.
*   For a domain field `distance :: Distance`, the `toTType` for the `distanceUnit` beam field should be `(distance <&> (.unit))|E`.
*   Crucially, the `toTType` for the `distanceValue` beam field must use a function that takes the `distance` object itself as input, not a non-existent top-level `distanceUnit` field. The transformation logic must be self-contained and derive all necessary values from the fields available in the domain record.

### Transformer Function Suffixes (`fromTType`/`toTType`)
*   `|I`: The function is imported from another module.
*   `|M`: The function is monadic.
*   `|E`: Embeds an expression directly. Allows calling other functions and using variables in a specific order.
*   These can be combined, e.g., `|IM`, `|EM`.

### Query `where` Clause Field Types
*   **Domain Type Fields**: Normal fields from the domain type. A domain-to-beam conversion function is applied automatically.
*   **Beam Type Fields**: Suffix the field with `|B` (e.g., `id|B`). No conversion is applied.
*   **Constants**: Suffix with `|C` or a more specific type. No conversion is applied.
    *   `|CS`: String (e.g., `myname|CS` -> `"myname"`)
    *   `|CI`: Integer (e.g., `123|CI` -> `123`)
    *   `|CB`: Boolean (e.g., `true|CB` -> `True`)
    *   `|CD`: Double (e.g., `0.23|CD` -> `0.23`)
    *   `|CIM` or `|C`: Any imported type (e.g., `Domain.Something.defaultValue|CIM`)

## IV. Advanced Patterns and Best Practices

### Price vs HighPrecMoney Field Handling
When dealing with monetary fields, follow these patterns based on your requirements:

**Pattern 1: Price Fields (with Currency)**
```yaml
fields:
  estimatedFare: Price
  currency: Currency

beamType:
  estimatedFare: HighPrecMoney
  currency: Maybe Currency

beamFields:
  estimatedFare:
    currency: Maybe Currency
    estimatedFare: HighPrecMoney

toTType:
  estimatedFare: (.amount)|E
  currency: (Just $ (.currency) estimatedFare)|E

fromTType:
  estimatedFare: Kernel.Types.Common.mkPrice currency estimatedFare|E
  currency: fromMaybe Kernel.Types.Common.INR currency|E

sqlType:
  estimatedFare: numeric(30,10)
  currency: character varying(255)
```

**Pattern 2: Direct HighPrecMoney Fields (no currency separation)**
```yaml
fields:
  totalFare: HighPrecMoney

# No beamType or beamFields needed for simple HighPrecMoney

sqlType:
  totalFare: numeric(30,10)
```

### Enum Type NoRelation Pattern
For enum fields that should not generate verification queries, use the `|NoRelation` suffix:
```yaml
fields:
  vehicleVariant: VehicleVariant|NoRelation
  serviceType: ServiceTierType|NoRelation
```
This prevents the generator from creating unnecessary validation queries for enum lookups.

### Distance Field Handling with Units
When dealing with distance fields that have units, follow this pattern:

```yaml
fields:
  estimatedDistance: Maybe Distance
  distanceUnit: DistanceUnit  # Required as separate field when used in transformations

beamType:
  distanceUnit: Maybe DistanceUnit

beamFields:
  estimatedDistance:
    estimatedDistance: Maybe HighPrecMeters
    estimatedDistanceValue: Maybe HighPrecDistance
    distanceUnit: Maybe DistanceUnit

toTType:
  estimatedDistance: (Kernel.Types.Common.distanceToHighPrecMeters <$> estimatedDistance)|E
  estimatedDistanceValue: (Kernel.Utils.Common.distanceToHighPrecDistance distanceUnit <$> estimatedDistance)|E
  distanceUnit: (estimatedDistance <&> (.unit))|E

fromTType:
  estimatedDistance: (Kernel.Utils.Common.mkDistanceWithDefault distanceUnit estimatedDistanceValue <$> estimatedDistance)|E

sqlType:
  estimatedDistance: double precision
  estimatedDistanceValue: double precision
  distanceUnit: character varying(255)
```

### Avoiding Duplicate Field Declarations
When a field is referenced in multiple contexts, avoid duplicate declarations:

**❌ Incorrect - Causes "Multiple declarations" error:**
```yaml
beamFields:
  estimatedDistance:
    distanceUnit: Maybe DistanceUnit
  distanceUnit:
    distanceUnit: Maybe DistanceUnit  # Duplicate!
```

**✅ Correct - Single declaration pattern:**
```yaml
fields:
  distanceUnit: DistanceUnit

beamType:
  distanceUnit: Maybe DistanceUnit

beamFields:
  estimatedDistance:
    estimatedDistance: Maybe HighPrecMeters
    estimatedDistanceValue: Maybe HighPrecDistance
    # distanceUnit is NOT included here to avoid duplicates
```

### Field Reference Scope in Transformations
When writing transformations, ensure field references are in scope:

**❌ Incorrect - References undefined variable:**
```yaml
toTType:
  distanceValue: (Kernel.Utils.Common.distanceToHighPrecDistance distanceUnit <$> distance)|E
  # 'distanceUnit' variable is not in scope
```

**✅ Correct - Derive from available fields:**
```yaml
toTType:
  distanceValue: (Kernel.Utils.Common.distanceToHighPrecDistance (distance <&> (.unit)) <$> distance)|E
  # Extracts unit from the distance field itself
```

### Currency Import Pattern
When referencing Currency constants, use the full module path:
```yaml
fromTType:
  currency: fromMaybe Kernel.Types.Common.INR currency|E
  estimatedTotalFare: Kernel.Types.Common.mkPrice (Just Kernel.Types.Common.INR) estimatedTotalFare|E
```

### SQL Type Best Practices
- Use `numeric(30,10)` for HighPrecMoney fields
- Use `numeric(30,2)` for Money fields  
- Use `double precision` for distance/measurement fields
- Use `character varying(255)` for enum and text fields
- Use `uuid` for Id fields
- Use `"uuid[]"` for Id array fields
- Use `"text[]"` for text array fields

### Complex Type Import Requirements
When using domain types from other modules, ensure proper imports:
```yaml
imports:
  ServiceTierType: Domain.Types.ServiceTierType
  VehicleVariant: Domain.Types.VehicleVariant
  HighPrecMoney: Kernel.Types.Common
  Currency: Kernel.Types.Common
```

### Error Prevention Checklist
Before generating code, verify:
1. ✅ All enum fields use `|NoRelation` if verification queries aren't needed
2. ✅ No duplicate field declarations in beamFields vs beamType
3. ✅ All transformation references are in scope
4. ✅ Proper SQL types for HighPrecMoney (`numeric(30,10)`)
5. ✅ Currency references use full module paths
6. ✅ Required imports are defined
7. ✅ Distance fields have corresponding `distanceUnit` field when needed
