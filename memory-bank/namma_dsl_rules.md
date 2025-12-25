# NammaDSL Usage Rules & Syntax

This document summarizes the rules and syntax for using NammaDSL, based on the provided documentation. It covers YAML file creation, code generation, and the specific DSL syntax for APIs and Storage.

## I. General Workflow

1.  **YAML File Creation**:
    *   Create YAML files inside the `spec` folder of the relevant module (e.g., `rider-platform/rider-app/spec`).
    *   Place API specifications in `spec/API/` and storage specifications in `spec/Storage/`.

2.  **Code Generation**:
    *   Run `, run-generator` to generate Haskell Beam, query, domain files (in `src-read-only`), and SQL queries.
    *   This command typically only processes new or changed spec files (by comparing file hashes with HEAD commit).
    *   Use `, run-generator --all` to force generation for all spec files.

3.  **Compilation**:
    *   Compile the generated and existing code using `cabal build all`.

## II. API DSL Syntax

Key top-level fields in an API YAML specification:

*   `imports`: (Object) For importing predefined Haskell types. Module path is key, type name is value.
*   `importPackageOverrides`: (Object) To override import packages for specific types.
*   `module`: (String) Specifies the Haskell module name for the generated API.
*   `apiPrefix`: (String, Optional, Dashboard-specific) Overwrites the default API prefix (camelCase of `module`) for main and helper dashboard APIs. Empty string `""` means no prefix.
*   `helperApiPrefix`: (String, Optional, Dashboard-specific) Overwrites API prefix for helper dashboard APIs.
*   `types`: (Object, Optional) Defines request and response data types.
    *   Format:
        ```yaml
        {TypeName}:
          - {field1}: {field1Type}
          - {field2}: {field2Type}
          - derive: {Any extra derivation, e.g., "'HideSecrets"} # For HideSecrets, use single quotes
          - recordType: NewType | Data (Default) | Type # Optional
        ```
    *   Enums:
        ```yaml
        {TypeName}:
          - enum: {enum1},{enum2}
          - derive: {Any extra derivation}
        ```
    *   Newtype:
        ```yaml
        {TypeName}:
          - recordType: NewType
          - {fieldName}: {fieldType}
          - derive: {Any extra derivation}
        ```
    *   Type Alias:
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
            *   `DashboardAuth {UserRole}` (e.g., `DASHBOARD_USER`, `MERCHANT_ADMIN`)
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
    *   `constraints`: (Object) Defines constraints for fields (e.g., `PrimaryKey`, `SecondaryKey`, `NotNull`, `AUTOINCREMENT`). Format: `{fieldName}: {Constraint1|Constraint2}`.
    *   `importPackageOverrides`: (Object) As in API DSL, for overriding import packages.
    *   `types`: (Object, Optional) Defines complex Haskell types (records, enums, newtypes) used within the main `{dataTypeName}` or other types. Same syntax as API DSL `types`. `fromTType` must be mentioned for complex types (excluding enums) if they are fields in the main data type.
    *   `derives`: (String, Optional) Overrides default derivations for the main domain data type (e.g., `"Show,Eq,Ord"`).
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
