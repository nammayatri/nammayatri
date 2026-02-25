# NammaDSL Code Generation

## Overview

NammaDSL uses YAML specification files as the source of truth for APIs and database schemas. The generator (`alchemist` package) produces Haskell code from these specs.

**Generator source**: `app/alchemist/` (package: `alchemist`)
**Documentation**: `app/alchemist/README.md`

## Commands

```bash
# Run from Backend/ inside nix shell
, run-generator                  # Only changed specs
, run-generator --all            # All specs
, run-generator --apply-hint     # With HLint auto-fixes
```

## Spec File Locations

| Service | API Specs | Storage Specs |
|---------|-----------|---------------|
| rider-app | `app/rider-platform/rider-app/Main/spec/API/` (37 files) | `app/rider-platform/rider-app/Main/spec/Storage/` (88 files) |
| driver-app | `app/provider-platform/dynamic-offer-driver-app/Main/spec/API/` (31 files) | `app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/` (99 files) |
| public-transport | `app/rider-platform/public-transport-rider-platform/Main/spec/` | Same |

## API YAML Spec Format

```yaml
imports:
  Domain.Types.Person: Person                    # Type imports
  Kernel.Prelude: UTCTime

module: Domain.Action.UI.MyFeature              # Target module

types:
  MyRequest:                                     # Custom type definition
    field1: Text
    field2: Maybe Int
  MyResponse:
    status: Text
    data: [MyRequest]

apis:
  - POST:                                        # HTTP method
      endpoint: /myFeature/create               # URL path (use camelCase, not hyphens)
      auth: TokenAuth                           # Auth type
      request:
        type: MyRequest                         # Request body type
      response:
        type: MyResponse                        # Response type

  - GET:
      endpoint: /myFeature/{featureId}          # Path parameter
      auth: TokenAuth
      params:
        featureId: Id MyFeature                 # Path param type
      query:
        limit: Maybe Int                        # Query parameter
      response:
        type: MyResponse
```

### Auth Options
- `TokenAuth` — Standard user token authentication
- `AdminTokenAuth` — Admin token
- `NoAuth` — No authentication required
- `DashboardAuth` — Dashboard authentication
- `ApiAuth` — API key authentication

### API YAML Rules
- Use **camelCase** for endpoint path segments (e.g., `/nyRegular/`), not hyphens
- Use full module paths for imports (e.g., `Domain.Types.IntegratedBPPConfig`)

## Storage YAML Spec Format

```yaml
imports:
  Domain.Types.Person: Person
  Kernel.Types.Id: Id

tableName: my_feature                           # DB table name

fields:
  id: Id MyFeature                              # Primary key
  personId: Id Person                           # Foreign key
  name: Text
  amount: HighPrecMoney
  currency: Currency
  status: MyFeatureStatus
  isActive: Bool
  metadata: Maybe Text
  createdAt: UTCTime
  updatedAt: UTCTime

types:
  MyFeatureStatus:                              # Enum definition
    enum: ACTIVE,INACTIVE,DELETED
    derive: HttpInstance

constraints:
  id: PrimaryKey
  personId: SecondaryKey

queries:
  findByPersonId:
    kvFunction: findAllWithKV                   # KV function to use
    where:
      personId: personId

  updateStatus:
    kvFunction: updateWithKV
    params:
      - status
    where:
      id: id

  deleteByPersonId:
    kvFunction: deleteWithKV
    where:
      personId: personId

cachedQueries:
  findByPersonIdCached:
    withCrossAppRedis: false
    queryType: FindOne
    dbQuery: findByPersonId
    dbQueryParams:
      - personId
    keyMaker: personId

extraOperations:
  - EXTRA_QUERY_FILE                            # Generate editable query extension
  - EXTRA_DOMAIN_TYPE_FILE                      # Generate editable type extension
  - EXTRA_CACHED_QUERY_FILE                     # Generate editable cached query extension

# Optional: beam field mappings when Haskell and DB names differ
beamFields:
  amount:
    amount: Maybe HighPrecMoney
    amountCurrency: Maybe Currency

# Optional: type transformations
fromTType:
  amount: Kernel.Prelude.fromMaybe 0 amount |> HighPrecMoney
  currency: Kernel.Prelude.fromMaybe INR amountCurrency

toTType:
  amount: Kernel.Prelude.Just (getHighPrecMoney amount)
  amountCurrency: Kernel.Prelude.Just currency
```

### Common Auto-Imported Types
`Text`, `Maybe`, `Int`, `Bool`, `Id`, `UTCTime`, `HighPrecMoney`, `Currency` — no need to import these.

### KV Functions
| Function | Purpose |
|----------|---------|
| `findOneWithKV` | Find single record |
| `findAllWithKV` | Find multiple records |
| `findAllWithOptionsKV` | Find with sorting/pagination |
| `updateWithKV` | Update records |
| `deleteWithKV` | Delete records |
| `createWithKV` | Insert record |

### Extra Operations
| Operation | Purpose | Output |
|-----------|---------|--------|
| `EXTRA_QUERY_FILE` | Hand-written queries | `src/Storage/Queries/<Name>.hs` |
| `EXTRA_DOMAIN_TYPE_FILE` | Extra types, orphan instances | `src/Domain/Types/Extra/<Name>.hs` |
| `EXTRA_CACHED_QUERY_FILE` | Hand-written cached queries | `src/Storage/CachedQueries/<Name>.hs` |

## Generated Output Locations

| Input | Output | Content |
|-------|--------|---------|
| `spec/API/*.yaml` | `src-read-only/API/` | Servant API type definitions |
| `spec/Storage/*.yaml` | `src-read-only/Domain/Types/` | Domain entity types |
| `spec/Storage/*.yaml` | `src-read-only/Storage/Beam/` | Beam ORM types |
| `spec/Storage/*.yaml` | `src-read-only/Storage/Queries/` | Database query modules |
| `spec/Storage/*.yaml` | `src-read-only/Storage/CachedQueries/` | Redis-cached queries |
| First run | `src/Domain/Action/UI/` | Stub files for business logic |

## Critical Rules

1. **NEVER edit files in `src-read-only/`** — modify the YAML and re-run generator
2. **Always compile after generation**: `cabal build all`
3. Use **full module paths** in YAML imports, not short names
4. If a `.hs` file is deleted, run `, hpack` to update `.cabal` file

## Troubleshooting

| Problem | Solution |
|---------|----------|
| Generated code won't compile | Check YAML for typos, verify import paths |
| New type not found | Ensure it's defined in `types:` section or properly imported |
| Query function missing | Check `kvFunction` matches available options |
| Cached query not caching | Verify `keyMaker` fields match query parameters |
| Beam mapping error | Check `fromTType`/`toTType` transformations |

## Related Docs

- Database patterns: `08-database-patterns.md`
- Build commands: `02-build-and-dev.md`
- Conventions: `15-conventions.md`
