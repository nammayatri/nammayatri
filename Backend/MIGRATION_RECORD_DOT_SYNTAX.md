# Migration from record-dot-preprocessor to RecordDotSyntax

This document describes the migration from the `record-dot-preprocessor` plugin to the native GHC `RecordDotSyntax` extensions available in GHC 9.2+.

## Changes Made

### Removed Dependencies
- Removed `record-dot-preprocessor` from all `package.yaml` files (44 files)
- Removed `record-dot-preprocessor` from all `.cabal` files (44 files)

### Removed GHC Plugin
- Removed `-fplugin=RecordDotPreprocessor` from all `ghc-options` sections

### Added Language Extensions
The following extensions were added to the `default-extensions` section of all relevant packages:

1. **NoFieldSelectors** - Disables automatic generation of field selector functions, preventing conflicts with the dot syntax
2. **OverloadedRecordDot** - Enables the `record.field` syntax for field access
3. **OverloadedRecordUpdate** - Enables the `record{field = value}` syntax for record updates

## Usage

### Basic Field Access
```haskell
-- Before (with record-dot-preprocessor)
personName = person.name
personCity = person.address.city

-- After (with OverloadedRecordDot)
personName = person.name  -- Same syntax!
personCity = person.address.city  -- Same syntax!
```

### Record Updates
```haskell
-- Regular record updates still work
updatedPerson = person { age = 30 }

-- For advanced record update syntax, you may need to add:
-- {-# LANGUAGE RebindableSyntax #-}
-- import GHC.Records
```

## Benefits of Migration

1. **Native GHC Support** - No external plugin dependency
2. **Better Performance** - Native compiler support instead of preprocessing
3. **GHC 9.2+ Compatibility** - `record-dot-preprocessor` doesn't support GHC 9.9+
4. **Standardized** - Part of the official GHC extension system

## Verification

The migration has been tested and verified to:
- ✅ Remove all `record-dot-preprocessor` dependencies (0 remaining)
- ✅ Add the required extensions to all packages (132 total extensions added)
- ✅ Maintain the same syntax for field access
- ✅ Compile successfully with GHC 9.12.2

## Notes

- All existing code using `record.field` syntax should continue to work without changes
- The migration is backward compatible for field access patterns
- If you encounter issues with complex record updates, consider adding `RebindableSyntax` extension and importing `GHC.Records`

## Files Modified

- 44 `package.yaml` files updated
- 44 `.cabal` files updated
- Total of 88 configuration files modified across the entire Backend codebase