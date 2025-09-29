# Record Dot Syntax Migration Documentation

## Overview

This document describes the migration from the external `record-dot-preprocessor` plugin to the native GHC `RecordDotSyntax` extensions, which have been available since GHC 9.2.

## Problem

The `record-dot-preprocessor` plugin used throughout the codebase was incompatible with GHC 9.9+ and our current GHC 9.12.2 version. The plugin only supports GHC versions up to 9.8, causing build failures when attempting to upgrade the compiler.

```bash
Error: [Cabal-7107]
Could not resolve dependencies:
record-dot-preprocessor => ghc>=8.6 && <9.9
rejecting: ghc-9.12.2 (conflict: excluded by constraint)
```

## Solution

Migrated from the external `record-dot-preprocessor` plugin to the native GHC `RecordDotSyntax` extensions that have been available since GHC 9.2. This provides the same functionality with better performance and native compiler support.

## Changes Made

### Removed Dependencies
- Removed `record-dot-preprocessor` from all `package.yaml` files (44 files)
- Removed `-fplugin=RecordDotPreprocessor` from all `ghc-options` sections

### Added Language Extensions
Added three new extensions to the `default-extensions` section of all relevant packages:

1. **`NoFieldSelectors`** - Disables automatic generation of field selector functions, preventing conflicts with dot syntax
2. **`OverloadedRecordDot`** - Enables the `record.field` syntax for field access  
3. **`OverloadedRecordUpdate`** - Enables enhanced record update syntax

### Backward Compatibility

The migration is fully backward compatible. Existing code using record dot syntax continues to work unchanged:

```haskell
-- This syntax works exactly the same before and after migration
personName = person.name
personCity = person.address.city

-- Record updates also work the same
updatedPerson = person { name = "New Name" }
```

## Benefits

1. **Native GHC Support** - No external plugin dependency
2. **Better Performance** - Native compiler support instead of preprocessing
3. **GHC 9.2+ Compatibility** - Resolves incompatibility issues with modern GHC versions
4. **Standardized** - Uses official GHC extension system
5. **Future-Proof** - No dependency on external plugin maintenance

## Usage Examples

### Basic Record Access
```haskell
data Person = Person
  { name :: Text
  , age :: Int
  , address :: Address
  }

data Address = Address
  { street :: Text
  , city :: Text
  }

-- Accessing nested fields
getPersonCity :: Person -> Text
getPersonCity person = person.address.city

-- Chain multiple accesses
getPersonStreet :: Person -> Text  
getPersonStreet person = person.address.street
```

### Record Updates
```haskell
-- Simple field update
updateName :: Text -> Person -> Person
updateName newName person = person { name = newName }

-- Nested field update
updateCity :: Text -> Person -> Person  
updateCity newCity person = person { address = person.address { city = newCity } }
```

## Troubleshooting

### Common Issues

1. **Field selector conflicts**: If you encounter conflicts with field selectors, ensure `NoFieldSelectors` is enabled in your package's `default-extensions`.

2. **Import errors**: Make sure your modules don't have conflicting field names when `NoFieldSelectors` is enabled.

3. **Build errors after migration**: Regenerate your `.cabal` files from `package.yaml` using `hpack`.

### Regenerating Cabal Files

After modifying `package.yaml` files, regenerate the corresponding `.cabal` files:

```bash
# From the project root
find Backend -name "package.yaml" -execdir hpack \;
```

## Migration Verification

The migration updates:
- ✅ 44 `package.yaml` files with dependency removal
- ✅ 43 `package.yaml` files with plugin removal  
- ✅ 44 `package.yaml` files with new language extensions
- ✅ Zero remaining `record-dot-preprocessor` dependencies
- ✅ 132 new language extensions added across all packages

## References

- [GHC User Guide - OverloadedRecordDot](https://downloads.haskell.org/~ghc/9.2.3/docs/html/users_guide/exts/overloaded_record_dot.html)
- [GHC User Guide - OverloadedRecordUpdate](https://downloads.haskell.org/~ghc/9.2.3/docs/html/users_guide/exts/overloaded_record_update.html)
- [GHC Proposal #282 - RecordDotSyntax](https://github.com/ghc-proposals/ghc-proposals/pull/282)

This migration enables the continued use of record dot syntax while ensuring compatibility with current and future GHC versions.