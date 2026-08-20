module Domain.Types.DocumentFieldConstraints where

import Kernel.Prelude

data CaseCorrection
  = Upper
  | Lower
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- Declarative per-field constraints carried in DocumentVerificationConfig.documentFields.
-- The backend does not enforce these on submit; clients read them to bound and normalise
-- input. Bounds are relative and signed: negative = past, positive = future.
--
-- Lives in beckn-spec so the driver app and the dashboard CommonAPIs share ONE definition --
-- a mirrored copy per side would need cast functions and would drift the moment one is edited.
--
-- Record field names are unique across all constructors on purpose: shared names would
-- need DuplicateRecordFields and make every selector ambiguous. The selectors are partial
-- either way, so match on the constructor rather than projecting.
data FieldConstraints
  = IntConstraints
      { minValue :: Maybe Int,
        maxValue :: Maybe Int
      }
  | YearConstraints
      { minYearsFromNow :: Maybe Int,
        maxYearsFromNow :: Maybe Int
      }
  | DateConstraints
      { minDaysFromNow :: Maybe Int,
        maxDaysFromNow :: Maybe Int
      }
  | TextConstraints
      { autoCaseCorrect :: Maybe CaseCorrection,
        trimSpaces :: Maybe Bool,
        removeCharacters :: Maybe [Text],
        minLength :: Maybe Int,
        maxLength :: Maybe Int
      }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
