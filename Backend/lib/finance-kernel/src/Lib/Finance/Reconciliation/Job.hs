{-
  Lib.Finance.Reconciliation.Job

  Job-input value + the tiny helpers apps need to plug the framework into
  their scheduler.

    * 'RecipeJobInput' is the shape stored in @scheduler_job.job_data@. It
      carries the recipe spec, merchant scope, and a single-chunk range.
      The scheduler enqueues one job per chunk; the runner is stateless.
    * 'RecipeRegistry' is the framework's expected shape of an app-side
      dispatch table (spec -> recipe).
-}
module Lib.Finance.Reconciliation.Job
  ( -- * Job input
    RecipeJobInput (..),

    -- * Registry
    RecipeRegistry,
    lookupRecipe,
  )
where

import qualified Data.Map.Strict as Map
import Kernel.Prelude
import Lib.Finance.Reconciliation.Recipe (Recipe)
import Lib.Finance.Reconciliation.Types

-- ─── Job input ─────────────────────────────────────────────────────────────

-- | Persisted as JSON in the scheduler's job_data column. The scheduler
--   treats it as opaque; only the framework runner and the app-side glue
--   understand its shape.
--
--   @range@ is a single chunk: the scheduler enqueues one job per chunk it
--   wants processed. There is no chain / resumeFrom state.
data RecipeJobInput = RecipeJobInput
  { spec :: ReconciliationSpec,
    scope :: MerchantScope,
    range :: DateRange
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- ─── Registry ──────────────────────────────────────────────────────────────

-- | An app-side registry maps recipe specs to the concrete recipes it
--   understands. Kept as a plain 'Data.Map' so the framework doesn't
--   pull in an extra hashing dependency.
type RecipeRegistry m = Map.Map ReconciliationSpec (Recipe m)

lookupRecipe :: RecipeRegistry m -> ReconciliationSpec -> Maybe (Recipe m)
lookupRecipe = flip Map.lookup
