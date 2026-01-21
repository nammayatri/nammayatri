{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

module Lib.Yudhishthira.SchemaInstances where

import Data.OpenApi
import Kernel.Prelude

-- This module is intended to house orphan ToSchema instances for types
-- that don't have them in their source packages but are needed for
-- config schema generation.

-- NominalDiffTime and TimeOfDay seem to have instances in this version of openapi3.
