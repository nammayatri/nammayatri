{-
  Local Tools.Beam.UtilsTH shim for shared-services.

  This module re-exports Template Haskell helpers from Kernel.Beam.Lib.UtilsTH
  so that generated code which references Tools.Beam.UtilsTH (for enum
  instances, etc.) can compile within the shared-services library.
-}

module Tools.Beam.UtilsTH (module Reexport) where

import Kernel.Beam.Lib.UtilsTH as Reexport
