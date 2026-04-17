{-# OPTIONS_GHC -Wno-orphans #-}

-- | Project-local OpenAPI ToSchema instances for upstream types we cannot
-- modify. Import this module for its side effects (orphan instances) wherever
-- a type derives ToSchema and contains one of these.
module Tools.OpenAPIInstances () where

import Data.OpenApi (ToSchema (..))
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude

instance ToSchema DbHash where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)
