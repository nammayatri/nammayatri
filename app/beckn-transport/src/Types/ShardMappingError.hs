module Types.ShardMappingError where

import EulerHS.Prelude

newtype ShardMappingError = ShardMappingError Text
  deriving (Show, Typeable)

instance Exception ShardMappingError
