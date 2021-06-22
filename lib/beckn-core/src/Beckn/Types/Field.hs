module Beckn.Types.Field where

import qualified Data.Generics.Labels as GL
import Data.Kind (Constraint, Type)
import GHC.Base (Symbol)
import GHC.Records.Extra (HasField)

-- | An alias for type-level pair of name and type.
type (name :: Symbol) ::: (ty :: Type) = '(name, ty)

-- | Version of 'HasField' which complies with both record-dot-preprocessor
-- and @.field@ syntax supported by generics-lens.
--
-- Re-evaluate this once we decide on a uniform way to access fields.
type HasFieldSuper name r ty = (HasField name r ty, GL.Field name r r ty ty)

-- | Bulk version of @HasField@.
type family HasFields (r :: Type) (fields :: [(Symbol, Type)]) :: Constraint where
  HasFields r '[] = () :: Constraint
  HasFields r ('(name, ty) ': fields) =
    (HasFieldSuper name r ty, HasFields r fields)
