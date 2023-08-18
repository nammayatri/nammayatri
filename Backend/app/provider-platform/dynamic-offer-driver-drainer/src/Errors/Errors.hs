module Errors.Errors
  ( DatabaseError (DatabaseError),
    DomainTypeParseError (DomainTypeParseError),
    module X,
  )
where

import Control.Exception (Exception)
import Data.Text (Text)
import Errors.PredefinedErrors as X
import Errors.Types as X
import GHC.Generics (Generic)
import Prelude

newtype DatabaseError = DatabaseError
  { errorMessage :: Text
  }
  deriving (Eq, Show, Generic)

instance Exception DatabaseError

newtype DomainTypeParseError = DomainTypeParseError
  { errorMessage :: Text
  }
  deriving (Eq, Show, Generic)

instance Exception DomainTypeParseError
