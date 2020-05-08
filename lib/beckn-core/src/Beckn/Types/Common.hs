module Beckn.Types.Common where

import           Data.Swagger
import qualified EulerHS.Language as L
import           EulerHS.Prelude

class GuidLike a where
  generateGUID :: L.Flow a

instance GuidLike Text where
  generateGUID = L.generateGUID

data ErrorResponse =
  ErrorResponse
    { status          :: Text
    , responseCode    :: Text
    , responseMessage :: Text
    }
  deriving (Show, Generic, ToJSON, ToSchema)
