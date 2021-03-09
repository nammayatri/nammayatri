{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Types.Common where

import Beckn.Utils.Logging (HasLogContext (..), Log (..), LogLevel (..))
import Data.Aeson
import Data.Generics.Labels ()
import Data.Swagger hiding (tags)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude

type FlowR r = ReaderT r L.Flow

instance HasLogContext r => Log (FlowR r) where
  logOutput logLevel tags message =
    case logLevel of
      DEBUG -> logWithFormat L.logDebug tags message
      INFO -> logWithFormat L.logInfo tags message
      WARNING -> logWithFormat L.logWarning tags message
      ERROR -> logWithFormat L.logError tags message

logWithFormat ::
  ( MonadReader env m,
    HasLogContext env
  ) =>
  (Text -> Text -> m ()) ->
  [Text] ->
  Text ->
  m ()
logWithFormat logFunction tags msg = do
  existingTags <- asks getLogContext
  logFunction (tagsToText (existingTags ++ tags)) msg
  where
    tagsToText = T.concat . map block
    block x = "[" <> x <> "]"

class GuidLike a where
  generateGUID :: FlowR r a

instance GuidLike Text where
  generateGUID = L.generateGUID

data ErrorResponse = ErrorResponse
  { status :: Text,
    responseCode :: Text,
    responseMessage :: Text
  }
  deriving (Show, Generic, ToJSON, ToSchema)

newtype IdObject = IdObject
  { id :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

class FromBeckn a b where
  fromBeckn :: a -> b

class ToBeckn a b where
  toBeckn :: b -> a

-- This type class is not strictly an isomorphism. We use the name 'Iso' to
-- denote that it is always expected that a type from the beckn spec should
-- have a corresponding type defined by us allowing conversion (which may be
-- lossy) between the two, when defined as an instance of this typeclass.
class (FromBeckn a b, ToBeckn a b) => BecknSpecIso a b
