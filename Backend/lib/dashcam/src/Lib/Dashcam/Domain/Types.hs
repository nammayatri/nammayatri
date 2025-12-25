module Lib.Dashcam.Domain.Types where

import Data.Text (unpack)
import Kernel.Prelude
import Servant.API (FromHttpApiData (..))

data DashcamService = Cautio
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData DashcamService where
  parseUrlPiece = pure . read . unpack
