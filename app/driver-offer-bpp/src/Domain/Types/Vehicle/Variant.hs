{-# LANGUAGE DerivingVia #-}

module Domain.Types.Vehicle.Variant where

import Beckn.Prelude hiding (first)
import Beckn.Utils.GenericPretty
import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Servant.API

data Variant = SEDAN | SUV | HATCHBACK | AUTO
  deriving
    ( Show,
      Eq,
      Read,
      Generic,
      ToJSON,
      FromJSON,
      ToSchema,
      ToParamSchema,
      Enum,
      Bounded
    )
  deriving (PrettyShow) via Showable Variant

instance FromHttpApiData Variant where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict
