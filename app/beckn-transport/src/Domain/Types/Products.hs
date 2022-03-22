{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Products where

import Beckn.Types.Amount
import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import EulerHS.Prelude hiding (id)
import Servant.API

data ProductsType = RIDE | PASS
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data ProductsStatus = INSTOCK | OUTOFSTOCK
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance FromHttpApiData ProductsStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data Products = Products
  { id :: Id Products,
    name :: Text,
    description :: Maybe Text,
    _type :: ProductsType,
    shortId :: ShortId Products,
    status :: ProductsStatus,
    price :: Amount,
    rating :: Maybe Text,
    review :: Maybe Text,
    info :: Maybe Text,
    udf1 :: Maybe Text,
    udf2 :: Maybe Text,
    udf3 :: Maybe Text,
    udf4 :: Maybe Text,
    udf5 :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)
