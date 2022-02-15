module Types.API.Quote where

import Beckn.Utils.JSON (objectWithSingleFieldParsing)
import Data.Char (toLower)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import Types.API.MetroOffer
import Types.Storage.Quote (QuoteAPIEntity)
import Types.Storage.SearchReqLocation (SearchReqLocationAPIEntity)

data GetQuotesRes = GetQuotesRes
  { fromLocation :: SearchReqLocationAPIEntity,
    toLocation :: SearchReqLocationAPIEntity,
    quotes :: [OfferRes]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data OfferRes
  = OnDemandCab QuoteAPIEntity
  | Metro MetroOffer
  deriving (Show, Generic, ToSchema)

instance ToJSON OfferRes where
  toJSON = genericToJSON $ objectWithSingleFieldParsing \(f : rest) -> toLower f : rest

instance FromJSON OfferRes where
  parseJSON = genericParseJSON $ objectWithSingleFieldParsing \(f : rest) -> toLower f : rest
