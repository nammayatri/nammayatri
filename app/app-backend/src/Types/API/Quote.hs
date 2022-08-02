module Types.API.Quote where

import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList (PublicTransportQuote)
import Beckn.Utils.JSON (objectWithSingleFieldParsing)
import qualified Beckn.Utils.Schema as S
import Data.Char (toLower)
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Domain.Types.Estimate (EstimateAPIEntity)
import Domain.Types.Quote (QuoteAPIEntity)
import Domain.Types.SearchRequest.SearchReqLocation (SearchReqLocationAPIEntity)
import EulerHS.Prelude hiding (id)
import Types.API.MetroOffer

data GetQuotesRes = GetQuotesRes
  { fromLocation :: SearchReqLocationAPIEntity,
    toLocation :: Maybe SearchReqLocationAPIEntity,
    quotes :: [OfferRes],
    estimates :: [EstimateAPIEntity]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data OfferRes
  = OnDemandCab QuoteAPIEntity
  | Metro MetroOffer
  | PublicTransport PublicTransportQuote
  deriving (Show, Generic)

instance ToJSON OfferRes where
  toJSON = genericToJSON $ objectWithSingleFieldParsing \(f : rest) -> toLower f : rest

instance FromJSON OfferRes where
  parseJSON = genericParseJSON $ objectWithSingleFieldParsing \(f : rest) -> toLower f : rest

instance ToSchema OfferRes where
  declareNamedSchema = genericDeclareNamedSchema $ S.objectWithSingleFieldParsing \(f : rest) -> toLower f : rest
