module Domain.Types.Invoice where

import Data.Aeson (eitherDecode, encode)
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as LBS
import Data.OpenApi (ToParamSchema (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import Data.Time (UTCTime (..))
import qualified Data.Time as DT
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Servant (FromHttpApiData (..), ToHttpApiData (..))

data InvoiceType = SubscriptionPurchase | Ride | RideCancellation | Commission | AggregatedCommission | Refund
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''InvoiceType)
$(mkHttpInstancesForEnum ''InvoiceType)

data IssuedToType = DRIVER | RIDER | CUSTOMER | FLEET_OWNER
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''IssuedToType)
$(mkHttpInstancesForEnum ''IssuedToType)

-- Needed for `QueryParam "issuedToTypes" [IssuedToType]` — Servant requires
-- explicit list instances (no generic FromHttpApiData [a]).
instance FromHttpApiData [IssuedToType] where
  parseUrlPiece = parseHeader . TEnc.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader bs = BF.first T.pack . eitherDecode . LBS.fromStrict $ bs

instance ToHttpApiData [IssuedToType] where
  toUrlPiece = TEnc.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = LBS.toStrict . encode

data DateOrTime
  = DateOnly DT.Day
  | DateTime UTCTime
  deriving (Eq, Show, Generic)

instance FromHttpApiData DateOrTime where
  parseQueryParam t =
    case parseQueryParam t of
      Right utc -> Right (DateTime utc)
      Left _ -> DateOnly <$> parseQueryParam t

instance ToHttpApiData DateOrTime where
  toQueryParam (DateTime t) = toQueryParam t
  toQueryParam (DateOnly d) = toQueryParam d

instance ToParamSchema DateOrTime where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)

toUTCTimeFrom :: DateOrTime -> UTCTime
toUTCTimeFrom (DateOnly day) = UTCTime day 0
toUTCTimeFrom (DateTime t) = t

toUTCTimeTo :: DateOrTime -> UTCTime
toUTCTimeTo (DateOnly day) = UTCTime (DT.addDays 1 day) 0
toUTCTimeTo (DateTime t) = t
