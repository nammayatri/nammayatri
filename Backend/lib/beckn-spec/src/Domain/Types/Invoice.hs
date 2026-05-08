module Domain.Types.Invoice where

import Data.Aeson (eitherDecode, encode)
import Data.OpenApi (ToParamSchema (..))
import Data.Time (UTCTime (..))
import qualified Data.Time as DT
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Servant (FromHttpApiData (..), ToHttpApiData (..))

data InvoiceType = SubscriptionPurchase | Ride | RideCancellation | Commission
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''InvoiceType)
$(mkHttpInstancesForEnum ''InvoiceType)

data IssuedToType = DRIVER | RIDER | CUSTOMER | FLEET_OWNER
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''IssuedToType)
$(mkHttpInstancesForEnum ''IssuedToType)

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
