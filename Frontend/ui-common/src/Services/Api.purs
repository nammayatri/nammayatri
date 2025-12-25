module Services.Common.API where

import Foreign.Generic (decode, encode, class Decode, class Encode)
import Data.Show.Generic (genericShow)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Generic.Rep (class Generic)
import Presto.Core.Types.API ( class StandardEncode, standardEncode)
import Prelude (class Show, ($), show)

data Variant = SEDAN | SUV | HATCHBACK | AUTO_RICKSHAW | TAXI | TAXI_PLUS | PREMIUM_SEDAN | BLACK | BLACK_XL | BIKE | AMBULANCE_TAXI | AMBULANCE_TAXI_OXY | AMBULANCE_AC | AMBULANCE_AC_OXY | AMBULANCE_VENTILATOR | SUV_PLUS | EV_AUTO_RICKSHAW | HERITAGE_CAB | None

newtype DriverReview = DriverReview
  { riderName :: Maybe String,
    review :: Maybe String,
    feedBackPills :: Array String,
    rating :: Int,
    tripDate :: String
  }

newtype DriverStatSummary = DriverStatSummary
  { avgRating :: Maybe Number,
    numTrips :: Int,
    cancellationRate :: Int,
    likedByRidersNum :: Int
  }

derive instance genericDriverReview :: Generic DriverReview _
instance standardEncodeDriverReview :: StandardEncode DriverReview where standardEncode (DriverReview body) = standardEncode body
instance showDriverReview :: Show DriverReview where show = genericShow
instance decodeDriverReview :: Decode DriverReview where decode = defaultDecode
instance encodeDriverReview  :: Encode DriverReview where encode = defaultEncode

derive instance genericDriverStatSummary :: Generic DriverStatSummary _
instance standardEncodeDriverStatSummary :: StandardEncode DriverStatSummary where standardEncode (DriverStatSummary body) = standardEncode body
instance showDriverStatSummary :: Show DriverStatSummary where show = genericShow
instance decodeDriverStatSummary :: Decode DriverStatSummary where decode = defaultDecode
instance encodeDriverStatSummary  :: Encode DriverStatSummary where encode = defaultEncode

derive instance genericVariant :: Generic Variant _
instance showVariant :: Show Variant where show = genericShow
instance decodeVariant :: Decode Variant where decode = defaultEnumDecode
instance encodeVariant :: Encode Variant where encode = defaultEncode
instance standardEncodeVariant :: StandardEncode Variant
    where
    standardEncode = defaultEnumEncode