module Beckn.Utils.Defaults where

import           Beckn.Types.Common
import           Data.Time
import           Data.Time.Calendar  (Day (..))
import           Data.Time.Clock
import           Data.Time.LocalTime
import           EulerHS.Prelude


localTime :: LocalTime
localTime = LocalTime (ModifiedJulianDay 58930) (TimeOfDay 1 1 1)

id :: Text
id = "762b65659b10460ca2ce93133b51385c"

email :: Text
email = "default@juspay.in"

orgId :: Text
orgId = "9617486e522f44c6b6d54f6ca0d2731d"

user :: Text
user = "beckn_user"

locationType :: LocationType
locationType = PINCODE

district :: Text
district = "Ernakulam"

city :: Text
city = "Kochi"

state :: Text
state = "Kerala"

country :: Text
country = "India"

pincode :: Int
pincode = 682005
