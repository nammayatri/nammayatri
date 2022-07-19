module Beckn.Types.Core.Taxi.OnSearch.Payment
  ( module Beckn.Types.Core.Taxi.OnSearch.Payment,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.PaymentType as Reexport
import Beckn.Types.Core.Taxi.Common.TimeDuration as Reexport
import Beckn.Utils.JSON
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State, (.=))

data Payment = Payment
  { collected_by :: Text,
    _type :: PaymentType,
    time :: TimeDuration
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
