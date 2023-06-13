{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.Payment
  ( module Beckn.Types.Core.Taxi.Common.Payment,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.PaymentCollector as Reexport
import Beckn.Types.Core.Taxi.Common.PaymentInstrument as Reexport
import Beckn.Types.Core.Taxi.Common.PaymentType as Reexport
import Beckn.Types.Core.Taxi.Common.TimeDuration as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State, (.=))
import Kernel.Utils.JSON

data Payment = Payment
  { collected_by :: PaymentCollector,
    _type :: PaymentType,
    instrument :: Maybe PaymentInstrument, -- FIXME find proper fields
    time :: TimeDuration -- FIXME: what is this?
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
