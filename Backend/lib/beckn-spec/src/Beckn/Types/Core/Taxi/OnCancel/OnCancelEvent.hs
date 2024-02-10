{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnCancel.OnCancelEvent
  ( module Beckn.Types.Core.Taxi.OnCancel.OnCancelEvent,
  )
where

import Beckn.Types.Core.Taxi.OnCancel.OnCancelEvent.BookingCancelledEvent
import Data.OpenApi
import EulerHS.Prelude hiding (fromList)
import qualified Kernel.Utils.JSON as J
import qualified Kernel.Utils.Schema as S

newtype OnCancelEvent
  = BookingCancelled BookingCancelledEvent
  deriving (Generic, Show)

instance ToJSON OnCancelEvent where
  toJSON = genericToJSON J.untaggedValue

instance FromJSON OnCancelEvent where
  parseJSON = genericParseJSON J.untaggedValue

instance ToSchema OnCancelEvent where
  declareNamedSchema = genericDeclareNamedSchema S.untaggedValue
