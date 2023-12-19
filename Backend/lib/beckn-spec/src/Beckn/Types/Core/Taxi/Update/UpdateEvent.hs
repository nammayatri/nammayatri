{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Update.UpdateEvent
  ( module Beckn.Types.Core.Taxi.Update.UpdateEvent,
  )
where

import Beckn.Types.Core.Taxi.Update.UpdateEvent.EditLocationEvent
import Beckn.Types.Core.Taxi.Update.UpdateEvent.PaymentCompletedEvent
import Data.OpenApi
import EulerHS.Prelude
import qualified Kernel.Utils.JSON as J
import qualified Kernel.Utils.Schema as S

data UpdateEventV2
  = PaymentCompletedV2 PaymentCompletedEventV2
  | EditLocationV2 EditLocationEventV2
  deriving (Generic, Show)

instance ToJSON UpdateEventV2 where
  toJSON = genericToJSON J.untaggedValue

instance FromJSON UpdateEventV2 where
  parseJSON = genericParseJSON J.untaggedValue

instance ToSchema UpdateEventV2 where
  declareNamedSchema = genericDeclareNamedSchema S.untaggedValue

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data UpdateEvent
  = PaymentCompleted PaymentCompletedEvent
  | EditLocation EditLocationEvent
  deriving (Generic, Show)

instance ToJSON UpdateEvent where
  toJSON = genericToJSON J.untaggedValue

instance FromJSON UpdateEvent where
  parseJSON = genericParseJSON J.untaggedValue

instance ToSchema UpdateEvent where
  declareNamedSchema = genericDeclareNamedSchema S.untaggedValue
