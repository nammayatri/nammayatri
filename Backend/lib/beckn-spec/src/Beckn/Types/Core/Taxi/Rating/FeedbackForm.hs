{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Rating.FeedbackForm where

import Data.OpenApi
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema

data FeedbackForm = FeedbackForm
  { question :: Text,
    answer :: Maybe Text,
    details :: Maybe [FeedbackChip]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data FeedbackChip
  = DRIVER_WAS_NOT_READY_TO_GO
  | ASKING_FOR_MORE_MONEY
  | AUTO_BROKEN
  | UNPROFESSIONAL_DRIVER
  | RASH_DRIVING
  | DRIVER_CHARGED_MORE
  | UNCOMFORTABLE_AUTO
  | TRIP_GOT_DELAYED
  | FELT_UNSAFE
  | POLITE_DRIVER
  | EXPERT_DRIVING
  | SAFE_RIDE
  | CLEAN_AUTO
  | ON_TIME
  | SKILLED_NAVIGATOR
  | RUDE_DRIVER
  | TOO_MANY_CALLS
  | RECKLESS_DRIVING
  | LATE_DROP_OFF
  | LATE_PICK_UP
  | POOR_EXPERIENCE
  | TERRIBLE_EXPERIENCE
  | NEEDS_IMPROVEMENT
  | ALMOST_PERFECT
  | AMAZING
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, Read)

instance ToSchema FeedbackForm where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
